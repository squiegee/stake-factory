;(enforce-pact-version "3.7")

(namespace (read-msg 'ns))

(module kadena-stake-fungiv2-vesting GOVERNANCE "Kadena Stake Factory Fungiible-v2 Vesting Pool- For creating vesting pools where fungible-v2 are emitted over time and transferred to beneficiary accounts."

;VESTING POOLS EMIT AND DISTRIBUTE FUNGIBLE-V2 TOKEN ALLOCATIONS TO ACCOUNTS DEPENDING ON THEIR WEIGHT IN THE VESTING POOL !
;THERE CAN ONLY EVER BE 100% WEIGHT IN A POOL WHICH EQUALS 100% OF ALLOCATIONS !
;WEIGHTS CAN BE ADJUSTED DURING CREATION TO SPLIT REWARD AMONGST BENEFICIARY ACCOUNTS !
;WEIGHTS ARE BEST SET ONCE! ALL WEIGHTS AFFECT OTHER WEIGHTS !
;ADJUSTING A BENEFICIARYS WEIGHTS AFTER THEY ARE CREATED WILL PAY THE BENEFICIARY ITS CURRENT EMITTED ALLOCATIONS !
;VESTING POOLS CAN ALSO BE DEACTIVATED MANUALLY TO RECOVER ALL PLANNED EMISSIONS AND CLOSE THE POOL PERMANENTLY !
;+ EQUAL WEIGHT VESTING POOL EXAMPLE:
;+ YOU CREATE A VESTING POOL THAT EMITS 10 TOKENS PER DAY AND WANT ALL ACCOUNTS TO RECEIVE AN EQUAL SHARE OF EMISSIONS,
;+ THESE 10 TOKENS PER DAY ARE SPLIT AMONGST ALL ACCOUNTS IN THE VESTING POOL DEPENDING ON THEIR WEIGHT IN THE POOL:
;+ USER#1 WITH A WEIGHT OF 100.0 IS ADDED TO POOL AND NOW RECIEVES ALL 10 TOKENS PER DAY.
;+ NEXT USER#2 WITH A WEIGHT OF 100.0 IS ADDED TO THE POOL AND BOTH USERS #1 & #2 NOW RECEIVE 5 TOKENS PER DAY.
;+ FINALLY, USER#3 WITH A WEIGHT OF 100.0 IS ADDED TO THE POOL AND NOW ALL 3 USERS RECEIVE 3.33 TOKENS PER DAY

    ;;;;; CONSTANTS
    (defconst KDS_BANK:string "kadena-stake-fungiv2-vesting-bank")

    (defconst SECONDS_IN_YEAR 31536000)

    (defconst ACCOUNT_ID_CHARSET CHARSET_LATIN1
    " Allowed character set for Account IDs. ")

    (defconst ACCOUNT_ID_MIN_LENGTH 3
      " Minimum character length for account IDs. ")

    (defconst ACCOUNT_ID_MAX_LENGTH 256
      " Maximum character length for account IDs. ")

    (defconst NAME_MIN_LENGTH 3
      " Minimum character length for account IDs. ")

    (defconst NAME_MAX_LENGTH 30
      " Maximum character length for account IDs. ")

    ;Warning - This is an ambiguous module guard
    ;(defun kadena-stake-vault-guard:guard () (create-module-guard "kadena-stake-holdings"))

    ;;;;; CAPABILITIES

    (defcap GOVERNANCE ()
      @doc " Give the admin full access to call and upgrade the module. "
      (enforce-keyset 'admin-kadena-stake)
    )

    (defcap ACCOUNT_GUARD(account:string)
        @doc "Verifies account meets format and belongs to caller"
        ;(enforce (= "k:" (take 2 account)) "For security only support k accounts")
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defcap POOL_CREATOR_GUARD(pool-id:string)
        @doc "Verifies account belongs to pool creator"
        (let
                (
                    (pool-data (read pools pool-id ["account"]))
                )
                (enforce-guard
                    (at "guard" (coin.details (at "account" pool-data) ))
                )
        )

    )

    (defcap PRIVATE ()
        @doc "Only to be called with private contexts"
        true
    )

    (defcap UPDATE ()
      @doc " Capability to perform UPDATEing operations. "
      true
    )

    ;;;;;;;;;; SCHEMAS AND TABLES ;;;;;;;;;;;;;;
    (defschema pools-schema
        @doc "Pool information, a unique id is the key"
        id:string
        name:string
        balance:decimal
        reward-token:module{fungible-v2}
        account:string
        active:bool
        max-reward-per-account:decimal
        claim-wait-seconds:decimal
        max-reward-per-claim:decimal
        start-time:time
        reward-duration:decimal
        reward-amount:decimal
        stakers:decimal
        withdraw-duration:decimal
        start-balance:decimal
        initialized:bool
        end-time:time
    )

    (defschema pools-usage-schema
        @doc "Pool usage data"
        tokens-locked:decimal
        last-updated:time
        paid:decimal
        multiplier:decimal
        next-multiplier:decimal
    )

    (defschema pools-user-stats-schema
      @doc "User total reward earnings in a pool"
      total-earned:decimal)


    (defschema stakes-schema
        @doc "Stores beneficiary account information, key is account + pool name"
        id:string
        pool-id:string
        balance:decimal
        last-updated:time
        account:string
        rewards:decimal
        last-claimed:time
        last-withdraw:time
        multiplier:decimal
        start-time:time
    )

    (deftable pools:{pools-schema})
    (deftable pools-usage:{pools-usage-schema})
    (deftable pool-user-stats:{pools-user-stats-schema})
    (deftable stakes:{stakes-schema})

    ;;;;;;;;; CODE THAT NEEDS PERMISSIONS / CAPABILITIES ;;;;;;;;;;;;;;;

    ;;;;; Pool Creator Related

    ;create pool: for creating vesting pool where fungible-v2 tokens are emitted to beneficiary account names
    ;id: id of pool, ex: "test-pool"
    ;name: name of pool, ex: "Test Pool"
    ;balance: amount of distribution token given by pool owner to be distributed to beneficiary accounts, ex: 200
    ;reward-token: name of fungible-v2 reward token provided by pool creator for emissions, ex: coin
    ;account: pool creator account, ex: k:mykaccount
    ;max-reward-per-account: max rewards a beneficiary account can ever claim in the pool, ex: 200
    ;claim-wait-seconds: minimum number of seconds between beneficary allocation claims, ex: 0
    ;max-reward-per-claim: max rewards a beneficiary account can claim per wait duration, ex: 200
    ;reward-duration: time it takes for reward amount to become 100% emitted and available to beneficiarys, ex: 86400
    ;reward-amount: the amount of rewards available each reward-duration, ex: 10

    (defun create-pool (id:string
                        name:string
                        balance:decimal
                        reward-token:module{fungible-v2}
                        account:string
                        max-reward-per-account:decimal
                        claim-wait-seconds:decimal
                        max-reward-per-claim:decimal
                        reward-duration:decimal
                        reward-amount:decimal )

        @doc "Creates a new pool where Stakers stake and earn fungible-v2 tokens"
        (with-capability (ACCOUNT_GUARD account)
            ;Enforce rules
            (enforce-pool-id id)
            (enforce-valid-id account)
            (enforce-valid-id id)
            (enforce-valid-name name)
            (enforce-unit balance (reward-token::precision))
            ;Create reward token account
            (reward-token::create-account id (create-module-guard "kadena-stake-holdings"))
            ;Transfer reward token
            (reward-token::transfer-create account id (create-module-guard "kadena-stake-holdings") balance)
            ;Insert pool
            (insert pools id
                {
                    "id": id,
                    "name": name,
                    "balance": balance,
                    "reward-token": reward-token,
                    "account": account,
                    "active": true,
                    "max-reward-per-account": max-reward-per-account,
                    "claim-wait-seconds": claim-wait-seconds,
                    "max-reward-per-claim": max-reward-per-claim,
                    "start-time": (at "block-time" (chain-data)),
                    "reward-duration": reward-duration,
                    "reward-amount": reward-amount,
                    "stakers": 0.0,
                    "withdraw-duration": claim-wait-seconds,
                    "start-balance": balance,
                    "initialized": false,
                    "end-time": (at "block-time" (chain-data))
                }
            )
            ;Insert pool blank usage
            (insert pools-usage id {
                "tokens-locked": 0.0,
                "last-updated": (at "block-time" (chain-data)),
                "paid": 0.0,
                "multiplier": 1.0,
                "next-multiplier": 1.0
            })
            ;Return a message
            (format "Created pool {} with {} {}" [name balance reward-token])
        )
    )

    (defun add-balance-to-pool (pool-id:string account:string amount:decimal)
        @doc "Adds more distribution token to a pool for emissions"
        (let
                (
                    (pool-data (read pools pool-id ["account" "reward-token" "balance" "start-balance" "active"]))
                )
                (let
                    (
                        (token:module{fungible-v2} (at "reward-token" pool-data))
                    )
                    ;Enforce active pool
                    (enforce (= (at "active" pool-data) true) "You cannot add rewards to exhausted pools.")
                    ;Enforce token precision
                    (enforce-unit amount (token::precision))
                    ;Transfer token to pool
                    (token::transfer account pool-id amount)
                    ;Update pool
                    (update pools pool-id
                          {
                              "balance": (+ (at "balance" pool-data) amount),
                              "start-balance": (+ (at "start-balance" pool-data) amount),
                              "end-time": (calculate-pool-end-time pool-id false true amount)
                          }
                    )
                    (format "Added {} {} distribution tokens to {}" [amount token pool-id])
                )
        )
    )

    (defun deactivate-pool (pool-id:string account:string)
        @doc "Deactivates a pool and withdraws all distribution tokens back to pool creator account"
        (with-capability (POOL_CREATOR_GUARD pool-id )
        (let
            (
                (pool-data (read pools pool-id))
                (pool-usage-data (read pools-usage pool-id))
            )
            (let
                (
                    (token:module{fungible-v2} (at "reward-token" pool-data))
                )
                (let
                    (
                       (to-pay (token::get-balance pool-id) )
                    )
                    ;Enforce pool owner
                    (enforce (= (at "account" pool-data) account) "Access prohibited.")
                    ;Transfer pool starting stake
                    (install-capability (token::TRANSFER pool-id account to-pay) )
                    (token::transfer pool-id account to-pay)
                    ;Update pool
                    (update pools pool-id
                      (+
                          {
                              "active": false,
                              "balance": (- (at "balance" pool-data) to-pay)
                          }
                          pool-data
                      )
                    )
                    ;Update pool usage
                    (update pools-usage pool-id
                      (+
                          {
                              "last-updated": (at "block-time" (chain-data))
                          }
                          pool-usage-data
                      )
                    )
                    ;Return a message
                    (format "Returned {} {} and deactivated pool {}" [to-pay token pool-id])
                )
              )
        )
        )
    )

    ;;;;; User Related

    (defun create-stake (pool-id:string account:string amount:decimal)
        @doc " Creates or adds a distribution allocation to a beneficiary account, transfering the accounts current allocations before so if there are any"
        (with-capability (POOL_CREATOR_GUARD pool-id )
            (let
                (
                    (pool-data (read pools pool-id))
                    (stake-id (get-stake-id-key account pool-id))
                )
                (if (and (= (at "active" pool-data) true) (> (at "balance" pool-data) 0.0) )
                  (let
                      (
                          (pool-usage-data (read pools-usage pool-id))
                      )
                      ;Claim rewards for this user
                      (claim pool-id account)
                      ;Enforce active pool
                      (enforce (= (at "active" pool-data) true) "Staking pool is not active.")
                      ;Insert stake data
                      (with-default-read stakes stake-id
                        { "id" : stake-id, "pool-id" : pool-id, "balance" : 0.0, "last-updated" : (at "block-time" (chain-data)), "account" : account, "rewards" : 0.0, "last-claimed" : (at "block-time" (chain-data)), "last-withdraw": (at "block-time" (chain-data))  }
                        { "id" := t_id, "pool-id" := t_pool-id, "balance" := t_balance, "last-updated" := t_last-updated, "account" := t_account, "rewards" := t_rewards, "last-claimed" := t_last-claimed, "last-withdraw" := t_last-withdraw }

                        ;If this is not the first beneficiary, update the pools emissions multiplier
                        (if (> (at "stakers" pool-data) 0.0)
                          (update pools-usage pool-id
                            {
                                ;BELOW WE CALCULATE THE NEW MULTIPLIER FOR EVERY STAKER AFTER THE FIRST
                                ;NEW MULTIPLIER = CURERNT MULTIPLIER + (TOKENS-EMITTED-PER-DAY / (TOTAL STAKED TOKENS))
                                "last-updated": (at "block-time" (chain-data)),
                                "multiplier": (calculate-multiplier pool-id )
                            }
                        )
                          true
                        )

                        ;Update user stake information
                        (write stakes stake-id {
                            "id": t_id,
                            "pool-id": t_pool-id,
                            "balance": (+ t_balance amount),
                            "last-updated": (at "block-time" (chain-data)),
                            "account": t_account,
                            "rewards": t_rewards,
                            "last-claimed": t_last-claimed,
                            "last-withdraw": t_last-withdraw, ;If this is not the first beneficiary, calculate a new multiplier
                            "multiplier": (if (> (at "stakers" pool-data) 0.0)
                                            (calculate-multiplier pool-id)
                                            (at "multiplier" pool-usage-data)
                                          ),
                            "start-time": (at "block-time" (chain-data))
                        })

                        ;Update pool usage data
                        (update pools-usage pool-id
                            {
                                "tokens-locked": (+ (at "tokens-locked" pool-usage-data) amount),
                                "last-updated": (if (= (at "active" (read pools pool-id)) true ) (at "block-time" (chain-data)) (at "last-updated" pool-usage-data)  )
                            }
                        )

                        ;If this pool is not initialized or if all beneficiarys left, set it's start-time + end-time now
                        (if (or (= (at "initialized" pool-data) false) (= (at "stakers" (read pools pool-id)) 0.0 ) )
                          (update pools pool-id
                            {
                              "initialized": true,
                              "start-time": (if (> (at "stakers" pool-data) 0.0)
                                                (at "start-time" pool-data)
                                                (at "block-time" (chain-data))
                                              ),
                              "end-time": (if (= (at "stakers" (read pools pool-id)) 0.0 ) (calculate-pool-end-time pool-id false false 0.0)  (calculate-pool-end-time pool-id true false 0.0) )
                            }
                          )
                          true
                        )

                        ;If this pool has no emissions to distribute then we stop and deactivate the pool
                        (let
                            (
                                (time-since-end-time (diff-time  (at "block-time" (chain-data)) (at "end-time" (read pools pool-id)) ))
                            )
                            (if (>= time-since-end-time 0.0 )
                            ;If pool can not pay this new beneficiary
                              (let
                                  (
                                      (EXHAUSTED true)
                                  )
                                  (update pools pool-id
                                    {
                                        "active": false
                                    }
                                  )
                                  (format "Pool {} is exhausted and can not distribute allocations any further." [pool-id])
                              )
                            ;If pool can pay new beneficiary
                              (let
                                  (
                                      (ALLOCATION (* (at "reward-amount" pool-data) (/ (at "balance"(read stakes stake-id)) (at "tokens-locked" (read pools-usage pool-id) )  ) )  )
                                  )
                                  ;If this beneficiarys balance is 0, this beneficiary is new, and we add +1 staker count to the pools data and set the pool start time:
                                  (if (= t_balance 0.0)
                                    (update pools pool-id
                                      {
                                        "stakers": (+ (at "stakers" pool-data) 1.0)
                                      }
                                    )
                                    true
                                  )
                                  (format "Allocated {}% of emissions to {} for {} {} every {} seconds" [(* (/ (at "balance"(read stakes stake-id)) (at "tokens-locked" (read pools-usage pool-id) )  ) 100) account ALLOCATION (at "reward-token" pool-data) (at "reward-duration" pool-data)  ])
                              )
                            )
                        )

                      )
                 )
                 (format "The vesting pool {} is deactivated." [pool-id]))
            )
        )
    )

    (defun claim (pool-id:string account:string)
        @doc "Transfers a beneficiarys current allocation or deletes a beneficiarys allocations from a pool"
        (with-capability (ACCOUNT_GUARD account)
        (with-default-read stakes (get-stake-id-key account pool-id)
          { "account" : 'nonulls, "balance" : 0.0 }
          { "account" := t_account, "balance" := t_balance }
          (if (and (= account t_account) (> t_balance 0.0) )
            (let
                  (
                      (stake-id (get-stake-id-key account pool-id))
                      (pool-data (read pools pool-id))
                      (pool-usage-data (read pools-usage pool-id))
                  )
                  (if (> (at "stakers" pool-data) 0.0)
                  (let
                      (
                        (stake (read stakes stake-id))
                        (reward-token:module{fungible-v2} (at "reward-token" pool-data))
                        (to-pay-max (calculate-rewards pool-id account) )
                        (available (at "balance" pool-data))
                      )
                      (let
                          (
                            (to-pay (if (>= (- available to-pay-max ) 0.0)
                                      (if (> to-pay-max (at "max-reward-per-claim" pool-data))
                                        (at "max-reward-per-claim" pool-data)
                                        to-pay-max
                                      )
                                      (if (> (at "balance" pool-data) (at "max-reward-per-claim" pool-data))
                                        (at "max-reward-per-claim" pool-data)
                                        (at "balance" pool-data)
                                      )
                                    )
                            )
                          )
                          ;Enforce account
                          (enforce (= account (at "account" stake)) "Authorization failed")
                          ;Enforce balance
                          (enforce (>= to-pay 0.0) "There are no allocations to claim for this account.")
                          ;Enforce claim allocation duration
                          (if (= (at "active" pool-data) false) true (enforce (> (diff-time (at "block-time" (chain-data)) (at 'last-claimed stake)) (at "claim-wait-seconds" pool-data) ) "This account must wait the full claim allocation duration before it can claim it's allocations.") )

                          (let
                                (
                                  (payALLOCATION true)
                                )

                                ;Compose allocation transfer
                                (if (> to-pay 0.0)
                                  (if (>= (- (at "balance" pool-data) to-pay ) 0.0)
                                    (install-capability (reward-token::TRANSFER pool-id account to-pay))
                                    (install-capability (reward-token::TRANSFER pool-id account (at "balance" pool-data)))
                                  )
                                true)
                                ;Transfer back the allocation amount composed above
                                (if (> to-pay 0.0)
                                  (if (>= (- (at "balance" pool-data) to-pay ) 0.0)
                                    (reward-token::transfer pool-id account to-pay)
                                    (reward-token::transfer pool-id account (at "balance" pool-data) )
                                  )
                                true)
                          )

                          ;Update pool usage data
                          (update pools-usage pool-id
                                  {
                                      "last-updated": (at "block-time" (chain-data)),
                                      "paid": (+ (at "paid" pool-usage-data) to-pay) ,
                                      "multiplier": (calculate-multiplier pool-id)
                                  }
                          )

                          ;Update pool data
                          (update pools pool-id
                            {
                                "balance": (- (at "balance" pool-data) to-pay),
                                "active": (if (<= (- (at "balance" pool-data) to-pay) 0.0) false (at "active" pool-data) )
                            }
                          )

                          ;Update user stake data
                          (update stakes stake-id
                            {
                              "last-updated":  (at "block-time" (chain-data)),
                              "rewards": (- to-pay-max to-pay),
                              "last-claimed":  (at "block-time" (chain-data)),
                              "multiplier": (at "multiplier" (read pools-usage pool-id)),
                              "last-withdraw": (at "block-time" (chain-data))
                              }
                          )

                          ;Return message
                          (format "Awarded {} with {} {}" [account to-pay reward-token])


                      )
                  )
                (format "{} has no token emissions to claim in {}" [account pool-id]))
                )
          (format "{} has no token emissions to claim in {}" [account pool-id]))
        )

        )
    )

    (defun remove-account (pool-id:string account:string pay-out:bool)
        @doc "Deletes a beneficiarys account from a pool"
        (with-capability (POOL_CREATOR_GUARD pool-id)
        (with-default-read stakes (get-stake-id-key account pool-id)
          { "account" : 'nonulls, "balance" : 0.0 }
          { "account" := t_account, "balance" := t_balance }
          (if (and (= account t_account) (> t_balance 0.0) )
            (let
                  (
                      (stake-id (get-stake-id-key account pool-id))
                      (pool-data (read pools pool-id))
                      (pool-usage-data (read pools-usage pool-id))
                  )
                  (if (> (at "stakers" pool-data) 0.0)
                  (let
                      (
                        (stake (read stakes stake-id))
                        (reward-token:module{fungible-v2} (at "reward-token" pool-data))
                        (to-pay-max (calculate-rewards pool-id account) )
                        (available (at "balance" pool-data))
                      )
                      (let
                          (
                            (to-pay (if (>= (- available to-pay-max ) 0.0)
                                      (if (> to-pay-max (at "max-reward-per-claim" pool-data))
                                        (at "max-reward-per-claim" pool-data)
                                        to-pay-max
                                      )
                                      (if (> (at "balance" pool-data) (at "max-reward-per-claim" pool-data))
                                        (at "max-reward-per-claim" pool-data)
                                        (at "balance" pool-data)
                                      )
                                    )
                            )
                            (to-pay-stake (at "balance" stake))
                          )
                          ;Enforce balance
                          (enforce (>= to-pay 0.0) "There are no allocations to claim for this account.")

                          ;Pay the account we are deleting, or dont and recycle its funds back into the pool
                          (if (= pay-out true)

                          ;If paying the deleted account:
                            (let
                                (
                                  (payALLOCATION true)
                                )
                                ;Compose allocation transfer
                                (if (> to-pay 0.0)
                                  (if (>= (- (at "balance" pool-data) to-pay ) 0.0)
                                    (install-capability (reward-token::TRANSFER pool-id account to-pay))
                                    (install-capability (reward-token::TRANSFER pool-id account (at "balance" pool-data)))
                                  )
                                true)
                                ;Transfer back the allocation amount composed above
                                (if (> to-pay 0.0)
                                  (if (>= (- (at "balance" pool-data) to-pay ) 0.0)
                                    (reward-token::transfer pool-id account to-pay)
                                    (reward-token::transfer pool-id account (at "balance" pool-data) )
                                  )
                                true)
                            )

                          ;If not paying the deleted account (recycle the funds)
                            (update pools pool-id
                                  {
                                      "end-time": (calculate-pool-end-time pool-id false true to-pay)
                                  }
                            )
                          )

                          ;Update pool usage data
                          (update pools-usage pool-id
                                  {
                                      "last-updated": (at "block-time" (chain-data)),
                                      "tokens-locked": (- (at "tokens-locked" pool-usage-data) to-pay-stake),
                                      "paid": (if (= pay-out false) (at "paid" pool-usage-data) (+ (at "paid" pool-usage-data) to-pay) ) ,
                                      "multiplier": (calculate-multiplier pool-id)
                                  }
                          )

                          ;Update pool data
                          (update pools pool-id
                            {
                                "balance": (if (= pay-out false) (at "balance" pool-data) (- (at "balance" pool-data) to-pay) ),
                                "active": (if (<= (- (at "balance" pool-data) to-pay) 0.0) false (at "active" pool-data) ),
                                "stakers": (if (> to-pay-stake 0.0) (- (at "stakers" pool-data) 1.0) (at "stakers" pool-data) )
                            }
                          )

                          ;Update user data
                          (update stakes stake-id
                            {
                              "last-updated":  (at "block-time" (chain-data)),
                              "rewards": (- to-pay-max to-pay),
                              "balance": (-(at "balance" stake) to-pay-stake),
                              "last-claimed":  (at "block-time" (chain-data)),
                              "multiplier": (at "multiplier" (read pools-usage pool-id)),
                              "last-withdraw": (if (= pay-out true) (at "block-time" (chain-data)) (at "last-withdraw" stake) )
                              }
                          )

                          ;Return message
                          (if (= pay-out true)
                            (format "Awarded {} with {} {} and deleted the account" [account to-pay reward-token])
                            (format "Removed all token emission allocations for account {}" [account])
                          )


                      )
                  )
                (format "{} has no token emissions to claim in {}" [account pool-id]))
                )
          (format "{} has no token emissions to claim in {}" [account pool-id]))
        )

        )
    )


    ;;///////////////////////
    ;;UTILITIES / GETTERS
    ;;//////////////////////

    (defun calculate-multiplier (pool-id:string)
      @doc " Calculates a pools current multiplier "
      (let
              (

                  (pool-data (read pools pool-id ["reward-amount" "reward-duration" "end-time"]))
                  (pool-usage-data (read pools-usage pool-id ["last-updated" "multiplier" "tokens-locked"]))
              )
              (let
                  (
                    (time-since-end-time (diff-time  (at "block-time" (chain-data)) (at "end-time" (read pools pool-id))))
                  )
                  (let
                      (
                        (days-since-last-update (floor (/ (diff-time  (at "block-time" (chain-data)) (at "last-updated" (read pools-usage pool-id))) (at "reward-duration" pool-data)) 0)  )
                        (days-since-end-time (floor (/ (diff-time (at "block-time" (chain-data))  (at "end-time" (read pools pool-id))) (at "reward-duration" pool-data)) 0)  )
                      )
                      (let
                          (
                            (days-since (- days-since-last-update days-since-end-time) )
                          )
                          (let
                              (
                                (multiplier (+ (at "multiplier" pool-usage-data) (/ (* (min days-since-last-update (abs days-since)) (at "reward-amount" pool-data) ) (at "tokens-locked" (read pools-usage pool-id))  ) ))


                              )
                              multiplier
                          )
                      )
                  )
              )
      )
    )

    (defun calculate-rewards (pool-id:string account:string)
      @doc " Calculates a pools rewards for a user "
      (let
              (
                  (pool-data (read pools pool-id ["reward-token"]))
                  (stake-id (get-stake-id-key account pool-id))
              )
              (let
                (
                  (stake (read stakes stake-id ["balance" "multiplier"]))
                  (token:module{fungible-v2} (at "reward-token" pool-data))
                )
                (floor (* (at "balance" stake) (- (calculate-multiplier pool-id) (at "multiplier" stake) ) ) (token::precision))
              )
      )
    )

    (defun calculate-total-emitted-tokens (pool-id:string)
        @doc " Calculates the number of tokens emitted by a pool since it began "
        (let
            (
                (pool-data (read pools pool-id ["reward-duration" "reward-amount" "reward-token" "start-time" "start-balance"]))
            )
            (let
                (
                    (token:module{fungible-v2} (at "reward-token" pool-data))
                    (time-passed (diff-time  (at "block-time" (chain-data)) (at "start-time" pool-data) ) )
                )
                ;We clamp our emission calculation under the pool's available balance so we don't exceed it
                (let
                    (
                        (total-available (floor (* (/ time-passed (at "reward-duration" pool-data)) (at "reward-amount" pool-data)) (token::precision) ))
                        (max-available (at "start-balance" pool-data))
                    )
                    (if (<= total-available max-available ) total-available max-available)
                )
            )
        )
    )

    (defun calculate-pool-end-time (pool-id:string do-start-balance:bool do-add-balance:bool amount-to-add:decimal)
        @doc " Calculates the time at which a pool has emitted all rewards "
        (let
            (
                (pool-data (read pools pool-id ["reward-duration" "reward-amount" "start-balance" "balance" "end-time"]))
            )
            (let
                (
                    (number-of-reward-durations-add-balance (/ amount-to-add (at "reward-amount" pool-data) ))
                    (number-of-reward-durations-balance-end-time (/ (at "balance" pool-data) (at "reward-amount" pool-data) ))
                    (number-of-reward-durations-end-time (/ (at "start-balance" pool-data) (at "reward-amount" pool-data) ))

                )
                (let
                    (
                        (number-of-reward-durations (if (= do-add-balance true)
                                                      number-of-reward-durations-add-balance
                                                      (if (= do-start-balance true)
                                                        number-of-reward-durations-end-time
                                                        number-of-reward-durations-balance-end-time
                                                      )
                                                    )
                        )
                    )
                    (let
                        (
                            (time-until-exhausted (* (at "reward-duration" pool-data) number-of-reward-durations) )
                        )
                        (let
                            (
                                (end-time-add-balance (add-time (at "end-time" pool-data) time-until-exhausted))
                                (end-time-other (add-time (at "block-time" (chain-data)) time-until-exhausted))
                            )
                            (let
                                (

                                    (end-time (if (= do-add-balance true)
                                                              end-time-add-balance
                                                              (if (= do-start-balance true)
                                                                end-time-other
                                                                end-time-other
                                                              )
                                                            )
                                    )
                                )
                                end-time
                            )
                      )
                    )
                )
            )
        )
    )



;///////////////////////////
;OLD UTILITIES
;//////////////////////////


    (defun apy-for-ratio-of-seconds-in-year (ratio:decimal apy:decimal)
        (* ratio (/ apy 100))
    )

    (defun seconds-over-seconds-in-year (time-passed:decimal)
        @doc "Given seconds, calculate the % of a total year those seconds made up"
        ( / time-passed SECONDS_IN_YEAR)
    )

    (defun get-stake-id-key ( account:string pool-id:string )
      @doc " Returns id/account data structure "
      (format "{}:{}" [account pool-id])
    )

    (defun enforce-pool-id ( id:string )
      @doc " Enforces a unique pool-id "
      (with-default-read pools id
          { 'id: 'nonulls }
          { 'id := id }
          (enforce (= 'nonulls id) "This ID already exists.")
      )
    )

    (defun enforce-stake-id ( id:string )
      @doc " Enforces a unique stake-id "
      (with-default-read stakes id
          { 'id: 'nonulls }
          { 'id := id }
          (enforce (= 'nonulls id) "This ID already exists.")
      )
    )

    (defun get-pools ()
      (keys pools)
    )

    (defun get-pool-info (pool-id:string)
      (+ (read pools pool-id) (read pools-usage pool-id))
    )

    (defun get-user-stakes (pool-id:string account:string)
      (read stakes (get-stake-id-key account pool-id))
    )

    (defun get-user-pools ( account:string )
      @doc " Get a list of pool IDs that a user is staking in "
        (select stakes ['pool-id]
          (and? (where 'account (= account))
            (where 'balance (< 0.0))))
    )

    (defun get-user-created-pools ( account:string )
      @doc " Get a list of pool IDs that a user has created "
        (select pools (where 'account (= account)))
    )

    (defun enforce-unit:bool (amount:decimal precision)
      @doc " Enforces precision "
      (enforce
        (= (floor amount precision)
           amount)
        "Minimum denomination exceeded.")
    )

   (defun enforce-valid-id ( id:string )
    @doc " Enforce that an account ID meets charset and length requirements. "
    (enforce
      (is-charset ACCOUNT_ID_CHARSET id)
      (format
        "Account ID does not conform to the required charset: {}"
        [id]))
    (let ((accountLength (length id)))
      (enforce
        (>= accountLength ACCOUNT_ID_MIN_LENGTH)
        (format
          "Account ID does not conform to the min length requirement: {}"
          [id]))
      (enforce
        (<= accountLength ACCOUNT_ID_MAX_LENGTH)
        (format
          "Account ID does not conform to the max length requirement: {}"
          [id]))))

  (defun enforce-valid-name ( name:string )
    @doc " Enforce that a Pool Name meets charset and length requirements. "
    (enforce
      (is-charset ACCOUNT_ID_CHARSET name)
      (format
        "Pool Name does not conform to the required charset: {}"
        [name]))
    (let ((nameLength (length name)))
      (enforce
        (>= nameLength NAME_MIN_LENGTH)
        (format
          "Pool Name does not conform to the min length requirement: {}"
          [name]))
      (enforce
        (<= nameLength NAME_MAX_LENGTH)
        (format
          "Pool Name does not conform to the max length requirement: {}"
          [name]))))

  (defun get-pair-key
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    " Create canonical key for lp pair staking."
    (format "{}:{}" (canonicalize tokenA tokenB))
  )

  (defun canonicalize:[module{fungible-v2}]
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (if (is-canonical tokenA tokenB) [tokenA tokenB] [tokenB tokenA])
  )

  (defun is-canonical
    ( tokenA:module{fungible-v2}
      tokenB:module{fungible-v2}
    )
    (< (format "{}" [tokenA]) (format "{}" [tokenB]))
  )

  (defun min
    ( num1
      num2
    )
    (if (is-min num1 num2) num1 num2)
  )

  (defun is-min
    ( num1
      num2
    )
    (< num1 num2)
  )

  (defun get-token-key
    ( tokenA:module{fungible-v2} )
    " Create key from token module"
    (format "{}" [tokenA])
  )

  (defun initialize ()
    @doc " Initialize the contract. Can only happen once. "
    (coin.create-account KDS_BANK (create-module-guard "kadena-stake-holdings"))
  )

)

;(create-table free.kadena-stake-fungiv2-vesting.pools)
;(create-table free.kadena-stake-fungiv2-vesting.pools-usage)
;(create-table free.kadena-stake-fungiv2-vesting.pool-user-stats)
;(create-table free.kadena-stake-fungiv2-vesting.stakes)
;(free.kadena-stake-fungiv2-vesting.initialize)
