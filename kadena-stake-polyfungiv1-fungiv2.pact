;(enforce-pact-version "3.7")

(namespace (read-msg 'ns))

(module kadena-stake-fungiv2 GOVERNANCE "Kadena Stake Factory Fungiible-v2 - For creating pools where fungible-v2 tokens are staked for fungible-v2 token rewards"

    ;;;;; CONSTANTS
    (defconst KDS_BANK:string "kadena-stake-fungiv2-bank")

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
        (enforce (= "k:" (take 2 account)) "For security only support k accounts")
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

    (defcap PRIVATE_RESERVE
          (id:string pair-key:string)
        true)

    (defun enforce-private-reserve:bool
        (id:string pair-key:string)
      (require-capability (PRIVATE_RESERVE id pair-key)))

    (defun create-pool-guard:guard
        (id:string pair-key:string)
      (create-user-guard (enforce-private-reserve id pair-key)))

    ;;;;;;;;;; SCHEMAS AND TABLES ;;;;;;;;;;;;;;
    (defschema pools-schema
        @doc "Pool information, a unique id is the key"
        id:string
        name:string
        balance:decimal
        reward-token:module{poly-fungible-v1}
        reward-token-id:string
        stake-token:module{fungible-v2}
        account:string
        active:bool
        claim-wait-seconds:decimal
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
        has-vesting-connection:bool
        vesting-pool-id:string
    )

    (defschema pools-user-stats-schema
      @doc "User total reward earnings in a pool"
      total-earned:decimal)


    (defschema stakes-schema
        @doc "Stores staking information for users, key is account + pool name"
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

    ;create pool: for creating staking pool of reward token/stake token type fungi-v2/fungi-v2
    ;id: id of pool, ex: "test-pool"
    ;name: name of pool, ex: "Test Pool"
    ;balance: amount of reward token given by pool owner to be distributed to stakers, ex: 200
    ;reward-token: name of fungible-v2 reward token provided by pool creator, ex: coin
    ;stake-token: name of fungible-v2 token stakers must stake for reward token, ex: coin
    ;account: pool creator account, ex: k:mykaccount
    ;claim-wait-seconds: minimum number of seconds between staker reward claims, ex: 0
    ;reward-duration: time it takes to distribute reward-amount to stakers, ex: 86400.0
    ;reward-amount: the amount of rewards available each reward-duration, ex: 10
    ;withdraw-duration: time in seconds a staker's tokens must be locked before a staker can withdraw

    (defun create-pool (id:string
                        name:string
                        balance:decimal
                        reward-token-id:string
                        reward-token:module{poly-fungible-v1}
                        stake-token:module{fungible-v2}
                        account:string
                        claim-wait-seconds:decimal
                        reward-duration:decimal
                        reward-amount:decimal
                        withdraw-duration:decimal )

        @doc "Creates a new pool where Stakers stake and earn fungible-v2 tokens"
        (with-capability (ACCOUNT_GUARD account)
            ;Enforce rules
            (enforce-pool-id id)
            (enforce-valid-id account)
            (enforce-valid-id id)
            (enforce-valid-name name)
            (enforce-unit balance (reward-token::precision reward-token-id))
            ;Create reward token account
            (reward-token::create-account reward-token-id id (create-pool-guard id (get-polyfungiv1-key reward-token)))
            ;Transfer reward token
            (reward-token::transfer-create reward-token-id account id (create-pool-guard id (get-polyfungiv1-key reward-token)) balance)
            ;If reward-token != stake-token then create a token account for stake-token too
            (stake-token::create-account id (create-pool-guard id (get-token-key stake-token)))
            ;Insert pool
            (insert pools id
                {
                    "id": id,
                    "name": name,
                    "balance": balance,
                    "reward-token": reward-token,
                    "reward-token-id": reward-token-id,
                    "stake-token": stake-token,
                    "account": account,
                    "active": true,
                    "claim-wait-seconds": claim-wait-seconds,
                    "start-time": (at "block-time" (chain-data)),
                    "reward-duration": reward-duration,
                    "reward-amount": reward-amount,
                    "stakers": 0.0,
                    "withdraw-duration": withdraw-duration,
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
                "next-multiplier": 1.0,
                "has-vesting-connection": false,
                "vesting-pool-id": "none"
            })
            ;Return a message
            (format "Created pool {} with {} {}" [name balance reward-token])
        )
    )

    (defun add-balance-to-pool (pool-id:string account:string amount:decimal)
        @doc "Adds more balance to a pool for reward distribution and reactivates a pool - Pool creator only"
        (let
                (
                    (pool-data (read pools pool-id ["account" "reward-token" "balance" "start-balance" "active" "reward-token-id"]))
                )
                (let
                    (
                        (token:module{poly-fungible-v1} (at "reward-token" pool-data))
                    )
                    ;Enforce active pool
                    (enforce (= (at "active" pool-data) true) "You cannot add rewards to exhausted pools.")
                    ;Enforce token precision
                    (enforce-unit amount (token::precision))
                    ;Transfer token to pool
                    (token::transfer (at "reward-token-id" pool-data) account pool-id amount)
                    ;Update pool
                    (update pools pool-id
                          {
                              "balance": (+ (at "balance" pool-data) amount),
                              "start-balance": (+ (at "start-balance" pool-data) amount),
                              "end-time": (calculate-pool-end-time pool-id false true amount)
                          }
                    )
                    (format "Added {} {} rewards to {}" [amount token pool-id])
                )
        )
    )

    (defun absorb-new-tokens (pool-id:string)
        @doc "Absorbs new tokens from a pools account into the pools distribution schedule"
        (let
                (
                    (pool-data (read pools pool-id ["account" "reward-token" "stake-token" "balance" "start-balance" "active" "reward-token-id"]))
                    (pool-usage-data (read pools-usage pool-id ["tokens-locked"] ))
                )
                (let
                    (
                        (reward-token:module{poly-fungible-v1} (at "reward-token" pool-data))
                        (stake-token:module{fungible-v2} (at "stake-token" pool-data))
                    )
                    (let
                        (
                            (new-tokens (- (reward-token::get-balance (at "reward-token-id" pool-data) pool-id) (at "balance" pool-data) )
                            )
                        )
                        ;Enforce active pool
                        (enforce (= (at "active" pool-data) true) "You cannot add rewards to exhausted pools.")
                        ;Update pool
                        (update pools pool-id
                              {
                                  "balance": (+ (at "balance" pool-data) new-tokens),
                                  "start-balance": (+ (at "start-balance" pool-data) new-tokens),
                                  "end-time": (calculate-pool-end-time pool-id false true new-tokens)
                              }
                        )
                        ;(format "new-tokens: {}" [new-tokens])
                    )

                )
        )
    )

    (defun migrate-vesting-allocations (pool-id:string)
        @doc "Migrates vesting pool allocations"
        (let
                (
                    (pool-data (read pools pool-id))
                    (pool-usage-data (read pools-usage pool-id))
                )
                (test.kadena-stake-fungiv2-vesting.claim (at "vesting-pool-id" pool-usage-data) pool-id)
        )
    )

    (defun connect-vesting-pool (pool-id:string vesting-pool-id:string set-active:bool)
        @doc "Connects a vesting pool to this staking pool"
        (with-capability (POOL_CREATOR_GUARD pool-id)
          (update pools-usage pool-id
            {
                "has-vesting-connection": set-active,
                "vesting-pool-id": vesting-pool-id
            }
          )
        )
    )

    ;;;;; User Related

    (defun create-stake (pool-id:string account:string amount:decimal)
        @doc " Creates or adds a stake to a pool for a user, claiming rewards first if they are due"
        ;Check if this pool can migrate funds from a vesting pool
        (with-capability (ACCOUNT_GUARD account)
        (if (= (at "has-vesting-connection" (read pools-usage pool-id)) true )
          (let
                  (
                      (hasVEST true)
                  )
                  (migrate-vesting-allocations pool-id)
                  (absorb-new-tokens pool-id)
          )
          true
        )
            (let
                (
                    (pool-data (read pools pool-id))
                    (stake-id (get-stake-id-key account pool-id))
                    (pool-usage-data (read pools-usage pool-id))
                )
                (if (and (= (at "active" pool-data) true) (> (at "balance" pool-data) 0.0) )
                  (let
                      (
                          (token:module{fungible-v2} (at "stake-token" pool-data))
                      )
                      ;Claim rewards for this user
                      (claim pool-id account false)
                      ;Enforce active pool
                      (enforce (= (at "active" pool-data) true) "Staking pool is not active.")
                      ;Enforce stakers only
                      (enforce (!= (at "account" pool-data) account) "Pool owners may not stake their own pools.")
                      ;Insert stake data
                      (with-default-read stakes stake-id
                        { "id" : stake-id, "pool-id" : pool-id, "balance" : 0.0, "last-updated" : (at "block-time" (chain-data)), "account" : account, "rewards" : 0.0, "last-claimed" : (at "block-time" (chain-data)), "last-withdraw": (at "block-time" (chain-data))  }
                        { "id" := t_id, "pool-id" := t_pool-id, "balance" := t_balance, "last-updated" := t_last-updated, "account" := t_account, "rewards" := t_rewards, "last-claimed" := t_last-claimed, "last-withdraw" := t_last-withdraw }

                        ;If this is not the first staker, update the pools rewards multiplier
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
                            "last-withdraw": t_last-withdraw, ;If this is not the first staker, calculate a new multiplier
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

                        ;If this pool is not initialized or all stakers left, set it's start-time + end-time now
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

                        ;If this pool has no rewards to distribute, deny a staker, otherwise make stake transfer
                        (let
                            (
                                (time-since-end-time (diff-time  (at "block-time" (chain-data)) (at "end-time" (read pools pool-id)) ))
                            )
                            (if (>= time-since-end-time 0.0 )
                              (let
                                  (
                                      (EXHAUSTED true)
                                  )
                                  (update pools pool-id
                                    {
                                        "active": false
                                    }
                                  )
                                  (format "Pool {} has exhausted rewards and is no longer accepting stakers" [pool-id])
                              )
                              (let
                                  (
                                      (ACTIVE true)
                                  )
                                  (token::transfer account pool-id amount)
                                  ;If this staker's balance is 0, this staker is new, and we add +1 staker count to the pools data and set the pool start time:
                                  (if (= t_balance 0.0)
                                    (update pools pool-id
                                      {
                                        "stakers": (+ (at "stakers" pool-data) 1.0)
                                      }
                                    )
                                    true
                                  )
                                  (format "Staked {} {} in pool {} with account {}" [amount (at "stake-token" pool-data)  pool-id account])
                              )
                            )
                        )


                      )
                 )
                 (format "The pool {} is currently deactivated and not accepting stakers" [pool-id]))
            )
        )
    )

    (defun claim (pool-id:string account:string claim-stake:bool)
        @doc "Claims the rewards a user is owed, or rewards and stake"
        (with-capability (ACCOUNT_GUARD account)
        (if (= (at "has-vesting-connection" (read pools-usage pool-id)) true )
          (let
                  (
                      (hasVEST true)
                  )
                  (migrate-vesting-allocations pool-id)
                  (absorb-new-tokens pool-id)
          )
          true
        )
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
                        (reward-token:module{poly-fungible-v1} (at "reward-token" pool-data))
                        (stake-token:module{fungible-v2} (at "stake-token" pool-data))
                        (to-pay-max (calculate-rewards pool-id account) )
                        (available (at "balance" pool-data))
                      )
                      (let
                          (
                            (to-pay (if (>= (- available to-pay-max ) 0.0)
                                      to-pay-max
                                      available
                                    )
                            )
                            (to-pay-stake (if (or
                                                (= claim-stake true)
                                                (= (at "active" pool-data) false))
                                                  (at "balance" stake)
                                                  0.0
                                          )
                            )
                          )
                          ;Enforce account
                          (enforce (= account (at "account" stake)) "Not authorised to claim this stake")
                          ;Enforce balance
                          (enforce (>= to-pay 0.0) "You have no rewards to claim in this pool.")
                          ;Enforce claim reward duration
                          (if (or (= claim-stake true) (= (at "active" pool-data) false) ) true (enforce (> (diff-time (at "block-time" (chain-data)) (at 'last-claimed stake)) (at "claim-wait-seconds" pool-data) ) "You must wait the full claim reward duration before claiming rewards.") )
                          ;If the user is requesting stake + rewards and reward token = stake token, we compose 1 transfer to transfer both
                          (if (or (= (at "active" pool-data) false) (= claim-stake true) )
                            (let
                                (
                                  (stakeANDreward true)
                                )
                                ;Here the user is withdrawing rewards AND stake tokens
                                ;Enforce withdraw rules
                                (if (and (= claim-stake true) (= (at "active" pool-data) true) ) (enforce (> (diff-time (at "block-time" (chain-data)) (at 'last-withdraw stake)) (at "withdraw-duration" pool-data) ) "You must wait the full withdraw wait duration before claiming your stake.") true )
                                ;Transfer stake back to user
                                (install-capability (stake-token::TRANSFER pool-id account to-pay-stake))
                                (with-capability (PRIVATE_RESERVE pool-id (get-token-key stake-token) ) (stake-token::transfer pool-id account to-pay-stake)  )

                                ;Compose rewards transfer allowance
                                (if (> to-pay 0.0)
                                  (if (>= (- (at "balance" pool-data) to-pay ) 0.0)
                                    (install-capability (reward-token::TRANSFER (at "reward-token-id" pool-data) pool-id account to-pay))
                                    (install-capability (reward-token::TRANSFER (at "reward-token-id" pool-data) pool-id account (at "balance" pool-data)))
                                  )
                                true)

                                ;Transfer rewards to user using the above allowance
                                (if (> to-pay 0.0)
                                  (if (>= (- (at "balance" pool-data) to-pay ) 0.0)
                                  (with-capability (PRIVATE_RESERVE pool-id (get-polyfungiv1-key reward-token)) (reward-token::transfer (at "reward-token-id" pool-data) pool-id account to-pay)  )
                                  (with-capability (PRIVATE_RESERVE pool-id (get-polyfungiv1-key reward-token)) (reward-token::transfer (at "reward-token-id" pool-data) pool-id account (at "balance" pool-data) )  )
                                  )
                                true)
                                ;Update user stake stats
                                (update stakes stake-id  {"last-updated":  (at "block-time" (chain-data)), "balance": (-(at "balance" stake) to-pay-stake), "last-withdraw":  (at "block-time" (chain-data)) })

                              )
                              ;If claiming reward only:
                              (let
                                (
                                  (rewardONLY true)
                                )
                                ;Compose single token reward type transfer
                                (if (and (> to-pay 0.0) (= claim-stake false) )
                                  (if (>= (- (at "balance" pool-data) to-pay ) 0.0)
                                    (install-capability (reward-token::TRANSFER (at "reward-token-id" pool-data) pool-id account to-pay))
                                    (install-capability (reward-token::TRANSFER (at "reward-token-id" pool-data) pool-id account (at "balance" pool-data)))
                                  )
                                true)
                                ;Transfer back the reward token amount composed above if any rewards are owed to the user
                                (if (> to-pay 0.0)
                                  (if (>= (- (at "balance" pool-data) to-pay ) 0.0)
                                    (with-capability (PRIVATE_RESERVE pool-id (get-polyfungiv1-key reward-token)) (reward-token::transfer (at "reward-token-id" pool-data) pool-id account to-pay)  )
                                    (with-capability (PRIVATE_RESERVE pool-id (get-polyfungiv1-key reward-token)) (reward-token::transfer (at "reward-token-id" pool-data) pool-id account (at "balance" pool-data) )  )
                                  )
                                true)
                              )

                          )

                          ;Update pool usage data
                          (update pools-usage pool-id
                                  {
                                      "last-updated": (at "block-time" (chain-data)),
                                      "tokens-locked": (- (at "tokens-locked" pool-usage-data) to-pay-stake),
                                      "paid": (+ (at "paid" pool-usage-data) to-pay),
                                      "multiplier": (calculate-multiplier pool-id)
                                  }
                          )

                          ;Update pool data
                          (update pools pool-id
                            {
                                "balance": (- (at "balance" pool-data) to-pay),
                                "active": (if (<= (- (at "balance" pool-data) to-pay) 0.0) false (at "active" pool-data) ),
                                "stakers": (if (> to-pay-stake 0.0) (- (at "stakers" pool-data) 1.0) (at "stakers" pool-data) )
                            }
                          )

                          ;Update user stake data
                          (update stakes stake-id
                            {
                              "last-updated":  (at "block-time" (chain-data)),
                              "rewards": (+ (at "rewards" stake) to-pay),
                              "last-claimed":  (at "block-time" (chain-data)),
                              "multiplier": (at "multiplier" (read pools-usage pool-id)),
                              "last-withdraw": (if (> to-pay-stake 0.0) (at "block-time" (chain-data)) (at "last-withdraw" stake) )
                              }
                          )

                          ;Return message
                          (format "Rewarded {} with {} {} {} for {}% of pool and unstaked {} {}" [account to-pay reward-token (at "reward-token-id" pool-data)  (floor (* (/ (at "balance" stake) (at "tokens-locked" pool-usage-data)  ) 100) 2) to-pay-stake stake-token])

                      )
                  )
                (format "{} has no rewards in {}" [account pool-id]))
                )
          (format "{} has no rewards in {}" [account pool-id]))
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
                  (pool-data (read pools pool-id ["reward-token" "reward-token-id"]))
                  (stake-id (get-stake-id-key account pool-id))
              )
              (let
                (
                  (stake (read stakes stake-id ["balance" "multiplier"]))
                  (token:module{poly-fungible-v1} (at "reward-token" pool-data))
                )
                (floor (* (at "balance" stake) (- (calculate-multiplier pool-id) (at "multiplier" stake) ) ) (token::precision (at "reward-token-id" pool-data)))
              )
      )
    )

    (defun calculate-total-emitted-tokens (pool-id:string)
        @doc " Calculates the number of tokens emitted by a pool since it began "
        (let
            (
                (pool-data (read pools pool-id ["reward-duration" "reward-amount" "reward-token" "start-time" "start-balance" "reward-token-id"]))
            )
            (let
                (
                    (token:module{poly-fungible-v1} (at "reward-token" pool-data))
                    (time-passed (diff-time  (at "block-time" (chain-data)) (at "start-time" pool-data) ) )
                )
                ;We clamp our reward calculation under the pool's available balance so we don't exceed it
                (let
                    (
                        (total-available (floor (* (/ time-passed (at "reward-duration" pool-data)) (at "reward-amount" pool-data)) (token::precision (at "reward-token-id" pool-data)) ))
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

  (defun format-token:string
    ( token:module{fungible-v2} )
    (format "{}" [token])
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

  (defun get-polyfungiv1-key
    ( tokenA:module{poly-fungible-v1} )
    " Create key from token module"
    (format "{}" [tokenA])
  )

  (defun initialize ()
    @doc " Initialize the contract. Can only happen once. "
    (coin.create-account KDS_BANK (create-module-guard "kadena-stake-tokens-bank"))
  )

)

;(create-table free.kadena-stake-polyfungiv1-fungiv2.pools)
;(create-table free.kadena-stake-polyfungiv1-fungiv2.pools-usage)
;(create-table free.kadena-stake-polyfungiv1-fungiv2.pool-user-stats)
;(create-table free.kadena-stake-polyfungiv1-fungiv2.stakes)
;(free.kadena-stake-polyfungiv1-fungiv2.initialize)
