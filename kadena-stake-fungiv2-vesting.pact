;(enforce-pact-version "3.7")

(namespace (read-msg 'ns))

(module kadena-stake-fungiv2-vesting GOVERNANCE "Stake Factory Fungible-v2 Vesting Pool"

;\|/          (__)       factory-stake-vesting-fungiv2
;     `\------(oo)       creates vesting pools where
;       ||    (__)       fungible-v2 tokens are emitted over time
;       ||w--||     \|/  to beneficiary accounts

;Stake Factory Fungible-v2 Vesting Pools distribute tokens over time amongst pool members

    ;;;;; CONSTANTS
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

    ;;;;; CAPABILITIES

    (defcap GOVERNANCE ()
      @doc " Give the admin full access to call and upgrade the module. "
      (enforce-keyset 'admin-kadena-stake)
    )

    (defcap ACCOUNT_GUARD(account:string)
        @doc "Verifies account meets format and belongs to caller"
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

    (defcap PRIVATE_RESERVE
          (id:string pair-key:string)
        true)

    (defun enforce-private-reserve:bool
        (id:string pair-key:string)
      (require-capability (PRIVATE_RESERVE id pair-key)))

    (defun create-pool-guard:guard
        (id:string pair-key:string)
      (create-user-guard (enforce-private-reserve id pair-key )))

    ;;;;;;;;;; SCHEMAS AND TABLES ;;;;;;;;;;;;;;
    (defschema pools-schema
        @doc "Pool information, a unique id is the key"
        id:string
        name:string
        description:string
        pool_balance:decimal
        reward-token:module{fungible-v2}
        account:string
        active:bool
        max-reward-per-account:decimal
        claim-wait-seconds:decimal
        start-time:time
        reward-duration:decimal
        reward-amount:decimal
        stakers:decimal
        withdraw-duration:decimal
        start-balance:decimal
        initialized:bool
        end-time:time
        locked:bool
    )

    (defschema pools-usage-schema
        @doc "Pool usage data"
        tokens-locked:decimal
        placeholder-tokens:decimal
        last-updated:time
        paid:decimal
        multiplier:decimal
        next-multiplier:decimal
        has-vesting-connection:bool
        vesting-pool-id:string
    )

    ;list of accounts in pool
    (defschema pool-accounts-schema
      @doc "key pool-id"
      ids:[string]
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
        withdraw-duration:decimal
        multiplier:decimal
        start-time:time
        max-rewards:decimal
    )

    (deftable pools:{pools-schema})
    (deftable pools-usage:{pools-usage-schema})
    (deftable pool-user-stats:{pools-user-stats-schema})
    (deftable stakes:{stakes-schema})
    (deftable pool-accounts:{pool-accounts-schema})

    ;;;;; Pool Creator Related

    ;create pool(): for creating a vesting pool where fungible-v2 tokens are emitted to beneficiary accounts
    ;id: id of pool, ex: "test-pool"
    ;name: name of pool, ex: "Test Pool"
    ;balance: amount of distribution token given by pool owner to be distributed to beneficiary accounts, ex: 200
    ;reward-token: name of fungible-v2 reward token provided by pool creator for emissions, ex: coin
    ;account: pool creator account, ex: k:mykaccount
    ;max-reward-per-account: max rewards a beneficiary account can ever claim in the pool, ex: 200
    ;claim-wait-seconds: minimum number of seconds between beneficary allocation claims, ex: 0
    ;reward-duration: time it takes for reward amount to become emitted and available to beneficiarys, ex: 86400
    ;reward-amount: the amount of rewards available each reward-duration, ex: 10
    ;withdraw-duration: the amount of time in seconds until anyone can begin withdrawing vested tokens from this pool, ex: 0
    ;stakeables: a list of accounts to add to the pool upon creation, ex [{ "pool-id" : "test-pool", "account" : "k:mykaccount", "amount" : 100.0, "set-withdraw-duration" : true, "withdraw-duration" : 0.0, "max-rewards" : 200.0}]

    (defun create-pool (id:string
                        name:string
                        description:string
                        balance:decimal
                        reward-token:module{fungible-v2}
                        account:string
                        max-reward-per-account:decimal
                        claim-wait-seconds:decimal
                        reward-duration:decimal
                        reward-amount:decimal
                        withdraw-duration:decimal
                        stakeables:[object:{mintable}] )

        @doc "Creates a new vesting pool"
        (with-capability (ACCOUNT_GUARD account)
            ;Enforce rules
            (enforce-pool-id id)
            (enforce-valid-id account)
            (enforce-valid-id id)
            (enforce-valid-name name)
            (enforce-unit balance (reward-token::precision))
            (enforce (>= (length stakeables) 1) "Must add alteast 1 pool member when creating a pool")
            ;Create reward token account
            (reward-token::create-account id (create-pool-guard id (get-token-key reward-token)))
            ;Transfer reward token
            (reward-token::transfer-create account id (create-pool-guard id (get-token-key reward-token)) balance)
            ;Insert pool
            (insert pools id
                {
                    "id": id,
                    "name": name,
                    "description": description,
                    "pool_balance": balance,
                    "reward-token": reward-token,
                    "account": account,
                    "active": true,
                    "max-reward-per-account": max-reward-per-account,
                    "claim-wait-seconds": claim-wait-seconds,
                    "start-time": (at "block-time" (chain-data)),
                    "reward-duration": reward-duration,
                    "reward-amount": reward-amount,
                    "stakers": 0.0,
                    "withdraw-duration": withdraw-duration,
                    "start-balance": balance,
                    "initialized": false,
                    "end-time": (at "block-time" (chain-data)),
                    "locked": false
                }
            )
            ;Insert pool blank usage
            (insert pools-usage id {
                "tokens-locked": 0.0,
                "placeholder-tokens": 0.0,
                "last-updated": (at "block-time" (chain-data)),
                "paid": 0.0,
                "multiplier": 1.0,
                "next-multiplier": 1.0,
                "has-vesting-connection": false,
                "vesting-pool-id": "none"
            })
            (mass-stake stakeables)
            ;Return a message
            (format "Created pool {} with {} {}" [name balance reward-token])
        )
    )

    (defun add-balance-to-pool (pool-id:string account:string amount:decimal)
        @doc "Adds more tokens to a vesting pool for it to distribute to pool members"
        (let
                (
                    (pool-data (read pools pool-id ["account" "reward-token" "pool_balance" "start-balance" "active"]))
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
                              "balance": (+ (at "pool_balance" pool-data) amount),
                              "start-balance": (+ (at "start-balance" pool-data) amount),
                              "end-time": (calculate-pool-end-time pool-id false true amount)
                          }
                    )
                    (format "Added {} {} distribution tokens to {}" [amount token pool-id])
                )
        )
    )

    (defun deactivate-pool (pool-id:string account:string)
        @doc "Deactivates a pool and recovers all undistributed tokens back to pool creator account"
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
                    ;Enforce unlocked pool
                    (enforce (= (at "locked" pool-data) false) "Locked pools may not be recovered.")
                    ;Transfer pool starting stake
                    (install-capability (token::TRANSFER pool-id account to-pay) )
                    (with-capability (PRIVATE_RESERVE pool-id (get-token-key token)) (token::transfer pool-id account to-pay)  )
                    ;Update pool
                    (update pools pool-id
                      (+
                          {
                              "active": false,
                              "pool_balance": (- (at "balance" pool-data) to-pay)
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


    (defun withdraw-funds (pool-id:string account:string amount:decimal)
        @doc "For withdrawing funds from the vesting pool - Pool Creator Only"
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
                       (canpay:decimal amount)
                    )
                    ;Enforce pool owner
                    (enforce (= (at "account" pool-data) account) "Access prohibited.")
                    ;Enforce unlocked pool
                    (enforce (= (at "locked" pool-data) false) "Locked pools may not be recovered.")
                    ;Transfer amount to account
                    (install-capability (token::TRANSFER pool-id account canpay) )
                    (with-capability (PRIVATE_RESERVE pool-id (get-token-key token)) (token::transfer pool-id account canpay)  )

                    ;Update pool
                    (update pools pool-id
                      (+
                          {
                              "pool_balance": (- (at "pool_balance" (read pools pool-id)) canpay),
                              "start-balance": (- (at "start-balance" pool-data) canpay),
                              "end-time": (calculate-pool-end-time pool-id false true (* canpay -1.0) )
                          }
                          pool-data
                      )
                    )

                    ;Return a message
                    (format "Withdrew {} {} from {} to {}" [canpay token pool-id account])
                )
              )
        )
        )
    )

    (defun lock-pool (pool-id:string account:string)
        @doc "Permanently locks a vesting pool from being recovered by pool creator"
        (with-capability (POOL_CREATOR_GUARD pool-id )
          (let
              (
                  (pool-data (read pools pool-id))
              )
              ;Enforce pool owner
              (enforce (= (at "account" pool-data) account) "Access prohibited.")
              ;Update pool
              (update pools pool-id
                    {
                        "locked": true
                    }
              )
              ;Return a message
              (format "Locked pool {}" [pool-id])
          )
        )
    )

    (defun absorb-new-tokens (pool-id:string)
        @doc "Absorbs new tokens from a pools account into the pools distribution schedule"
        (let
                (
                    (pool-data (read pools pool-id ["account" "reward-token" "pool_balance" "start-balance" "active"]))
                    (pool-usage-data (read pools-usage pool-id ["tokens-locked"] ))
                )
                (let
                    (
                        (reward-token:module{fungible-v2} (at "reward-token" pool-data))
                    )
                    (let
                        (
                            (new-tokens (- (reward-token::get-balance pool-id) (at "pool_balance" pool-data) )
                            )
                        )
                        ;Enforce active pool
                        (enforce (= (at "active" pool-data) true) "You cannot add rewards to exhausted pools.")
                        ;Update pool
                        (update pools pool-id
                              {
                                  "pool_balance": (+ (at "pool_balance" pool-data) new-tokens),
                                  "start-balance": (+ (at "start-balance" pool-data) new-tokens),
                                  "end-time": (calculate-pool-end-time pool-id false true new-tokens)
                              }
                        )
                        (format "Absorbed {} reward-tokens into pool {}" [new-tokens pool-id])
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
                (claim-vest (at "vesting-pool-id" pool-usage-data) pool-id)
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


    ;stakeable schemea
    (defschema stakeable
      @doc "Mass edit helper object"
      pool-id:string
      account:string
      amount:decimal
      set-withdraw-duration:bool
      withdraw-duration:decimal
      max-rewards:decimal
    )

    (defun mass-staker (stakeable:object{stakeable})
      @doc " Mass edit helper funciton "
      (bind stakeable {
                          "pool-id" := s_poolid,
                          "account" := s_account,
                          "amount" := s_amount,
                          "set-withdraw-duration" := s_setwithdrawduration,
                          "withdraw-duration" := s_withdrawduration,
                          "max-rewards" := s_maxrewards
                        }
                        (create-stake s_poolid s_account s_amount s_setwithdrawduration s_withdrawduration s_maxrewards)
      )
    )

    ;mass-stale: runs multiple create-stake() at once
    ;useful for setting up an entire pool at once, or adjusting all pool member allocations at once so weights are always balanced properly

    (defun mass-stake
      (
        stakeables:[object:{mintable}]
      )
      @doc " Mass edit pool "
        (map (mass-staker) stakeables )
    )


    ;create-stake: adds account to pool, automatically adjusting other pool members rewards
    ;pool-id: pool to add account to
    ;account: account to add to pool
    ;amount: weight of account in pool - all weights add up to 100% of the pools vest
    ;set-withdraw-duration: true/false if to update this users withdraw-duration (always set to true at first and set a starting withdraw duration, even if its only 0 seconds)
    ;withdraw-duration: time in seconds until this user can begin withdrawing, ex: 0 (always set a withdraw duration unless updating the users allocation after it was already set previously)
    ;max-rewards: maximum number of tokens this account will ever be able to withdraw from this pool

    (defun create-stake (
                        pool-id:string
                        account:string
                        amount:decimal
                        set-withdraw-duration:bool
                        withdraw-duration:decimal
                        max-rewards:decimal
                        )
        @doc " Creates or adds more distribution allocations to a pool member account, transfering the accounts current allocations before so if there are any"
        (with-capability (POOL_CREATOR_GUARD pool-id )
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
                )
                (if (and (= (at "active" pool-data) true) (> (at "pool_balance" pool-data) 0.0) )
                  (let
                      (
                          (pool-usage-data (read pools-usage pool-id))
                      )
                      ;Claim rewards for this user
                      (claim pool-id account)
                      ;Enforce active pool
                      (enforce (= (at "active" pool-data) true) "Vesting pool is not active.")
                      ;Insert stake data
                      (with-default-read stakes stake-id
                        { "id" : stake-id, "pool-id" : pool-id, "balance" : 0.0, "last-updated" : (at "block-time" (chain-data)), "account" : account, "rewards" : 0.0, "last-claimed" : (at "block-time" (chain-data)), "withdraw-duration": withdraw-duration  }
                        { "id" := t_id, "pool-id" := t_pool-id, "balance" := t_balance, "last-updated" := t_last-updated, "account" := t_account, "rewards" := t_rewards, "last-claimed" := t_last-claimed, "withdraw-duration" := t_withdraw-duration }

                        ;If this is not the first beneficiary, update the pools emissions multiplier
                        (if (> (at "stakers" pool-data) 0.0)
                          (update pools-usage pool-id
                            {
                                "last-updated": (at "block-time" (chain-data)),
                                "multiplier": (calculate-multiplier pool-id )
                            }
                        )
                          true
                        )

                        ;Update pool usage data
                        (update pools-usage pool-id
                            {
                                "tokens-locked": (+ (- (- (at "tokens-locked" pool-usage-data) t_balance ) (at "placeholder-tokens" pool-usage-data) ) amount),
                                "placeholder-tokens": 0.0,
                                "last-updated": (if (= (at "active" (read pools pool-id)) true ) (at "block-time" (chain-data)) (at "last-updated" pool-usage-data)  )
                            }
                        )

                        ;Update user stake information
                        (write stakes stake-id {
                            "id": t_id,
                            "pool-id": t_pool-id,
                            "balance": amount,
                            "last-updated": (at "block-time" (chain-data)),
                            "account": t_account,
                            "rewards": t_rewards,
                            "last-claimed": t_last-claimed,
                            "withdraw-duration": (if (= set-withdraw-duration true) withdraw-duration t_withdraw-duration ),
                            "multiplier": (if (> (at "stakers" pool-data) 0.0)
                                            (calculate-multiplier pool-id)
                                            (at "multiplier" pool-usage-data)
                                          ),
                            "start-time": (at "block-time" (chain-data)),
                            "max-rewards": max-rewards
                        })

                        ;Record account to pool-accounts
                        (if (= (check-if-account-in-pool t_pool-id account) false)
                          (with-default-read pool-accounts t_pool-id
                            { "ids" : [] }
                            { "ids" := t_ids }
                            (write pool-accounts t_pool-id {"ids": (+ [account] t_ids ) } )
                          )
                        true)


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
                                  (format "Allocated {}% of emissions to {} for {} {} every {} seconds" [(* (/ (at "balance"(read stakes stake-id)) (at "tokens-locked" (read pools-usage pool-id) )  ) 100) account ALLOCATION (at "reward-token" pool-data) (at "reward-duration" pool-data) ])
                              )
                            )
                        )

                      )
                 )
                 (format "The vesting pool {} is deactivated." [pool-id]))
            )
        )
    )

    ;Vesting pools can be connected to other vesting pools to auto feed tokens from 1 pool to the other
    ;Only 2 vesting pools can be connected together at a time - 3 vesting pools cannot be linked together

    (defun claim-vest (pool-id:string account:string)
        @doc "For pool to pool vesting claims"
        (with-default-read stakes (get-stake-id-key account pool-id)
          { "account" : 'nonulls, "balance" : 0.0 }
          { "account" := t_account, "balance" := t_balance }
          (if (= (at "has-vesting-connection" (read pools-usage pool-id)) true )
          (let
                  (
                      (hasVEST true)
                  )
                  ;(migrate-vesting-allocations pool-id)
                  (absorb-new-tokens pool-id)
          )
          true
        )
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
                        (available (at "pool_balance" pool-data))
                      )
                      (let
                          (
                            (to-pay-total (if (>= (- available to-pay-max ) 0.0)
                                      to-pay-max
                                      available
                                    )
                            )
                            (can-pay (- (at "max-rewards" stake) (at "rewards" stake) ))
                          )
                          (let
                              (
                                (to-pay (if (>= (- can-pay to-pay-total ) 0.0)
                                          to-pay-total
                                          (- (at "max-rewards" stake) (at "rewards" stake) )
                                          )

                                )
                              )
                              ;Enforce account
                              (enforce (= account (at "account" stake)) "Authorization failed")
                              ;Enforce balance
                              (if (>= to-pay 0.0)
                                (let
                                      (
                                        (hasCLAIM true)
                                      )
                                      ;Enforce claim wait duration
                                      (if (>= (diff-time (at "block-time" (chain-data)) (at 'last-claimed stake)) (at "claim-wait-seconds" pool-data) )
                                        (let
                                              (
                                                (canCLAIM true)
                                              )
                                              ;Enforce pool withdraw wait duration
                                              (if (>= (diff-time (at "block-time" (chain-data)) (at 'start-time pool-data)) (at "withdraw-duration" pool-data) )
                                                (let
                                                      (
                                                        (canWITHDRAW true)
                                                      )
                                                      ;Enforce user withdraw wait duration
                                                      (if (>= (diff-time (at "block-time" (chain-data)) (at 'start-time stake)) (at "withdraw-duration" stake) )
                                                        (let
                                                              (
                                                                (userWITHDRAW true)
                                                              )

                                                              ;Compose allocation transfer
                                                              (if (> to-pay 0.0)
                                                                (if (>= (- (at "pool_balance" pool-data) to-pay ) 0.0)
                                                                  (install-capability (reward-token::TRANSFER pool-id account to-pay))
                                                                  (install-capability (reward-token::TRANSFER pool-id account (at "pool_balance" pool-data)))
                                                                )
                                                              true)
                                                              ;Transfer back the allocation amount composed above
                                                              (if (> to-pay 0.0)
                                                                (if (>= (- (at "pool_balance" pool-data) to-pay ) 0.0)
                                                                  (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token)) (reward-token::transfer pool-id account to-pay)  )
                                                                  (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token)) (reward-token::transfer pool-id account (at "pool_balance" pool-data) )  )
                                                                )
                                                              true)

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
                                                                    "pool_balance": (- (at "pool_balance" pool-data) (if (> to-pay 0.0) to-pay 0.0) ),
                                                                    "active": (if (<= (- (at "pool_balance" pool-data) to-pay) 0.0) false (at "active" pool-data) )
                                                                }
                                                              )

                                                              ;Update user stake data
                                                              (update stakes stake-id
                                                                {
                                                                  "last-updated":  (at "block-time" (chain-data)),
                                                                  "rewards": (+ (at "rewards" stake) (if (> to-pay 0.0) to-pay 0.0)),
                                                                  "last-claimed":  (at "block-time" (chain-data)),
                                                                  "multiplier": (at "multiplier" (read pools-usage pool-id))
                                                                  }
                                                              )

                                                              ;If this account has claimed its max rewards and can no longer claim rewards, then remove its allocation and recycle what it was claiming back into the pool for distribution
                                                              (if (or (= (- (at "max-rewards" stake) (at "rewards" stake) ) 0.0) (= (- (at "max-rewards" (read stakes stake-id)) (at "rewards" (read stakes stake-id)) ) 0.0) )
                                                              (let
                                                                  (
                                                                    (userEXPIRED true)
                                                                  )
                                                                  (update pools pool-id
                                                                      {
                                                                          "end-time": (calculate-pool-end-time pool-id false true to-pay-max),
                                                                          "stakers": (- (at "stakers" pool-data) 1.0)
                                                                      }
                                                                  )
                                                                  (update pools-usage pool-id
                                                                      {
                                                                          "tokens-locked": (- (at "tokens-locked" pool-usage-data) (at "balance" stake) )
                                                                      }
                                                                  )
                                                                  (update stakes stake-id
                                                                      {
                                                                          "balance": (-(at "balance" stake) (at "balance" stake) )
                                                                      }
                                                                  )
                                                                  (with-default-read pool-accounts pool-id
                                                                    { "ids" : [] }
                                                                    { "ids" := t_ids }
                                                                    (let*
                                                                        (
                                                                          (newlist:[string]  (filter (compose (composelist) (!= account)) t_ids) )
                                                                        )
                                                                        (write pool-accounts pool-id {"ids": newlist } )
                                                                    )
                                                                  )
                                                              )
                                                              true)

                                                              ;Return message
                                                              (format "Awarded {} with {} {}" [account to-pay reward-token])
                                                          )
                                                        (format "Withdraw actions for {} are still locked under a wait-limit in pool {}" [account pool-id])
                                                      )
                                                  )
                                                (format "Withdraw actions are still locked under a wait-limit in pool {}" [pool-id])
                                              )
                                          )
                                        (format "{} must wait the full wait-limit in {} before claiming emissions" [account pool-id])
                                      )
                                  )
                                (format "{} has no emissions to claim right now in pool {}" [account pool-id])
                              )
                          )
                      )
                  )
                (format "{} has no token emissions to claim in {}" [account pool-id]))
                )
          (format "{} has no token emissions to claim in {}" [account pool-id]))
        )
    )

    (defun claim (pool-id:string account:string)
        @doc "Claims an accounts current rewards from a pool"
        (with-default-read stakes (get-stake-id-key account pool-id)
          { "account" : 'nonulls, "balance" : 0.0 }
          { "account" := t_account, "balance" := t_balance }
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
                        (available (at "pool_balance" pool-data))
                      )
                      (let
                          (
                            (to-pay-total (if (>= (- available to-pay-max ) 0.0)
                                      to-pay-max
                                      available
                                    )
                            )
                            (can-pay (- (at "max-rewards" stake) (at "rewards" stake) ))
                          )
                          (let
                              (
                                (to-pay (if (>= (- can-pay to-pay-total ) 0.0)
                                          to-pay-total
                                          (- (at "max-rewards" stake) (at "rewards" stake) )
                                          )

                                )
                              )
                              ;Enforce account
                              (enforce (= account (at "account" stake)) "Authorization failed")
                              ;Enforce balance
                              (if (>= to-pay 0.0)
                                (let
                                      (
                                        (hasCLAIM true)
                                      )
                                      ;Enforce claim wait duration
                                      (if (>= (diff-time (at "block-time" (chain-data)) (at 'last-claimed stake)) (at "claim-wait-seconds" pool-data) )
                                        (let
                                              (
                                                (canCLAIM true)
                                              )
                                              ;Enforce pool withdraw wait duration
                                              (if (>= (diff-time (at "block-time" (chain-data)) (at 'start-time pool-data)) (at "withdraw-duration" pool-data) )
                                                (let
                                                      (
                                                        (canWITHDRAW true)
                                                      )
                                                      ;Enforce user withdraw wait duration
                                                      (if (>= (diff-time (at "block-time" (chain-data)) (at 'start-time stake)) (at "withdraw-duration" stake) )
                                                        (let
                                                              (
                                                                (userWITHDRAW true)
                                                              )
                                                              ;Compose allocation transfer
                                                              (if (> to-pay 0.0)
                                                                (if (>= (- (at "pool_balance" pool-data) to-pay ) 0.0)
                                                                  (install-capability (reward-token::TRANSFER pool-id account to-pay))
                                                                  (install-capability (reward-token::TRANSFER pool-id account (at "balance" pool-data)))
                                                                )
                                                              true)
                                                              ;Transfer back the allocation amount composed above
                                                              (if (> to-pay 0.0)
                                                                (if (>= (- (at "pool_balance" pool-data) to-pay ) 0.0)
                                                                  (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token)) (reward-token::transfer pool-id account to-pay)  )
                                                                  (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token)) (reward-token::transfer pool-id account (at "pool_balance" pool-data) )  )
                                                                )
                                                              true)

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
                                                                    "pool_balance": (- (at "pool_balance" pool-data) (if (> to-pay 0.0) to-pay 0.0) ),
                                                                    "active": (if (<= (- (at "pool_balance" pool-data) to-pay) 0.0) false (at "active" pool-data) )
                                                                }
                                                              )

                                                              ;Update user stake data
                                                              (update stakes stake-id
                                                                {
                                                                  "last-updated":  (at "block-time" (chain-data)),
                                                                  "rewards": (+ (at "rewards" stake) (if (> to-pay 0.0) to-pay 0.0)),
                                                                  "last-claimed":  (at "block-time" (chain-data)),
                                                                  "multiplier": (at "multiplier" (read pools-usage pool-id))
                                                                  }
                                                              )

                                                              ;If this account has claimed its max rewards and can no longer claim rewards, then remove its allocation and recycle what it was claiming back into the pool for distribution
                                                              (if (or (= (- (at "max-rewards" stake) (at "rewards" stake) ) 0.0) (= (- (at "max-rewards" (read stakes stake-id)) (at "rewards" (read stakes stake-id)) ) 0.0) )
                                                              (let
                                                                  (
                                                                    (userEXPIRED true)
                                                                  )
                                                                  (update pools pool-id
                                                                      {
                                                                          "end-time": (calculate-pool-end-time pool-id false true to-pay-max),
                                                                          "stakers": (- (at "stakers" pool-data) 1.0)
                                                                      }
                                                                  )
                                                                  (update pools-usage pool-id
                                                                      {
                                                                        "tokens-locked": (- (at "tokens-locked" pool-usage-data) (at "balance" stake) )
                                                                      }
                                                                  )
                                                                  (update stakes stake-id
                                                                      {
                                                                          "balance": (-(at "balance" stake) (at "balance" stake) )
                                                                      }
                                                                  )
                                                                  (with-default-read pool-accounts pool-id
                                                                    { "ids" : [] }
                                                                    { "ids" := t_ids }
                                                                    (let*
                                                                        (
                                                                          (newlist:[string]  (filter (compose (composelist) (!= account)) t_ids) )
                                                                        )
                                                                        (write pool-accounts pool-id {"ids": newlist } )
                                                                    )
                                                                  )
                                                              )
                                                              true)

                                                              ;Return message
                                                              (format "Awarded {} with {} {}" [account to-pay reward-token])
                                                          )
                                                        (format "Withdraw actions for {} are still locked under a wait-limit in pool {}" [account pool-id])
                                                      )
                                                  )
                                                (format "Withdraw actions are still locked under a wait-limit in pool {}" [pool-id])
                                              )
                                          )
                                        (format "{} must wait the full wait-limit in {} before claiming emissions" [account pool-id])
                                      )
                                  )
                                (format "{} has no emissions to claim right now in pool {}" [account pool-id])
                              )
                          )
                      )
                  )
                (format "{} has no token emissions to claim in {}" [account pool-id]))
                )
          (format "{} has no token emissions to claim in {}" [account pool-id]))
        )
    )

    (defun remove-account (pool-id:string account:string pay-out:bool)
        @doc "Deletes an account from a vesting pool, with option to pay out what is owed or not"
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
                        (available (at "pool_balance" pool-data))
                      )
                      (let
                          (
                            (to-pay (if (>= (- available to-pay-max ) 0.0)
                                      to-pay-max
                                      available
                                    )
                            )
                            (to-pay-stake (at "balance" stake))
                          )
                          ;Enforce balance
                          (enforce (>= to-pay 0.0) "There are no allocations to claim for this account.")




                          ;Update pool data
                          (update pools pool-id
                            {
                                "pool_balance": (if (= pay-out false) (at "pool_balance" pool-data) (- (at "pool_balance" pool-data) to-pay) ),
                                "active": (if (<= (- (at "pool_balance" pool-data) to-pay) 0.0) false (at "active" pool-data) ),
                                "stakers": (if (> to-pay-stake 0.0) (- (at "stakers" pool-data) 1.0) (at "stakers" pool-data) )
                            }
                          )

                          ;Update pool usage data
                          ;"tokens-locked": (- (at "tokens-locked" pool-usage-data) to-pay-stake),
                          (update pools-usage pool-id
                                  {
                                      "last-updated": (at "block-time" (chain-data)),
                                      "placeholder-tokens": to-pay-stake,
                                      "paid": (if (= pay-out false) (at "paid" pool-usage-data) (+ (at "paid" pool-usage-data) to-pay) ) ,
                                      "multiplier": (calculate-multiplier pool-id)
                                  }
                          )

                          ;Update user data
                          (update stakes stake-id
                            {
                              "last-updated":  (at "block-time" (chain-data)),
                              "rewards": (- to-pay-max to-pay),
                              "balance": (-(at "balance" stake) to-pay-stake),
                              "last-claimed":  (at "block-time" (chain-data)),
                              "multiplier": (at "multiplier" (read pools-usage pool-id))
                              }
                          )

                          ;Pay the account we are deleting, or dont and recycle its funds back into the pool
                          (if (= pay-out true)

                          ;If paying the deleted account:
                            (let
                                (
                                  (payALLOCATION true)
                                )
                                ;Compose allocation transfer
                                (if (> to-pay 0.0)
                                  (if (>= (- (at "pool_balance" pool-data) to-pay ) 0.0)
                                    (install-capability (reward-token::TRANSFER pool-id account to-pay))
                                    (install-capability (reward-token::TRANSFER pool-id account (at "pool_balance" pool-data)))
                                  )
                                true)
                                ;Transfer back the allocation amount composed above
                                (if (> to-pay 0.0)
                                  (if (>= (- (at "pool_balance" pool-data) to-pay ) 0.0)
                                  (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token)) (reward-token::transfer pool-id account to-pay)  )
                                  (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token)) (reward-token::transfer pool-id account (at "pool_balance" pool-data) )  )
                                  )
                                true)
                            )

                          ;If not paying the deleted account (recycle the funds)
                            ; (update pools pool-id
                            ;       {
                            ;           "end-time": (calculate-pool-end-time pool-id false false (* to-pay -1.0))
                            ;       }
                            ; )
                            true
                          )


                          ;Update pool accounts
                          (with-default-read pool-accounts pool-id
                            { "ids" : [] }
                            { "ids" := t_ids }
                            (let*
                                (
                                  (newlist:[string]  (filter (compose (composelist) (!= account)) t_ids) )
                                )
                                (write pool-accounts pool-id {"ids": newlist } )
                            )
                          )

                          ;Return
                          (if (= pay-out true)
                            (format "Awarded {} with {} {} and deleted the account" [account to-pay reward-token])
                            (format "Removed all token emission allocations for account {}" [account])
                          )

                      )
                  )
                (format "{} has no token allocations to claim in {}" [account pool-id]))
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
                (days-since-last-update (floor (/ (diff-time  (at "block-time" (chain-data)) (at "last-updated" (read pools-usage pool-id))) (at "reward-duration" pool-data)) 0)  )
                (days-since-end-time (floor (/ (diff-time (at "block-time" (chain-data))  (at "end-time" (read pools pool-id))) (at "reward-duration" pool-data)) 0)  )
                (tokenslocked (at "tokens-locked" (read pools-usage pool-id)))
              )
              (let
                  (
                    (days-since (- days-since-last-update days-since-end-time) )
                  )
                  (let
                      (
                        (multiplier (if (> tokenslocked 0.0) (+ (at "multiplier" pool-usage-data) (/ (* (min days-since-last-update (abs days-since)) (at "reward-amount" pool-data) ) (at "tokens-locked" (read pools-usage pool-id))  ) ) 0.0) )
                      )
                      multiplier
                  )
              )
          )
      )
    )

    (defun calculate-rewards (pool-id:string account:string)
      @doc " Calculates an accounts pending rewards in a pool "
      (let
              (
                  (pool-data (read pools pool-id ["reward-token" "pool_balance"]))
                  (stake-id (get-stake-id-key account pool-id))
              )
              (let
                (
                  (stake (read stakes stake-id ["balance" "multiplier"]))
                  (token:module{fungible-v2} (at "reward-token" pool-data))
                  (pool-balance (at "pool_balance" pool-data))
                )
                (let
                    (
                      (rewards-max (min (floor (* (at "balance" stake) (- (calculate-multiplier pool-id) (at "multiplier" stake) ) ) (token::precision)) pool-balance)  )
                    )
                    (let
                    (
                      (rewards (if (> rewards-max 0.0) rewards-max 0.0) )
                    )
                    rewards
                  )
                  )
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
                (pool-data (read pools pool-id ["reward-duration" "reward-amount" "start-balance" "pool_balance" "end-time"]))
            )
            (let
                (
                    (number-of-reward-durations-add-balance (/ amount-to-add (at "reward-amount" pool-data) ))
                    (number-of-reward-durations-balance-end-time (/ (at "pool_balance" pool-data) (at "reward-amount" pool-data) ))
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

    (defun calculate-apy (pool-id:string)
        @doc " Calculates a pools current apy "
        (let
            (
                (pool-data (read pools pool-id ["reward-duration" "reward-amount" "reward-token" "start-time" "start-balance"]))
                (pool-usage-data (read pools-usage pool-id ["tokens-locked" "paid"]))
            )
            (let
                (
                    (token:module{fungible-v2} (at "reward-token" pool-data))
                    (time-passed (diff-time (add-time (at "block-time" (chain-data)) (days 365) )  (at "start-time" pool-data) ) )
                )
                (let
                    (
                        (total-available (floor (* (/ time-passed (at "reward-duration" pool-data)) (at "reward-amount" pool-data)) (token::precision) ))
                        (max-available (at "start-balance" pool-data) )
                    )
                    (if (<= total-available max-available )
                      (if (> (at "tokens-locked" pool-usage-data) 0.0)
                        (/ (* 100 (- total-available (at "paid" pool-usage-data) ) ) (at "tokens-locked" pool-usage-data) )
                        (/ (* 100 (- total-available (at "paid" pool-usage-data) ) ) 100) )
                      (if (> (at "tokens-locked" pool-usage-data) 0.0)
                        (/ (* 100 (- max-available (at "paid" pool-usage-data) ) ) (at "tokens-locked" pool-usage-data) )
                        (/ (* 100 (- max-available (at "paid" pool-usage-data) ) ) 100) )
                    )
                )
            )
        )
    )

    (defun composelist
      (
        stringlist:string
      )
      @doc " Utility to compose lists "
      (let*
        (
          (current:string stringlist)
        )
        current
      )
    )

    (defun check-if-account-in-pool:bool (pool-id:string account:string)
      @doc " Returns true/false if a account is in pool "
        (with-default-read pool-accounts pool-id
          {"ids": []}
          {"ids":= t_ids}
          (contains account t_ids)
          )
    )

    (defun get-pool-accounts:string (pool-id:string)
      @doc " Gets pool account information "
          (with-default-read pool-accounts pool-id
                  { "ids" : [] }
                  { "ids" := t_ids }
                  (map (get-user-stakes pool-id) t_ids)
          )
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
      (+ (+ (read pools pool-id) (read pools-usage pool-id)) (read pool-accounts pool-id) )
    )

    (defun get-pool-stakes (pool-id:string)
      (let
                (
                    (poolids (at "ids" (read pool-accounts pool-id)))
                )
                (map (get-user-stakes pool-id) poolids)
      )
    )

    (defun get-user-stakes (pool-id:string account:string)
      (let*
          (
              (pool-data (read pools pool-id ["reward-amount"]))
              (pool-usage-data (read pools-usage pool-id ["tokens-locked"] ))
              (USERSTAKE (read stakes (get-stake-id-key account pool-id)))
              (ALLOCATION (if (> (at "balance"(read stakes (get-stake-id-key account pool-id))) 0.0) (* (at "reward-amount" pool-data) (/ (at "balance"(read stakes (get-stake-id-key account pool-id))) (at "tokens-locked" (read pools-usage pool-id) )  ) ) 0 )   )
              (PERCENTAGE (if (> (at "balance"(read stakes (get-stake-id-key account pool-id))) 0.0) (* (/ (at "balance"(read stakes (get-stake-id-key account pool-id))) (at "tokens-locked" (read pools-usage pool-id) )  ) 100) 0 ) )
          )
          (+ (+ USERSTAKE { "pool_allocation" : ALLOCATION , "pool_percentage" : PERCENTAGE } )
          {
            "pending_rewards" : (calculate-rewards pool-id account)
          }
          )
      )
    )


    (defun get-pool-obj
      ( stakes-schema:object{stakes-schema} )
      @doc " Get the details of a stake obj "
      (bind stakes-schema { "pool-id" := poolid, "account" := account }
      (+ (get-pool-info poolid) (get-user-stakes poolid account))
      )
    )

    (defun get-pool-obj2
      ( pools-schema:object{pools-schema} )
      @doc " Get the details of a stake obj "
      (bind pools-schema { "id" := poolid }
      (get-pool-info poolid)
      )
    )

    (defun get-user-pools:string (account:string)
      @doc " Gets all of a users pools "
      (let ((x (select stakes ['pool-id 'account]
            (and? (where 'account (= account))
              (where 'balance (< 0.0))))))
            (map (get-pool-obj) x))
    )

    (defun get-user-created-pools:string (account:string)
      @doc " Gets all of a users pools "
      (let ((x (select pools ['id ]
            (where 'account (= account)))))
            (map (get-pool-obj2) x))
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

)

;(create-table free.kadena-stake-fungiv2-vesting.pools)
;(create-table free.kadena-stake-fungiv2-vesting.pools-usage)
;(create-table free.kadena-stake-fungiv2-vesting.pool-user-stats)
;(create-table free.kadena-stake-fungiv2-vesting.stakes)
;(create-table free.kadena-stake-fungiv2-vesting.pool-accounts)
