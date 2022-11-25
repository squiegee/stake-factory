(namespace (read-msg 'ns))

(module factory-stake-fungiv2 GOVERNANCE "Stake Factory Staking Fungible-v2 - For creating staking pools where fungible-v2 tokens are staked for fungible-v2 token rewards"

;\|/          (__)       factory-stake-fungiv2
;     `\------(oo)       creates staking pools where:
;       ||    (__)       reward token type: fungible-v2
;       ||w--||     \|/  stake token type: fungible-v2

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
        reward-token:module{fungible-v2}
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

    ;create pool: creates a permissionless staking pool where fungible-v2 tokens are staked and fungible-v2 tokens are earned

    ;Staking pools distribute tokens to stakers over the course of time
    ;In the example below we make a pool 'test-pool', where stakers will stake coin token to earn coin token
    ;The pool will distribute 10 coin tokens per day to stakers by emitting 0.00011574019 tokens every 1.0 second
    ;It is advised to create pools that release tokens in tiny time-frames so stakers don't have to wait long to claim rewards

    ;id: id of pool, ex: 'test-pool'
    ;name: name of pool, ex: 'Test Pool'
    ;balance: total amount of rewards to be distributed by this pool, ex: 1000.0
    ;reward-token: name of fungible-v2 reward token module, ex: coin
    ;stake-token: name of fungible-v2 stake token module, ex: coin
    ;account: pool creator account, ex: 'k:mykaccount'
    ;claim-wait-seconds: minimum number of seconds between staker reward claims, ex: 0 (stakers can claim any time)
    ;reward-duration: number of seconds it takes to distribute reward-amount, ex: 1.0 (every 1.0 second 0.00011574019 tokens are distributed to stakers)
    ;reward-amount: the amount of rewards available each reward-duration, ex: 0.00011574019 (0.00011574019 tokens are distributed every second)
    ;withdraw-duration: time in seconds a staker's tokens must be locked before a staker can withdraw, ex 0 (stakers can withdraw any time)

    (defun create-pool (id:string
                        name:string
                        balance:decimal
                        reward-token:module{fungible-v2}
                        stake-token:module{fungible-v2}
                        account:string
                        claim-wait-seconds:decimal
                        reward-duration:decimal
                        reward-amount:decimal
                        withdraw-duration:decimal )

        @doc "Creates a new staking pool where users stake and earn fungible-v2 tokens"
        (with-capability (ACCOUNT_GUARD account)
            ;Enforce rules
            (enforce-pool-id id)
            (enforce-valid-id account)
            (enforce-valid-id id)
            (enforce-valid-name name)
            (enforce-unit balance (reward-token::precision))
            ;Create reward token account
            (reward-token::create-account id (create-pool-guard id (get-token-key reward-token)))
            ;Transfer reward token
            (reward-token::transfer-create account id (create-pool-guard id (get-token-key reward-token)) balance)
            ;If reward-token != stake-token then create a token account for stake-token too
            (if (= (get-token-key reward-token) (get-token-key stake-token)) true (stake-token::create-account id (create-pool-guard id (get-token-key stake-token))) )
            ;Insert pool
            (insert pools id
                {
                    "id": id,
                    "name": name,
                    "balance": balance,
                    "reward-token": reward-token,
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
                    (format "Added {} {} rewards to {}" [amount token pool-id])
                )
        )
    )

    (defun absorb-new-tokens (pool-id:string)
        @doc "Absorbs new tokens from a pools account into the pools distribution schedule"
        (let
                (
                    (pool-data (read pools pool-id ["account" "reward-token" "stake-token" "balance" "start-balance" "active"]))
                    (pool-usage-data (read pools-usage pool-id ["tokens-locked"] ))
                )
                (let
                    (
                        (reward-token:module{fungible-v2} (at "reward-token" pool-data))
                        (stake-token:module{fungible-v2} (at "stake-token" pool-data))
                    )
                    (let
                        (
                            (new-tokens (if (!= (get-token-key reward-token) (get-token-key stake-token))
                                      (- (reward-token::get-balance pool-id) (at "balance" pool-data) )
                                      (- (- (reward-token::get-balance pool-id) (at "balance" pool-data) ) (at "tokens-locked" pool-usage-data) )
                                     )
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
                ;For connecting vesting pools to staking pools
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
                        ;If this pool has no rewards to distribute, deny a staker, otherwise make stake transfer
                        (let
                            (
                                (time-since-end-time (diff-time  (at "block-time" (chain-data)) (at "end-time" (read pools pool-id)) ))
                            )
                            (if (and (and (>= time-since-end-time 0.0 ) (= (at "initialized" pool-data) true) ) (> (at "stakers" pool-data) 0.0) )
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
                                  ;Insert stake data
                                  (with-default-read stakes stake-id
                                    { "id" : stake-id, "pool-id" : pool-id, "balance" : 0.0, "last-updated" : (at "block-time" (chain-data)), "account" : account, "rewards" : 0.0, "last-claimed" : (at "block-time" (chain-data)), "last-withdraw": (at "block-time" (chain-data))  }
                                    { "id" := t_id, "pool-id" := t_pool-id, "balance" := t_balance, "last-updated" := t_last-updated, "account" := t_account, "rewards" := t_rewards, "last-claimed" := t_last-claimed, "last-withdraw" := t_last-withdraw }

                                    ;If this is not the first staker, update the pools rewards multiplier
                                    (if (> (at "stakers" pool-data) 0.0)
                                      (update pools-usage pool-id
                                        {
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

                                    ;Transfer stake
                                    (token::transfer account pool-id amount)

                                    ;If this staker's balance is 0, this staker is new
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
                  (if (or (> (at "stakers" pool-data) 0.0) (> (at "balance" (read stakes stake-id)) 0.0) )
                  (let
                      (
                        (stake (read stakes stake-id))
                        (reward-token:module{fungible-v2} (at "reward-token" pool-data))
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
                                                  (if (<= (- available to-pay-max ) 0.0)
                                                    (at "balance" stake)
                                                    0.0
                                                  )
                                          )
                            )
                          )
                          ;Enforce account
                          (enforce (= account (at "account" stake)) "Not authorised to claim this stake")
                          ;Enforce balance
                          (enforce (>= to-pay 0.0) "You have no rewards to claim in this pool.")
                          ;Enforce claim reward duration
                          (if (or (= claim-stake true) (= (at "active" pool-data) false) ) true (enforce (>= (diff-time (at "block-time" (chain-data)) (at 'last-claimed stake)) (at "claim-wait-seconds" pool-data) ) "You must wait the full claim reward duration before claiming rewards.") )
                          ;If the user is requesting stake + rewards and reward token = stake token, we compose 1 transfer to transfer both
                          (if (or (or (= (at "active" pool-data) false) (= claim-stake true) ) (<= (- available to-pay-max ) 0.0))
                            (let
                                (
                                  (stakeANDreward true)
                                )
                                ;Here the user is withdrawing rewards AND stake tokens
                                ;Enforce withdraw rules
                                (if (and (= claim-stake true) (= (at "active" pool-data) true) ) (enforce (>= (diff-time (at "block-time" (chain-data)) (at 'last-withdraw stake)) (at "withdraw-duration" pool-data) ) "You must wait the full withdraw wait duration before claiming your stake.") true )
                                ;Now, we should determine if stake token = reward token
                                ;If so, we compose a single transfer capability to transfer 1 token type back (stake + reward token)
                                ;If not, we compose 2 different transfer capabilitys to transfer 2 different token types back
                                (if (= (reward-token::details pool-id) (stake-token::details pool-id))
                                  (if (= (get-token-key reward-token) (get-token-key stake-token))
                                    (if (>= (- (at "balance" pool-data) to-pay ) 0.0)
                                      (install-capability (reward-token::TRANSFER pool-id account (+ to-pay-stake to-pay)))
                                      (install-capability (reward-token::TRANSFER pool-id account (+ to-pay-stake (at "balance" pool-data))))
                                    )
                                    (if (and (>= (- (at "balance" pool-data) to-pay ) 0.0)  (> to-pay 0.0))
                                      (install-capability (reward-token::TRANSFER pool-id account to-pay))
                                      (if (> (at "balance" pool-data) 0.0) (install-capability (reward-token::TRANSFER pool-id account (at "balance" pool-data))) true)
                                    )
                                  )
                                  (if (and (>= (- (at "balance" pool-data) to-pay ) 0.0)  (> to-pay 0.0))
                                      (install-capability (reward-token::TRANSFER pool-id account to-pay))
                                      (if (> (at "balance" pool-data) 0.0) (install-capability (reward-token::TRANSFER pool-id account (at "balance" pool-data))) true)
                                  )
                                )
                                ;Make reward transfer or reward + stake transfer depending on token type
                                (if (= (reward-token::details pool-id) (stake-token::details pool-id))
                                  (if (= (get-token-key reward-token) (get-token-key stake-token))
                                    (if (>= (- (at "balance" pool-data) to-pay ) 0.0)
                                      (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token) ) (reward-token::transfer pool-id account (+ to-pay-stake to-pay))  )
                                      (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token) ) (reward-token::transfer pool-id account (+ to-pay-stake (at "balance" pool-data)))  )
                                    )
                                    (if (and (>= (- (at "balance" pool-data) to-pay ) 0.0)  (> to-pay 0.0))
                                      (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token) ) (reward-token::transfer-create pool-id account (at "guard" (coin.details account)) to-pay )  )
                                      (if (> (at "balance" pool-data) 0.0) (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token) ) (reward-token::transfer-create pool-id account (at "guard" (coin.details account)) (at "balance" pool-data) )  ) true)
                                    )
                                  )
                                  (if (and (>= (- (at "balance" pool-data) to-pay ) 0.0)  (> to-pay 0.0))
                                    (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token) ) (reward-token::transfer-create pool-id account (at "guard" (coin.details account)) to-pay )  )
                                    (if (> (at "balance" pool-data) 0.0) (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token) ) (reward-token::transfer-create pool-id account (at "guard" (coin.details account)) (at "balance" pool-data) )  ) true)
                                  )
                                )
                                ;Now lets transfer the users stake back if we haven't already in the logic above
                                ;If stake token type is different from reward token type, compose a single stake token transfer allowance
                                (if (= (reward-token::details pool-id) (stake-token::details pool-id))
                                  true
                                  (if (= (get-token-key reward-token) (get-token-key stake-token))
                                    true
                                    (install-capability (stake-token::TRANSFER pool-id account to-pay-stake))
                                  )
                                )
                                ;If stake token type is different from reward token type, make stake token transfer now.
                                (if (= (reward-token::details pool-id) (stake-token::details pool-id))
                                  true
                                  (if (= (get-token-key reward-token) (get-token-key stake-token))
                                    true
                                    (with-capability (PRIVATE_RESERVE pool-id (get-token-key stake-token) ) (stake-token::transfer pool-id account to-pay-stake)  )
                                  )
                                )
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
                                  (if (and (>= (- (at "balance" pool-data) to-pay ) 0.0)  (> to-pay 0.0))
                                    (install-capability (reward-token::TRANSFER pool-id account to-pay))
                                    (if (> (at "balance" pool-data) 0.0) (install-capability (reward-token::TRANSFER pool-id account (at "balance" pool-data))) true)
                                  )
                                true)
                                ;Transfer back the reward token amount composed above if any rewards are owed to the user
                                (if (> to-pay 0.0)
                                  (if (and (>= (- (at "balance" pool-data) to-pay ) 0.0)  (> to-pay 0.0))
                                    (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token)) (reward-token::transfer-create pool-id account (at "guard" (coin.details account)) to-pay)  )
                                    (if (> (at "balance" pool-data) 0.0) (with-capability (PRIVATE_RESERVE pool-id (get-token-key reward-token) ) (reward-token::transfer-create pool-id account (at "guard" (coin.details account)) (at "balance" pool-data) )  ) true)
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
                          (format "Rewarded {} with {} {} and unstaked {} {}" [account to-pay reward-token to-pay-stake stake-token])

                      )
                  )
                (format "{} has no rewards in {}" [account pool-id]))
                )
          (format "{} has no rewards in {}" [account pool-id]))
        )

        )
    )

    ;;///////////////////////
    ;;UTILITIES
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
      @doc " Calculates a users rewards in a pool "
      (let
              (
                  (pool-data (read pools pool-id ["reward-token" "balance"]))
                  (stake-id (get-stake-id-key account pool-id))
              )
              (let
                (
                  (stake (read stakes stake-id ["balance" "multiplier"]))
                  (token:module{fungible-v2} (at "reward-token" pool-data))
                  (pool-balance (at "balance" pool-data))
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

    (defun calculate-total-emitted-tokens (pool-id:string)
        @doc " Calculates the current number of tokens emitted by a pool "
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
    ;MORE UTILITIES
    ;//////////////////////////


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

    (defun get-pools ()
      (keys pools)
    )

    (defun get-pool-info (pool-id:string)
      (+ (+ (read pools pool-id) (read pools-usage pool-id)) { "apy" : (calculate-apy pool-id) } )
    )

    (defun get-user-stakes (pool-id:string account:string)
      (read stakes (get-stake-id-key account pool-id))
    )

    (defun get-pool-info-obj (pool-id:object{stakes-schema})
      (bind pool-id {
                      "pool-id" := id,
                      "balance" := stake_balance,
                      "last-updated" := stake_last-updated,
                      "rewards" := stake_rewards,
                      "last-claimed" := stake_last-claimed,
                      "last-withdraw" := stake_last-withdraw,
                      "multiplier" := stake_multiplier,
                      "start-time" := stake_start-time,
                      "account" := stake_account
                    }
                     (+
                       (+
                         (read pools id)
                         (read pools-usage id)
                        )
                        {
                          "stake_balance" : stake_balance,
                          "stake_last-updated" : stake_last-updated,
                          "stake_rewards" : stake_rewards,
                          "stake_last-claimed" : stake_last-claimed,
                          "stake_last-withdraw" : stake_last-withdraw,
                          "stake_multiplier" : stake_multiplier,
                          "stake_start-time" : stake_start-time,
                          "apy" : (calculate-apy id),
                          "stake_pending_rewards" : (calculate-rewards id stake_account)
                          }
                      )
      )
    )

    (defun get-pool-info-obj2 (pool-id:object{pools-schema})
      (bind pool-id { "id" := id }
      (+ (+ (read pools id) (read pools-usage id)) { "apy" : (calculate-apy id) } )
      )
    )

    (defun get-all-user-pools ( account:string )
      @doc " Get a detailed list of pools a user is staking in "
      (let ((x (select stakes ['pool-id 'balance 'last-updated 'rewards 'last-claimed 'last-withdraw 'account 'multiplier 'start-time]
          (and? (where 'account (= account))
            (where 'balance (< 0.0))))))
            (map (get-pool-info-obj) x ))
    )

    (defun get-all-pools ( )
      @doc " Get a detailed list of pools a user is staking "
      (let ((x (select pools ['id] (constantly true))))
            (map (get-pool-info-obj2) x ))
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

)


;(create-table free.factory-stake-fungiv2.pools)
;(create-table free.factory-stake-fungiv2.pools-usage)
;(create-table free.factory-stake-fungiv2.pool-user-stats)
;(create-table free.factory-stake-fungiv2.stakes)
