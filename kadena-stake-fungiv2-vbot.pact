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

    (defun kadena-stake-vault-guard:guard () (create-module-guard "kadena-stake-holdings"))

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
        apy:decimal
        balance:decimal
        reward-token:module{fungible-v2}
        stake-token:module{fungible-v2}
        account:string
        active:bool
        max-reward-per-account:decimal
        claim-wait-seconds:decimal
        max-reward-per-claim:decimal
        start-time:time
        reward-duration:decimal
        reward-amount:decimal
        apy-fixed:bool
        stakers:decimal
        withdraw-duration:decimal
    )

    (defschema pools-usage-schema
        @doc "Pool usage data"
        tokens-locked:decimal
        last-updated:time
        owed:decimal
        paid:decimal
        distributed:decimal
        multiplier:decimal
        next-multiplier:decimal
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
    ;apy: constant apy of pool reward, ex: 10.0
    ;reward-token: name of fungible-v2 reward token provided by pool creator, ex: coin
    ;stake-token: name of fungible-v2 token stakers must stake for reward token, ex: coin
    ;account: pool creator account, ex: k:mykaccount
    ;max-reward-per-account: max rewards a stakers account can ever claim in the pool, ex: 200
    ;claim-wait-seconds: minimum number of seconds between staker reward claims, ex: 0
    ;max-reward-per-claim: max rewards a staker account can claim per wait duration, ex: 200
    ;reward-duration: if apy is not fixed, this is time it takes for rewards to become available to stakers, ex: 86400
    ;reward-amount: if apy is not fixed, this is the amount of rewards available each reward-duration, ex: 10
    ;apy-fixed: true or false for creating a pool with fixed apy (use apy variable) or variable apy (use reward-duration/reward-amount)
    ;withdraw-duration: time in seconds a staker's tokens must be locked before a staker can withdraw

    (defun create-pool (id:string
                        name:string
                        apy:decimal
                        balance:decimal
                        reward-token:module{fungible-v2}
                        stake-token:module{fungible-v2}
                        account:string
                        max-reward-per-account:decimal
                        claim-wait-seconds:decimal
                        max-reward-per-claim:decimal
                        reward-duration:decimal
                        reward-amount:decimal
                        apy-fixed:bool
                        withdraw-duration:decimal )

        @doc "Creates a new pool where Stakers stake and earn fungible-v2 tokens"
        (with-capability (ACCOUNT_GUARD account)
            ;Enforce rules
            (enforce-pool-id id)
            (enforce-valid-id account)
            (enforce-valid-id id)
            (enforce-valid-name name)
            (enforce-unit balance (reward-token::precision))
            ;Create reward token account
            (reward-token::create-account id (kadena-stake-vault-guard))
            ;Transfer reward token
            (reward-token::transfer-create account id (kadena-stake-vault-guard) balance)
            ;If reward-token != stake-token then create a token account for stake-token too
            (if (= (get-token-key reward-token) (get-token-key stake-token)) true (stake-token::create-account id (kadena-stake-vault-guard)) )
            ;Insert pool
            (insert pools id
                {
                    "id": id,
                    "name": name,
                    "apy": apy,
                    "balance": balance,
                    "reward-token": reward-token,
                    "stake-token": stake-token,
                    "account": account,
                    "active": true,
                    "max-reward-per-account": max-reward-per-account,
                    "claim-wait-seconds": (if (= apy-fixed true) claim-wait-seconds reward-duration ),
                    "max-reward-per-claim": max-reward-per-claim,
                    "start-time": (at "block-time" (chain-data)),
                    "reward-duration": reward-duration,
                    "reward-amount": reward-amount,
                    "apy-fixed": apy-fixed,
                    "stakers": 0.0,
                    "withdraw-duration": withdraw-duration
                }
            )
            ;Insert pool blank usage
            (insert pools-usage id {
                "tokens-locked": 0.0,
                "last-updated": (at "block-time" (chain-data)),
                "owed": 0.0,
                "paid": 0.0,
                "distributed": 0.0,
                "multiplier": 1.0,
                "next-multiplier": 1.0
            })
            ;Return a message
            (format "Created pool {} with {} {}" [name balance reward-token])
        )
    )

    ;;;;; User Related

    (defun create-stake (pool-id:string account:string amount:decimal)
        @doc " Creates or adds a stake to a pool for a user, claiming rewards first if they are due"
        (with-capability (ACCOUNT_GUARD account)
        ;(This spot is for a future function for claiming rewards)
            (let
                (
                    (pool-data (read pools pool-id))
                    (stake-id (get-stake-id-key account pool-id))
                    (pool-usage-data (read pools-usage pool-id))
                )
                (let
                    (
                        (token:module{fungible-v2} (at "stake-token" pool-data))
                    )
                    ;Enforce active pool
                    (enforce (= (at "active" pool-data) true) "Staking pool is not active.")
                    ;Enforce stakers only
                    (enforce (!= (at "account" pool-data) account) "Pool owners may not stake their own pools.")
                    ;Transfer stake to pool
                    (token::transfer account pool-id amount)
                    ;Insert stake data
                    (with-default-read stakes stake-id
                      { "id" : stake-id, "pool-id" : pool-id, "balance" : 0.0, "last-updated" : (at "block-time" (chain-data)), "account" : account, "rewards" : 0.0, "last-claimed" : (at "block-time" (chain-data)), "last-withdraw": (at "block-time" (chain-data))  }
                      { "id" := t_id, "pool-id" := t_pool-id, "balance" := t_balance, "last-updated" := t_last-updated, "account" := t_account, "rewards" := t_rewards, "last-claimed" := t_last-claimed, "last-withdraw" := t_last-withdraw }
                      (write stakes stake-id {
                          "id": t_id,
                          "pool-id": t_pool-id,
                          "balance": (+ t_balance amount),
                          "last-updated": (at "block-time" (chain-data)),
                          "account": t_account,
                          "rewards": t_rewards,
                          "last-claimed": t_last-claimed,
                          "last-withdraw": t_last-withdraw,
                          "multiplier": (at "multiplier" pool-usage-data), ;THE MULTIPLIER IS RECORDED FROM THE POOL
                          "start-time": (at "block-time" (chain-data))
                      })

                      (update pools-usage pool-id
                          {
                              "tokens-locked": (+ (at "tokens-locked" pool-usage-data) amount),
                              "last-updated": (at "block-time" (chain-data)),

                              ;BELOW WE CALCULATE THE NEW MULTIPLIER
                              ;NEW MULTIPLIER = CURERNT MULTIPLIER + (TOKENS-EMITTED-PER-DAY / (TOTAL STAKED TOKENS))

                              "next-multiplier":
                                            (+
                                              (at "next-multiplier" pool-usage-data)    ;current pool multiplier
                                              (/
                                                (at "reward-amount" pool-data)          ;tokens emitted per day
                                                (+
                                                  (at "tokens-locked" pool-usage-data)  ;tokens already in pool
                                                  amount                                ;tokens staked by user
                                                )
                                              )
                                            )
                          }
                      )

                      ;If this staker's balance is 0, this staker is new, and we add +1 staker count to the pools data:
                      (if (= t_balance 0.0)
                        (update pools pool-id {"stakers": (+ (at "stakers" pool-data) 1.0) })
                        true
                      )

                    )

                    (format "Staked {} {} in pool {} with account {}" [amount (at "stake-token" pool-data)  pool-id account])
               )
            )
        )
    )





    ;;///////////////////////
    ;;UTILITIES / GETTERS
    ;;//////////////////////


    (defun calculate-multiplier (pool-id:string account:string)
      @doc " Calculates a pools current multiplier "
      (let
              (

                  (pool-data (read pools pool-id))
                  (pool-usage-data (read pools-usage pool-id))
                  (stake-id (get-stake-id-key account pool-id))
              )
              ;(days-since-last-update (floor (/ (diff-time  (at "block-time" (chain-data)) (at "last-updated" pool-usage-data)) 86400) 0)  )
              (let*
                  (
                    (stake (read stakes stake-id))
                    (days-since-last-update (floor (/ (diff-time  (at "block-time" (chain-data)) (at "last-updated" pool-usage-data)) 86400) 0)  )
                  )
                  (+ (at "multiplier" pool-usage-data) (/ (* days-since-last-update (at "reward-amount" pool-data) ) (at "tokens-locked" pool-usage-data) ) )

                  ;Testing below:
                  ;(+ (at "multiplier" pool-usage-data) (/ (* days-since-update (at "reward-amount" pool-data) ) (at "tokens-locked" pool-usage-data) ) )
                  ;days-since-update
                  ;time-since-update
                  ;(+ 1.10 (/ (* 1.0 10.0 )  200) )
                  ;(+ 1.0 (/ (* (- 2.0 1.0) 10.0 )  100) )
              )
      )
    )

    (defun calculate-rewards (pool-id:string account:string)
      @doc " Calculates a pools rewards for a user "
      (let
              (

                  (pool-data (read pools pool-id))
                  (pool-usage-data (read pools-usage pool-id))
                  (stake-id (get-stake-id-key account pool-id))
              )
              (let*
                (
                  (stake (read stakes stake-id))
                  (days-since-stake (floor (/ (diff-time  (at "block-time" (chain-data)) (at "last-updated" stake)) 86400 ) 0)  )
                  (token:module{fungible-v2} (at "reward-token" pool-data))
                )

                (if (>= days-since-stake 1.0)
                  (- (* (at "balance" stake) (- (calculate-multiplier pool-id account) (at "multiplier" stake) ) ) (at "reward-amount" pool-data)  )
                  0.0
                )

                (floor (* (at "balance" stake) (- (calculate-multiplier pool-id account) (at "multiplier" stake) ) ) (token::precision))



                ;Testing below:
                ;(* (at "balance" stake) (- (calculate-multiplier pool-id) (at "multiplier" stake) ) )
                ;(- (* (at "balance" stake) (- (calculate-multiplier pool-id) (at "multiplier" stake) ) ) 10.0  )
                ;(- (* (at "balance" stake) (- (calculate-multiplier pool-id) (at "multiplier" stake) ) ) (calculate-available-now (at "start-time" stake) "test-pool" ) )

                ;(*  (* (at "balance" stake) (- (at "multiplier" pool-usage-data) (at "multiplier" stake) ) ) days-since-stake )

                ;(at "multiplier" pool-usage-data)

                ;(- (* (at "balance" stake) (- (calculate-multiplier pool-id) (at "multiplier" stake) ) ) ( (/ (at "reward-amount" pool-data) (at "tokens-locked" pool-usage-data) )  )
              )
      )
    )

    (defun update-pool-rewards (pool-id:string)
      @doc " Updates a pools rewards multiplier "
      (let
          (
              (pool-data (read pools pool-id))
              (pool-usage-data (read pools-usage pool-id))
          )
          (let*
                  (
                    (days-since-last-update (floor (/ (diff-time  (at "block-time" (chain-data)) (at "last-updated" pool-usage-data)) 86400) 0)  )
                  )
                  (if (>= days-since-last-update 1.0)

                  (update pools-usage pool-id
                      {
                          "last-updated": (at "block-time" (chain-data)),

                          ;BELOW WE CALCULATE THE NEW MULTIPLIER
                          ;NEW MULTIPLIER = CURERNT MULTIPLIER + (TOKENS-EMITTED-PER-DAY / (TOTAL STAKED TOKENS))

                          "multiplier": (+
                                          (at "multiplier" pool-usage-data)         ;current pool multiplier
                                          (/
                                            (at "reward-amount" pool-data)          ;tokens emitted per day
                                            (at "tokens-locked" pool-usage-data)   ;tokens in pool
                                          )
                                        )
                      }
                  )

                  (format "{} is not ready to be updated" [pool-id])
                  )

          )

      )
    )




;///////////////////////////
;OLD UTILITIES
;//////////////////////////


    (defun calculate-total-emitted-tokens (pool-id:string)
        @doc " Calculates tokens emitted by pool since it began "
        (let
            (
                (pool-data (read pools pool-id ["balance" "reward-duration" "reward-amount" "reward-token" "start-time"]))
                (pool-usage-data (read pools-usage pool-id ["owed"]))
            )
            (let
                (
                    (token:module{fungible-v2} (at "reward-token" pool-data))
                    (time-passed (diff-time  (at "block-time" (chain-data)) (at "start-time" pool-data) ) )
                )
                ;We clamp our reward calculation under the pool's available balance so we don't exceed it
                (let
                    (
                        (total-available (floor (* (/ time-passed (at "reward-duration" pool-data)) (at "reward-amount" pool-data)) (token::precision) ))
                        (max-available (floor (- (at "balance" pool-data) (at "owed" pool-usage-data) ) (token::precision)))
                    )
                    (enforce ( >= time-passed 0.0) "From date is in the future")
                    (if (<= total-available max-available ) total-available max-available)
                )
            )
        )
    )

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

  (defun get-token-key
    ( tokenA:module{fungible-v2} )
    " Create key from token module"
    (format "{}" [tokenA])
  )

  (defun initialize ()
    @doc " Initialize the contract. Can only happen once. "
    (coin.create-account KDS_BANK (create-module-guard "kadena-stake-tokens-bank"))
  )

)

;(create-table free.kadena-stake-fungiv2.pools)
;(create-table free.kadena-stake-fungiv2.pools-usage)
;(create-table free.kadena-stake-fungiv2.pool-user-stats)
;(create-table free.kadena-stake-fungiv2.stakes)
;(free.kadena-stake-fungiv2.initialize)
