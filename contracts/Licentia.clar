(define-trait nft-trait
  (
    (get-owner (uint) (response (optional principal) uint))
    (transfer (uint principal principal) (response bool uint))
  )
)

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_LICENSE (err u103))
(define-constant ERR_EXPIRED_LICENSE (err u104))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u105))
(define-constant ERR_AUCTION_NOT_FOUND (err u106))
(define-constant ERR_AUCTION_ENDED (err u107))
(define-constant ERR_AUCTION_ACTIVE (err u108))
(define-constant ERR_BID_TOO_LOW (err u109))
(define-constant ERR_NO_BIDS (err u110))
(define-constant ERR_CANNOT_BID_OWN_AUCTION (err u111))
(define-constant ERR_RESERVE_NOT_MET (err u112))
(define-constant ERR_NOT_STAKED (err u113))
(define-constant ERR_ALREADY_STAKED (err u114))
(define-constant ERR_INVALID_STAKE_PERIOD (err u115))
(define-constant ERR_STAKE_LOCKED (err u116))
(define-constant ERR_INSUFFICIENT_REWARDS (err u117))
(define-constant ERR_INVALID_POOL (err u118))

(define-data-var license-id-nonce uint u0)
(define-data-var platform-fee uint u50)
(define-data-var auction-id-nonce uint u0)
(define-data-var total-staked-licenses uint u0)

(define-map licenses
  uint
  {
    nft-contract: principal,
    token-id: uint,
    license-type: (string-ascii 50),
    terms: (string-utf8 500),
    price: uint,
    duration: uint,
    created-at: uint,
    creator: principal,
    active: bool
  }
)

(define-map license-holders
  { license-id: uint, holder: principal }
  {
    purchased-at: uint,
    expires-at: uint,
    usage-count: uint,
    max-usage: uint
  }
)

(define-map nft-licenses
  { contract: principal, token-id: uint }
  (list 10 uint)
)

(define-map license-templates
  (string-ascii 50)
  {
    default-terms: (string-utf8 500),
    base-price: uint,
    max-duration: uint
  }
)

(define-map creator-earnings principal uint)

(define-map license-auctions
  uint
  {
    license-id: uint,
    auctioneer: principal,
    start-price: uint,
    reserve-price: uint,
    buy-now-price: (optional uint),
    start-block: uint,
    end-block: uint,
    highest-bid: uint,
    highest-bidder: (optional principal),
    total-bids: uint,
    concluded: bool
  }
)

(define-map auction-bids
  { auction-id: uint, bidder: principal }
  {
    bid-amount: uint,
    bid-block: uint,
    refunded: bool
  }
)

(define-map auction-bid-history
  uint
  (list 50 { bidder: principal, amount: uint, block: uint })
)

(define-map bidder-escrow principal uint)

(define-map staked-licenses
  { license-id: uint, holder: principal }
  {
    stake-amount: uint,
    stake-start: uint,
    stake-period: uint,
    lock-end: uint,
    reward-rate: uint,
    last-claim: uint,
    total-rewards: uint
  }
)

(define-map license-stake-pools
  uint
  {
    creator: principal,
    total-pool: uint,
    reward-per-block: uint,
    min-stake-period: uint,
    bonus-multiplier: uint,
    active: bool,
    total-stakers: uint,
    pool-end: uint
  }
)

(define-map staker-rewards
  principal
  {
    total-earned: uint,
    total-claimed: uint,
    active-stakes: uint
  }
)

(define-map stake-multipliers
  uint
  uint
)

(define-public (create-stake-pool
  (license-id uint)
  (total-pool-amount uint)
  (reward-per-block uint)
  (min-stake-period uint)
  (bonus-multiplier uint)
  (pool-duration uint))
  (let
    (
      (license-data (unwrap! (map-get? licenses license-id) ERR_NOT_FOUND))
      (pool-end-block (+ stacks-block-height pool-duration))
    )
    (asserts! (is-eq tx-sender (get creator license-data)) ERR_NOT_AUTHORIZED)
    (asserts! (> total-pool-amount u0) ERR_INVALID_POOL)
    (asserts! (> reward-per-block u0) ERR_INVALID_POOL)
    (asserts! (> min-stake-period u0) ERR_INVALID_STAKE_PERIOD)
    (asserts! (>= (stx-get-balance tx-sender) total-pool-amount) ERR_INSUFFICIENT_PAYMENT)
    
    (try! (stx-transfer? total-pool-amount tx-sender (as-contract tx-sender)))
    
    (map-set license-stake-pools license-id
      {
        creator: tx-sender,
        total-pool: total-pool-amount,
        reward-per-block: reward-per-block,
        min-stake-period: min-stake-period,
        bonus-multiplier: bonus-multiplier,
        active: true,
        total-stakers: u0,
        pool-end: pool-end-block
      }
    )
    
    (map-set stake-multipliers u144 u110)
    (map-set stake-multipliers u1008 u125)
    (map-set stake-multipliers u4320 u150)
    (ok true)
  )
)

(define-public (stake-license 
  (license-id uint)
  (stake-period uint))
  (let
    (
      (holder-key { license-id: license-id, holder: tx-sender })
      (license-data (unwrap! (map-get? licenses license-id) ERR_NOT_FOUND))
      (holder-data (unwrap! (map-get? license-holders holder-key) ERR_NOT_AUTHORIZED))
      (pool-data (unwrap! (map-get? license-stake-pools license-id) ERR_INVALID_POOL))
      (stake-key { license-id: license-id, holder: tx-sender })
      (license-price (get price license-data))
      (lock-end-block (+ stacks-block-height stake-period))
      (multiplier (calculate-multiplier stake-period))
      (reward-rate (/ (* (get reward-per-block pool-data) multiplier) u100))
      (current-staker-rewards (default-to { total-earned: u0, total-claimed: u0, active-stakes: u0 } (map-get? staker-rewards tx-sender)))
    )
    (asserts! (get active pool-data) ERR_INVALID_POOL)
    (asserts! (< stacks-block-height (get expires-at holder-data)) ERR_EXPIRED_LICENSE)
    (asserts! (>= stake-period (get min-stake-period pool-data)) ERR_INVALID_STAKE_PERIOD)
    (asserts! (< stacks-block-height (get pool-end pool-data)) ERR_INVALID_POOL)
    (asserts! (is-none (map-get? staked-licenses stake-key)) ERR_ALREADY_STAKED)
    
    (map-set staked-licenses stake-key
      {
        stake-amount: license-price,
        stake-start: stacks-block-height,
        stake-period: stake-period,
        lock-end: lock-end-block,
        reward-rate: reward-rate,
        last-claim: stacks-block-height,
        total-rewards: u0
      }
    )
    
    (map-set license-stake-pools license-id
      (merge pool-data { total-stakers: (+ (get total-stakers pool-data) u1) })
    )
    
    (map-set staker-rewards tx-sender
      (merge current-staker-rewards { active-stakes: (+ (get active-stakes current-staker-rewards) u1) })
    )
    
    (var-set total-staked-licenses (+ (var-get total-staked-licenses) u1))
    (ok true)
  )
)

(define-public (unstake-license (license-id uint))
  (let
    (
      (stake-key { license-id: license-id, holder: tx-sender })
      (stake-data (unwrap! (map-get? staked-licenses stake-key) ERR_NOT_STAKED))
      (pool-data (unwrap! (map-get? license-stake-pools license-id) ERR_INVALID_POOL))
      (current-staker-rewards (default-to { total-earned: u0, total-claimed: u0, active-stakes: u0 } (map-get? staker-rewards tx-sender)))
      (blocks-staked (- stacks-block-height (get stake-start stake-data)))
      (earned-rewards (/ (* (get reward-rate stake-data) blocks-staked) u144))
      (early-penalty (if (< stacks-block-height (get lock-end stake-data)) (/ earned-rewards u4) u0))
      (final-rewards (- earned-rewards early-penalty))
    )
    (asserts! (>= (get total-pool pool-data) final-rewards) ERR_INSUFFICIENT_REWARDS)
    
    (if (> final-rewards u0)
      (unwrap! (as-contract (stx-transfer? final-rewards tx-sender tx-sender)) ERR_INSUFFICIENT_REWARDS)
      true
    )
    
    (map-delete staked-licenses stake-key)
    
    (map-set license-stake-pools license-id
      (merge pool-data { 
        total-stakers: (- (get total-stakers pool-data) u1),
        total-pool: (- (get total-pool pool-data) final-rewards)
      })
    )
    
    (map-set staker-rewards tx-sender
      (merge current-staker-rewards { 
        total-earned: (+ (get total-earned current-staker-rewards) final-rewards),
        total-claimed: (+ (get total-claimed current-staker-rewards) final-rewards),
        active-stakes: (- (get active-stakes current-staker-rewards) u1)
      })
    )
    
    (var-set total-staked-licenses (- (var-get total-staked-licenses) u1))
    (ok final-rewards)
  )
)

(define-public (claim-staking-rewards (license-id uint))
  (let
    (
      (stake-key { license-id: license-id, holder: tx-sender })
      (stake-data (unwrap! (map-get? staked-licenses stake-key) ERR_NOT_STAKED))
      (pool-data (unwrap! (map-get? license-stake-pools license-id) ERR_INVALID_POOL))
      (current-staker-rewards (default-to { total-earned: u0, total-claimed: u0, active-stakes: u0 } (map-get? staker-rewards tx-sender)))
      (blocks-since-claim (- stacks-block-height (get last-claim stake-data)))
      (pending-rewards (/ (* (get reward-rate stake-data) blocks-since-claim) u144))
    )
    (asserts! (> pending-rewards u0) ERR_INSUFFICIENT_REWARDS)
    (asserts! (>= (get total-pool pool-data) pending-rewards) ERR_INSUFFICIENT_REWARDS)
    
    (try! (as-contract (stx-transfer? pending-rewards tx-sender tx-sender)))
    
    (map-set staked-licenses stake-key
      (merge stake-data { 
        last-claim: stacks-block-height,
        total-rewards: (+ (get total-rewards stake-data) pending-rewards)
      })
    )
    
    (map-set license-stake-pools license-id
      (merge pool-data { total-pool: (- (get total-pool pool-data) pending-rewards) })
    )
    
    (map-set staker-rewards tx-sender
      (merge current-staker-rewards { 
        total-earned: (+ (get total-earned current-staker-rewards) pending-rewards),
        total-claimed: (+ (get total-claimed current-staker-rewards) pending-rewards)
      })
    )
    
    (ok pending-rewards)
  )
)

(define-private (calculate-multiplier (stake-period uint))
  (if (>= stake-period u4320)
    u150
    (if (>= stake-period u1008)
      u125
      (if (>= stake-period u144)
        u110
        u100
      )
    )
  )
)

(define-public (create-license-auction
  (license-id uint)
  (start-price uint)
  (reserve-price uint)
  (buy-now-price (optional uint))
  (duration-blocks uint))
  (let
    (
      (license-data (unwrap! (map-get? licenses license-id) ERR_NOT_FOUND))
      (auction-id (+ (var-get auction-id-nonce) u1))
      (end-block (+ stacks-block-height duration-blocks))
    )
    (asserts! (is-eq tx-sender (get creator license-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get active license-data) ERR_INVALID_LICENSE)
    (asserts! (> start-price u0) ERR_INVALID_LICENSE)
    (asserts! (>= reserve-price start-price) ERR_INVALID_LICENSE)
    (asserts! (> duration-blocks u0) ERR_INVALID_LICENSE)
    
    (map-set license-auctions auction-id
      {
        license-id: license-id,
        auctioneer: tx-sender,
        start-price: start-price,
        reserve-price: reserve-price,
        buy-now-price: buy-now-price,
        start-block: stacks-block-height,
        end-block: end-block,
        highest-bid: u0,
        highest-bidder: none,
        total-bids: u0,
        concluded: false
      }
    )
    
    (map-set auction-bid-history auction-id (list))
    (var-set auction-id-nonce auction-id)
    (ok auction-id)
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let
    (
      (auction-data (unwrap! (map-get? license-auctions auction-id) ERR_AUCTION_NOT_FOUND))
      (current-balance (stx-get-balance tx-sender))
      (current-escrow (default-to u0 (map-get? bidder-escrow tx-sender)))
      (bid-history (default-to (list) (map-get? auction-bid-history auction-id)))
      (bid-entry { bidder: tx-sender, amount: bid-amount, block: stacks-block-height })
    )
    (asserts! (not (get concluded auction-data)) ERR_AUCTION_ENDED)
    (asserts! (< stacks-block-height (get end-block auction-data)) ERR_AUCTION_ENDED)
    (asserts! (not (is-eq tx-sender (get auctioneer auction-data))) ERR_CANNOT_BID_OWN_AUCTION)
    (asserts! (> bid-amount (if (> (get highest-bid auction-data) (get start-price auction-data)) (get highest-bid auction-data) (get start-price auction-data))) ERR_BID_TOO_LOW)
    (asserts! (>= current-balance bid-amount) ERR_INSUFFICIENT_PAYMENT)
    
    (match (get buy-now-price auction-data)
      buy-now-value
        (if (>= bid-amount buy-now-value)
          (try! (conclude-auction-internal auction-id tx-sender bid-amount))
          (begin
            (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
            (map-set bidder-escrow tx-sender (+ current-escrow bid-amount))
            (map-set auction-bids { auction-id: auction-id, bidder: tx-sender }
              { bid-amount: bid-amount, bid-block: stacks-block-height, refunded: false })
            (map-set license-auctions auction-id
              (merge auction-data 
                { 
                  highest-bid: bid-amount,
                  highest-bidder: (some tx-sender),
                  total-bids: (+ (get total-bids auction-data) u1)
                }))
            (map-set auction-bid-history auction-id
              (unwrap! (as-max-len? (append bid-history bid-entry) u50) ERR_INVALID_LICENSE))
          )
        )
      (begin
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        (map-set bidder-escrow tx-sender (+ current-escrow bid-amount))
        (map-set auction-bids { auction-id: auction-id, bidder: tx-sender }
          { bid-amount: bid-amount, bid-block: stacks-block-height, refunded: false })
        (map-set license-auctions auction-id
          (merge auction-data 
            { 
              highest-bid: bid-amount,
              highest-bidder: (some tx-sender),
              total-bids: (+ (get total-bids auction-data) u1)
            }))
        (map-set auction-bid-history auction-id
          (unwrap! (as-max-len? (append bid-history bid-entry) u50) ERR_INVALID_LICENSE))
      )
    )
    (ok true)
  )
)

(define-public (conclude-auction (auction-id uint))
  (let
    (
      (auction-data (unwrap! (map-get? license-auctions auction-id) ERR_AUCTION_NOT_FOUND))
    )
    (asserts! (>= stacks-block-height (get end-block auction-data)) ERR_AUCTION_ACTIVE)
    (asserts! (not (get concluded auction-data)) ERR_AUCTION_ENDED)
    
    (match (get highest-bidder auction-data)
      winner-principal
        (begin
          (asserts! (>= (get highest-bid auction-data) (get reserve-price auction-data)) ERR_RESERVE_NOT_MET)
          (conclude-auction-internal auction-id winner-principal (get highest-bid auction-data))
        )
      (begin
        (map-set license-auctions auction-id (merge auction-data { concluded: true }))
        (ok false)
      )
    )
  )
)

(define-private (conclude-auction-internal (auction-id uint) (winner principal) (winning-bid uint))
  (let
    (
      (auction-data (unwrap! (map-get? license-auctions auction-id) ERR_AUCTION_NOT_FOUND))
      (license-data (unwrap! (map-get? licenses (get license-id auction-data)) ERR_NOT_FOUND))
      (platform-cut (/ (* winning-bid (var-get platform-fee)) u1000))
      (creator-cut (- winning-bid platform-cut))
      (expires-at (+ stacks-block-height (get duration license-data)))
      (winner-escrow (default-to u0 (map-get? bidder-escrow winner)))
    )
    (try! (as-contract (stx-transfer? creator-cut tx-sender (get creator license-data))))
    (try! (as-contract (stx-transfer? platform-cut tx-sender CONTRACT_OWNER)))
    
    (map-set license-holders
      { license-id: (get license-id auction-data), holder: winner }
      {
        purchased-at: stacks-block-height,
        expires-at: expires-at,
        usage-count: u0,
        max-usage: u100
      }
    )
    
    (map-set creator-earnings 
      (get creator license-data)
      (+ (default-to u0 (map-get? creator-earnings (get creator license-data))) creator-cut)
    )
    
    (map-set bidder-escrow winner (- winner-escrow winning-bid))
    (map-set auction-bids { auction-id: auction-id, bidder: winner }
      { bid-amount: winning-bid, bid-block: stacks-block-height, refunded: false })
    
    (map-set license-auctions auction-id (merge auction-data { concluded: true }))
    (ok true)
  )
)

(define-public (refund-losing-bids (auction-id uint) (bidders (list 20 principal)))
  (let
    (
      (auction-data (unwrap! (map-get? license-auctions auction-id) ERR_AUCTION_NOT_FOUND))
    )
    (asserts! (get concluded auction-data) ERR_AUCTION_ACTIVE)
    (ok (map refund-bidder-helper (zip bidders (list auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id auction-id))))
  )
)

(define-private (refund-bidder-helper (bidder-data { bidder: principal, auction-id: uint }))
  (let
    (
      (bidder (get bidder bidder-data))
      (auction-id (get auction-id bidder-data))
      (auction-data (unwrap! (map-get? license-auctions auction-id) ERR_AUCTION_NOT_FOUND))
      (bid-data (map-get? auction-bids { auction-id: auction-id, bidder: bidder }))
      (current-escrow (default-to u0 (map-get? bidder-escrow bidder)))
    )
    (match bid-data
      bid-info
        (if (and 
              (not (get refunded bid-info))
              (not (is-eq (some bidder) (get highest-bidder auction-data))))
          (begin
            (map-set bidder-escrow bidder (- current-escrow (get bid-amount bid-info)))
            (map-set auction-bids { auction-id: auction-id, bidder: bidder }
              (merge bid-info { refunded: true }))
            (as-contract (stx-transfer? (get bid-amount bid-info) tx-sender bidder))
          )
          (ok true)
        )
      (ok true)
    )
  )
)

(define-private (zip (list-a (list 20 principal)) (list-b (list 20 uint)))
  (map create-bidder-auction-pair list-a list-b)
)

(define-private (create-bidder-auction-pair (bidder principal) (auction-id uint))
  { bidder: bidder, auction-id: auction-id }
)

(define-public (create-license 
  (nft-contract principal)
  (token-id uint)
  (license-type (string-ascii 50))
  (terms (string-utf8 500))
  (price uint)
  (duration uint)
  (max-usage uint))
  (let
    (
      (license-id (+ (var-get license-id-nonce) u1))
      (current-licenses (default-to (list) (map-get? nft-licenses { contract: nft-contract, token-id: token-id })))
    )
    (asserts! (> price u0) ERR_INVALID_LICENSE)
    (asserts! (> duration u0) ERR_INVALID_LICENSE)
    (asserts! (< (len current-licenses) u10) ERR_INVALID_LICENSE)
    
    (map-set licenses license-id
      {
        nft-contract: nft-contract,
        token-id: token-id,
        license-type: license-type,
        terms: terms,
        price: price,
        duration: duration,
        created-at: stacks-block-height,
        creator: tx-sender,
        active: true
      }
    )
    
    (map-set nft-licenses 
      { contract: nft-contract, token-id: token-id }
      (unwrap! (as-max-len? (append current-licenses license-id) u10) ERR_INVALID_LICENSE)
    )
    
    (var-set license-id-nonce license-id)
    (ok license-id)
  )
)

(define-public (purchase-license (license-id uint))
  (let
    (
      (license-data (unwrap! (map-get? licenses license-id) ERR_NOT_FOUND))
      (license-price (get price license-data))
      (platform-cut (/ (* license-price (var-get platform-fee)) u1000))
      (creator-cut (- license-price platform-cut))
      (expires-at (+ stacks-block-height (get duration license-data)))
    )
    (asserts! (get active license-data) ERR_INVALID_LICENSE)
    (asserts! (>= (stx-get-balance tx-sender) license-price) ERR_INSUFFICIENT_PAYMENT)
    
    (try! (stx-transfer? creator-cut tx-sender (get creator license-data)))
    (try! (stx-transfer? platform-cut tx-sender CONTRACT_OWNER))
    
    (map-set license-holders
      { license-id: license-id, holder: tx-sender }
      {
        purchased-at: stacks-block-height,
        expires-at: expires-at,
        usage-count: u0,
        max-usage: u100
      }
    )
    
    (map-set creator-earnings 
      (get creator license-data)
      (+ (default-to u0 (map-get? creator-earnings (get creator license-data))) creator-cut)
    )
    
    (ok true)
  )
)

(define-public (use-license (license-id uint))
  (let
    (
      (holder-key { license-id: license-id, holder: tx-sender })
      (holder-data (unwrap! (map-get? license-holders holder-key) ERR_NOT_AUTHORIZED))
      (current-usage (get usage-count holder-data))
    )
    (asserts! (< stacks-block-height (get expires-at holder-data)) ERR_EXPIRED_LICENSE)
    (asserts! (< current-usage (get max-usage holder-data)) ERR_INVALID_LICENSE)
    
    (map-set license-holders holder-key
      (merge holder-data { usage-count: (+ current-usage u1) })
    )
    
    (ok true)
  )
)

(define-public (revoke-license (license-id uint))
  (let
    (
      (license-data (unwrap! (map-get? licenses license-id) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator license-data)) ERR_NOT_AUTHORIZED)
    
    (map-set licenses license-id
      (merge license-data { active: false })
    )
    
    (ok true)
  )
)

(define-public (update-license-price (license-id uint) (new-price uint))
  (let
    (
      (license-data (unwrap! (map-get? licenses license-id) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator license-data)) ERR_NOT_AUTHORIZED)
    (asserts! (> new-price u0) ERR_INVALID_LICENSE)
    
    (map-set licenses license-id
      (merge license-data { price: new-price })
    )
    
    (ok true)
  )
)

(define-public (create-license-template 
  (template-name (string-ascii 50))
  (default-terms (string-utf8 500))
  (base-price uint)
  (max-duration uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    (map-set license-templates template-name
      {
        default-terms: default-terms,
        base-price: base-price,
        max-duration: max-duration
      }
    )
    
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee u200) ERR_INVALID_LICENSE)
    
    (var-set platform-fee new-fee)
    (ok true)
  )
)

(define-read-only (get-license (license-id uint))
  (map-get? licenses license-id)
)

(define-read-only (get-license-holder (license-id uint) (holder principal))
  (map-get? license-holders { license-id: license-id, holder: holder })
)

(define-read-only (get-nft-licenses (nft-contract principal) (token-id uint))
  (map-get? nft-licenses { contract: nft-contract, token-id: token-id })
)

(define-read-only (get-license-template (template-name (string-ascii 50)))
  (map-get? license-templates template-name)
)

(define-read-only (get-creator-earnings (creator principal))
  (default-to u0 (map-get? creator-earnings creator))
)

(define-read-only (is-license-valid (license-id uint) (holder principal))
  (match (map-get? license-holders { license-id: license-id, holder: holder })
    holder-data (< stacks-block-height (get expires-at holder-data))
    false
  )
)

(define-read-only (get-license-usage (license-id uint) (holder principal))
  (match (map-get? license-holders { license-id: license-id, holder: holder })
    holder-data (get usage-count holder-data)
    u0
  )
)

(define-read-only (can-use-license (license-id uint) (holder principal))
  (match (map-get? license-holders { license-id: license-id, holder: holder })
    holder-data 
      (and 
        (< stacks-block-height (get expires-at holder-data))
        (< (get usage-count holder-data) (get max-usage holder-data))
      )
    false
  )
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-read-only (get-total-licenses)
  (var-get license-id-nonce)
)

(define-read-only (get-auction (auction-id uint))
  (map-get? license-auctions auction-id)
)

(define-read-only (get-auction-bid (auction-id uint) (bidder principal))
  (map-get? auction-bids { auction-id: auction-id, bidder: bidder })
)

(define-read-only (get-auction-bid-history (auction-id uint))
  (map-get? auction-bid-history auction-id)
)

(define-read-only (get-bidder-escrow (bidder principal))
  (default-to u0 (map-get? bidder-escrow bidder))
)

(define-read-only (is-auction-active (auction-id uint))
  (match (map-get? license-auctions auction-id)
    auction-data
      (and 
        (not (get concluded auction-data))
        (< stacks-block-height (get end-block auction-data))
      )
    false
  )
)

(define-read-only (get-auction-winner (auction-id uint))
  (match (map-get? license-auctions auction-id)
    auction-data
      (if (get concluded auction-data)
        (get highest-bidder auction-data)
        none
      )
    none
  )
)

(define-read-only (get-total-auctions)
  (var-get auction-id-nonce)
)

(define-read-only (get-auction-time-remaining (auction-id uint))
  (match (map-get? license-auctions auction-id)
    auction-data
      (if (< stacks-block-height (get end-block auction-data))
        (some (- (get end-block auction-data) stacks-block-height))
        none
      )
    none
  )
)

(define-read-only (get-stake-info (license-id uint) (holder principal))
  (map-get? staked-licenses { license-id: license-id, holder: holder })
)

(define-read-only (get-stake-pool (license-id uint))
  (map-get? license-stake-pools license-id)
)

(define-read-only (get-staker-rewards (staker principal))
  (default-to { total-earned: u0, total-claimed: u0, active-stakes: u0 } (map-get? staker-rewards staker))
)

(define-read-only (calculate-pending-rewards (license-id uint) (holder principal))
  (match (map-get? staked-licenses { license-id: license-id, holder: holder })
    stake-data
      (let
        (
          (blocks-since-claim (- stacks-block-height (get last-claim stake-data)))
          (pending-rewards (/ (* (get reward-rate stake-data) blocks-since-claim) u144))
        )
        (some pending-rewards)
      )
    none
  )
)

(define-read-only (is-stake-locked (license-id uint) (holder principal))
  (match (map-get? staked-licenses { license-id: license-id, holder: holder })
    stake-data (< stacks-block-height (get lock-end stake-data))
    false
  )
)

(define-read-only (get-stake-time-remaining (license-id uint) (holder principal))
  (match (map-get? staked-licenses { license-id: license-id, holder: holder })
    stake-data
      (if (< stacks-block-height (get lock-end stake-data))
        (some (- (get lock-end stake-data) stacks-block-height))
        none
      )
    none
  )
)

(define-read-only (get-total-staked-licenses)
  (var-get total-staked-licenses)
)

(define-read-only (get-stake-multiplier (period uint))
  (calculate-multiplier period)
)



