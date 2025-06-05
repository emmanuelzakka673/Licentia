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

(define-data-var license-id-nonce uint u0)
(define-data-var platform-fee uint u50)

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