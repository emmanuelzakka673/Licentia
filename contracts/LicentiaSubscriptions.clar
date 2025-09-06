;; title: LicentiaSubscriptions
;; version: 1.0.0
;; summary: Subscription-based licensing system for Licentia
;; description: Enables recurring license access through subscription plans

;; Constants
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_NOT_FOUND (err u201))
(define-constant ERR_ALREADY_EXISTS (err u202))
(define-constant ERR_INVALID_SUBSCRIPTION (err u203))
(define-constant ERR_SUBSCRIPTION_EXPIRED (err u204))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u205))
(define-constant ERR_SUBSCRIPTION_CANCELLED (err u206))
(define-constant ERR_INVALID_PERIOD (err u207))

;; Data vars
(define-data-var subscription-id-nonce uint u0)
(define-data-var contract-owner principal tx-sender)

;; Subscription plans data
(define-map subscription-plans
  uint
  {
    creator: principal,
    license-id: uint,
    plan-name: (string-ascii 50),
    price-per-period: uint,
    period-duration: uint,
    max-subscribers: uint,
    current-subscribers: uint,
    active: bool,
    created-at: uint
  }
)

;; Active subscriptions
(define-map active-subscriptions
  { plan-id: uint, subscriber: principal }
  {
    started-at: uint,
    current-period-start: uint,
    current-period-end: uint,
    auto-renew: bool,
    total-periods: uint,
    total-paid: uint,
    status: (string-ascii 20)
  }
)

;; Subscription usage tracking
(define-map subscription-usage
  { plan-id: uint, subscriber: principal, period: uint }
  {
    usage-count: uint,
    first-use: uint,
    last-use: uint
  }
)

;; Subscription payments history
(define-map subscription-payments
  { plan-id: uint, subscriber: principal, payment-id: uint }
  {
    amount: uint,
    period-covered: uint,
    paid-at: uint,
    payment-method: (string-ascii 20)
  }
)

;; Creator subscription analytics
(define-map creator-subscription-stats
  principal
  {
    total-plans: uint,
    active-plans: uint,
    total-subscribers: uint,
    total-revenue: uint,
    avg-retention-rate: uint
  }
)

;; Create subscription plan
(define-public (create-subscription-plan
  (license-id uint)
  (plan-name (string-ascii 50))
  (price-per-period uint)
  (period-duration uint)
  (max-subscribers uint))
  (let (
    (plan-id (+ (var-get subscription-id-nonce) u1))
    (current-block stacks-block-height)
  )
    (asserts! (> price-per-period u0) ERR_INVALID_SUBSCRIPTION)
    (asserts! (> period-duration u144) ERR_INVALID_PERIOD) ;; At least 1 day
    (asserts! (> max-subscribers u0) ERR_INVALID_SUBSCRIPTION)
    
    ;; Verify license ownership through main contract
    (let (
      (license-data (unwrap! (contract-call? .Licentia get-license license-id) ERR_NOT_FOUND))
    )
      (asserts! (is-eq tx-sender (get creator license-data)) ERR_NOT_AUTHORIZED)
    )
    
    (map-set subscription-plans plan-id
      {
        creator: tx-sender,
        license-id: license-id,
        plan-name: plan-name,
        price-per-period: price-per-period,
        period-duration: period-duration,
        max-subscribers: max-subscribers,
        current-subscribers: u0,
        active: true,
        created-at: current-block
      }
    )
    
    ;; Update creator stats
    (let (
      (current-stats (default-to 
        { total-plans: u0, active-plans: u0, total-subscribers: u0, total-revenue: u0, avg-retention-rate: u100 }
        (map-get? creator-subscription-stats tx-sender)
      ))
    )
      (map-set creator-subscription-stats tx-sender
        (merge current-stats {
          total-plans: (+ (get total-plans current-stats) u1),
          active-plans: (+ (get active-plans current-stats) u1)
        })
      )
    )
    
    (var-set subscription-id-nonce plan-id)
    (ok plan-id)
  )
)

;; Subscribe to a plan
(define-public (subscribe-to-plan (plan-id uint) (auto-renew bool))
  (let (
    (plan-data (unwrap! (map-get? subscription-plans plan-id) ERR_NOT_FOUND))
    (current-block stacks-block-height)
    (period-end (+ current-block (get period-duration plan-data)))
    (subscription-key { plan-id: plan-id, subscriber: tx-sender })
    (price (get price-per-period plan-data))
  )
    (asserts! (get active plan-data) ERR_SUBSCRIPTION_CANCELLED)
    (asserts! (< (get current-subscribers plan-data) (get max-subscribers plan-data)) ERR_INVALID_SUBSCRIPTION)
    (asserts! (is-none (map-get? active-subscriptions subscription-key)) ERR_ALREADY_EXISTS)
    (asserts! (>= (stx-get-balance tx-sender) price) ERR_INSUFFICIENT_PAYMENT)
    
    ;; Process payment
    (try! (stx-transfer? price tx-sender (get creator plan-data)))
    
    ;; Create subscription record
    (map-set active-subscriptions subscription-key
      {
        started-at: current-block,
        current-period-start: current-block,
        current-period-end: period-end,
        auto-renew: auto-renew,
        total-periods: u1,
        total-paid: price,
        status: "active"
      }
    )
    
    ;; Record payment
    (map-set subscription-payments
      { plan-id: plan-id, subscriber: tx-sender, payment-id: u1 }
      {
        amount: price,
        period-covered: u1,
        paid-at: current-block,
        payment-method: "stx"
      }
    )
    
    ;; Update plan subscriber count
    (map-set subscription-plans plan-id
      (merge plan-data { current-subscribers: (+ (get current-subscribers plan-data) u1) })
    )
    
    ;; Update creator stats
    (let (
      (creator-stats (default-to 
        { total-plans: u0, active-plans: u0, total-subscribers: u0, total-revenue: u0, avg-retention-rate: u100 }
        (map-get? creator-subscription-stats (get creator plan-data))
      ))
    )
      (map-set creator-subscription-stats (get creator plan-data)
        (merge creator-stats {
          total-subscribers: (+ (get total-subscribers creator-stats) u1),
          total-revenue: (+ (get total-revenue creator-stats) price)
        })
      )
    )
    
    (ok true)
  )
)

;; Renew subscription (automatic or manual)
(define-public (renew-subscription (plan-id uint))
  (let (
    (plan-data (unwrap! (map-get? subscription-plans plan-id) ERR_NOT_FOUND))
    (subscription-key { plan-id: plan-id, subscriber: tx-sender })
    (subscription-data (unwrap! (map-get? active-subscriptions subscription-key) ERR_NOT_FOUND))
    (current-block stacks-block-height)
    (price (get price-per-period plan-data))
    (new-period-end (+ current-block (get period-duration plan-data)))
  )
    (asserts! (get active plan-data) ERR_SUBSCRIPTION_CANCELLED)
    (asserts! (is-eq (get status subscription-data) "active") ERR_SUBSCRIPTION_CANCELLED)
    (asserts! (>= (stx-get-balance tx-sender) price) ERR_INSUFFICIENT_PAYMENT)
    
    ;; Process payment
    (try! (stx-transfer? price tx-sender (get creator plan-data)))
    
    ;; Update subscription
    (map-set active-subscriptions subscription-key
      (merge subscription-data {
        current-period-start: current-block,
        current-period-end: new-period-end,
        total-periods: (+ (get total-periods subscription-data) u1),
        total-paid: (+ (get total-paid subscription-data) price)
      })
    )
    
    ;; Record payment
    (let (
      (next-payment-id (+ (get total-periods subscription-data) u1))
    )
      (map-set subscription-payments
        { plan-id: plan-id, subscriber: tx-sender, payment-id: next-payment-id }
        {
          amount: price,
          period-covered: next-payment-id,
          paid-at: current-block,
          payment-method: "stx"
        }
      )
    )
    
    ;; Update creator revenue
    (let (
      (creator-stats (unwrap-panic (map-get? creator-subscription-stats (get creator plan-data))))
    )
      (map-set creator-subscription-stats (get creator plan-data)
        (merge creator-stats {
          total-revenue: (+ (get total-revenue creator-stats) price)
        })
      )
    )
    
    (ok true)
  )
)

;; Cancel subscription
(define-public (cancel-subscription (plan-id uint))
  (let (
    (subscription-key { plan-id: plan-id, subscriber: tx-sender })
    (subscription-data (unwrap! (map-get? active-subscriptions subscription-key) ERR_NOT_FOUND))
    (plan-data (unwrap! (map-get? subscription-plans plan-id) ERR_NOT_FOUND))
  )
    (asserts! (is-eq (get status subscription-data) "active") ERR_SUBSCRIPTION_CANCELLED)
    
    ;; Update subscription status
    (map-set active-subscriptions subscription-key
      (merge subscription-data { status: "cancelled", auto-renew: false })
    )
    
    ;; Update plan subscriber count
    (map-set subscription-plans plan-id
      (merge plan-data { 
        current-subscribers: (if (> (get current-subscribers plan-data) u0)
          (- (get current-subscribers plan-data) u1)
          u0)
      })
    )
    
    (ok true)
  )
)

;; Use subscription for license access
(define-public (use-subscription-license (plan-id uint))
  (let (
    (subscription-key { plan-id: plan-id, subscriber: tx-sender })
    (subscription-data (unwrap! (map-get? active-subscriptions subscription-key) ERR_NOT_FOUND))
    (plan-data (unwrap! (map-get? subscription-plans plan-id) ERR_NOT_FOUND))
    (current-block stacks-block-height)
    (current-period (get total-periods subscription-data))
    (usage-key { plan-id: plan-id, subscriber: tx-sender, period: current-period })
    (current-usage (default-to 
      { usage-count: u0, first-use: u0, last-use: u0 }
      (map-get? subscription-usage usage-key)
    ))
  )
    (asserts! (is-eq (get status subscription-data) "active") ERR_SUBSCRIPTION_CANCELLED)
    (asserts! (< current-block (get current-period-end subscription-data)) ERR_SUBSCRIPTION_EXPIRED)
    
    ;; Record usage
    (map-set subscription-usage usage-key
      {
        usage-count: (+ (get usage-count current-usage) u1),
        first-use: (if (is-eq (get first-use current-usage) u0) current-block (get first-use current-usage)),
        last-use: current-block
      }
    )
    
    ;; Call main license usage function
    (contract-call? .Licentia use-license (get license-id plan-data))
  )
)

;; Admin function to deactivate plan
(define-public (deactivate-plan (plan-id uint))
  (let (
    (plan-data (unwrap! (map-get? subscription-plans plan-id) ERR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get creator plan-data)) ERR_NOT_AUTHORIZED)
    
    (map-set subscription-plans plan-id
      (merge plan-data { active: false })
    )
    
    ;; Update creator stats
    (let (
      (creator-stats (unwrap-panic (map-get? creator-subscription-stats tx-sender)))
    )
      (map-set creator-subscription-stats tx-sender
        (merge creator-stats {
          active-plans: (if (> (get active-plans creator-stats) u0)
            (- (get active-plans creator-stats) u1)
            u0)
        })
      )
    )
    
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-subscription-plan (plan-id uint))
  (map-get? subscription-plans plan-id)
)

(define-read-only (get-subscription (plan-id uint) (subscriber principal))
  (map-get? active-subscriptions { plan-id: plan-id, subscriber: subscriber })
)

(define-read-only (is-subscription-active (plan-id uint) (subscriber principal))
  (match (map-get? active-subscriptions { plan-id: plan-id, subscriber: subscriber })
    subscription-data 
      (and 
        (is-eq (get status subscription-data) "active")
        (< stacks-block-height (get current-period-end subscription-data))
      )
    false
  )
)

(define-read-only (get-subscription-usage (plan-id uint) (subscriber principal) (period uint))
  (map-get? subscription-usage { plan-id: plan-id, subscriber: subscriber, period: period })
)

(define-read-only (get-creator-stats (creator principal))
  (default-to 
    { total-plans: u0, active-plans: u0, total-subscribers: u0, total-revenue: u0, avg-retention-rate: u100 }
    (map-get? creator-subscription-stats creator)
  )
)

(define-read-only (get-next-subscription-id)
  (var-get subscription-id-nonce)
)

(define-read-only (calculate-subscription-value (plan-id uint) (subscriber principal))
  (match (map-get? active-subscriptions { plan-id: plan-id, subscriber: subscriber })
    subscription-data 
      (let (
        (periods (get total-periods subscription-data))
        (total-paid (get total-paid subscription-data))
        (avg-per-period (if (> periods u0) (/ total-paid periods) u0))
      )
        (some { 
          total-periods: periods,
          total-paid: total-paid,
          avg-per-period: avg-per-period
        })
      )
    none
  )
)

(define-read-only (get-subscription-time-remaining (plan-id uint) (subscriber principal))
  (match (map-get? active-subscriptions { plan-id: plan-id, subscriber: subscriber })
    subscription-data
      (if (< stacks-block-height (get current-period-end subscription-data))
        (some (- (get current-period-end subscription-data) stacks-block-height))
        none
      )
    none
  )
)

(define-read-only (get-total-subscription-plans)
  (var-get subscription-id-nonce)
)
