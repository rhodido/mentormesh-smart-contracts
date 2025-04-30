;; MentorMesh - Peer Learning Ecosystem
;;
;; A decentralized platform that enables users to offer their expertise
;; and acquire knowledge from others through a secure token-based system.
;; Users can register as knowledge providers, set their rates, and exchange
;; expertise with others in a trustless environment.

;; ========== OWNER CONFIGURATION ==========
;; Define the contract administrator
(define-constant admin-address tx-sender)

;; ========== ERROR CODES ==========
;; Various error codes for validation and authorization
(define-constant error-admin-only (err u200))
(define-constant error-funds-insufficient (err u201))
(define-constant error-expertise-invalid (err u202))
(define-constant error-price-invalid (err u203))
(define-constant error-capacity-limit-reached (err u204))
(define-constant error-access-denied (err u205))
(define-constant error-capacity-reached (err u206))
(define-constant error-amount-zero (err u207))
(define-constant error-fee-excessive (err u208))
(define-constant error-limit-zero (err u209))
(define-constant error-capacity-reduction (err u210))
(define-constant error-not-certified (err u211))
(define-constant error-rating-minimum (err u212))
(define-constant error-rating-maximum (err u213))
(define-constant error-discount-minimum (err u214))
(define-constant error-discount-maximum (err u215))

;; ========== PLATFORM CONFIGURATION ==========
;; Platform-wide settings that can be adjusted by the administrator
(define-data-var hourly-cost uint u10)           ;; Base cost per hour (in microstacks)
(define-data-var user-expertise-limit uint u100) ;; Maximum hours a single user can offer
(define-data-var platform-fee-percent uint u10)  ;; Platform's commission percentage
(define-data-var global-expertise-pool uint u0)  ;; Total available expertise hours in the system
(define-data-var max-expertise-capacity uint u1000) ;; Maximum system-wide expertise capacity

;; ========== USER DATA STORAGE ==========
;; Maps to track user balances and offerings
(define-map expertise-balance principal uint)    ;; User's available expertise hours
(define-map token-balance principal uint)        ;; User's available token balance
(define-map available-expertise {provider: principal} {hours: uint, price: uint})

;; ========== VERIFICATION SYSTEM ==========
;; Certified providers have undergone verification
(define-map certified-providers principal bool)
(define-map premium-expertise-offerings {provider: principal} {hours: uint, price: uint, certified: bool})

;; ========== RATING SYSTEM ==========
;; Track user ratings to build reputation
(define-map provider-rating {expert: principal, evaluator: principal} uint)
(define-map provider-score principal {total-points: uint, review-count: uint})

;; ========== DISCOUNT PACKAGES ==========
;; Bundle hours together for a discounted rate
(define-map expertise-packages {provider: principal} {hours: uint, price: uint, discount-rate: uint})

;; ========== GROUP SESSIONS ==========
;; Multiple users can participate in group knowledge transfers
(define-map group-sessions uint {organizer: principal, members: (list 10 principal), duration: uint, price: uint, state: (string-ascii 20)})
(define-data-var session-counter uint u0)

;; ========== UTILITY FUNCTIONS ==========

;; Calculate the platform's commission on a transaction
(define-private (calculate-commission (transaction-value uint))
  (let ((fee-rate (var-get platform-fee-percent)))
    (/ (* transaction-value fee-rate) u100)))

;; Update the global expertise pool when offerings change
(define-private (adjust-expertise-pool (hours-change int))
  (let (
    (current-pool (var-get global-expertise-pool))
    (updated-pool (if (< hours-change 0)
                     ;; If removing hours, ensure we don't go below zero
                     (if (>= current-pool (to-uint (- 0 hours-change)))
                         (- current-pool (to-uint (- 0 hours-change)))
                         u0)
                     ;; If adding hours
                     (+ current-pool (to-uint hours-change))))
  )
    ;; Ensure we don't exceed the maximum capacity
    (asserts! (<= updated-pool (var-get max-expertise-capacity)) error-capacity-limit-reached)
    ;; Update the pool size
    (var-set global-expertise-pool updated-pool)
    (ok true)))

;; ========== CORE FUNCTIONALITY ==========

;; Register new expertise hours to user's account
(define-public (register-expertise (hours uint))
  (let (
    (user tx-sender)
    (current-hours (default-to u0 (map-get? expertise-balance user)))
    (max-allowed (var-get user-expertise-limit))
    (acquisition-cost (* hours (var-get hourly-cost)))
    (user-funds (default-to u0 (map-get? token-balance user)))
  )
    ;; Validate the input
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (<= (+ current-hours hours) max-allowed) error-capacity-reached)
    (asserts! (>= user-funds acquisition-cost) error-funds-insufficient)

    ;; Update user's expertise and token balances
    (map-set expertise-balance user (+ current-hours hours))
    (map-set token-balance user (- user-funds acquisition-cost))

    ;; Add funds to the admin's balance
    (map-set token-balance admin-address (+ (default-to u0 (map-get? token-balance admin-address)) acquisition-cost))

    (ok true)))

;; Make expertise available for others to acquire
(define-public (publish-expertise (hours uint) (price uint))
  (let (
    (current-hours (default-to u0 (map-get? expertise-balance tx-sender)))
    (currently-published (get hours (default-to {hours: u0, price: u0} (map-get? available-expertise {provider: tx-sender}))))
    (total-published (+ hours currently-published))
  )
    ;; Validate the input
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (> price u0) error-price-invalid)
    (asserts! (>= current-hours total-published) error-funds-insufficient)

    ;; Update the global expertise pool
    (try! (adjust-expertise-pool (to-int hours)))

    ;; Update the available expertise map
    (map-set available-expertise {provider: tx-sender} {hours: total-published, price: price})

    (ok true)))

;; Acquire expertise from another user
(define-public (acquire-expertise (provider principal) (hours uint))
  (let (
    (offering (default-to {hours: u0, price: u0} (map-get? available-expertise {provider: provider})))
    (transaction-cost (* hours (get price offering)))
    (platform-commission (calculate-commission transaction-cost))
    (total-cost (+ transaction-cost platform-commission))
    (provider-hours (default-to u0 (map-get? expertise-balance provider)))
    (acquirer-funds (default-to u0 (map-get? token-balance tx-sender)))
    (provider-funds (default-to u0 (map-get? token-balance provider)))
  )
    ;; Verify conditions
    (asserts! (not (is-eq tx-sender provider)) error-access-denied)
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (>= (get hours offering) hours) error-funds-insufficient)
    (asserts! (>= provider-hours hours) error-funds-insufficient)
    (asserts! (>= acquirer-funds total-cost) error-funds-insufficient)

    ;; Update provider's expertise balance and available offerings
    (map-set expertise-balance provider (- provider-hours hours))
    (map-set available-expertise {provider: provider} 
             {hours: (- (get hours offering) hours), price: (get price offering)})

    ;; Update token balances
    (map-set token-balance tx-sender (- acquirer-funds total-cost))
    (map-set token-balance provider (+ provider-funds transaction-cost))
    (map-set expertise-balance tx-sender (+ (default-to u0 (map-get? expertise-balance tx-sender)) hours))

    ;; Add commission to admin balance
    (map-set token-balance admin-address (+ (default-to u0 (map-get? token-balance admin-address)) platform-commission))

    (ok true)))

;; Offer certified premium expertise (requires verification)
(define-public (publish-premium-expertise (hours uint) (price uint))
  (let (
    (current-hours (default-to u0 (map-get? expertise-balance tx-sender)))
    (is-certified (default-to false (map-get? certified-providers tx-sender)))
    (currently-published (get hours (default-to {hours: u0, price: u0} (map-get? available-expertise {provider: tx-sender}))))
    (total-published (+ hours currently-published))
  )
    ;; Validate the input
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (> price u0) error-price-invalid)
    (asserts! is-certified error-not-certified)
    (asserts! (>= current-hours total-published) error-funds-insufficient)

    ;; Update the global expertise pool
    (try! (adjust-expertise-pool (to-int hours)))

    ;; Update regular expertise offerings
    (map-set available-expertise {provider: tx-sender} {hours: total-published, price: price})

    ;; Update premium expertise offerings
    (map-set premium-expertise-offerings {provider: tx-sender} {hours: hours, price: price, certified: true})

    (ok true)))

;; Create a bundled package of expertise hours at a discount
(define-public (create-expertise-package (hours uint) (price uint) (discount-rate uint))
  (let (
    (current-hours (default-to u0 (map-get? expertise-balance tx-sender)))
    (currently-published (get hours (default-to {hours: u0, price: u0} (map-get? available-expertise {provider: tx-sender}))))
    (current-package (default-to {hours: u0, price: u0, discount-rate: u0} (map-get? expertise-packages {provider: tx-sender})))
    (total-published (+ hours currently-published))
    (total-packaged-hours (+ hours (get hours current-package)))
  )
    ;; Validate the input
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (> price u0) error-price-invalid)
    (asserts! (> discount-rate u0) error-discount-minimum)
    (asserts! (<= discount-rate u50) error-discount-maximum)
    (asserts! (>= current-hours total-published) error-funds-insufficient)

    ;; Update the global expertise pool
    (try! (adjust-expertise-pool (to-int hours)))

    ;; Update expertise availability
    (map-set available-expertise {provider: tx-sender} {hours: total-published, price: price})

    ;; Create or update the package offering
    (map-set expertise-packages {provider: tx-sender} {
      hours: total-packaged-hours, 
      price: price, 
      discount-rate: discount-rate
    })

    (ok true)))

;; Initialize a group expertise session
(define-public (initialize-group-session (participants (list 10 principal)) (hours uint) (price uint))
  (let (
    (current-hours (default-to u0 (map-get? expertise-balance tx-sender)))
    (session-id (var-get session-counter))
    (participant-count (len participants))
    (total-session-hours (* hours participant-count))
  )
    ;; Validate the input
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (> price u0) error-price-invalid)
    (asserts! (>= current-hours total-session-hours) error-funds-insufficient)

    ;; Update the expertise pool
    (try! (adjust-expertise-pool (to-int total-session-hours)))

    ;; Update organizer's expertise balance
    (map-set expertise-balance tx-sender (- current-hours total-session-hours))

    ;; Increment the session counter
    (var-set session-counter (+ session-id u1))

    (ok session-id)))

;; Evaluate a provider after acquiring expertise
(define-public (evaluate-provider (provider principal) (score uint))
  (let (
    (provider-metrics (default-to {total-points: u0, review-count: u0} (map-get? provider-score provider)))
    (current-total (get total-points provider-metrics))
    (current-count (get review-count provider-metrics))
    (new-total (+ current-total score))
    (new-count (+ current-count u1))
  )
    ;; Validate the input
    (asserts! (not (is-eq tx-sender provider)) error-access-denied)
    (asserts! (>= score u1) error-rating-minimum)
    (asserts! (<= score u5) error-rating-maximum)

    ;; Update the provider's rating data
    (map-set provider-rating {expert: provider, evaluator: tx-sender} score)
    (map-set provider-score provider {total-points: new-total, review-count: new-count})

    (ok true)))

;; Deposit tokens into the platform
(define-public (deposit-tokens (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? token-balance tx-sender)))
    (new-balance (+ current-balance amount))
  )
    ;; Validate the input
    (asserts! (> amount u0) error-amount-zero)

    ;; Transfer tokens from sender to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Update user's token balance in the platform
    (map-set token-balance tx-sender new-balance)

    (ok true)))

;; Withdraw tokens from the platform
(define-public (withdraw-tokens (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? token-balance tx-sender)))
    (contract-balance (as-contract (stx-get-balance tx-sender)))
  )
    ;; Validate the input
    (asserts! (> amount u0) error-amount-zero)
    (asserts! (>= current-balance amount) error-funds-insufficient)
    (asserts! (>= contract-balance amount) error-funds-insufficient)

    ;; Transfer tokens from contract to user
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))

    ;; Update user's token balance in the platform
    (map-set token-balance tx-sender (- current-balance amount))

    (ok true)))

;; Reclaim published expertise that hasn't been acquired
(define-public (reclaim-published-expertise (hours uint))
  (let (
    (offering (default-to {hours: u0, price: u0} (map-get? available-expertise {provider: tx-sender})))
    (available-hours (get hours offering))
    (user-hours (default-to u0 (map-get? expertise-balance tx-sender)))
  )
    ;; Validate the input
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (>= available-hours hours) error-funds-insufficient)

    ;; Update the user's published expertise
    (map-set available-expertise {provider: tx-sender} {
      hours: (- available-hours hours),
      price: (get price offering)
    })

    ;; Update user's expertise balance
    (map-set expertise-balance tx-sender user-hours)

    ;; Handle premium offerings if applicable
    (if (is-some (map-get? premium-expertise-offerings {provider: tx-sender}))
        (let (
          (premium-offering (unwrap-panic (map-get? premium-expertise-offerings {provider: tx-sender})))
          (premium-hours (get hours premium-offering))
        )
          (if (>= premium-hours hours)
              (map-set premium-expertise-offerings {provider: tx-sender} {
                hours: (- premium-hours hours),
                price: (get price premium-offering),
                certified: (get certified premium-offering)
              })
              (map-delete premium-expertise-offerings {provider: tx-sender})
          )
        )
        true
    )

    (ok true)))

;; Update platform configuration (admin only)
(define-public (update-platform-configuration (new-hourly-cost uint) 
                                           (new-platform-fee uint) 
                                           (new-user-limit uint) 
                                           (new-capacity-limit uint))
  (begin
    ;; Verify admin privileges
    (asserts! (is-eq tx-sender admin-address) error-admin-only)

    ;; Validate the input
    (asserts! (> new-hourly-cost u0) error-price-invalid)
    (asserts! (<= new-platform-fee u30) error-fee-excessive)
    (asserts! (> new-user-limit u0) error-limit-zero)
    (asserts! (>= new-capacity-limit (var-get global-expertise-pool)) error-capacity-reduction)

    ;; Update the platform configuration
    (var-set hourly-cost new-hourly-cost)
    (var-set platform-fee-percent new-platform-fee)
    (var-set user-expertise-limit new-user-limit)
    (var-set max-expertise-capacity new-capacity-limit)

    (ok true)))

