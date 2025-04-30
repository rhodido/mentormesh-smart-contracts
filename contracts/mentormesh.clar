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
