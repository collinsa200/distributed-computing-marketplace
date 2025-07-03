;; Distributed Computing Marketplace Smart Contract
;; Version: 1.0.0
;; Author: Collins Agbo
;; Description: A decentralized marketplace for distributed computing resources

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-job-not-available (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-job-already-assigned (err u106))
(define-constant err-invalid-status (err u107))
(define-constant err-worker-not-found (err u108))

;; Data Variables
(define-data-var next-job-id uint u1)
(define-data-var next-worker-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% fee (250 basis points)
(define-data-var total-platform-fees uint u0)

;; Data Maps
(define-map jobs
  uint
  {
    client: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    reward: uint,
    status: (string-ascii 20),
    assigned-worker: (optional principal),
    created-at: uint,
    deadline: uint,
    result-hash: (optional (string-ascii 64))
  }
)

(define-map workers
  principal
  {
    worker-id: uint,
    name: (string-ascii 50),
    skills: (string-ascii 200),
    reputation-score: uint,
    total-jobs-completed: uint,
    total-earnings: uint,
    is-active: bool,
    registered-at: uint
  }
)

(define-map job-applications
  {job-id: uint, worker: principal}
  {
    applied-at: uint,
    proposed-timeline: uint,
    worker-message: (string-ascii 300)
  }
)

(define-map escrow-balances
  uint
  uint
)

;; Public Functions

;; Register as a worker
(define-public (register-worker (name (string-ascii 50)) (skills (string-ascii 200)))
  (let
    (
      (worker-id (var-get next-worker-id))
      (caller tx-sender)
    )
    (asserts! (is-none (map-get? workers caller)) (err u109))
    (map-set workers caller {
      worker-id: worker-id,
      name: name,
      skills: skills,
      reputation-score: u100,
      total-jobs-completed: u0,
      total-earnings: u0,
      is-active: true,
      registered-at: block-height
    })
    (var-set next-worker-id (+ worker-id u1))
    (ok worker-id)
  )
)

;; Create a new computing job
(define-public (create-job 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (reward uint)
  (deadline uint)
)
  (let
    (
      (job-id (var-get next-job-id))
      (caller tx-sender)
    )
    (asserts! (> reward u0) err-invalid-amount)
    (asserts! (> deadline block-height) err-invalid-status)
    
    ;; Transfer reward to escrow
    (try! (stx-transfer? reward caller (as-contract tx-sender)))
    
    ;; Create job record
    (map-set jobs job-id {
      client: caller,
      title: title,
      description: description,
      reward: reward,
      status: "open",
      assigned-worker: none,
      created-at: block-height,
      deadline: deadline,
      result-hash: none
    })
    
    ;; Set escrow balance
    (map-set escrow-balances job-id reward)
    
    (var-set next-job-id (+ job-id u1))
    (ok job-id)
  )
)

;; Apply for a job
(define-public (apply-for-job 
  (job-id uint)
  (proposed-timeline uint)
  (worker-message (string-ascii 300))
)
  (let
    (
      (job-data (unwrap! (map-get? jobs job-id) err-not-found))
      (worker-data (unwrap! (map-get? workers tx-sender) err-worker-not-found))
      (caller tx-sender)
    )
    (asserts! (is-eq (get status job-data) "open") err-job-not-available)
    (asserts! (get is-active worker-data) err-unauthorized)
    
    (map-set job-applications {job-id: job-id, worker: caller} {
      applied-at: block-height,
      proposed-timeline: proposed-timeline,
      worker-message: worker-message
    })
    
    (ok true)
  )
)

;; Assign job to a worker
(define-public (assign-job (job-id uint) (worker principal))
  (let
    (
      (job-data (unwrap! (map-get? jobs job-id) err-not-found))
      (worker-data (unwrap! (map-get? workers worker) err-worker-not-found))
      (caller tx-sender)
    )
    (asserts! (is-eq caller (get client job-data)) err-unauthorized)
    (asserts! (is-eq (get status job-data) "open") err-job-already-assigned)
    (asserts! (get is-active worker-data) err-unauthorized)
    
    ;; Update job status and assign worker
    (map-set jobs job-id (merge job-data {
      status: "assigned",
      assigned-worker: (some worker)
    }))
    
    (ok true)
  )
)

;; Submit job result
(define-public (submit-result (job-id uint) (result-hash (string-ascii 64)))
  (let
    (
      (job-data (unwrap! (map-get? jobs job-id) err-not-found))
      (caller tx-sender)
    )
    (asserts! (is-eq (some caller) (get assigned-worker job-data)) err-unauthorized)
    (asserts! (is-eq (get status job-data) "assigned") err-invalid-status)
    
    ;; Update job with result
    (map-set jobs job-id (merge job-data {
      status: "submitted",
      result-hash: (some result-hash)
    }))
    
    (ok true)
  )
)

;; Accept job result and release payment
(define-public (accept-result (job-id uint))
  (let
    (
      (job-data (unwrap! (map-get? jobs job-id) err-not-found))
      (escrow-amount (unwrap! (map-get? escrow-balances job-id) err-not-found))
      (worker (unwrap! (get assigned-worker job-data) err-worker-not-found))
      (worker-data (unwrap! (map-get? workers worker) err-worker-not-found))
      (caller tx-sender)
      (platform-fee (/ (* escrow-amount (var-get platform-fee-rate)) u10000))
      (worker-payment (- escrow-amount platform-fee))
    )
    (asserts! (is-eq caller (get client job-data)) err-unauthorized)
    (asserts! (is-eq (get status job-data) "submitted") err-invalid-status)
    
    ;; Transfer payment to worker
    (try! (as-contract (stx-transfer? worker-payment tx-sender worker)))
    
    ;; Update platform fees
    (var-set total-platform-fees (+ (var-get total-platform-fees) platform-fee))
    
    ;; Update job status
    (map-set jobs job-id (merge job-data {
      status: "completed"
    }))
    
    ;; Update worker stats
    (map-set workers worker (merge worker-data {
      total-jobs-completed: (+ (get total-jobs-completed worker-data) u1),
      total-earnings: (+ (get total-earnings worker-data) worker-payment),
      reputation-score: (min (+ (get reputation-score worker-data) u10) u1000)
    }))
    
    ;; Clear escrow
    (map-delete escrow-balances job-id)
    
    (ok true)
  )
)

;; Dispute job (for future implementation)
(define-public (dispute-job (job-id uint))
  (let
    (
      (job-data (unwrap! (map-get? jobs job-id) err-not-found))
      (caller tx-sender)
    )
    (asserts! (or 
      (is-eq caller (get client job-data))
      (is-eq (some caller) (get assigned-worker job-data))
    ) err-unauthorized)
    
    ;; Update job status to disputed
    (map-set jobs job-id (merge job-data {
      status: "disputed"
    }))
    
    (ok true)
  )
)

;; Read-only functions

;; Get job details
(define-read-only (get-job (job-id uint))
  (map-get? jobs job-id)
)

;; Get worker details
(define-read-only (get-worker (worker principal))
  (map-get? workers worker)
)

;; Get job application
(define-read-only (get-job-application (job-id uint) (worker principal))
  (map-get? job-applications {job-id: job-id, worker: worker})
)

;; Get escrow balance
(define-read-only (get-escrow-balance (job-id uint))
  (map-get? escrow-balances job-id)
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-jobs: (var-get next-job-id),
    total-workers: (var-get next-worker-id),
    platform-fee-rate: (var-get platform-fee-rate),
    total-platform-fees: (var-get total-platform-fees)
  }
)

;; Admin functions

;; Update platform fee rate (only owner)
(define-public (update-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u1000) err-invalid-amount) ;; Max 10%
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

;; Withdraw platform fees (only owner)
(define-public (withdraw-platform-fees)
  (let
    (
      (fees (var-get total-platform-fees))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> fees u0) err-invalid-amount)
    
    (try! (as-contract (stx-transfer? fees tx-sender contract-owner)))
    (var-set total-platform-fees u0)
    (ok fees)
  )
)