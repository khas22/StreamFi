;; ;; Livestreaming Platform Smart Contract
;; ;; Developed for Stacks Blockchain
;; ;; Handles creator registration, stream management, viewer engagement, and rewards

;; ;; Define constant values
;; (define-constant CONTRACT-OWNER tx-sender)
;; (define-constant ENGAGEMENT-POINT-RATE u10) ;; Points per minute watched (fixed to uint)
;; (define-constant ERR-NOT-AUTHORIZED (err u100))
;; (define-constant ERR-NOT-FOUND (err u101))
;; (define-constant ERR-ALREADY-EXISTS (err u102))
;; (define-constant ERR-INACTIVE (err u103))

;; ;; Define data structures
;; (define-map Creators 
;;   { creator-id: principal } 
;;   {
;;     username: (string-utf8 64),
;;     bio: (string-utf8 256),
;;     registered-at: uint,
;;     total-stream-time: uint,
;;     total-earnings: uint,
;;     active: bool
;;   }
;; )

;; (define-map Streams 
;;   { stream-id: uint } 
;;   {
;;     creator: principal,
;;     title: (string-utf8 128),
;;     description: (string-utf8 512),
;;     started-at: uint,
;;     ended-at: uint,
;;     storage-url: (string-utf8 256),
;;     category: (string-utf8 64),
;;     active: bool,
;;     viewer-count: uint,
;;     total-points-awarded: uint
;;   }
;; )

;; (define-map Engagements
;;   { stream-id: uint, user: principal }
;;   {
;;     watch-time: uint,
;;     points-earned: uint,
;;     last-interaction: uint,
;;     tipped-amount: uint
;;   }
;; )

;; (define-map UserPoints
;;   { user: principal }
;;   {
;;     total-points: uint,
;;     points-redeemed: uint,
;;     points-available: uint
;;   }
;; )

;; ;; Define variables
;; (define-data-var stream-nonce uint u0)
;; (define-data-var total-points-distributed uint u0)
;; (define-data-var platform-fee-percent uint u5) ;; 5% platform fee

;; ;; Read-only functions

;; (define-read-only (get-creator-info (creator-id principal))
;;   (map-get? Creators { creator-id: creator-id })
;; )

;; (define-read-only (get-stream-info (stream-id uint))
;;   (map-get? Streams { stream-id: stream-id })
;; )

;; (define-read-only (get-user-engagement (stream-id uint) (user principal))
;;   (map-get? Engagements { stream-id: stream-id, user: user })
;; )

;; (define-read-only (get-user-points (user principal))
;;   (default-to 
;;     { total-points: u0, points-redeemed: u0, points-available: u0 }
;;     (map-get? UserPoints { user: user })
;;   )
;; )

;; (define-read-only (get-active-streams)
;;   ;; This would need to be implemented with an indexer in practice
;;   ;; as Clarity doesn't support returning multiple map entries directly
;;   (ok "Use an off-chain indexer to query active streams")
;; )

;; ;; Public functions

;; ;; Register as a creator
;; (define-public (register-creator (username (string-utf8 64)) (bio (string-utf8 256)))
;;   (let ((existing-creator (map-get? Creators { creator-id: tx-sender })))
;;     (asserts! (is-none existing-creator) ERR-ALREADY-EXISTS)
    
;;     (map-set Creators
;;       { creator-id: tx-sender }
;;       {
;;         username: username,
;;         bio: bio,
;;         registered-at: (unwrap-panic (get-block-info? time (- block-height u1))),
;;         total-stream-time: u0,
;;         total-earnings: u0,
;;         active: true
;;       }
;;     )
;;     (ok tx-sender)
;;   )
;; )

;; ;; Start a new livestream
;; (define-public (start-stream 
;;                 (title (string-utf8 128)) 
;;                 (description (string-utf8 512))
;;                 (storage-url (string-utf8 256))
;;                 (category (string-utf8 64)))
;;   (let (
;;         (creator-info (map-get? Creators { creator-id: tx-sender }))
;;         (stream-id (+ (var-get stream-nonce) u1))
;;         (block-time (unwrap-panic (get-block-info? time (- block-height u1))))
;;        )
    
;;     ;; Ensure creator is registered
;;     (asserts! (is-some creator-info) ERR-NOT-FOUND)
;;     (asserts! (get active (unwrap-panic creator-info)) ERR-INACTIVE)
    
;;     ;; Set the new stream
;;     (map-set Streams
;;       { stream-id: stream-id }
;;       {
;;         creator: tx-sender,
;;         title: title,
;;         description: description,
;;         started-at: block-time,
;;         ended-at: u0,
;;         storage-url: storage-url,
;;         category: category,
;;         active: true,
;;         viewer-count: u0,
;;         total-points-awarded: u0
;;       }
;;     )
    
;;     ;; Update the nonce
;;     (var-set stream-nonce stream-id)
    
;;     (ok stream-id)
;;   )
;; )

;; ;; End a livestream
;; (define-public (end-stream (stream-id uint))
;;   (let (
;;         (stream (map-get? Streams { stream-id: stream-id }))
;;         (block-time (unwrap-panic (get-block-info? time (- block-height u1))))
;;        )
    
;;     ;; Check stream exists and is owned by tx-sender
;;     (asserts! (is-some stream) ERR-NOT-FOUND)
;;     (asserts! (is-eq (get creator (unwrap-panic stream)) tx-sender) ERR-NOT-AUTHORIZED)
;;     (asserts! (get active (unwrap-panic stream)) ERR-INACTIVE)
    
;;     ;; Update stream status
;;     (map-set Streams
;;       { stream-id: stream-id }
;;       (merge (unwrap-panic stream)
;;         {
;;           active: false,
;;           ended-at: block-time
;;         }
;;       )
;;     )
    
;;     ;; Update creator's total stream time
;;     (let (
;;           (creator-info (unwrap-panic (map-get? Creators { creator-id: tx-sender })))
;;           (stream-duration (- block-time (get started-at (unwrap-panic stream))))
;;          )
;;       (map-set Creators
;;         { creator-id: tx-sender }
;;         (merge creator-info
;;           {
;;             total-stream-time: (+ (get total-stream-time creator-info) stream-duration)
;;           }
;;         )
;;       )
;;     )
    
;;     (ok true)
;;   )
;; )

;; ;; Record viewer engagement
;; (define-public (record-engagement (stream-id uint) (watch-minutes uint))
;;   (let (
;;         (stream (map-get? Streams { stream-id: stream-id }))
;;         (block-time (unwrap-panic (get-block-info? time (- block-height u1))))
;;         (points-to-award (* watch-minutes ENGAGEMENT-POINT-RATE))
;;         (current-engagement (map-get? Engagements { stream-id: stream-id, user: tx-sender }))
;;        )
    
;;     ;; Verify stream exists and is active
;;     (asserts! (is-some stream) ERR-NOT-FOUND)
;;     (asserts! (get active (unwrap-panic stream)) ERR-INACTIVE)
    
;;     ;; Update or create engagement record
;;     (if (is-some current-engagement)
;;       (map-set Engagements
;;         { stream-id: stream-id, user: tx-sender }
;;         (merge (unwrap-panic current-engagement)
;;           {
;;             watch-time: (+ (get watch-time (unwrap-panic current-engagement)) watch-minutes),
;;             points-earned: (+ (get points-earned (unwrap-panic current-engagement)) points-to-award),
;;             last-interaction: block-time
;;           }
;;         )
;;       )
;;       (map-set Engagements
;;         { stream-id: stream-id, user: tx-sender }
;;         {
;;           watch-time: watch-minutes,
;;           points-earned: points-to-award,
;;           last-interaction: block-time,
;;           tipped-amount: u0
;;         }
;;       )
;;     )
    
;;     ;; Update stream total points awarded
;;     (map-set Streams
;;       { stream-id: stream-id }
;;       (merge (unwrap-panic stream)
;;         {
;;           total-points-awarded: (+ (get total-points-awarded (unwrap-panic stream)) points-to-award),
;;           viewer-count: (+ (get viewer-count (unwrap-panic stream)) u1)
;;         }
;;       )
;;     )
    
;;     ;; Update user's total points
;;     (update-user-points tx-sender points-to-award)
    
;;     ;; Update global points counter
;;     (var-set total-points-distributed (+ (var-get total-points-distributed) points-to-award))
    
;;     (ok points-to-award)
;;   )
;; )

;; ;; Send a tip to a creator
;; (define-public (tip-creator (stream-id uint) (amount uint))
;;   (let (
;;         (stream (map-get? Streams { stream-id: stream-id }))
;;         (platform-fee (/ (* amount (var-get platform-fee-percent)) u100))
;;         (creator-amount (- amount platform-fee))
;;        )
    
;;     ;; Verify stream exists
;;     (asserts! (is-some stream) ERR-NOT-FOUND)
    
;;     ;; Get the creator from the stream
;;     (let ((creator (get creator (unwrap-panic stream))))
;;       ;; Transfer STX from sender to creator
;;       (try! (stx-transfer? creator-amount tx-sender creator))
      
;;       ;; Transfer platform fee to contract owner
;;       (try! (stx-transfer? platform-fee tx-sender CONTRACT-OWNER))
      
;;       ;; Update creator earnings
;;       (let ((creator-info (unwrap-panic (map-get? Creators { creator-id: creator }))))
;;         (map-set Creators
;;           { creator-id: creator }
;;           (merge creator-info 
;;             { 
;;               total-earnings: (+ (get total-earnings creator-info) creator-amount) 
;;             }
;;           )
;;         )
;;       )
      
;;       ;; Update engagement record for the tipper
;;       (let ((engagement (map-get? Engagements { stream-id: stream-id, user: tx-sender })))
;;         (if (is-some engagement)
;;           (map-set Engagements
;;             { stream-id: stream-id, user: tx-sender }
;;             (merge (unwrap-panic engagement)
;;               {
;;                 tipped-amount: (+ (get tipped-amount (unwrap-panic engagement)) amount)
;;               }
;;             )
;;           )
;;           (map-set Engagements
;;             { stream-id: stream-id, user: tx-sender }
;;             {
;;               watch-time: u0,
;;               points-earned: u0,
;;               last-interaction: (unwrap-panic (get-block-info? time (- block-height u1))),
;;               tipped-amount: amount
;;             }
;;           )
;;         )
;;       )
      
;;       (ok true)
;;     )
;;   )
;; )

;; ;; Redeem points for STX
;; (define-public (redeem-points-for-stx (points-amount uint))
;;   (let (
;;         (user-points-data (get-user-points tx-sender))
;;         (available-points (get points-available user-points-data))
;;         (stx-amount (/ points-amount u100)) ;; 100 points = 1 STX (example rate)
;;        )
    
;;     ;; Verify user has enough points
;;     (asserts! (>= available-points points-amount) (err u403))
    
;;     ;; Transfer STX to user (this would come from a treasury)
;;     (try! (as-contract (stx-transfer? stx-amount CONTRACT-OWNER tx-sender)))
    
;;     ;; Update user points
;;     (map-set UserPoints
;;       { user: tx-sender }
;;       (merge user-points-data
;;         {
;;           points-redeemed: (+ (get points-redeemed user-points-data) points-amount),
;;           points-available: (- available-points points-amount)
;;         }
;;       )
;;     )
    
;;     (ok stx-amount)
;;   )
;; )

;; ;; Private helper functions

;; (define-private (update-user-points (user principal) (points uint))
;;   (let ((user-points-data (get-user-points user)))
;;     (map-set UserPoints
;;       { user: user }
;;       {
;;         total-points: (+ (get total-points user-points-data) points),
;;         points-redeemed: (get points-redeemed user-points-data),
;;         points-available: (+ (get points-available user-points-data) points)
;;       }
;;     )
;;   )
;; )

;; ;; Contract initialization
;; ;; The begin statement needs at least one expression
;; (begin
;;   (var-set stream-nonce u0)
;;   (var-set total-points-distributed u0)
;;   (var-set platform-fee-percent u5)
;;   (ok true)
;; )


;; Livestreaming Platform Smart Contract
;; Developed for Stacks Blockchain
;; Handles creator registration, stream management, viewer engagement, and rewards

;; Define constant values



;; Additional constants
(define-constant ERR-INVALID-AMOUNT (err u105))
(define-constant ERR-INSUFFICIENT-FUNDS (err u106))
(define-constant ERR-STREAM-NOT-ACTIVE (err u107))
(define-constant ERR-INVALID-DURATION (err u108))

;; New data structures
(define-map Subscriptions
  { subscriber: principal, creator: principal }
  {
    start-time: uint,
    end-time: uint,
    amount-paid: uint,
    active: bool
  }
)

(define-map CreatorTiers
  { creator-id: principal, tier-id: uint }
  {
    name: (string-utf8 64),
    description: (string-utf8 256),
    price: uint,
    duration-days: uint,
    benefits: (string-utf8 512),
    active: bool
  }
)

(define-map StreamChallenges
  { stream-id: uint, challenge-id: uint }
  {
    creator: principal,
    title: (string-utf8 128),
    description: (string-utf8 256),
    goal-amount: uint,
    current-amount: uint,
    completed: bool,
    expires-at: uint
  }
)

(define-map UserChallengeContributions
  { stream-id: uint, challenge-id: uint, user: principal }
  {
    contribution-amount: uint,
    contribution-time: uint
  }
)
;; Define nonce variables for new features
(define-data-var tier-nonce uint u0)
(define-data-var challenge-nonce uint u0)
(define-data-var poll-nonce uint u0)
(define-data-var reward-nonce uint u0)

;; Read-only functions for new features

(define-read-only (get-subscription-info (subscriber principal) (creator principal))
  (map-get? Subscriptions { subscriber: subscriber, creator: creator })
)

(define-read-only (get-creator-tier (creator-id principal) (tier-id uint))
  (map-get? CreatorTiers { creator-id: creator-id, tier-id: tier-id })
)

(define-read-only (get-creator-tiers (creator-id principal))
  ;; This would require off-chain indexing in practice
  (ok "Use an off-chain indexer to retrieve all creator tiers")
)

(define-read-only (get-stream-challenge (stream-id uint) (challenge-id uint))
  (map-get? StreamChallenges { stream-id: stream-id, challenge-id: challenge-id })
)

(define-read-only (get-user-challenge-contribution (stream-id uint) (challenge-id uint) (user principal))
  (map-get? UserChallengeContributions { stream-id: stream-id, challenge-id: challenge-id, user: user })
)

;;only defined for subscriptions and challenges features. when adding more features add define their variables

;; Public functions for new features

;; Create a subscription tier
(define-public (create-subscription-tier 
                (name (string-utf8 64)) 
                (description (string-utf8 256))
                (price uint)
                (duration-days uint)
                (benefits (string-utf8 512)))
  (let (
        (creator-info (map-get? Creators { creator-id: tx-sender }))
        (tier-id (+ (var-get tier-nonce) u1))
       )
    
    ;; Check creator exists
    (asserts! (is-some creator-info) ERR-NOT-FOUND)
    
    ;; Ensure creator is active
    (let ((unwrapped-creator (unwrap! creator-info ERR-NOT-FOUND)))
      (asserts! (get active unwrapped-creator) ERR-INACTIVE)
      
      ;; Validate price and duration
      (asserts! (> price u0) ERR-INVALID-AMOUNT)
      (asserts! (> duration-days u0) ERR-INVALID-DURATION)
      
      ;; Create tier
      (map-set CreatorTiers
        { creator-id: tx-sender, tier-id: tier-id }
        {
          name: name,
          description: description,
          price: price,
          duration-days: duration-days,
          benefits: benefits,
          active: true
        }
      )
      
      ;; Update nonce
      (var-set tier-nonce tier-id)
      
      (ok tier-id)
    )
  )
)

;; Subscribe to a creator
(define-public (subscribe-to-creator (creator-id principal) (tier-id uint))
  (let (
        (creator-info (map-get? Creators { creator-id: creator-id }))
        (tier-info (map-get? CreatorTiers { creator-id: creator-id, tier-id: tier-id }))
        (block-time (default-to u0 (get-block-info? time (- block-height u1))))
       )
    
    ;; Check creator and tier exist
    (asserts! (is-some creator-info) ERR-NOT-FOUND)
    (asserts! (is-some tier-info) ERR-NOT-FOUND)
    (asserts! (> block-time u0) ERR-NOT-FOUND)
    
    ;; Unwrap tier info
    (let (
          (unwrapped-tier (unwrap! tier-info ERR-NOT-FOUND))
          (unwrapped-creator (unwrap! creator-info ERR-NOT-FOUND))
         )
      
      ;; Check tier is active
      (asserts! (get active unwrapped-tier) ERR-INACTIVE)
      (asserts! (get active unwrapped-creator) ERR-INACTIVE)
      
      (let (
            (subscription-price (get price unwrapped-tier))
            (duration-days (get duration-days unwrapped-tier))
            (platform-fee (/ (* subscription-price (var-get platform-fee-percent)) u100))
            (creator-amount (- subscription-price platform-fee))
            (seconds-in-day u86400)
            (duration-seconds (* duration-days seconds-in-day))
            (end-time (+ block-time duration-seconds))
           )
        
        ;; Transfer STX from subscriber to creator
        (try! (stx-transfer? creator-amount tx-sender creator-id))
        
        ;; Transfer platform fee
        (try! (stx-transfer? platform-fee tx-sender CONTRACT-OWNER))
        
        ;; Record subscription
        (map-set Subscriptions
          { subscriber: tx-sender, creator: creator-id }
          {
            start-time: block-time,
            end-time: end-time,
            amount-paid: subscription-price,
            active: true
          }
        )
        
        ;; Update creator earnings
        (map-set Creators
          { creator-id: creator-id }
          (merge unwrapped-creator
            {
              total-earnings: (+ (get total-earnings unwrapped-creator) creator-amount)
            }
          )
        )
        
        ;; Award subscription points to subscriber (bonus for subscribing)
        (let (
              (bonus-points (* subscription-price u2)) ;; 2 points per STX spent on subscription
             )
          (update-user-points tx-sender bonus-points)
          (var-set total-points-distributed (+ (var-get total-points-distributed) bonus-points))
        )
        
        (ok true)
      )
    )
  )
)
