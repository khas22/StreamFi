;; Livestreaming Platform Smart Contract
;; Developed for Stacks Blockchain
;; Handles creator registration, stream management, viewer engagement, and rewards

;; Define constant values
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ENGAGEMENT-POINT-RATE u10) ;; Points per minute watched (fixed to uint)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INACTIVE (err u103))

;; Define data structures
(define-map Creators 
  { creator-id: principal } 
  {
    username: (string-utf8 64),
    bio: (string-utf8 256),
    registered-at: uint,
    total-stream-time: uint,
    total-earnings: uint,
    active: bool
  }
)

(define-map Streams 
  { stream-id: uint } 
  {
    creator: principal,
    title: (string-utf8 128),
    description: (string-utf8 512),
    started-at: uint,
    ended-at: uint,
    storage-url: (string-utf8 256),
    category: (string-utf8 64),
    active: bool,
    viewer-count: uint,
    total-points-awarded: uint
  }
)

(define-map Engagements
  { stream-id: uint, user: principal }
  {
    watch-time: uint,
    points-earned: uint,
    last-interaction: uint,
    tipped-amount: uint
  }
)

(define-map UserPoints
  { user: principal }
  {
    total-points: uint,
    points-redeemed: uint,
    points-available: uint
  }
)

;; Define variables
(define-data-var stream-nonce uint u0)
(define-data-var total-points-distributed uint u0)
(define-data-var platform-fee-percent uint u5) ;; 5% platform fee

;; Read-only functions

(define-read-only (get-creator-info (creator-id principal))
  (map-get? Creators { creator-id: creator-id })
)

(define-read-only (get-stream-info (stream-id uint))
  (map-get? Streams { stream-id: stream-id })
)

(define-read-only (get-user-engagement (stream-id uint) (user principal))
  (map-get? Engagements { stream-id: stream-id, user: user })
)

(define-read-only (get-user-points (user principal))
  (default-to 
    { total-points: u0, points-redeemed: u0, points-available: u0 }
    (map-get? UserPoints { user: user })
  )
)

(define-read-only (get-active-streams)
  ;; This would need to be implemented with an indexer in practice
  ;; as Clarity doesn't support returning multiple map entries directly
  (ok "Use an off-chain indexer to query active streams")
)

;; Public functions

;; Register as a creator
(define-public (register-creator (username (string-utf8 64)) (bio (string-utf8 256)))
  (let ((existing-creator (map-get? Creators { creator-id: tx-sender })))
    (asserts! (is-none existing-creator) ERR-ALREADY-EXISTS)
    
    (map-set Creators
      { creator-id: tx-sender }
      {
        username: username,
        bio: bio,
        registered-at: (unwrap-panic (get-block-info? time (- block-height u1))),
        total-stream-time: u0,
        total-earnings: u0,
        active: true
      }
    )
    (ok tx-sender)
  )
)

;; Start a new livestream
(define-public (start-stream 
                (title (string-utf8 128)) 
                (description (string-utf8 512))
                (storage-url (string-utf8 256))
                (category (string-utf8 64)))
  (let (
        (creator-info (map-get? Creators { creator-id: tx-sender }))
        (stream-id (+ (var-get stream-nonce) u1))
        (block-time (unwrap-panic (get-block-info? time (- block-height u1))))
       )
    
    ;; Ensure creator is registered
    (asserts! (is-some creator-info) ERR-NOT-FOUND)
    (asserts! (get active (unwrap-panic creator-info)) ERR-INACTIVE)
    
    ;; Set the new stream
    (map-set Streams
      { stream-id: stream-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        started-at: block-time,
        ended-at: u0,
        storage-url: storage-url,
        category: category,
        active: true,
        viewer-count: u0,
        total-points-awarded: u0
      }
    )
    
    ;; Update the nonce
    (var-set stream-nonce stream-id)
    
    (ok stream-id)
  )
)

