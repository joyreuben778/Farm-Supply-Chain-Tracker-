(define-non-fungible-token farm-batch uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-stage (err u103))

(define-map BatchDetails
  { batch-id: uint }
  {
    farm-id: (string-ascii 34),
    product-type: (string-ascii 34),
    quantity: uint,
    planting-date: uint,
    current-stage: (string-ascii 10),
    last-updated: uint
  }
)

(define-map StageHistory
  { batch-id: uint, stage-id: uint }
  {
    stage-name: (string-ascii 10),
    timestamp: uint,
    handler: principal,
    location: (string-ascii 34),
    notes: (string-ascii 100)
  }
)

(define-data-var last-batch-id uint u0)
(define-data-var last-stage-id uint u0)

(define-public (create-batch (farm-id (string-ascii 34)) 
                           (product-type (string-ascii 34))
                           (quantity uint)
                           (planting-date uint))
  (let ((new-batch-id (+ (var-get last-batch-id) u1)))
    (try! (nft-mint? farm-batch new-batch-id tx-sender))
    (map-set BatchDetails
      { batch-id: new-batch-id }
      {
        farm-id: farm-id,
        product-type: product-type,
        quantity: quantity,
        planting-date: planting-date,
        current-stage: "planted",
        last-updated: stacks-block-height
      }
    )
    (var-set last-batch-id new-batch-id)
    (try! (record-stage new-batch-id "planted" tx-sender "farm" "Initial planting recorded"))
    (ok new-batch-id)))

(define-public (record-stage (batch-id uint) 
                           (stage-name (string-ascii 10))
                           (handler principal)
                           (location (string-ascii 34))
                           (notes (string-ascii 100)))
  (let ((new-stage-id (+ (var-get last-stage-id) u1))
        (batch (unwrap! (map-get? BatchDetails { batch-id: batch-id }) err-not-found)))
    (asserts! (is-eq tx-sender (unwrap! (nft-get-owner? farm-batch batch-id) err-not-found)) err-owner-only)
    (map-set StageHistory
      { batch-id: batch-id, stage-id: new-stage-id }
      {
        stage-name: stage-name,
        timestamp: stacks-block-height,
        handler: handler,
        location: location,
        notes: notes
      }
    )
    (map-set BatchDetails
      { batch-id: batch-id }
      (merge batch
        { current-stage: stage-name, last-updated: stacks-block-height })
    )
    (var-set last-stage-id new-stage-id)
    (ok new-stage-id)))
(define-read-only (get-batch-details (batch-id uint))
  (ok (map-get? BatchDetails { batch-id: batch-id })))

(define-read-only (get-stage-history (batch-id uint) (stage-id uint))
  (ok (map-get? StageHistory { batch-id: batch-id, stage-id: stage-id })))

(define-read-only (get-batch-owner (batch-id uint))
  (ok (nft-get-owner? farm-batch batch-id)))

(define-public (transfer-batch (batch-id uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (unwrap! (nft-get-owner? farm-batch batch-id) err-not-found)) err-owner-only)
    (try! (nft-transfer? farm-batch batch-id tx-sender recipient))
    (ok true)))
