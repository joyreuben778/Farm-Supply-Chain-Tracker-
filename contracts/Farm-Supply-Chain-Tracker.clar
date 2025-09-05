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

(define-map QualityInspections
  { batch-id: uint, inspection-id: uint }
  {
    inspector: principal,
    inspection-type: (string-ascii 20),
    quality-score: uint,
    passed: bool,
    inspection-date: uint,
    notes: (string-ascii 200),
    location: (string-ascii 50)
  }
)

(define-map AuthorizedInspectors
  { inspector: principal }
  {
    name: (string-ascii 50),
    certification: (string-ascii 30),
    authorized-by: principal,
    authorization-date: uint,
    active: bool
  }
)

(define-data-var last-inspection-id uint u0)

(define-constant err-not-authorized (err u200))
(define-constant err-invalid-score (err u201))
(define-constant err-batch-not-found (err u202))

(define-public (authorize-inspector (inspector principal)
                                  (name (string-ascii 50))
                                  (certification (string-ascii 30)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set AuthorizedInspectors
      { inspector: inspector }
      {
        name: name,
        certification: certification,
        authorized-by: tx-sender,
        authorization-date: stacks-block-height,
        active: true
      }
    )
    (ok true)))

(define-public (conduct-inspection (batch-id uint)
                                 (inspection-type (string-ascii 20))
                                 (quality-score uint)
                                 (passed bool)
                                 (notes (string-ascii 200))
                                 (location (string-ascii 50)))
  (let ((new-inspection-id (+ (var-get last-inspection-id) u1))
        (inspector-info (unwrap! (map-get? AuthorizedInspectors { inspector: tx-sender }) err-not-authorized)))
    (asserts! (get active inspector-info) err-not-authorized)
    (asserts! (<= quality-score u100) err-invalid-score)
    (asserts! (is-some (map-get? BatchDetails { batch-id: batch-id })) err-batch-not-found)
    (map-set QualityInspections
      { batch-id: batch-id, inspection-id: new-inspection-id }
      {
        inspector: tx-sender,
        inspection-type: inspection-type,
        quality-score: quality-score,
        passed: passed,
        inspection-date: stacks-block-height,
        notes: notes,
        location: location
      }
    )
    (var-set last-inspection-id new-inspection-id)
    (ok new-inspection-id)))

(define-public (revoke-inspector (inspector principal))
  (let ((inspector-info (unwrap! (map-get? AuthorizedInspectors { inspector: inspector }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set AuthorizedInspectors
      { inspector: inspector }
      (merge inspector-info { active: false })
    )
    (ok true)))

(define-read-only (get-inspection-details (batch-id uint) (inspection-id uint))
  (ok (map-get? QualityInspections { batch-id: batch-id, inspection-id: inspection-id })))

(define-read-only (get-inspector-info (inspector principal))
  (ok (map-get? AuthorizedInspectors { inspector: inspector })))

(define-read-only (is-authorized-inspector (inspector principal))
  (match (map-get? AuthorizedInspectors { inspector: inspector })
    inspector-data (ok (get active inspector-data))
    (ok false)))

    (define-map TemperatureReadings
  { batch-id: uint, reading-id: uint }
  {
    temperature: int,
    humidity: uint,
    recorded-by: principal,
    device-id: (string-ascii 30),
    timestamp: uint,
    location: (string-ascii 50),
    alert-triggered: bool
  }
)

(define-map TemperatureThresholds
  { batch-id: uint }
  {
    min-temp: int,
    max-temp: int,
    max-humidity: uint,
    set-by: principal,
    set-date: uint
  }
)

(define-map AuthorizedDevices
  { device-id: (string-ascii 30) }
  {
    device-name: (string-ascii 50),
    owner: principal,
    authorized-date: uint,
    active: bool
  }
)

(define-data-var last-reading-id uint u0)

(define-constant err-device-not-authorized (err u300))
(define-constant err-invalid-temperature (err u301))
(define-constant err-threshold-not-set (err u302))

(define-public (authorize-device (device-id (string-ascii 30))
                                (device-name (string-ascii 50))
                                (owner principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set AuthorizedDevices
      { device-id: device-id }
      {
        device-name: device-name,
        owner: owner,
        authorized-date: stacks-block-height,
        active: true
      }
    )
    (ok true)))

(define-public (set-temperature-threshold (batch-id uint)
                                        (min-temp int)
                                        (max-temp int)
                                        (max-humidity uint))
  (begin
    (asserts! (is-eq tx-sender (unwrap! (nft-get-owner? farm-batch batch-id) err-not-found)) err-owner-only)
    (asserts! (< min-temp max-temp) err-invalid-temperature)
    (map-set TemperatureThresholds
      { batch-id: batch-id }
      {
        min-temp: min-temp,
        max-temp: max-temp,
        max-humidity: max-humidity,
        set-by: tx-sender,
        set-date: stacks-block-height
      }
    )
    (ok true)))

(define-public (record-temperature (batch-id uint)
                                 (temperature int)
                                 (humidity uint)
                                 (device-id (string-ascii 30))
                                 (location (string-ascii 50)))
  (let ((new-reading-id (+ (var-get last-reading-id) u1))
        (device-info (unwrap! (map-get? AuthorizedDevices { device-id: device-id }) err-device-not-authorized))
        (threshold (map-get? TemperatureThresholds { batch-id: batch-id }))
        (alert-triggered (match threshold
                          threshold-data (or (< temperature (get min-temp threshold-data))
                                           (> temperature (get max-temp threshold-data))
                                           (> humidity (get max-humidity threshold-data)))
                          false)))
    (asserts! (get active device-info) err-device-not-authorized)
    (asserts! (is-some (map-get? BatchDetails { batch-id: batch-id })) err-batch-not-found)
    (map-set TemperatureReadings
      { batch-id: batch-id, reading-id: new-reading-id }
      {
        temperature: temperature,
        humidity: humidity,
        recorded-by: tx-sender,
        device-id: device-id,
        timestamp: stacks-block-height,
        location: location,
        alert-triggered: alert-triggered
      }
    )
    (var-set last-reading-id new-reading-id)
    (ok new-reading-id)))

(define-public (deactivate-device (device-id (string-ascii 30)))
  (let ((device-info (unwrap! (map-get? AuthorizedDevices { device-id: device-id }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set AuthorizedDevices
      { device-id: device-id }
      (merge device-info { active: false })
    )
    (ok true)))

(define-read-only (get-temperature-reading (batch-id uint) (reading-id uint))
  (ok (map-get? TemperatureReadings { batch-id: batch-id, reading-id: reading-id })))

(define-read-only (get-temperature-threshold (batch-id uint))
  (ok (map-get? TemperatureThresholds { batch-id: batch-id })))

(define-read-only (get-device-info (device-id (string-ascii 30)))
  (ok (map-get? AuthorizedDevices { device-id: device-id })))

(define-read-only (is-device-authorized (device-id (string-ascii 30)))
  (match (map-get? AuthorizedDevices { device-id: device-id })
    device-data (ok (get active device-data))
    (ok false)))

(define-map BatchExpiration
  { batch-id: uint }
  {
    expiration-date: uint,
    shelf-life-days: uint,
    is-expired: bool,
    recall-status: (string-ascii 20),
    recall-reason: (string-ascii 200),
    recall-initiated-by: (optional principal),
    recall-date: (optional uint)
  }
)

(define-map RecallNotifications
  { batch-id: uint, notification-id: uint }
  {
    recipient: principal,
    notification-type: (string-ascii 30),
    message: (string-ascii 300),
    sent-date: uint,
    acknowledged: bool
  }
)

(define-data-var last-notification-id uint u0)

(define-constant err-batch-expired (err u400))
(define-constant err-batch-recalled (err u401))
(define-constant err-invalid-shelf-life (err u402))
(define-constant err-expiration-not-set (err u403))

(define-public (set-batch-expiration (batch-id uint) (shelf-life-days uint))
  (let ((batch-details (unwrap! (map-get? BatchDetails { batch-id: batch-id }) err-not-found))
        (expiration-date (+ (get planting-date batch-details) (* shelf-life-days u144))))
    (asserts! (is-eq tx-sender (unwrap! (nft-get-owner? farm-batch batch-id) err-not-found)) err-owner-only)
    (asserts! (> shelf-life-days u0) err-invalid-shelf-life)
    (map-set BatchExpiration
      { batch-id: batch-id }
      {
        expiration-date: expiration-date,
        shelf-life-days: shelf-life-days,
        is-expired: (>= stacks-block-height expiration-date),
        recall-status: "none",
        recall-reason: "",
        recall-initiated-by: none,
        recall-date: none
      }
    )
    (ok true)))

(define-public (initiate-recall (batch-id uint) (reason (string-ascii 200)))
  (let ((expiration-info (unwrap! (map-get? BatchExpiration { batch-id: batch-id }) err-expiration-not-set)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set BatchExpiration
      { batch-id: batch-id }
      (merge expiration-info
        {
          recall-status: "recalled",
          recall-reason: reason,
          recall-initiated-by: (some tx-sender),
          recall-date: (some stacks-block-height)
        }
      )
    )
    (ok true)))

(define-public (send-recall-notification (batch-id uint)
                                       (recipient principal)
                                       (notification-type (string-ascii 30))
                                       (message (string-ascii 300)))
  (let ((new-notification-id (+ (var-get last-notification-id) u1)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? BatchExpiration { batch-id: batch-id })) err-expiration-not-set)
    (map-set RecallNotifications
      { batch-id: batch-id, notification-id: new-notification-id }
      {
        recipient: recipient,
        notification-type: notification-type,
        message: message,
        sent-date: stacks-block-height,
        acknowledged: false
      }
    )
    (var-set last-notification-id new-notification-id)
    (ok new-notification-id)))

(define-public (acknowledge-notification (batch-id uint) (notification-id uint))
  (let ((notification (unwrap! (map-get? RecallNotifications { batch-id: batch-id, notification-id: notification-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get recipient notification)) err-owner-only)
    (map-set RecallNotifications
      { batch-id: batch-id, notification-id: notification-id }
      (merge notification { acknowledged: true })
    )
    (ok true)))

(define-read-only (check-batch-status (batch-id uint))
  (match (map-get? BatchExpiration { batch-id: batch-id })
    expiration-data
    (let ((current-expired (>= stacks-block-height (get expiration-date expiration-data))))
      (ok {
        batch-id: batch-id,
        is-expired: current-expired,
        expiration-date: (get expiration-date expiration-data),
        recall-status: (get recall-status expiration-data),
        days-until-expiry: (if current-expired u0 (- (get expiration-date expiration-data) stacks-block-height))
      }))
    err-expiration-not-set))

(define-read-only (get-batch-expiration (batch-id uint))
  (ok (map-get? BatchExpiration { batch-id: batch-id })))

(define-read-only (get-recall-notification (batch-id uint) (notification-id uint))
  (ok (map-get? RecallNotifications { batch-id: batch-id, notification-id: notification-id })))

(define-read-only (is-batch-safe-for-transfer (batch-id uint))
  (match (map-get? BatchExpiration { batch-id: batch-id })
    expiration-data
    (let ((is-expired (>= stacks-block-height (get expiration-date expiration-data)))
          (is-recalled (is-eq (get recall-status expiration-data) "recalled")))
      (ok (and (not is-expired) (not is-recalled))))
    (ok true)))

(define-map BatchCertifications
  { batch-id: uint, certification-id: uint }
  {
    certification-type: (string-ascii 30),
    certifier: principal,
    issue-date: uint,
    expiry-date: uint,
    certificate-number: (string-ascii 50),
    is-valid: bool,
    verification-notes: (string-ascii 200)
  }
)

(define-map AuthorizedCertifiers
  { certifier: principal }
  {
    organization-name: (string-ascii 100),
    accreditation-body: (string-ascii 50),
    authorized-date: uint,
    active: bool,
    certification-types: (list 10 (string-ascii 30))
  }
)

(define-data-var last-certification-id uint u0)

(define-constant err-certifier-not-authorized (err u500))
(define-constant err-certification-type-not-allowed (err u501))
(define-constant err-certificate-expired (err u502))
(define-constant err-certificate-not-found (err u503))

(define-public (authorize-certifier (certifier principal)
                                  (organization-name (string-ascii 100))
                                  (accreditation-body (string-ascii 50))
                                  (certification-types (list 10 (string-ascii 30))))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set AuthorizedCertifiers
      { certifier: certifier }
      {
        organization-name: organization-name,
        accreditation-body: accreditation-body,
        authorized-date: stacks-block-height,
        active: true,
        certification-types: certification-types
      }
    )
    (ok true)))

(define-public (issue-certificate (batch-id uint)
                                (certification-type (string-ascii 30))
                                (expiry-date uint)
                                (certificate-number (string-ascii 50))
                                (verification-notes (string-ascii 200)))
  (let ((new-certification-id (+ (var-get last-certification-id) u1))
        (certifier-info (unwrap! (map-get? AuthorizedCertifiers { certifier: tx-sender }) err-certifier-not-authorized)))
    (asserts! (get active certifier-info) err-certifier-not-authorized)
    (asserts! (is-some (index-of (get certification-types certifier-info) certification-type)) err-certification-type-not-allowed)
    (asserts! (is-some (map-get? BatchDetails { batch-id: batch-id })) err-batch-not-found)
    (asserts! (> expiry-date stacks-block-height) err-certificate-expired)
    (map-set BatchCertifications
      { batch-id: batch-id, certification-id: new-certification-id }
      {
        certification-type: certification-type,
        certifier: tx-sender,
        issue-date: stacks-block-height,
        expiry-date: expiry-date,
        certificate-number: certificate-number,
        is-valid: true,
        verification-notes: verification-notes
      }
    )
    (var-set last-certification-id new-certification-id)
    (ok new-certification-id)))

(define-public (revoke-certificate (batch-id uint) (certification-id uint))
  (let ((certificate (unwrap! (map-get? BatchCertifications { batch-id: batch-id, certification-id: certification-id }) err-certificate-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (get certifier certificate))) err-owner-only)
    (map-set BatchCertifications
      { batch-id: batch-id, certification-id: certification-id }
      (merge certificate { is-valid: false })
    )
    (ok true)))

(define-public (deactivate-certifier (certifier principal))
  (let ((certifier-info (unwrap! (map-get? AuthorizedCertifiers { certifier: certifier }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set AuthorizedCertifiers
      { certifier: certifier }
      (merge certifier-info { active: false })
    )
    (ok true)))

(define-read-only (get-certificate (batch-id uint) (certification-id uint))
  (ok (map-get? BatchCertifications { batch-id: batch-id, certification-id: certification-id })))

(define-read-only (get-certifier-info (certifier principal))
  (ok (map-get? AuthorizedCertifiers { certifier: certifier })))

(define-read-only (is-certificate-valid (batch-id uint) (certification-id uint))
  (match (map-get? BatchCertifications { batch-id: batch-id, certification-id: certification-id })
    certificate-data
    (ok (and (get is-valid certificate-data) (< stacks-block-height (get expiry-date certificate-data))))
    (ok false)))

(define-read-only (get-batch-certifications-count (batch-id uint))
  (ok (var-get last-certification-id)))

(define-read-only (is-authorized-certifier (certifier principal))
  (match (map-get? AuthorizedCertifiers { certifier: certifier })
    certifier-data (ok (get active certifier-data))
    (ok false)))