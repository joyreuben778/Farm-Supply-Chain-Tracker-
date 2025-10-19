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

(define-map CarbonFootprint
  { batch-id: uint }
  {
    total-emissions: uint,
    transportation-co2: uint,
    energy-usage-co2: uint,
    water-usage-liters: uint,
    fertilizer-co2: uint,
    sustainability-score: uint,
    carbon-neutral: bool,
    last-updated: uint,
    calculated-by: principal
  }
)

(define-map CarbonEvents
  { batch-id: uint, event-id: uint }
  {
    event-type: (string-ascii 30),
    co2-amount: uint,
    distance-km: uint,
    energy-kwh: uint,
    recorded-by: principal,
    timestamp: uint,
    notes: (string-ascii 150)
  }
)

(define-map CarbonOffsets
  { batch-id: uint, offset-id: uint }
  {
    offset-type: (string-ascii 30),
    co2-offset: uint,
    verification-authority: (string-ascii 50),
    offset-date: uint,
    cost-per-ton: uint,
    verified: bool
  }
)

(define-data-var last-carbon-event-id uint u0)
(define-data-var last-offset-id uint u0)

(define-constant err-invalid-emissions (err u500))
(define-constant err-footprint-not-found (err u501))
(define-constant err-insufficient-offset (err u502))

(define-public (initialize-carbon-tracking (batch-id uint))
  (begin
    (asserts! (is-eq tx-sender (unwrap! (nft-get-owner? farm-batch batch-id) err-not-found)) err-owner-only)
    (asserts! (is-some (map-get? BatchDetails { batch-id: batch-id })) err-batch-not-found)
    (map-set CarbonFootprint
      { batch-id: batch-id }
      {
        total-emissions: u0,
        transportation-co2: u0,
        energy-usage-co2: u0,
        water-usage-liters: u0,
        fertilizer-co2: u0,
        sustainability-score: u0,
        carbon-neutral: false,
        last-updated: stacks-block-height,
        calculated-by: tx-sender
      }
    )
    (ok true)))

(define-public (record-carbon-event (batch-id uint)
                                  (event-type (string-ascii 30))
                                  (co2-amount uint)
                                  (distance-km uint)
                                  (energy-kwh uint)
                                  (notes (string-ascii 150)))
  (let ((new-event-id (+ (var-get last-carbon-event-id) u1))
        (current-footprint (unwrap! (map-get? CarbonFootprint { batch-id: batch-id }) err-footprint-not-found)))
    (asserts! (is-eq tx-sender (unwrap! (nft-get-owner? farm-batch batch-id) err-not-found)) err-owner-only)
    (map-set CarbonEvents
      { batch-id: batch-id, event-id: new-event-id }
      {
        event-type: event-type,
        co2-amount: co2-amount,
        distance-km: distance-km,
        energy-kwh: energy-kwh,
        recorded-by: tx-sender,
        timestamp: stacks-block-height,
        notes: notes
      }
    )
    (let ((updated-total (+ (get total-emissions current-footprint) co2-amount))
          (updated-transport (if (is-eq event-type "transport") 
                               (+ (get transportation-co2 current-footprint) co2-amount)
                               (get transportation-co2 current-footprint)))
          (updated-energy (if (is-eq event-type "energy")
                            (+ (get energy-usage-co2 current-footprint) co2-amount)
                            (get energy-usage-co2 current-footprint)))
          (updated-fertilizer (if (is-eq event-type "fertilizer")
                                (+ (get fertilizer-co2 current-footprint) co2-amount)
                                (get fertilizer-co2 current-footprint))))
      (map-set CarbonFootprint
        { batch-id: batch-id }
        (merge current-footprint
          {
            total-emissions: updated-total,
            transportation-co2: updated-transport,
            energy-usage-co2: updated-energy,
            fertilizer-co2: updated-fertilizer,
            sustainability-score: (calculate-sustainability-score updated-total),
            last-updated: stacks-block-height
          }
        )
      ))
    (var-set last-carbon-event-id new-event-id)
    (ok new-event-id)))

(define-public (purchase-carbon-offset (batch-id uint)
                                     (offset-type (string-ascii 30))
                                     (co2-offset uint)
                                     (verification-authority (string-ascii 50))
                                     (cost-per-ton uint))
  (let ((new-offset-id (+ (var-get last-offset-id) u1))
        (current-footprint (unwrap! (map-get? CarbonFootprint { batch-id: batch-id }) err-footprint-not-found)))
    (asserts! (is-eq tx-sender (unwrap! (nft-get-owner? farm-batch batch-id) err-not-found)) err-owner-only)
    (asserts! (> co2-offset u0) err-invalid-emissions)
    (map-set CarbonOffsets
      { batch-id: batch-id, offset-id: new-offset-id }
      {
        offset-type: offset-type,
        co2-offset: co2-offset,
        verification-authority: verification-authority,
        offset-date: stacks-block-height,
        cost-per-ton: cost-per-ton,
        verified: false
      }
    )
    (let ((total-emissions (get total-emissions current-footprint))
          (is-now-neutral (>= co2-offset total-emissions)))
      (map-set CarbonFootprint
        { batch-id: batch-id }
        (merge current-footprint
          {
            carbon-neutral: is-now-neutral,
            sustainability-score: (if is-now-neutral u100 (get sustainability-score current-footprint)),
            last-updated: stacks-block-height
          }
        )
      ))
    (var-set last-offset-id new-offset-id)
    (ok new-offset-id)))

(define-public (verify-carbon-offset (batch-id uint) (offset-id uint))
  (let ((offset-info (unwrap! (map-get? CarbonOffsets { batch-id: batch-id, offset-id: offset-id }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set CarbonOffsets
      { batch-id: batch-id, offset-id: offset-id }
      (merge offset-info { verified: true })
    )
    (ok true)))

(define-private (calculate-sustainability-score (total-emissions uint))
  (if (<= total-emissions u1000) u90
    (if (<= total-emissions u2500) u75
      (if (<= total-emissions u5000) u60
        (if (<= total-emissions u10000) u45
          (if (<= total-emissions u20000) u30
            u15))))))

(define-read-only (get-carbon-footprint (batch-id uint))
  (ok (map-get? CarbonFootprint { batch-id: batch-id })))

(define-read-only (get-carbon-event (batch-id uint) (event-id uint))
  (ok (map-get? CarbonEvents { batch-id: batch-id, event-id: event-id })))

(define-read-only (get-carbon-offset (batch-id uint) (offset-id uint))
  (ok (map-get? CarbonOffsets { batch-id: batch-id, offset-id: offset-id })))

(define-read-only (is-carbon-neutral (batch-id uint))
  (match (map-get? CarbonFootprint { batch-id: batch-id })
    footprint-data (ok (get carbon-neutral footprint-data))
    (ok false)))

(define-read-only (get-sustainability-rating (batch-id uint))
  (match (map-get? CarbonFootprint { batch-id: batch-id })
    footprint-data
    (let ((score (get sustainability-score footprint-data)))
      (ok (if (>= score u90) "A+"
            (if (>= score u75) "A"
              (if (>= score u60) "B"
                (if (>= score u45) "C"
                  "D"))))))
    err-footprint-not-found))

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

;; === SUPPLY CHAIN AUDIT TRAIL SYSTEM ===
;; Independent feature for comprehensive audit tracking and analytics

(define-map AuditTrails
  { batch-id: uint, audit-id: uint }
  {
    audit-type: (string-ascii 40),
    auditor: principal,
    audit-scope: (string-ascii 50),
    compliance-score: uint,
    findings-count: uint,
    critical-issues: uint,
    recommendations: (string-ascii 300),
    audit-date: uint,
    completion-date: uint,
    audit-status: (string-ascii 20),
    verification-hash: (string-ascii 64)
  }
)

(define-map AuditFindings
  { batch-id: uint, audit-id: uint, finding-id: uint }
  {
    severity-level: (string-ascii 10),
    category: (string-ascii 30),
    description: (string-ascii 200),
    location: (string-ascii 50),
    detected-at: uint,
    resolved: bool,
    resolution-date: (optional uint),
    corrective-action: (string-ascii 150)
  }
)

(define-map BatchAnalytics
  { batch-id: uint }
  {
    total-audits: uint,
    avg-compliance-score: uint,
    last-audit-date: uint,
    risk-level: (string-ascii 10),
    chain-integrity-score: uint,
    data-completeness: uint,
    traceability-index: uint,
    overall-health: (string-ascii 15)
  }
)

(define-map AuditMetrics
  { metric-id: uint }
  {
    total-batches-audited: uint,
    avg-global-compliance: uint,
    high-risk-batches: uint,
    audit-frequency: uint,
    improvement-trend: int,
    last-calculated: uint
  }
)

(define-data-var last-audit-id uint u0)
(define-data-var last-finding-id uint u0)
(define-data-var global-metric-id uint u1)

(define-constant err-audit-not-found (err u600))
(define-constant err-invalid-audit-data (err u601))
(define-constant err-audit-in-progress (err u602))
(define-constant err-insufficient-permissions (err u603))
(define-constant err-invalid-compliance-score (err u604))

(define-public (initiate-audit (batch-id uint)
                             (audit-type (string-ascii 40))
                             (audit-scope (string-ascii 50))
                             (verification-hash (string-ascii 64)))
  (let ((new-audit-id (+ (var-get last-audit-id) u1)))
    (asserts! (is-some (map-get? BatchDetails { batch-id: batch-id })) err-batch-not-found)
    (asserts! (> (len audit-type) u0) err-invalid-audit-data)
    (map-set AuditTrails
      { batch-id: batch-id, audit-id: new-audit-id }
      {
        audit-type: audit-type,
        auditor: tx-sender,
        audit-scope: audit-scope,
        compliance-score: u0,
        findings-count: u0,
        critical-issues: u0,
        recommendations: "",
        audit-date: stacks-block-height,
        completion-date: u0,
        audit-status: "in-progress",
        verification-hash: verification-hash
      }
    )
    (var-set last-audit-id new-audit-id)
    (ok new-audit-id)))

(define-public (add-audit-finding (batch-id uint)
                                (audit-id uint)
                                (severity-level (string-ascii 10))
                                (category (string-ascii 30))
                                (description (string-ascii 200))
                                (location (string-ascii 50))
                                (corrective-action (string-ascii 150)))
  (let ((new-finding-id (+ (var-get last-finding-id) u1))
        (audit-info (unwrap! (map-get? AuditTrails { batch-id: batch-id, audit-id: audit-id }) err-audit-not-found)))
    (asserts! (is-eq tx-sender (get auditor audit-info)) err-insufficient-permissions)
    (asserts! (is-eq (get audit-status audit-info) "in-progress") err-audit-in-progress)
    (map-set AuditFindings
      { batch-id: batch-id, audit-id: audit-id, finding-id: new-finding-id }
      {
        severity-level: severity-level,
        category: category,
        description: description,
        location: location,
        detected-at: stacks-block-height,
        resolved: false,
        resolution-date: none,
        corrective-action: corrective-action
      }
    )
    ;; Update audit trail with new finding count
    (let ((updated-findings (+ (get findings-count audit-info) u1))
          (updated-critical (if (is-eq severity-level "critical")
                              (+ (get critical-issues audit-info) u1)
                              (get critical-issues audit-info))))
      (map-set AuditTrails
        { batch-id: batch-id, audit-id: audit-id }
        (merge audit-info
          { findings-count: updated-findings, critical-issues: updated-critical })
      ))
    (var-set last-finding-id new-finding-id)
    (ok new-finding-id)))

(define-public (complete-audit (batch-id uint)
                             (audit-id uint)
                             (compliance-score uint)
                             (recommendations (string-ascii 300)))
  (let ((audit-info (unwrap! (map-get? AuditTrails { batch-id: batch-id, audit-id: audit-id }) err-audit-not-found)))
    (asserts! (is-eq tx-sender (get auditor audit-info)) err-insufficient-permissions)
    (asserts! (is-eq (get audit-status audit-info) "in-progress") err-audit-in-progress)
    (asserts! (<= compliance-score u100) err-invalid-compliance-score)
    (map-set AuditTrails
      { batch-id: batch-id, audit-id: audit-id }
      (merge audit-info
        {
          compliance-score: compliance-score,
          recommendations: recommendations,
          completion-date: stacks-block-height,
          audit-status: "completed"
        }
      )
    )
    ;; Update batch analytics
    (try! (update-batch-analytics batch-id))
    (ok true)))

(define-public (resolve-finding (batch-id uint)
                              (audit-id uint)
                              (finding-id uint)
                              (corrective-action (string-ascii 150)))
  (let ((finding-info (unwrap! (map-get? AuditFindings { batch-id: batch-id, audit-id: audit-id, finding-id: finding-id }) err-not-found)))
    (asserts! (is-eq tx-sender (unwrap! (nft-get-owner? farm-batch batch-id) err-not-found)) err-owner-only)
    (map-set AuditFindings
      { batch-id: batch-id, audit-id: audit-id, finding-id: finding-id }
      (merge finding-info
        {
          resolved: true,
          resolution-date: (some stacks-block-height),
          corrective-action: corrective-action
        }
      )
    )
    (ok true)))

(define-private (update-batch-analytics (batch-id uint))
  (match (get-latest-audit-for-batch batch-id)
    latest-audit
    (let ((current-analytics (default-to
                              { total-audits: u0, avg-compliance-score: u0, last-audit-date: u0,
                                risk-level: "unknown", chain-integrity-score: u0, data-completeness: u0,
                                traceability-index: u0, overall-health: "unknown" }
                              (map-get? BatchAnalytics { batch-id: batch-id })))
          (audit-count (+ (get total-audits current-analytics) u1))
          (new-avg-score (calculate-average-compliance batch-id audit-count))
          (risk-level (calculate-risk-level new-avg-score (get critical-issues latest-audit)))
          (integrity-score (calculate-chain-integrity batch-id))
          (completeness (calculate-data-completeness batch-id))
          (traceability (calculate-traceability-index batch-id))
          (health-status (determine-overall-health new-avg-score risk-level)))
      (map-set BatchAnalytics
        { batch-id: batch-id }
        {
          total-audits: audit-count,
          avg-compliance-score: new-avg-score,
          last-audit-date: stacks-block-height,
          risk-level: risk-level,
          chain-integrity-score: integrity-score,
          data-completeness: completeness,
          traceability-index: traceability,
          overall-health: health-status
        }
      )
      (ok true))
    (err u0)))

(define-private (get-latest-audit-for-batch (batch-id uint))
  (let ((current-audit-id (var-get last-audit-id)))
    (map-get? AuditTrails { batch-id: batch-id, audit-id: current-audit-id })))

(define-private (calculate-average-compliance (batch-id uint) (audit-count uint))
  (if (is-eq audit-count u1)
    (match (get-latest-audit-for-batch batch-id)
      latest-audit (get compliance-score latest-audit)
      u0)
    (let ((current-avg (get avg-compliance-score (default-to { avg-compliance-score: u0 } (map-get? BatchAnalytics { batch-id: batch-id }))))
          (new-score (match (get-latest-audit-for-batch batch-id)
                       latest-audit (get compliance-score latest-audit)
                       u0)))
      (/ (+ (* current-avg (- audit-count u1)) new-score) audit-count))))

(define-private (calculate-risk-level (compliance-score uint) (critical-issues uint))
  (if (and (>= compliance-score u90) (is-eq critical-issues u0)) "low"
    (if (and (>= compliance-score u70) (<= critical-issues u2)) "medium"
      "high")))

(define-private (calculate-chain-integrity (batch-id uint))
  (let ((batch-details (map-get? BatchDetails { batch-id: batch-id }))
        (stage-count (if (is-some batch-details) u1 u0))
        (inspection-count (if (is-some (map-get? QualityInspections { batch-id: batch-id, inspection-id: u1 })) u1 u0))
        (temp-readings (if (is-some (map-get? TemperatureReadings { batch-id: batch-id, reading-id: u1 })) u1 u0)))
    (+ (* stage-count u30) (* inspection-count u35) (* temp-readings u35))))

(define-private (calculate-data-completeness (batch-id uint))
  (let ((has-batch (if (is-some (map-get? BatchDetails { batch-id: batch-id })) u20 u0))
        (has-expiration (if (is-some (map-get? BatchExpiration { batch-id: batch-id })) u20 u0))
        (has-carbon (if (is-some (map-get? CarbonFootprint { batch-id: batch-id })) u20 u0))
        (has-certification (if (is-some (map-get? BatchCertifications { batch-id: batch-id, certification-id: u1 })) u20 u0))
        (has-inspection (if (is-some (map-get? QualityInspections { batch-id: batch-id, inspection-id: u1 })) u20 u0)))
    (+ has-batch has-expiration has-carbon has-certification has-inspection)))

(define-private (calculate-traceability-index (batch-id uint))
  (let ((integrity (calculate-chain-integrity batch-id))
        (completeness (calculate-data-completeness batch-id)))
    (/ (+ integrity completeness) u2)))

(define-private (determine-overall-health (compliance uint) (risk-level (string-ascii 10)))
  (if (and (>= compliance u85) (is-eq risk-level "low")) "excellent"
    (if (and (>= compliance u70) (not (is-eq risk-level "high"))) "good"
      (if (>= compliance u50) "fair"
        "poor"))))

;; Read-only functions for audit trail access

(define-read-only (get-audit-details (batch-id uint) (audit-id uint))
  (ok (map-get? AuditTrails { batch-id: batch-id, audit-id: audit-id })))

(define-read-only (get-audit-finding (batch-id uint) (audit-id uint) (finding-id uint))
  (ok (map-get? AuditFindings { batch-id: batch-id, audit-id: audit-id, finding-id: finding-id })))

(define-read-only (get-batch-analytics (batch-id uint))
  (ok (map-get? BatchAnalytics { batch-id: batch-id })))

(define-read-only (get-compliance-summary (batch-id uint))
  (match (map-get? BatchAnalytics { batch-id: batch-id })
    analytics
    (ok {
      batch-id: batch-id,
      compliance-score: (get avg-compliance-score analytics),
      risk-level: (get risk-level analytics),
      health-status: (get overall-health analytics),
      last-audited: (get last-audit-date analytics),
      audit-count: (get total-audits analytics)
    })
    err-audit-not-found))

(define-read-only (get-audit-history-summary (batch-id uint))
  (let ((analytics (map-get? BatchAnalytics { batch-id: batch-id })))
    (if (is-some analytics)
      (ok {
        total-audits: (get total-audits (unwrap-panic analytics)),
        avg-compliance: (get avg-compliance-score (unwrap-panic analytics)),
        integrity-score: (get chain-integrity-score (unwrap-panic analytics)),
        traceability-index: (get traceability-index (unwrap-panic analytics)),
        data-completeness: (get data-completeness (unwrap-panic analytics))
      })
      (ok {
        total-audits: u0,
        avg-compliance: u0,
        integrity-score: u0,
        traceability-index: u0,
        data-completeness: u0
      }))))

(define-read-only (is-batch-audit-compliant (batch-id uint) (min-score uint))
  (match (map-get? BatchAnalytics { batch-id: batch-id })
    analytics (ok (>= (get avg-compliance-score analytics) min-score))
    (ok false)))

(define-read-only (get-current-audit-metrics)
  (ok (default-to
        { total-batches-audited: u0, avg-global-compliance: u0, high-risk-batches: u0,
          audit-frequency: u0, improvement-trend: 0, last-calculated: u0 }
        (map-get? AuditMetrics { metric-id: (var-get global-metric-id) }))))
