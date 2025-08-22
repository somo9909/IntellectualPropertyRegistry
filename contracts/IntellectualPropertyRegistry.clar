;; Intellectual Property Registry Contract
;; A blockchain-based system for registering and licensing intellectual property

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-ip-owner (err u101))
(define-constant err-ip-already-exists (err u102))
(define-constant err-ip-not-found (err u103))
(define-constant err-invalid-license-fee (err u104))  
(define-constant err-insufficient-payment (err u105))

;; Data structures
(define-map intellectual-properties 
  { ip-id: uint }
  { 
    owner: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    ip-type: (string-ascii 20),
    registration-date: uint,
    license-fee: uint,
    is-licensable: bool
  })

(define-map ip-licenses
  { ip-id: uint, licensee: principal }
  {
    license-date: uint,
    fee-paid: uint,
    is-active: bool
  })

;; Tracking variables
(define-data-var next-ip-id uint u1)
(define-data-var total-registrations uint u0)

;; Function 1: Register Intellectual Property
(define-public (register-ip 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (ip-type (string-ascii 20))
  (license-fee uint)
  (is-licensable bool))
  (let 
    (
      (ip-id (var-get next-ip-id))
      (registration-date stacks-block-height)
    )
    (begin
      ;; Check if IP with same title already exists (simplified check)
      (asserts! (is-none (map-get? intellectual-properties {ip-id: ip-id})) err-ip-already-exists)
      
      ;; Register the IP
      (map-set intellectual-properties
        {ip-id: ip-id}
        {
          owner: tx-sender,
          title: title,
          description: description,
          ip-type: ip-type,
          registration-date: registration-date,
          license-fee: license-fee,
          is-licensable: is-licensable
        })
      
      ;; Update tracking variables
      (var-set next-ip-id (+ ip-id u1))
      (var-set total-registrations (+ (var-get total-registrations) u1))
      
      ;; Return success with IP ID
      (ok {ip-id: ip-id, message: "IP successfully registered"}))))

;; Function 2: Purchase License for Intellectual Property
(define-public (purchase-license (ip-id uint))
  (let
    (
      (ip-data (unwrap! (map-get? intellectual-properties {ip-id: ip-id}) err-ip-not-found))
      (license-fee (get license-fee ip-data))
      (ip-owner (get owner ip-data))
      (license-date stacks-block-height)
    )
    (begin
      ;; Check if IP exists and is licensable
      (asserts! (get is-licensable ip-data) err-invalid-license-fee)
      
      ;; Check if licensee is not the owner
      (asserts! (not (is-eq tx-sender ip-owner)) err-not-ip-owner)
      
      ;; Check if license fee is greater than 0
      (asserts! (> license-fee u0) err-invalid-license-fee)
      
      ;; Transfer payment from licensee to IP owner
      (try! (stx-transfer? license-fee tx-sender ip-owner))
      
      ;; Record the license
      (map-set ip-licenses
        {ip-id: ip-id, licensee: tx-sender}
        {
          license-date: license-date,
          fee-paid: license-fee,
          is-active: true
        })
      
      ;; Return success
      (ok {
        ip-id: ip-id,
        licensee: tx-sender,
        fee-paid: license-fee,
        message: "License successfully purchased"
      }))))

;; Read-only functions for querying data

;; Get IP details
(define-read-only (get-ip-details (ip-id uint))
  (ok (map-get? intellectual-properties {ip-id: ip-id})))

;; Get license details
(define-read-only (get-license-details (ip-id uint) (licensee principal))
  (ok (map-get? ip-licenses {ip-id: ip-id, licensee: licensee})))

;; Get total registrations
(define-read-only (get-total-registrations)
  (ok (var-get total-registrations)))

;; Check if user has license for specific IP
(define-read-only (has-license (ip-id uint) (user principal))
  (let ((license-data (map-get? ip-licenses {ip-id: ip-id, licensee: user})))
    (ok (and (is-some license-data) 
             (get is-active (unwrap! license-data (err u0)))))))