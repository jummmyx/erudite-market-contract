;; Erudite Market - Peer Knowledge Economy System
;; A decentralized platform for trading knowledge capital and specialized capabilities
;; through a tokenized expertise economy powered by Stacks blockchain

;; ========== Participant Data Structures ==========
;; Tracks each participant's expertise contribution capacity
(define-map participant-expertise-repository principal uint)

;; Tracks each participant's financial reserves
(define-map participant-financial-reserves principal uint)

;; Details of expertise offered for exchange
(define-map available-expertise-offerings {contributor: principal} {expertise-units: uint, compensation-rate: uint})

;; ========== Reputation Management System ==========
;; Individual reputation assessments
(define-map expertise-quality-assessments {contributor: principal, evaluator: principal} uint)

;; Total assessments received by contributor
(define-map contributor-assessment-count principal uint)

;; Cumulative assessment score for contributor
(define-map contributor-assessment-aggregate principal uint)

;; ========== Exchange Proposal Framework ==========
;; Complete proposal lifecycle data
(define-map expertise-acquisition-proposals 
  {proposal-identifier: uint} 
  {
    initiator: principal,
    contributor: principal,
    expertise-units: uint,
    proposed-compensation: uint,
    proposal-state: uint, ;; 0=awaiting response, 1=accepted, 2=declined, 3=fulfilled
    timestamp: uint
  }
)
(define-data-var next-proposal-identifier uint u1)

;; ========== Core System Constants ==========
(define-constant contract-administrator tx-sender)
(define-constant error-unauthorized-admin (err u200))
(define-constant error-expertise-deficit (err u201))
(define-constant error-invalid-expertise-type (err u202))
(define-constant error-invalid-compensation (err u203))
(define-constant error-global-capacity-exceeded (err u204))
(define-constant error-operation-forbidden (err u205))

;; ========== Network Configuration Variables ==========
;; Base value for expertise units in microstacks
(define-data-var expertise-valuation uint u10)

;; Maximum expertise units a participant can contribute to network
(define-data-var max-participant-contribution uint u100)

;; Platform facilitation percentage applied to transactions
(define-data-var network-facilitation-fee uint u10)

;; Current aggregate expertise units in circulation
(define-data-var aggregate-expertise-pool uint u0)

;; Maximum aggregate expertise units allowed in network ecosystem
(define-data-var expertise-ecosystem-capacity uint u1000)



;; ========== Private Operational Functions ==========

;; Calculate network facilitation allocation for transactions
(define-private (calculate-facilitation-allocation (transaction-value uint))
  (/ (* transaction-value (var-get network-facilitation-fee)) u100))

;; Manage expertise ecosystem capacity
(define-private (adjust-expertise-ecosystem (delta-units int))
  (let (
    (current-pool (var-get aggregate-expertise-pool))
    (adjusted-pool (if (< delta-units 0)
                     (if (>= current-pool (to-uint (- 0 delta-units)))
                         (- current-pool (to-uint (- 0 delta-units)))
                         u0)
                     (+ current-pool (to-uint delta-units))))
  )
    (asserts! (<= adjusted-pool (var-get expertise-ecosystem-capacity)) error-global-capacity-exceeded)
    (var-set aggregate-expertise-pool adjusted-pool)
    (ok true)))

;; ========== Participant Interface Functions ==========

;; Register expertise to participant portfolio
;; Allows participants to formalize their capacity to contribute expertise
;; @param units: quantified expertise units to be registered
(define-public (register-expertise-capacity (units uint))
  (let (
    (current-capacity (default-to u0 (map-get? participant-expertise-repository tx-sender)))
    (ecosystem-limit (var-get max-participant-contribution))
    (projected-capacity (+ current-capacity units))
  )
    (asserts! (> units u0) error-invalid-expertise-type)
    (asserts! (<= projected-capacity ecosystem-limit) (err u211))
    (map-set participant-expertise-repository tx-sender projected-capacity)
    (ok projected-capacity)))

;; Publish expertise offering for network exchange
;; @param units: quantified expertise units available for exchange
;; @param compensation: requested compensation rate per unit
(define-public (publish-expertise-offering (units uint) (compensation uint))
  (let (
    (current-capacity (default-to u0 (map-get? participant-expertise-repository tx-sender)))
    (current-published (get expertise-units (default-to {expertise-units: u0, compensation-rate: u0} 
                      (map-get? available-expertise-offerings {contributor: tx-sender}))))
    (total-published (+ units current-published))
  )
    (asserts! (> units u0) error-invalid-expertise-type)
    (asserts! (> compensation u0) error-invalid-compensation)
    (asserts! (>= current-capacity total-published) error-expertise-deficit)
    (try! (adjust-expertise-ecosystem (to-int units)))
    (map-set available-expertise-offerings {contributor: tx-sender} 
             {expertise-units: total-published, compensation-rate: compensation})
    (ok true)))

;; Withdraw expertise from exchange marketplace
;; @param units: quantified expertise units to withdraw from availability
(define-public (withdraw-expertise-offering (units uint))
  (let (
    (current-published (get expertise-units (default-to {expertise-units: u0, compensation-rate: u0} 
                      (map-get? available-expertise-offerings {contributor: tx-sender}))))
  )
    (asserts! (>= current-published units) error-expertise-deficit)
    (try! (adjust-expertise-ecosystem (to-int (- units))))
    (map-set available-expertise-offerings {contributor: tx-sender} 
             {expertise-units: (- current-published units), 
              compensation-rate: (get compensation-rate (default-to {expertise-units: u0, compensation-rate: u0} 
                               (map-get? available-expertise-offerings {contributor: tx-sender})))})
    (ok true)))

;; Directly acquire expertise from network contributor
;; @param contributor: principal identifier of expertise contributor
;; @param units: quantified expertise units to acquire
(define-public (acquire-expertise (contributor principal) (units uint))
  (let (
    (offering-data (default-to {expertise-units: u0, compensation-rate: u0} 
                  (map-get? available-expertise-offerings {contributor: contributor})))
    (transaction-value (* units (get compensation-rate offering-data)))
    (facilitation-allocation (calculate-facilitation-allocation transaction-value))
    (total-transaction-cost (+ transaction-value facilitation-allocation))
    (contributor-capacity (default-to u0 (map-get? participant-expertise-repository contributor)))
    (acquirer-reserves (default-to u0 (map-get? participant-financial-reserves tx-sender)))
    (contributor-reserves (default-to u0 (map-get? participant-financial-reserves contributor)))
  )
    (asserts! (not (is-eq tx-sender contributor)) error-operation-forbidden)
    (asserts! (> units u0) error-invalid-expertise-type)
    (asserts! (>= (get expertise-units offering-data) units) error-expertise-deficit)
    (asserts! (>= contributor-capacity units) error-expertise-deficit)
    (asserts! (>= acquirer-reserves total-transaction-cost) error-expertise-deficit)

    ;; Update contributor's expertise capacity and offerings
    (map-set participant-expertise-repository contributor (- contributor-capacity units))
    (map-set available-expertise-offerings {contributor: contributor} 
             {expertise-units: (- (get expertise-units offering-data) units), 
              compensation-rate: (get compensation-rate offering-data)})

    ;; Update financial reserves and distribute compensation
    (map-set participant-financial-reserves tx-sender (- acquirer-reserves total-transaction-cost))
    (map-set participant-expertise-repository tx-sender 
             (+ (default-to u0 (map-get? participant-expertise-repository tx-sender)) units))
    (map-set participant-financial-reserves contributor (+ contributor-reserves transaction-value))
    (map-set participant-financial-reserves contract-administrator 
             (+ (default-to u0 (map-get? participant-financial-reserves contract-administrator)) facilitation-allocation))

    (ok true)))

;; Fund participant financial reserves
;; Allows participants to deposit STX into the platform for future transactions
;; @param amount: the amount of STX (in ustx) to deposit
(define-public (fund-participant-reserves (amount uint))
  (let (
    (participant tx-sender)
    (current-reserves (default-to u0 (map-get? participant-financial-reserves participant)))
    (updated-reserves (+ current-reserves amount))
  )
    (asserts! (> amount u0) (err u210))
    (try! (stx-transfer? amount participant (as-contract tx-sender)))
    (map-set participant-financial-reserves participant updated-reserves)
    (ok updated-reserves)))

;; Evaluate expertise contributor quality
;; Allows participants to rate contributors after expertise exchange
;; @param contributor: the principal of the contributor being evaluated
;; @param quality-score: the score (1-5) assigned to the contributor
(define-public (evaluate-contributor (contributor principal) (quality-score uint))
  (let (
    (evaluator tx-sender)
    (previous-evaluation (default-to u0 (map-get? expertise-quality-assessments 
                        {contributor: contributor, evaluator: evaluator})))
    (evaluation-count (default-to u0 (map-get? contributor-assessment-count contributor)))
    (evaluation-aggregate (default-to u0 (map-get? contributor-assessment-aggregate contributor)))
    (adjusted-count (if (is-eq previous-evaluation u0) (+ evaluation-count u1) evaluation-count))
    (adjusted-aggregate (+ (- evaluation-aggregate previous-evaluation) quality-score))
  )
    (asserts! (not (is-eq evaluator contributor)) error-operation-forbidden)
    (asserts! (and (>= quality-score u1) (<= quality-score u5)) (err u215))

    ;; Update evaluation data structures
    (map-set expertise-quality-assessments {contributor: contributor, evaluator: evaluator} quality-score)
    (map-set contributor-assessment-count contributor adjusted-count)
    (map-set contributor-assessment-aggregate contributor adjusted-aggregate)

    (ok true)))

;; Submit expertise acquisition proposal
;; Initiates a formal proposal for expertise exchange with specific terms
;; @param contributor: principal of the expertise contributor
;; @param units: quantified expertise units requested
;; @param offered-compensation: compensation rate offered for the exchange
(define-public (submit-acquisition-proposal (contributor principal) (units uint) (offered-compensation uint))
  (let (
    (initiator tx-sender)
    (proposal-identifier (var-get next-proposal-identifier))
    (offering-data (default-to {expertise-units: u0, compensation-rate: u0} 
                  (map-get? available-expertise-offerings {contributor: contributor})))
    (transaction-value (* units offered-compensation))
    (facilitation-calculation (calculate-facilitation-allocation transaction-value))
    (total-transaction-cost (+ transaction-value facilitation-calculation))
    (initiator-reserves (default-to u0 (map-get? participant-financial-reserves initiator)))
  )
    (asserts! (not (is-eq initiator contributor)) error-operation-forbidden)
    (asserts! (> units u0) error-invalid-expertise-type)
    (asserts! (>= (get expertise-units offering-data) units) error-expertise-deficit)
    (asserts! (> offered-compensation u0) error-invalid-compensation)
    (asserts! (>= initiator-reserves total-transaction-cost) error-expertise-deficit)

    ;; Create the acquisition proposal
    (map-set expertise-acquisition-proposals
      {proposal-identifier: proposal-identifier}
      {
        initiator: initiator,
        contributor: contributor,
        expertise-units: units,
        proposed-compensation: offered-compensation,
        proposal-state: u0, ;; awaiting response
        timestamp: block-height
      }
    )

    ;; Reserve financial resources for the proposal
    (map-set participant-financial-reserves initiator (- initiator-reserves total-transaction-cost))

    ;; Update proposal tracking
    (var-set next-proposal-identifier (+ proposal-identifier u1))

    (ok proposal-identifier)))

;; Modify network configuration parameters
;; Allows administrator to adjust operational parameters of the network
;; @param updated-valuation: new base value for expertise units (in microstacks)
;; @param updated-contribution-limit: new maximum expertise units per participant
;; @param updated-facilitation-fee: new platform facilitation percentage
;; @param updated-ecosystem-capacity: new maximum aggregate expertise in ecosystem
(define-public (update-network-configuration 
               (updated-valuation uint) 
               (updated-contribution-limit uint) 
               (updated-facilitation-fee uint) 
               (updated-ecosystem-capacity uint))
  (begin
    (asserts! (is-eq tx-sender contract-administrator) error-unauthorized-admin)
    (asserts! (<= updated-facilitation-fee u100) (err u212))
    (asserts! (> updated-valuation u0) error-invalid-compensation)
    (asserts! (> updated-contribution-limit u0) (err u213))
    (asserts! (>= updated-ecosystem-capacity (var-get aggregate-expertise-pool)) (err u214))

    (var-set expertise-valuation updated-valuation)
    (var-set max-participant-contribution updated-contribution-limit)
    (var-set network-facilitation-fee updated-facilitation-fee)
    (var-set expertise-ecosystem-capacity updated-ecosystem-capacity)

    (ok true)))

;; ========== Additional Advanced Functions ==========

;; Process expertise exchange completion
;; Confirms successful completion of expertise exchange between participants
;; @param proposal-id: identifier of the proposal being completed
(define-public (complete-expertise-exchange (proposal-id uint))
  (let (
    (proposal-data (default-to 
                   {
                     initiator: tx-sender,
                     contributor: tx-sender,
                     expertise-units: u0,
                     proposed-compensation: u0,
                     proposal-state: u0,
                     timestamp: u0
                   } 
                   (map-get? expertise-acquisition-proposals {proposal-identifier: proposal-id})))
    (contributor (get contributor proposal-data))
    (initiator (get initiator proposal-data))
  )
    (asserts! (or (is-eq tx-sender contributor) (is-eq tx-sender initiator)) error-operation-forbidden)
    (asserts! (is-eq (get proposal-state proposal-data) u1) (err u216)) ;; Must be accepted proposal

    (ok true)))

;; Process expertise exchange response
;; Allows contributor to accept or decline an expertise exchange proposal
;; @param proposal-id: identifier of the proposal to respond to
;; @param accept: boolean indicating acceptance (true) or rejection (false)
(define-public (respond-to-acquisition-proposal (proposal-id uint) (accept bool))
  (let (
    (proposal-data (default-to 
                   {
                     initiator: tx-sender,
                     contributor: tx-sender,
                     expertise-units: u0,
                     proposed-compensation: u0,
                     proposal-state: u0,
                     timestamp: u0
                   } 
                   (map-get? expertise-acquisition-proposals {proposal-identifier: proposal-id})))
    (contributor (get contributor proposal-data))
    (initiator (get initiator proposal-data))
    (units (get expertise-units proposal-data))
    (compensation (get proposed-compensation proposal-data))
    (transaction-value (* units compensation))
    (facilitation-allocation (calculate-facilitation-allocation transaction-value))
    (contributor-capacity (default-to u0 (map-get? participant-expertise-repository contributor)))
    (contributor-reserves (default-to u0 (map-get? participant-financial-reserves contributor)))
  )
    (asserts! (is-eq tx-sender contributor) error-operation-forbidden)
    (asserts! (is-eq (get proposal-state proposal-data) u0) (err u217)) ;; Must be pending proposal

    (if accept
        (begin
          ;; Process accepted proposal
          (asserts! (>= contributor-capacity units) error-expertise-deficit)

          ;; Update contributor's expertise capacity
          (map-set participant-expertise-repository contributor (- contributor-capacity units))
          (map-set participant-expertise-repository initiator 
                   (+ (default-to u0 (map-get? participant-expertise-repository initiator)) units))

          ;; Update contributor's financial reserves
          (map-set participant-financial-reserves contributor (+ contributor-reserves transaction-value))
          (map-set participant-financial-reserves contract-administrator 
                   (+ (default-to u0 (map-get? participant-financial-reserves contract-administrator)) facilitation-allocation))

        )
        (begin
          ;; Process declined proposal
          (let (
            (initiator-reserves (default-to u0 (map-get? participant-financial-reserves initiator)))
            (total-refund (+ transaction-value facilitation-allocation))
          )
            ;; Return funds to initiator
            (map-set participant-financial-reserves initiator (+ initiator-reserves total-refund))

          )
        )
    )

    (ok accept)))

;; Query participant reputation score
;; Calculates and returns the current reputation score for a participant
;; @param participant: principal of the participant to query
(define-read-only (get-participant-reputation (participant principal))
  (let (
    (assessment-count (default-to u0 (map-get? contributor-assessment-count participant)))
    (assessment-total (default-to u0 (map-get? contributor-assessment-aggregate participant)))
  )
    (if (is-eq assessment-count u0)
        u0
        (/ assessment-total assessment-count))
  ))

;; Query available expertise offerings
;; Returns information about expertise currently available from a participant
;; @param contributor: principal of the potential contributor
(define-read-only (get-available-expertise (contributor principal))
  (default-to {expertise-units: u0, compensation-rate: u0} 
            (map-get? available-expertise-offerings {contributor: contributor})))

;; Query participant expertise capacity
;; Returns the current expertise capacity for a participant
;; @param participant: principal of the participant to query
(define-read-only (get-expertise-capacity (participant principal))
  (default-to u0 (map-get? participant-expertise-repository participant)))

;; Query participant financial reserves
;; Returns the current financial reserves for a participant
;; @param participant: principal of the participant to query
(define-read-only (get-financial-reserves (participant principal))
  (default-to u0 (map-get? participant-financial-reserves participant)))

;; Query network ecosystem metrics
;; Returns current operational metrics for the entire network
(define-read-only (get-network-metrics)
  {
    expertise-valuation: (var-get expertise-valuation),
    max-participant-contribution: (var-get max-participant-contribution),
    network-facilitation-fee: (var-get network-facilitation-fee),
    aggregate-expertise-pool: (var-get aggregate-expertise-pool),
    expertise-ecosystem-capacity: (var-get expertise-ecosystem-capacity),
    next-proposal-identifier: (var-get next-proposal-identifier)
  })

