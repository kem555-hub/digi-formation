;; DigiFormation - AI-Assisted DAO Governance Platform
;; A next-generation governance system with skill-based voting and dynamic reputation

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-params (err u104))
(define-constant err-proposal-closed (err u105))
(define-constant err-insufficient-reputation (err u106))

;; Data Variables
(define-data-var proposal-nonce uint u0)
(define-data-var committee-nonce uint u0)
(define-data-var min-reputation-threshold uint u10)

;; Member reputation and skills
(define-map members
    principal
    {
        reputation-score: uint,
        total-contributions: uint,
        joined-at: uint,
        active: bool
    }
)

;; Skill domains for expertise tracking
(define-map member-skills
    {member: principal, domain: (string-ascii 50)}
    {
        competency-level: uint,
        verified: bool,
        last-updated: uint
    }
)

;; Governance proposals
(define-map proposals
    uint
    {
        proposer: principal,
        title: (string-utf8 200),
        description: (string-utf8 1000),
        category: (string-ascii 50),
        required-expertise: (string-ascii 50),
        votes-for: uint,
        votes-against: uint,
        weighted-votes-for: uint,
        weighted-votes-against: uint,
        start-block: uint,
        end-block: uint,
        executed: bool,
        passed: bool
    }
)

;; Vote tracking
(define-map votes
    {proposal-id: uint, voter: principal}
    {
        support: bool,
        vote-weight: uint,
        expertise-multiplier: uint,
        block-height: uint
    }
)

;; Committees for specialized governance
(define-map committees
    uint
    {
        name: (string-utf8 100),
        domain: (string-ascii 50),
        min-expertise: uint,
        member-count: uint,
        active: bool,
        created-at: uint
    }
)

;; Committee membership
(define-map committee-members
    {committee-id: uint, member: principal}
    bool
)

;; Delegation system for liquid democracy
(define-map delegations
    {delegator: principal, domain: (string-ascii 50)}
    {
        delegate: principal,
        active: bool,
        delegated-at: uint
    }
)

;; Read-only functions

(define-read-only (get-member-info (member principal))
    (map-get? members member)
)

(define-read-only (get-member-skill (member principal) (domain (string-ascii 50)))
    (map-get? member-skills {member: member, domain: domain})
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-committee (committee-id uint))
    (map-get? committees committee-id)
)

(define-read-only (is-committee-member (committee-id uint) (member principal))
    (default-to false (map-get? committee-members {committee-id: committee-id, member: member}))
)

(define-read-only (get-delegation (delegator principal) (domain (string-ascii 50)))
    (map-get? delegations {delegator: delegator, domain: domain})
)

(define-read-only (calculate-vote-weight (voter principal) (domain (string-ascii 50)))
    (let
        (
            (member-data (unwrap! (get-member-info voter) u0))
            (skill-data (get-member-skill voter domain))
            (base-weight (get reputation-score member-data))
            (expertise-bonus (match skill-data
                skill-info (if (get verified skill-info)
                    (get competency-level skill-info)
                    u0)
                u0))
        )
        (+ base-weight expertise-bonus)
    )
)

;; Public functions

;; Register as a member
(define-public (register-member)
    (let
        (
            (existing-member (get-member-info tx-sender))
        )
        (asserts! (is-none existing-member) err-already-exists)
        (ok (map-set members tx-sender {
            reputation-score: u10,
            total-contributions: u0,
            joined-at: block-height,
            active: true
        }))
    )
)

;; Add or update skill
(define-public (add-skill (domain (string-ascii 50)) (competency-level uint))
    (let
        (
            (member-data (unwrap! (get-member-info tx-sender) err-not-found))
        )
        (asserts! (get active member-data) err-unauthorized)
        (asserts! (<= competency-level u100) err-invalid-params)
        (ok (map-set member-skills 
            {member: tx-sender, domain: domain}
            {
                competency-level: competency-level,
                verified: false,
                last-updated: block-height
            }
        ))
    )
)

;; Verify member skill (owner only)
(define-public (verify-skill (member principal) (domain (string-ascii 50)))
    (let
        (
            (skill-data (unwrap! (get-member-skill member domain) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set member-skills
            {member: member, domain: domain}
            (merge skill-data {verified: true, last-updated: block-height})
        ))
    )
)

;; Create a new proposal
(define-public (create-proposal 
    (title (string-utf8 200))
    (description (string-utf8 1000))
    (category (string-ascii 50))
    (required-expertise (string-ascii 50))
    (voting-period uint))
    (let
        (
            (proposal-id (var-get proposal-nonce))
            (member-data (unwrap! (get-member-info tx-sender) err-not-found))
        )
        (asserts! (get active member-data) err-unauthorized)
        (asserts! (>= (get reputation-score member-data) (var-get min-reputation-threshold)) err-insufficient-reputation)
        (map-set proposals proposal-id {
            proposer: tx-sender,
            title: title,
            description: description,
            category: category,
            required-expertise: required-expertise,
            votes-for: u0,
            votes-against: u0,
            weighted-votes-for: u0,
            weighted-votes-against: u0,
            start-block: block-height,
            end-block: (+ block-height voting-period),
            executed: false,
            passed: false
        })
        (var-set proposal-nonce (+ proposal-id u1))
        (ok proposal-id)
    )
)

;; Cast a vote on a proposal
(define-public (cast-vote (proposal-id uint) (support bool))
    (let
        (
            (proposal (unwrap! (get-proposal proposal-id) err-not-found))
            (member-data (unwrap! (get-member-info tx-sender) err-not-found))
            (existing-vote (get-vote proposal-id tx-sender))
            (vote-weight (calculate-vote-weight tx-sender (get required-expertise proposal)))
            (expertise-mult (match (get-member-skill tx-sender (get required-expertise proposal))
                skill-info (if (get verified skill-info) u2 u1)
                u1))
            (weighted-vote (* vote-weight expertise-mult))
        )
        (asserts! (is-none existing-vote) err-already-exists)
        (asserts! (get active member-data) err-unauthorized)
        (asserts! (< block-height (get end-block proposal)) err-proposal-closed)
        (asserts! (not (get executed proposal)) err-proposal-closed)
        
        (map-set votes
            {proposal-id: proposal-id, voter: tx-sender}
            {
                support: support,
                vote-weight: vote-weight,
                expertise-multiplier: expertise-mult,
                block-height: block-height
            }
        )
        
        (map-set proposals proposal-id
            (merge proposal {
                votes-for: (if support (+ (get votes-for proposal) u1) (get votes-for proposal)),
                votes-against: (if support (get votes-against proposal) (+ (get votes-against proposal) u1)),
                weighted-votes-for: (if support (+ (get weighted-votes-for proposal) weighted-vote) (get weighted-votes-for proposal)),
                weighted-votes-against: (if support (get weighted-votes-against proposal) (+ (get weighted-votes-against proposal) weighted-vote))
            })
        )
        
        (ok true)
    )
)

;; Create a specialized committee
(define-public (create-committee 
    (name (string-utf8 100))
    (domain (string-ascii 50))
    (min-expertise uint))
    (let
        (
            (committee-id (var-get committee-nonce))
            (member-data (unwrap! (get-member-info tx-sender) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set committees committee-id {
            name: name,
            domain: domain,
            min-expertise: min-expertise,
            member-count: u0,
            active: true,
            created-at: block-height
        })
        (var-set committee-nonce (+ committee-id u1))
        (ok committee-id)
    )
)

;; Join a committee
(define-public (join-committee (committee-id uint))
    (let
        (
            (committee (unwrap! (get-committee committee-id) err-not-found))
            (member-data (unwrap! (get-member-info tx-sender) err-not-found))
            (skill-data (unwrap! (get-member-skill tx-sender (get domain committee)) err-not-found))
        )
        (asserts! (get active member-data) err-unauthorized)
        (asserts! (get active committee) err-unauthorized)
        (asserts! (get verified skill-data) err-unauthorized)
        (asserts! (>= (get competency-level skill-data) (get min-expertise committee)) err-insufficient-reputation)
        
        (map-set committee-members {committee-id: committee-id, member: tx-sender} true)
        (map-set committees committee-id
            (merge committee {member-count: (+ (get member-count committee) u1)})
        )
        (ok true)
    )
)

;; Delegate voting power
(define-public (delegate-votes (delegate principal) (domain (string-ascii 50)))
    (let
        (
            (delegator-data (unwrap! (get-member-info tx-sender) err-not-found))
            (delegate-data (unwrap! (get-member-info delegate) err-not-found))
        )
        (asserts! (get active delegator-data) err-unauthorized)
        (asserts! (get active delegate-data) err-unauthorized)
        (asserts! (not (is-eq tx-sender delegate)) err-invalid-params)
        
        (ok (map-set delegations
            {delegator: tx-sender, domain: domain}
            {
                delegate: delegate,
                active: true,
                delegated-at: block-height
            }
        ))
    )
)

;; Update member reputation (owner only)
(define-public (update-reputation (member principal) (new-score uint))
    (let
        (
            (member-data (unwrap! (get-member-info member) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set members member
            (merge member-data {reputation-score: new-score})
        ))
    )
)