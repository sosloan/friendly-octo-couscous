// ╔══════════════════════════════════════════════════════════════════════════════╗
// ║                                                                              ║
// ║   VA NAPA HOSPITAL - COMPLETE SYSTEM ARCHITECTURE                            ║
// ║                                                                              ║
// ║   "The Hospital IS the Morphism Engine. Every patient journey IS a pipeline." ║
// ║                                                                              ║
// ║   Organization → Location → Encounter → CarePlan → Task → Annotation         ║
// ║                                                                              ║
// ╚══════════════════════════════════════════════════════════════════════════════╝

import Foundation
import FMCore

// MARK: - Hospital Infrastructure (The Foundation)

/// VA Napa Hospital Organization
public struct VANapaHospital {
    public let organization: Organization
    public let locations: [Location]
    public let practitioners: [Practitioner]
    public let practitionerRoles: [PractitionerRole]
    
    public init() {
        // Create the hospital organization
        self.organization = Organization(
            name: FHIRPrimitive(FHIRString("VA Napa Healthcare System")),
            alias: [FHIRPrimitive(FHIRString("VA Napa"))],
            identifier: [
                Identifier(
                    system: FHIRPrimitive(FHIRURI("http://va.gov/identifiers/facilities")),
                    value: FHIRPrimitive(FHIRString("528"))
                )
            ]
        )
        
        // Create hospital locations (wards, departments)
        self.locations = Self.createLocations()
        self.practitioners = Self.createPractitioners()
        self.practitionerRoles = Self.createPractitionerRoles(practitioners: self.practitioners)
    }
    
    private static func createLocations() -> [Location] {
        return [
            // Emergency Department
            Location(
                name: FHIRPrimitive(FHIRString("Emergency Department")),
                alias: [FHIRPrimitive(FHIRString("ED"))],
                identifier: [
                    Identifier(
                        system: FHIRPrimitive(FHIRURI("http://va.gov/napa/locations")),
                        value: FHIRPrimitive(FHIRString("ED-001"))
                    )
                ],
                status: FHIRPrimitive(LocationStatus.active),
                type: [
                    CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://terminology.hl7.org/CodeSystem/v3-RoleCode")),
                            code: FHIRPrimitive(FHIRString("ER"))
                        )]
                    )
                ]
            ),
            
            // Medical-Surgical Ward
            Location(
                name: FHIRPrimitive(FHIRString("Medical-Surgical Ward 3A")),
                alias: [FHIRPrimitive(FHIRString("Med-Surg 3A"))],
                identifier: [
                    Identifier(
                        system: FHIRPrimitive(FHIRURI("http://va.gov/napa/locations")),
                        value: FHIRPrimitive(FHIRString("MS-3A"))
                    )
                ],
                status: FHIRPrimitive(LocationStatus.active),
                type: [
                    CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://terminology.hl7.org/CodeSystem/v3-RoleCode")),
                            code: FHIRPrimitive(FHIRString("HOSP"))
                        )]
                    )
                ]
            ),
            
            // Intensive Care Unit
            Location(
                name: FHIRPrimitive(FHIRString("Intensive Care Unit")),
                alias: [FHIRPrimitive(FHIRString("ICU"))],
                identifier: [
                    Identifier(
                        system: FHIRPrimitive(FHIRURI("http://va.gov/napa/locations")),
                        value: FHIRPrimitive(FHIRString("ICU-001"))
                    )
                ],
                status: FHIRPrimitive(LocationStatus.active),
                type: [
                    CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://terminology.hl7.org/CodeSystem/v3-RoleCode")),
                            code: FHIRPrimitive(FHIRString("ICU"))
                        )]
                    )
                ]
            ),
            
            // Primary Care Clinic
            Location(
                name: FHIRPrimitive(FHIRString("Primary Care Clinic")),
                alias: [FHIRPrimitive(FHIRString("PCC"))],
                identifier: [
                    Identifier(
                        system: FHIRPrimitive(FHIRURI("http://va.gov/napa/locations")),
                        value: FHIRPrimitive(FHIRString("PCC-001"))
                    )
                ],
                status: FHIRPrimitive(LocationStatus.active),
                type: [
                    CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://terminology.hl7.org/CodeSystem/v3-RoleCode")),
                            code: FHIRPrimitive(FHIRString("HOSP"))
                        )]
                    )
                ]
            )
        ]
    }
    
    private static func createPractitioners() -> [Practitioner] {
        return [
            Practitioner(
                identifier: [
                    Identifier(
                        system: FHIRPrimitive(FHIRURI("http://va.gov/napa/practitioners")),
                        value: FHIRPrimitive(FHIRString("P001"))
                    )
                ],
                name: [
                    HumanName(
                        family: FHIRPrimitive(FHIRString("Johnson")),
                        given: [FHIRPrimitive(FHIRString("Sarah"))],
                        prefix: [FHIRPrimitive(FHIRString("Dr."))]
                    )
                ]
            ),
            Practitioner(
                identifier: [
                    Identifier(
                        system: FHIRPrimitive(FHIRURI("http://va.gov/napa/practitioners")),
                        value: FHIRPrimitive(FHIRString("N001"))
                    )
                ],
                name: [
                    HumanName(
                        family: FHIRPrimitive(FHIRString("Smith")),
                        given: [FHIRPrimitive(FHIRString("Mary"))],
                        prefix: [FHIRPrimitive(FHIRString("RN"))]
                    )
                ]
            ),
            Practitioner(
                identifier: [
                    Identifier(
                        system: FHIRPrimitive(FHIRURI("http://va.gov/napa/practitioners")),
                        value: FHIRPrimitive(FHIRString("P002"))
                    )
                ],
                name: [
                    HumanName(
                        family: FHIRPrimitive(FHIRString("Williams")),
                        given: [FHIRPrimitive(FHIRString("Robert"))],
                        prefix: [FHIRPrimitive(FHIRString("Dr."))]
                    )
                ]
            )
        ]
    }
    
    private static func createPractitionerRoles(practitioners: [Practitioner]) -> [PractitionerRole] {
        return [
            PractitionerRole(
                practitioner: Reference(
                    reference: FHIRPrimitive(FHIRString("Practitioner/P001"))
                ),
                organization: Reference(
                    reference: FHIRPrimitive(FHIRString("Organization/VA-Napa-528"))
                ),
                code: [
                    CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://terminology.hl7.org/CodeSystem/practitioner-role")),
                            code: FHIRPrimitive(FHIRString("doctor")),
                            display: FHIRPrimitive(FHIRString("Doctor"))
                        )],
                        text: FHIRPrimitive(FHIRString("Attending Physician"))
                    )
                ],
                specialty: [
                    CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://snomed.info/sct")),
                            code: FHIRPrimitive(FHIRString("419192003")),
                            display: FHIRPrimitive(FHIRString("Internal Medicine"))
                        )]
                    )
                ],
                location: [
                    Reference(reference: FHIRPrimitive(FHIRString("Location/MS-3A")))
                ]
            ),
            PractitionerRole(
                practitioner: Reference(
                    reference: FHIRPrimitive(FHIRString("Practitioner/N001"))
                ),
                organization: Reference(
                    reference: FHIRPrimitive(FHIRString("Organization/VA-Napa-528"))
                ),
                code: [
                    CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://terminology.hl7.org/CodeSystem/practitioner-role")),
                            code: FHIRPrimitive(FHIRString("nurse")),
                            display: FHIRPrimitive(FHIRString("Nurse"))
                        )],
                        text: FHIRPrimitive(FHIRString("Registered Nurse"))
                    )
                ],
                location: [
                    Reference(reference: FHIRPrimitive(FHIRString("Location/MS-3A")))
                ]
            )
        ]
    }
}

// MARK: - Hospital World State (Complete System State)

public struct HospitalWorld: Sendable {
    private final class Storage: @unchecked Sendable {
        var hospital: VANapaHospital
        var taskStore: TaskStore
        var resourceStore: FHIRResourceStore
        var annotationStore: AnnotationStore
        var encounterStore: ContiguousArray<Encounter> = []
        var carePlanStore: ContiguousArray<CarePlan> = []
        var episodeStore: ContiguousArray<EpisodeOfCare> = []
        var epoch: UInt64 = 0
        
        init(hospital: VANapaHospital = VANapaHospital()) {
            self.hospital = hospital
            self.taskStore = TaskStore()
            self.resourceStore = FHIRResourceStore()
            self.annotationStore = AnnotationStore()
        }
        
        func copy() -> Storage {
            let copy = Storage(hospital: self.hospital)
            copy.taskStore = taskStore.snapshot()
            copy.resourceStore = resourceStore.snapshot()
            copy.annotationStore = annotationStore.snapshot()
            copy.encounterStore = encounterStore
            copy.carePlanStore = carePlanStore
            copy.episodeStore = episodeStore
            copy.epoch = epoch
            return copy
        }
    }
    
    private var _storage: Storage
    private let _lock = NSLock()
    
    public init(hospital: VANapaHospital = VANapaHospital()) {
        self._storage = Storage(hospital: hospital)
    }
    
    @discardableResult
    public mutating func evolve() -> UInt64 {
        _lock.lock()
        defer { _lock.unlock() }
        
        if !isKnownUniquelyReferenced(&_storage) {
            _storage = _storage.copy()
        }
        _storage.epoch += 1
        return _storage.epoch
    }
    
    public var epoch: UInt64 {
        _lock.lock()
        defer { _lock.unlock() }
        return _storage.epoch
    }
    
    public var hospital: VANapaHospital {
        _lock.lock()
        defer { _lock.unlock() }
        return _storage.hospital
    }
    
    public var taskStore: TaskStore {
        _lock.lock()
        defer { _lock.unlock() }
        return _storage.taskStore
    }

    public var resourceStore: FHIRResourceStore {
        _lock.lock()
        defer { _lock.unlock() }
        return _storage.resourceStore
    }
    
    public var annotationStore: AnnotationStore {
        _lock.lock()
        defer { _lock.unlock() }
        return _storage.annotationStore
    }
    
    // Resource operations
    internal mutating func addEncounter(_ encounter: Encounter) {
        evolve()
        _lock.lock()
        defer { _lock.unlock() }
        _storage.encounterStore.append(encounter)
    }
    
    internal mutating func addCarePlan(_ carePlan: CarePlan) {
        evolve()
        _lock.lock()
        defer { _lock.unlock() }
        _storage.carePlanStore.append(carePlan)
    }
    
    internal mutating func addTask(_ task: Task) {
        evolve()
        _storage.taskStore.addTask(task)
        if let taskId = task.id?.value?.string {
            annotateTask(taskId: taskId, text: "Task created", author: task.requester?.reference?.value?.string ?? "system")
        }
    }
    
    internal mutating func addFlag(_ flag: Flag) {
        evolve()
        _storage.resourceStore.addFlag(flag)
    }
    
    internal mutating func addGoal(_ goal: Goal) {
        evolve()
        _storage.resourceStore.addGoal(goal)
    }
    
    internal mutating func addGroup(_ group: Group) {
        evolve()
        _storage.resourceStore.addGroup(group)
    }
    
    internal mutating func annotateTask(taskId: String, text: String, author: String) {
        let epoch = evolve()
        let annotation = Annotation(
            text: FHIRPrimitive(FHIRString(text)),
            author: .string(FHIRPrimitive(FHIRString(author))),
            time: FHIRPrimitive(DateTime(date: Date()))
        )
        _storage.annotationStore.annotate(resource: "Task/\(taskId)", annotation: annotation, at: epoch)
    }
    
    public func getEncounters(for patientId: String) -> ContiguousArray<Encounter> {
        _lock.lock()
        defer { _lock.unlock() }
        return ContiguousArray(_storage.encounterStore.filter { encounter in
            encounter.subject.reference?.value?.string == "Patient/\(patientId)"
        })
    }
    
    public func getCarePlans(for patientId: String) -> ContiguousArray<CarePlan> {
        _lock.lock()
        defer { _lock.unlock() }
        return ContiguousArray(_storage.carePlanStore.filter { plan in
            plan.subject.reference?.value?.string == "Patient/\(patientId)"
        })
    }
}

// MARK: - Hospital Workflows (Complete Patient Journeys)

/// Patient Admission Workflow
public struct AdmissionWorkflow {
    /// Create a complete admission workflow
    public static func createAdmissionTasks(
        patientId: String,
        encounterId: String,
        locationId: String,
        physicianId: String,
        nurseId: String,
        chiefComplaint: String
    ) -> [Task] {
        let now = DateTime(date: Date())
        
        return [
            // Task 1: Register Encounter
            Task(
                intent: FHIRPrimitive(FHIRString("order")),
                status: FHIRPrimitive(TaskStatus.ready),
                id: FHIRPrimitive(FHIRString(UUID().uuidString)),
                code: CodeableConcept(
                    coding: [Coding(
                        system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-codes")),
                        code: FHIRPrimitive(FHIRString("register-encounter"))
                    )],
                    text: FHIRPrimitive(FHIRString("Register Patient Encounter"))
                ),
                description_fhir: FHIRPrimitive(FHIRString("Register patient admission encounter")),
                focus: [
                    TaskFocus(value: .reference(Reference(
                        reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))
                    )))
                ],
                `for`: Reference(reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))),
                requester: Reference(reference: FHIRPrimitive(FHIRString("Practitioner/\(physicianId)"))),
                input: [
                    TaskInput(
                        type: CodeableConcept(
                            coding: [Coding(
                                system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-input-types")),
                                code: FHIRPrimitive(FHIRString("encounter-id"))
                            )]
                        ),
                        value: .string(FHIRPrimitive(FHIRString(encounterId)))
                    ),
                    TaskInput(
                        type: CodeableConcept(
                            coding: [Coding(
                                system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-input-types")),
                                code: FHIRPrimitive(FHIRString("location-id"))
                            )]
                        ),
                        value: .string(FHIRPrimitive(FHIRString(locationId)))
                    ),
                    TaskInput(
                        type: CodeableConcept(
                            coding: [Coding(
                                system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-input-types")),
                                code: FHIRPrimitive(FHIRString("chief-complaint"))
                            )]
                        ),
                        value: .string(FHIRPrimitive(FHIRString(chiefComplaint)))
                    )
                ],
                note: [
                    Annotation(
                        text: FHIRPrimitive(FHIRString("Patient admitted with: \(chiefComplaint)")),
                        author: .reference(Reference(
                            reference: FHIRPrimitive(FHIRString("Practitioner/\(physicianId)"))
                        )),
                        time: FHIRPrimitive(now)
                    )
                ]
            ),
            
            // Task 2: Create Initial Assessment Flags
            VAHospitalTasks.createFlagTask(
                patientId: patientId,
                flagCode: "allergy-penicillin",
                flagDisplay: "Allergy: Penicillin - Severe",
                requester: physicianId
            ),
            
            VAHospitalTasks.createFlagTask(
                patientId: patientId,
                flagCode: "fall-risk",
                flagDisplay: "Fall Risk - Requires Assessment",
                requester: nurseId
            ),
            
            // Task 3: Create Care Goals
            VAHospitalTasks.createGoalTask(
                patientId: patientId,
                goalDescription: "Stabilize vital signs within normal range",
                priority: "high-priority",
                requester: physicianId
            ),
            
            // Task 4: Form Care Team
            createCareTeamTask(
                patientId: patientId,
                teamId: "team-\(patientId)-\(UUID().uuidString.prefix(8))",
                members: [
                    (id: physicianId, role: "attending-physician"),
                    (id: nurseId, role: "primary-nurse")
                ],
                requester: physicianId
            ),
            
            // Task 5: Create Care Plan
            createCarePlanTask(
                patientId: patientId,
                encounterId: encounterId,
                carePlanId: UUID().uuidString,
                requester: physicianId
            )
        ]
    }
    
    private static func createCareTeamTask(
        patientId: String,
        teamId: String,
        members: [(id: String, role: String)],
        requester: String
    ) -> Task {
        return Task(
            intent: FHIRPrimitive(FHIRString("order")),
            status: FHIRPrimitive(TaskStatus.ready),
            id: FHIRPrimitive(FHIRString(UUID().uuidString)),
            code: CodeableConcept(
                coding: [Coding(
                    system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-codes")),
                    code: FHIRPrimitive(FHIRString("form-care-team"))
                )],
                text: FHIRPrimitive(FHIRString("Form Care Team"))
            ),
            description_fhir: FHIRPrimitive(FHIRString("Form multidisciplinary care team for patient")),
            focus: [
                TaskFocus(value: .reference(Reference(
                    reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))
                )))
            ],
            `for`: Reference(reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))),
            requester: Reference(reference: FHIRPrimitive(FHIRString("Practitioner/\(requester)"))),
            input: [
                TaskInput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-input-types")),
                            code: FHIRPrimitive(FHIRString("team-id"))
                        )]
                    ),
                    value: .string(FHIRPrimitive(FHIRString(teamId)))
                ),
                TaskInput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-input-types")),
                            code: FHIRPrimitive(FHIRString("member-count"))
                        )]
                    ),
                    value: .integer(FHIRPrimitive(FHIRInteger(integerLiteral: Int32(members.count))))
                )
            ]
        )
    }
    
    private static func createCarePlanTask(
        patientId: String,
        encounterId: String,
        carePlanId: String,
        requester: String
    ) -> Task {
        return Task(
            intent: FHIRPrimitive(FHIRString("plan")),
            status: FHIRPrimitive(TaskStatus.ready),
            id: FHIRPrimitive(FHIRString(UUID().uuidString)),
            code: CodeableConcept(
                coding: [Coding(
                    system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-codes")),
                    code: FHIRPrimitive(FHIRString("create-care-plan"))
                )],
                text: FHIRPrimitive(FHIRString("Create Care Plan"))
            ),
            description_fhir: FHIRPrimitive(FHIRString("Create comprehensive care plan for patient admission")),
            focus: [
                TaskFocus(value: .reference(Reference(
                    reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))
                )))
            ],
            `for`: Reference(reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))),
            requester: Reference(reference: FHIRPrimitive(FHIRString("Practitioner/\(requester)"))),
            input: [
                TaskInput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-input-types")),
                            code: FHIRPrimitive(FHIRString("care-plan-id"))
                        )]
                    ),
                    value: .string(FHIRPrimitive(FHIRString(carePlanId)))
                ),
                TaskInput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-input-types")),
                            code: FHIRPrimitive(FHIRString("encounter-id"))
                        )]
                    ),
                    value: .string(FHIRPrimitive(FHIRString(encounterId)))
                )
            ]
        )
    }
}

// MARK: - Enhanced Task Executor (With Hospital-Specific Handlers)

extension TaskExecutor {
    public func registerHospitalHandlers() {
        registerVANapaHandlers()
        
        // Handler for register-encounter
        registerHandler(code: "register-encounter") { task, world in
            var updatedTask = task
            
            guard let inputs = task.input,
                  let focus = task.focus?.first,
                  case .reference(let patientRef) = focus.value,
                  let patientId = patientRef.reference?.value?.string?.replacingOccurrences(of: "Patient/", with: "") else {
                throw TaskExecutionError.invalidTask("Missing required inputs")
            }
            
            var encounterId: String?
            var locationId: String?
            var chiefComplaint: String?
            
            for input in inputs {
                if let code = input.type.coding?.first?.code?.value?.string {
                    switch code {
                    case "encounter-id":
                        if case .string(let str) = input.value {
                            encounterId = str.value?.string
                        }
                    case "location-id":
                        if case .string(let str) = input.value {
                            locationId = str.value?.string
                        }
                    case "chief-complaint":
                        if case .string(let str) = input.value {
                            chiefComplaint = str.value?.string
                        }
                    default:
                        break
                    }
                }
            }
            
            guard let encId = encounterId, let locId = locationId else {
                throw TaskExecutionError.invalidTask("Missing encounter or location ID")
            }
            
            // Create Encounter
            let encounter = Encounter(
                status: FHIRPrimitive(EncounterStatus.inProgress),
                `class`: Coding(
                    system: FHIRPrimitive(FHIRURI("http://terminology.hl7.org/CodeSystem/v3-ActCode")),
                    code: FHIRPrimitive(FHIRString("IMP")),
                    display: FHIRPrimitive(FHIRString("inpatient encounter"))
                ),
                subject: Reference(
                    reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))
                ),
                id: FHIRPrimitive(FHIRString(encId)),
                location: [
                    EncounterLocation(
                        location: Reference(
                            reference: FHIRPrimitive(FHIRString("Location/\(locId)"))
                        ),
                        status: FHIRPrimitive(EncounterLocationStatus.active),
                        period: Period(
                            start: FHIRPrimitive(DateTime(date: Date()))
                        )
                    )
                ],
                reasonCode: chiefComplaint.map { complaint in
                    [CodeableConcept(
                        text: FHIRPrimitive(FHIRString(complaint))
                    )]
                }
            )
            
            world.addEncounter(encounter)
            
            // Set output
            updatedTask.output = [
                TaskOutput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-output-types")),
                            code: FHIRPrimitive(FHIRString("encounter-created"))
                        )]
                    ),
                    value: .reference(Reference(
                        reference: FHIRPrimitive(FHIRString("Encounter/\(encId)"))
                    ))
                )
            ]
            
            world.annotateTask(
                taskId: task.id?.value?.string ?? "",
                text: "Encounter registered: \(chiefComplaint ?? "unknown") at Location/\(locId)",
                author: "task-executor"
            )
            
            return updatedTask
        }
        
        // Handler for form-care-team
        registerHandler(code: "form-care-team") { task, world in
            var updatedTask = task
            
            guard let inputs = task.input,
                  let focus = task.focus?.first,
                  case .reference(let patientRef) = focus.value,
                  let patientId = patientRef.reference?.value?.string?.replacingOccurrences(of: "Patient/", with: "") else {
                throw TaskExecutionError.invalidTask("Missing required inputs")
            }
            
            var teamId: String?
            
            for input in inputs {
                if let code = input.type.coding?.first?.code?.value?.string,
                   code == "team-id",
                   case .string(let str) = input.value {
                    teamId = str.value?.string
                }
            }
            
            guard let tid = teamId else {
                throw TaskExecutionError.invalidTask("Missing team ID")
            }
            
            // Create Group (Care Team)
            let group = Group(
                membership: FHIRPrimitive(GroupMembershipBasis.enumerated),
                id: FHIRPrimitive(FHIRString(tid)),
                type: FHIRPrimitive(GroupType.person),
                name: FHIRPrimitive(FHIRString("Care Team for Patient \(patientId)")),
                member: [
                    GroupMember(
                        entity: Reference(
                            reference: FHIRPrimitive(FHIRString("Practitioner/P001"))
                        ),
                        involvement: [
                            CodeableConcept(
                                coding: [Coding(
                                    system: FHIRPrimitive(FHIRURI("http://va.gov/codes/practitioner-roles")),
                                    code: FHIRPrimitive(FHIRString("attending-physician"))
                                )]
                            )
                        ]
                    ),
                    GroupMember(
                        entity: Reference(
                            reference: FHIRPrimitive(FHIRString("Practitioner/N001"))
                        ),
                        involvement: [
                            CodeableConcept(
                                coding: [Coding(
                                    system: FHIRPrimitive(FHIRURI("http://va.gov/codes/practitioner-roles")),
                                    code: FHIRPrimitive(FHIRString("primary-nurse"))
                                )]
                            )
                        ]
                    )
                ],
                quantity: FHIRPrimitive(FHIRUnsignedInteger(integerLiteral: 2))
            )
            
            world.addGroup(group)
            
            updatedTask.output = [
                TaskOutput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-output-types")),
                            code: FHIRPrimitive(FHIRString("care-team-created"))
                        )]
                    ),
                    value: .reference(Reference(
                        reference: FHIRPrimitive(FHIRString("Group/\(tid)"))
                    ))
                )
            ]
            
            world.annotateTask(
                taskId: task.id?.value?.string ?? "",
                text: "Care team formed with 2 members",
                author: "task-executor"
            )
            
            return updatedTask
        }
        
        // Handler for create-care-plan
        registerHandler(code: "create-care-plan") { task, world in
            var updatedTask = task
            
            guard let inputs = task.input,
                  let focus = task.focus?.first,
                  case .reference(let patientRef) = focus.value,
                  let patientId = patientRef.reference?.value?.string?.replacingOccurrences(of: "Patient/", with: "") else {
                throw TaskExecutionError.invalidTask("Missing required inputs")
            }
            
            var carePlanId: String?
            var encounterId: String?
            
            for input in inputs {
                if let code = input.type.coding?.first?.code?.value?.string {
                    switch code {
                    case "care-plan-id":
                        if case .string(let str) = input.value {
                            carePlanId = str.value?.string
                        }
                    case "encounter-id":
                        if case .string(let str) = input.value {
                            encounterId = str.value?.string
                        }
                    default:
                        break
                    }
                }
            }
            
            guard let cpId = carePlanId, let encId = encounterId else {
                throw TaskExecutionError.invalidTask("Missing care plan or encounter ID")
            }
            
            // Get flags and goals for this patient
            let flags = world.resourceStore.getFlags(forPatientId: patientId)
            let goals = world.resourceStore.getGoals(forPatientId: patientId)
            
            // Create CarePlan
            let carePlan = CarePlan(
                intent: FHIRPrimitive(RequestIntent.plan),
                status: FHIRPrimitive(RequestStatus.active),
                subject: Reference(
                    reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))
                ),
                id: FHIRPrimitive(FHIRString(cpId)),
                encounter: Reference(
                    reference: FHIRPrimitive(FHIRString("Encounter/\(encId)"))
                ),
                created: FHIRPrimitive(DateTime(date: Date())),
                title: FHIRPrimitive(FHIRString("Inpatient Care Plan")),
                description_fhir: FHIRPrimitive(FHIRString("Comprehensive care plan for patient admission")),
                goal: goals.prefix(5).compactMap { goal in
                    guard let goalId = goal.id?.value?.string else { return nil }
                    return Reference(reference: FHIRPrimitive(FHIRString("Goal/\(goalId)")))
                },
                addresses: flags.prefix(3).compactMap { flag in
                    guard let flagId = flag.id?.value?.string else { return nil }
                    return CodeableReference(
                        reference: Reference(reference: FHIRPrimitive(FHIRString("Flag/\(flagId)")))
                    )
                },
                note: [
                    Annotation(
                        text: FHIRPrimitive(FHIRString("Care plan created automatically from admission workflow. Includes \(goals.count) goals and \(flags.count) flags.")),
                        author: .string(FHIRPrimitive(FHIRString("task-executor"))),
                        time: FHIRPrimitive(DateTime(date: Date()))
                    )
                ]
            )
            
            world.addCarePlan(carePlan)
            
            updatedTask.output = [
                TaskOutput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/napa/task-output-types")),
                            code: FHIRPrimitive(FHIRString("care-plan-created"))
                        )]
                    ),
                    value: .reference(Reference(
                        reference: FHIRPrimitive(FHIRString("CarePlan/\(cpId)"))
                    ))
                )
            ]
            
            world.annotateTask(
                taskId: task.id?.value?.string ?? "",
                text: "Care plan created with \(goals.count) goals and \(flags.count) flags",
                author: "task-executor"
            )
            
            return updatedTask
        }
    }
}

// MARK: - Complete Hospital Admission

public func executeVANapaAdmission(
    patientId: String,
    physicianId: String = "P001",
    nurseId: String = "N001",
    locationId: String = "MS-3A",
    chiefComplaint: String = "Chest pain and shortness of breath"
) async throws {
    var world = HospitalWorld()
    
    // Register all handlers
    await TaskExecutor.shared.registerHospitalHandlers()
    
    // Create admission workflow
    let encounterId = UUID().uuidString
    let tasks = AdmissionWorkflow.createAdmissionTasks(
        patientId: patientId,
        encounterId: encounterId,
        locationId: locationId,
        physicianId: physicianId,
        nurseId: nurseId,
        chiefComplaint: chiefComplaint
    )
    
    // Execute workflow
    let finalWorld = try await TaskWorkflowEngine.shared.executePipeline(
        name: "VA_NAPA_PATIENT_ADMISSION",
        initialState: world
    ) {
        // Add all tasks
        for task in tasks {
            task
        }
    }
    
    // Print comprehensive report
    print("\n" + String(repeating: "═", count: 70))
    print("🏥 VA NAPA HOSPITAL - ADMISSION COMPLETE")
    print(String(repeating: "═", count: 70))
    print("\n📊 ADMISSION SUMMARY:")
    print("   Patient: \(patientId)")
    print("   Encounter: \(encounterId)")
    print("   Location: \(locationId)")
    print("   Chief Complaint: \(chiefComplaint)")
    print("\n📋 TASKS:")
    print("   Completed: \(finalWorld.taskStore.countTasks(status: .completed))")
    print("   Failed: \(finalWorld.taskStore.countTasks(status: .failed))")
    print("\n🚩 CLINICAL FLAGS:")
    print("   Active: \(finalWorld.resourceStore.countFlags(active: true))")
    print("\n🎯 CARE GOALS:")
    print("   In Progress: \(finalWorld.resourceStore.countGoals(inStatus: .inProgress))")
    print("\n👥 CARE TEAMS:")
    print("   Groups: \(finalWorld.resourceStore.groupCount)")
    print("\n📋 CARE PLANS:")
    print("   Active: \(finalWorld.getCarePlans(for: patientId).count)")
    print("\n📜 ANNOTATIONS:")
    let allAnnotations = finalWorld.annotationStore.getAnnotations(by: physicianId).count +
                         finalWorld.annotationStore.getAnnotations(by: "task-executor").count
    print("   Total: \(allAnnotations)")
    print("\n" + String(repeating: "═", count: 70))
}

// MARK: - Main Entry Point

@main
struct VANapaHospitalMain {
    static func main() async {
        print("""
        
        ╔══════════════════════════════════════════════════════════════════════╗
        ║                                                                      ║
        ║        VA NAPA HEALTHCARE SYSTEM                                     ║
        ║        Task-Driven Morphism Engine                                   ║
        ║                                                                      ║
        ║        "Every patient journey is a pipeline."                        ║
        ║        "Every Task is a morphism. Every Annotation is a proof."       ║
        ║                                                                      ║
        ╚══════════════════════════════════════════════════════════════════════╝
        
        """)
        
        do {
            try await executeVANapaAdmission(
                patientId: "V123456789",
                chiefComplaint: "Chest pain with history of hypertension and penicillin allergy"
            )
        } catch {
            print("❌ Admission failed: \(error)")
        }
    }
}
