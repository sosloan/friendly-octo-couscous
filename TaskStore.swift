//
//  TaskStore.swift
//  Borrored from Apple https://github.com/apple/FHIRModels/
//  
//
//  Created by Sloan Sloan on 12/28/25.
//


// ╔══════════════════════════════════════════════════════════════════════════════╗
// ║                                                                              ║
// ║   VA NAPA HOSPITAL SYSTEM - TASK-DRIVEN MORPHISM ENGINE                     ║
// ║                                                                              ║
// ║   "Task IS the Verb. Task.input IS the Source. Task.output IS the Target."   ║
// ║   "Task.focus IS the Morphism Target. Task.status IS the Execution State."   ║
// ║                                                                              ║
// ║   We don't wrap Task. We execute Task.                                       ║
// ║                                                                              ║
// ╚══════════════════════════════════════════════════════════════════════════════╝

import Foundation
import FMCore

// MARK: - Task Store (The Hospital's Task Queue)

public final class TaskStore: @unchecked Sendable {
    private var _tasks: ContiguousArray<Task> = []
    private var _tasksByStatus: [TaskStatus: ContiguousArray<Task>] = [:]
    private var _tasksByFocus: [String: ContiguousArray<Task>] = [:] // resourceType/id -> Tasks
    private var _taskIndex: [String: Task] = [:] // taskId -> Task
    private var _taskGraph: [String: Set<String>] = [:] // taskId -> [childTaskIds] (partOf relationships)
    
    private let lock = NSLock()
    
    public init() {}
    
    public func addTask(_ task: Task) {
        lock.lock()
        defer { lock.unlock() }
        
        guard let taskId = task.id?.value?.string else { return }
        
        _tasks.append(task)
        _taskIndex[taskId] = task
        
        // Index by status
        let status = task.status.value
        _tasksByStatus[status, default: []].append(task)
        
        // Index by focus
        if let focus = task.focus {
            for focusItem in focus {
                switch focusItem.value {
                case .reference(let ref):
                    if let refString = ref.reference?.value?.string {
                        _tasksByFocus[refString, default: []].append(task)
                    }
                case .canonical:
                    break
                }
            }
        }
        
        // Build task graph (partOf relationships)
        if let partOf = task.partOf {
            for parentRef in partOf {
                if let parentId = parentRef.reference?.value?.string?.replacingOccurrences(of: "Task/", with: "") {
                    _taskGraph[parentId, default: []].insert(taskId)
                }
            }
        }
    }
    
    public func getTask(id: String) -> Task? {
        lock.lock()
        defer { lock.unlock() }
        return _taskIndex[id]
    }
    
    public func getTasks(status: TaskStatus) -> ContiguousArray<Task> {
        lock.lock()
        defer { lock.unlock() }
        return _tasksByStatus[status] ?? []
    }
    
    public func getTasks(focus: String) -> ContiguousArray<Task> {
        lock.lock()
        defer { lock.unlock() }
        return _tasksByFocus[focus] ?? []
    }
    
    public func getChildTasks(parentTaskId: String) -> ContiguousArray<Task> {
        lock.lock()
        defer { lock.unlock() }
        guard let childIds = _taskGraph[parentTaskId] else { return [] }
        return ContiguousArray(childIds.compactMap { _taskIndex[$0] })
    }
    
    public func updateTaskStatus(taskId: String, newStatus: TaskStatus) {
        lock.lock()
        defer { lock.unlock() }
        
        guard var task = _taskIndex[taskId] else { return }
        
        // Remove from old status index
        let oldStatus = task.status.value
        if var oldStatusTasks = _tasksByStatus[oldStatus] {
            oldStatusTasks.removeAll { $0.id?.value?.string == taskId }
            _tasksByStatus[oldStatus] = oldStatusTasks
        }
        
        // Update task
        task.status = FHIRPrimitive(newStatus)
        task.lastModified = FHIRPrimitive(DateTime(date: Date()))
        _taskIndex[taskId] = task
        
        // Add to new status index
        _tasksByStatus[newStatus, default: []].append(task)
    }
    
    public func countTasks(status: TaskStatus) -> Int {
        lock.lock()
        defer { lock.unlock() }
        return _tasksByStatus[status]?.count ?? 0
    }
    
    public func snapshot() -> TaskStore {
        lock.lock()
        defer { lock.unlock() }
        let snapshot = TaskStore()
        snapshot._tasks = _tasks
        snapshot._tasksByStatus = _tasksByStatus
        snapshot._tasksByFocus = _tasksByFocus
        snapshot._taskIndex = _taskIndex
        snapshot._taskGraph = _taskGraph
        return snapshot
    }
}

// MARK: - World State (Task-Driven)

public struct HospitalWorld: Sendable {
    private final class Storage: @unchecked Sendable {
        var taskStore: TaskStore
        var resourceStore: FHIRResourceStore
        var epoch: UInt64
        var auditTrail: ContiguousArray<AuditEntry>
        
        init() {
            self.taskStore = TaskStore()
            self.resourceStore = FHIRResourceStore()
            self.epoch = 0
            self.auditTrail = []
        }
        
        func copy() -> Storage {
            let copy = Storage()
            copy.taskStore = taskStore.snapshot()
            copy.resourceStore = resourceStore.snapshot()
            copy.epoch = epoch
            copy.auditTrail = auditTrail
            return copy
        }
    }
    
    private var _storage: Storage
    private let _lock = NSLock()
    
    public init() {
        self._storage = Storage()
    }
    
    private init(storage: Storage) {
        self._storage = storage
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
    
    internal mutating func addTask(_ task: Task) {
        evolve()
        _storage.taskStore.addTask(task)
    }
    
    internal mutating func updateTaskStatus(taskId: String, status: TaskStatus) {
        evolve()
        _storage.taskStore.updateTaskStatus(taskId: taskId, newStatus: status)
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
    
    internal mutating func logAudit(_ entry: AuditEntry) {
        evolve()
        _lock.lock()
        defer { _lock.unlock() }
        _storage.auditTrail.append(entry)
    }
}

// MARK: - Task Executor (The Morphism Interpreter)

/// Executes Tasks as morphisms: Task.input → Task.output
public actor TaskExecutor {
    public static let shared = TaskExecutor()
    
    private var executingTasks: Set<String> = []
    private var taskHandlers: [String: @Sendable (Task, inout HospitalWorld) async throws -> Task] = [:]
    
    private init() {}
    
    /// Register a handler for a specific task code
    public func registerHandler(
        code: String,
        handler: @escaping @Sendable (Task, inout HospitalWorld) async throws -> Task
    ) {
        taskHandlers[code] = handler
    }
    
    /// Execute a Task as a morphism
    public func execute(
        task: Task,
        in world: inout HospitalWorld
    ) async throws -> Task {
        guard let taskId = task.id?.value?.string else {
            throw TaskExecutionError.invalidTask("Task missing ID")
        }
        
        guard !executingTasks.contains(taskId) else {
            throw TaskExecutionError.taskAlreadyExecuting(taskId)
        }
        
        executingTasks.insert(taskId)
        defer { executingTasks.remove(taskId) }
        
        // Update status to in-progress
        world.updateTaskStatus(taskId: taskId, status: .inProgress)
        
        // Set execution period start
        var updatedTask = task
        let now = DateTime(date: Date())
        updatedTask.executionPeriod = Period(start: FHIRPrimitive(now))
        
        // Find handler by task code
        let taskCode = task.code?.coding?.first?.code?.value?.string ?? "default"
        
        guard let handler = taskHandlers[taskCode] else {
            // Default handler: pass through input to output
            updatedTask = try await defaultHandler(task: updatedTask, world: &world)
        } else {
            updatedTask = try await handler(updatedTask, &world)
        }
        
        // Set execution period end
        updatedTask.executionPeriod?.end = FHIRPrimitive(DateTime(date: Date()))
        
        // Update status to completed
        world.updateTaskStatus(taskId: taskId, status: .completed)
        
        // Check for TriggerDefinitions in output that spawn new Tasks
        if let output = updatedTask.output {
            for outputItem in output {
                if case .triggerDefinition(let trigger) = outputItem.value {
                    try await spawnTasksFromTrigger(trigger, parentTask: updatedTask, world: &world)
                }
            }
        }
        
        return updatedTask
    }
    
    /// Default handler: passes input to output
    private func defaultHandler(
        task: Task,
        world: inout HospitalWorld
    ) async throws -> Task {
        var updatedTask = task
        
        // Copy input to output (simplified - in real system would transform)
        if let input = task.input {
            updatedTask.output = input.map { inputItem in
                TaskOutput(type: inputItem.type, value: inputItem.value)
            }
        }
        
        return updatedTask
    }
    
    /// Spawn new Tasks from TriggerDefinitions in output
    private func spawnTasksFromTrigger(
        _ trigger: TriggerDefinition,
        parentTask: Task,
        world: inout HospitalWorld
    ) async throws {
        // Create new Task based on trigger
        let newTaskId = UUID().uuidString
        
        let newTask = Task(
            intent: FHIRPrimitive(FHIRString("order")),
            status: FHIRPrimitive(TaskStatus.ready),
            id: FHIRPrimitive(FHIRString(newTaskId)),
            partOf: [Reference(reference: FHIRPrimitive(FHIRString("Task/\(parentTask.id?.value?.string ?? "")")))],
            basedOn: parentTask.basedOn,
            focus: parentTask.focus,
            input: parentTask.output // Output of parent becomes input of child
        )
        
        world.addTask(newTask)
        
        world.logAudit(AuditEntry(
            verb: "SPAWN_TASK_FROM_TRIGGER",
            actor: "task-executor",
            resourceType: "Task",
            resourceId: newTaskId,
            details: "Task spawned from trigger in parent task \(parentTask.id?.value?.string ?? "")"
        ))
    }
    
    public enum TaskExecutionError: Error {
        case invalidTask(String)
        case taskAlreadyExecuting(String)
        case handlerNotFound(String)
    }
}

// MARK: - Task Pipeline Builder (Compose Tasks)

@resultBuilder
public struct TaskPipelineBuilder {
    public static func buildBlock(_ tasks: Task...) -> [Task] {
        return tasks
    }
    
    public static func buildOptional(_ component: [Task]?) -> [Task] {
        return component ?? []
    }
    
    public static func buildEither(first component: [Task]) -> [Task] {
        return component
    }
    
    public static func buildEither(second component: [Task]) -> [Task] {
        return component
    }
    
    public static func buildArray(_ components: [[Task]]) -> [Task] {
        return components.flatMap { $0 }
    }
}

// MARK: - Task Workflow Engine

public actor TaskWorkflowEngine {
    public static let shared = TaskWorkflowEngine()
    
    private init() {}
    
    /// Execute a pipeline of Tasks
    public func executePipeline(
        name: String,
        initialState: HospitalWorld,
        @TaskPipelineBuilder _ content: () -> [Task]
    ) async throws -> HospitalWorld {
        
        let tasks = content()
        var currentWorld = initialState
        let sessionId = UUID()
        
        print("\n" + String(repeating: "═", count: 70))
        print("🏥 VA NAPA HOSPITAL SYSTEM - TASK-DRIVEN MORPHISM ENGINE")
        print(String(repeating: "═", count: 70))
        print("📋 Pipeline: \(name)")
        print("🆔 Session: \(sessionId.uuidString.prefix(8))")
        print("📦 Tasks: \(tasks.count)")
        print(String(repeating: "─", count: 70))
        print()
        
        let startTime = mach_absolute_time()
        
        // Add all tasks to world first
        for task in tasks {
            currentWorld.addTask(task)
        }
        
        // Execute tasks in dependency order (respecting partOf/basedOn)
        let executionOrder = topologicalSort(tasks: tasks)
        
        for (index, taskId) in executionOrder.enumerated() {
            guard let task = currentWorld.taskStore.getTask(id: taskId) else {
                print("  ⚠️  Task \(taskId) not found, skipping")
                continue
            }
            
            let stepStart = mach_absolute_time()
            
            print("  [\(String(format: "%2d", index + 1))/\(executionOrder.count))] Executing Task: \(task.code?.coding?.first?.code?.value?.string ?? "unknown")")
            print("      ID: \(taskId)")
            if let focus = task.focus?.first {
                switch focus.value {
                case .reference(let ref):
                    print("      Focus: \(ref.reference?.value?.string ?? "none")")
                case .canonical:
                    break
                }
            }
            
            do {
                let result = try await TaskExecutor.shared.execute(task: task, in: &currentWorld)
                
                let stepEnd = mach_absolute_time()
                let duration = stepEnd - stepStart
                let durationMs = Double(duration) / 1_000_000.0
                
                print("      ✅ Complete (\(String(format: "%.2f", durationMs))ms)")
                print("      Status: \(result.status.value.rawValue)")
                if let output = result.output {
                    print("      Outputs: \(output.count)")
                }
                
            } catch {
                let stepEnd = mach_absolute_time()
                let duration = stepEnd - stepStart
                let durationMs = Double(duration) / 1_000_000.0
                
                print("      ❌ FAILED (\(String(format: "%.2f", durationMs))ms): \(error)")
                currentWorld.updateTaskStatus(taskId: taskId, status: .failed)
            }
        }
        
        let totalTime = mach_absolute_time() - startTime
        let totalMs = Double(totalTime) / 1_000_000.0
        
        print()
        print(String(repeating: "─", count: 70))
        print("✅ Pipeline Complete")
        print("⏱️  Total Time: \(String(format: "%.3f", totalMs))ms")
        print("🌍 World Epoch: \(currentWorld.epoch)")
        print("📊 Tasks: \(currentWorld.taskStore.countTasks(status: .completed)) completed, \(currentWorld.taskStore.countTasks(status: .failed)) failed")
        print(String(repeating: "═", count: 70))
        print()
        
        return currentWorld
    }
    
    /// Topological sort of tasks respecting partOf/basedOn dependencies
    private func topologicalSort(tasks: [Task]) -> [String] {
        var taskIds: [String] = []
        var visited: Set<String> = []
        var visiting: Set<String> = []
        
        func visit(_ taskId: String) {
            if visiting.contains(taskId) {
                return // Cycle detected
            }
            if visited.contains(taskId) {
                return
            }
            
            visiting.insert(taskId)
            
            // Visit dependencies first
            if let task = tasks.first(where: { $0.id?.value?.string == taskId }) {
                if let partOf = task.partOf {
                    for parentRef in partOf {
                        if let parentId = parentRef.reference?.value?.string?.replacingOccurrences(of: "Task/", with: "") {
                            visit(parentId)
                        }
                    }
                }
                if let basedOn = task.basedOn {
                    for basedOnRef in basedOn {
                        if let basedOnId = basedOnRef.reference?.value?.string?.replacingOccurrences(of: "Task/", with: "") {
                            visit(basedOnId)
                        }
                    }
                }
            }
            
            visiting.remove(taskId)
            visited.insert(taskId)
            taskIds.append(taskId)
        }
        
        for task in tasks {
            if let taskId = task.id?.value?.string {
                visit(taskId)
            }
        }
        
        return taskIds
    }
}

// MARK: - Task Factories (Create Tasks from Verbs)

public struct VAHospitalTasks {
    
    /// Create a Task to flag a patient
    public static func createFlagTask(
        patientId: String,
        flagCode: String,
        flagDisplay: String,
        requester: String
    ) -> Task {
        let taskId = UUID().uuidString
        
        return Task(
            intent: FHIRPrimitive(FHIRString("order")),
            status: FHIRPrimitive(TaskStatus.ready),
            id: FHIRPrimitive(FHIRString(taskId)),
            code: CodeableConcept(
                coding: [Coding(
                    system: FHIRPrimitive(FHIRURI("http://va.gov/codes/task-types")),
                    code: FHIRPrimitive(FHIRString("create-flag"))
                )],
                text: FHIRPrimitive(FHIRString("Create Clinical Flag"))
            ),
            description_fhir: FHIRPrimitive(FHIRString("Create flag: \(flagDisplay)")),
            focus: [
                TaskFocus(
                    value: .reference(Reference(
                        reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))
                    ))
                )
            ],
            `for`: Reference(
                reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))
            ),
            requester: Reference(
                reference: FHIRPrimitive(FHIRString("Practitioner/\(requester)"))
            ),
            input: [
                TaskInput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/codes/task-input-types")),
                            code: FHIRPrimitive(FHIRString("flag-code"))
                        )]
                    ),
                    value: .string(FHIRPrimitive(FHIRString(flagCode)))
                ),
                TaskInput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/codes/task-input-types")),
                            code: FHIRPrimitive(FHIRString("flag-display"))
                        )]
                    ),
                    value: .string(FHIRPrimitive(FHIRString(flagDisplay)))
                )
            ]
        )
    }
    
    /// Create a Task to create a care goal
    public static func createGoalTask(
        patientId: String,
        goalDescription: String,
        priority: String,
        requester: String
    ) -> Task {
        let taskId = UUID().uuidString
        
        return Task(
            intent: FHIRPrimitive(FHIRString("order")),
            status: FHIRPrimitive(TaskStatus.ready),
            id: FHIRPrimitive(FHIRString(taskId)),
            code: CodeableConcept(
                coding: [Coding(
                    system: FHIRPrimitive(FHIRURI("http://va.gov/codes/task-types")),
                    code: FHIRPrimitive(FHIRString("create-goal"))
                )],
                text: FHIRPrimitive(FHIRString("Create Care Goal"))
            ),
            description_fhir: FHIRPrimitive(FHIRString("Create goal: \(goalDescription)")),
            focus: [
                TaskFocus(
                    value: .reference(Reference(
                        reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))
                    ))
                )
            ],
            `for`: Reference(
                reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))
            ),
            requester: Reference(
                reference: FHIRPrimitive(FHIRString("Practitioner/\(requester)"))
            ),
            input: [
                TaskInput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/codes/task-input-types")),
                            code: FHIRPrimitive(FHIRString("goal-description"))
                        )]
                    ),
                    value: .string(FHIRPrimitive(FHIRString(goalDescription)))
                ),
                TaskInput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/codes/task-input-types")),
                            code: FHIRPrimitive(FHIRString("goal-priority"))
                        )]
                    ),
                    value: .string(FHIRPrimitive(FHIRString(priority)))
                )
            ]
        )
    }
    
    /// Create a composite Task that spawns child Tasks
    public static func createCompositeTask(
        name: String,
        childTasks: [Task],
        requester: String
    ) -> Task {
        let parentTaskId = UUID().uuidString
        
        // Link children to parent
        let linkedChildren = childTasks.map { child in
            var updated = child
            if updated.partOf == nil {
                updated.partOf = []
            }
            updated.partOf?.append(Reference(
                reference: FHIRPrimitive(FHIRString("Task/\(parentTaskId)"))
            ))
            return updated
        }
        
        return Task(
            intent: FHIRPrimitive(FHIRString("plan")),
            status: FHIRPrimitive(TaskStatus.ready),
            id: FHIRPrimitive(FHIRString(parentTaskId)),
            code: CodeableConcept(
                coding: [Coding(
                    system: FHIRPrimitive(FHIRURI("http://va.gov/codes/task-types")),
                    code: FHIRPrimitive(FHIRString("composite-workflow"))
                )],
                text: FHIRPrimitive(FHIRString(name))
            ),
            description_fhir: FHIRPrimitive(FHIRString("Composite workflow: \(name)")),
            requester: Reference(
                reference: FHIRPrimitive(FHIRString("Practitioner/\(requester)"))
            ),
            input: [
                TaskInput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/codes/task-input-types")),
                            code: FHIRPrimitive(FHIRString("child-task-count"))
                        )]
                    ),
                    value: .integer(FHIRPrimitive(FHIRInteger(integerLiteral: Int32(linkedChildren.count))))
                )
            ],
            output: [
                TaskOutput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/codes/task-output-types")),
                            code: FHIRPrimitive(FHIRString("child-tasks"))
                        )]
                    ),
                    value: .string(FHIRPrimitive(FHIRString("\(linkedChildren.count) child tasks spawned")))
                )
            ]
        )
    }
}

// MARK: - Task Handlers (The Actual Work)

extension TaskExecutor {
    /// Register VA Napa task handlers
    public func registerVANapaHandlers() {
        // Handler for create-flag tasks
        registerHandler(code: "create-flag") { task, world in
            var updatedTask = task
            
            // Extract inputs
            guard let inputs = task.input else {
                throw TaskExecutionError.invalidTask("Missing inputs")
            }
            
            var flagCode: String?
            var flagDisplay: String?
            var patientId: String?
            
            for input in inputs {
                if let code = input.type.coding?.first?.code?.value?.string {
                    switch code {
                    case "flag-code":
                        if case .string(let str) = input.value {
                            flagCode = str.value?.string
                        }
                    case "flag-display":
                        if case .string(let str) = input.value {
                            flagDisplay = str.value?.string
                        }
                    default:
                        break
                    }
                }
            }
            
            // Extract patient ID from focus
            if let focus = task.focus?.first {
                if case .reference(let ref) = focus.value {
                    if let refString = ref.reference?.value?.string {
                        patientId = refString.replacingOccurrences(of: "Patient/", with: "")
                    }
                }
            }
            
            guard let code = flagCode, let display = flagDisplay, let pid = patientId else {
                throw TaskExecutionError.invalidTask("Missing required inputs")
            }
            
            // Create the Flag
            let flag = Flag(
                code: CodeableConcept(
                    coding: [Coding(
                        system: FHIRPrimitive(FHIRURI("http://va.gov/codes/flags")),
                        code: FHIRPrimitive(FHIRString(code)),
                        display: FHIRPrimitive(FHIRString(display))
                    )],
                    text: FHIRPrimitive(FHIRString(display))
                ),
                subject: Reference(
                    reference: FHIRPrimitive(FHIRString("Patient/\(pid)"))
                ),
                status: FHIRPrimitive(FlagStatus.active)
            )
            
            world.addFlag(flag)
            
            // Set output
            updatedTask.output = [
                TaskOutput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/codes/task-output-types")),
                            code: FHIRPrimitive(FHIRString("flag-created"))
                        )]
                    ),
                    value: .reference(Reference(
                        reference: FHIRPrimitive(FHIRString("Flag/\(flag.id?.value?.string ?? "")"))
                    ))
                )
            ]
            
            world.logAudit(AuditEntry(
                verb: "CREATE_FLAG",
                actor: task.requester?.reference?.value?.string ?? "system",
                patientId: pid,
                resourceType: "Flag",
                resourceId: flag.id?.value?.string,
                details: "Flag created via Task: \(code)"
            ))
            
            return updatedTask
        }
        
        // Handler for create-goal tasks
        registerHandler(code: "create-goal") { task, world in
            var updatedTask = task
            
            // Extract inputs (similar pattern)
            guard let inputs = task.input,
                  let focus = task.focus?.first,
                  case .reference(let patientRef) = focus.value,
                  let patientId = patientRef.reference?.value?.string?.replacingOccurrences(of: "Patient/", with: "") else {
                throw TaskExecutionError.invalidTask("Missing required inputs")
            }
            
            var goalDescription: String?
            var priority: String?
            
            for input in inputs {
                if let code = input.type.coding?.first?.code?.value?.string {
                    switch code {
                    case "goal-description":
                        if case .string(let str) = input.value {
                            goalDescription = str.value?.string
                        }
                    case "goal-priority":
                        if case .string(let str) = input.value {
                            priority = str.value?.string
                        }
                    default:
                        break
                    }
                }
            }
            
            guard let description = goalDescription else {
                throw TaskExecutionError.invalidTask("Missing goal description")
            }
            
            // Create the Goal
            let goal = Goal(
                description_fhir: CodeableConcept(
                    text: FHIRPrimitive(FHIRString(description))
                ),
                lifecycleStatus: FHIRPrimitive(GoalLifecycleStatus.inProgress),
                subject: Reference(
                    reference: FHIRPrimitive(FHIRString("Patient/\(patientId)"))
                ),
                priority: priority.map { prio in
                    CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/codes/goal-priority")),
                            code: FHIRPrimitive(FHIRString(prio))
                        )]
                    )
                }
            )
            
            world.addGoal(goal)
            
            // Set output
            updatedTask.output = [
                TaskOutput(
                    type: CodeableConcept(
                        coding: [Coding(
                            system: FHIRPrimitive(FHIRURI("http://va.gov/codes/task-output-types")),
                            code: FHIRPrimitive(FHIRString("goal-created"))
                        )]
                    ),
                    value: .reference(Reference(
                        reference: FHIRPrimitive(FHIRString("Goal/\(goal.id?.value?.string ?? "")"))
                    ))
                )
            ]
            
            world.logAudit(AuditEntry(
                verb: "CREATE_GOAL",
                actor: task.requester?.reference?.value?.string ?? "system",
                patientId: patientId,
                resourceType: "Goal",
                resourceId: goal.id?.value?.string,
                details: "Goal created via Task: \(description)"
            ))
            
            return updatedTask
        }
    }
}

// MARK: - Complete Example: Task-Driven VA Napa Workflow

public func createVANapaTaskWorkflow(
    patientId: String,
    physicianId: String
) async throws {
    var world = HospitalWorld()
    
    // Register handlers
    await TaskExecutor.shared.registerVANapaHandlers()
    
    // Create workflow using Tasks
    let finalWorld = try await TaskWorkflowEngine.shared.executePipeline(
        name: "VA_NAPA_PATIENT_ADMISSION_TASKS",
        initialState: world
    ) {
        // Task 1: Create allergy flag
        VAHospitalTasks.createFlagTask(
            patientId: patientId,
            flagCode: "allergy-penicillin",
            flagDisplay: "Allergy: Penicillin",
            requester: physicianId
        )
        
        // Task 2: Create fall risk flag
        VAHospitalTasks.createFlagTask(
            patientId: patientId,
            flagCode: "fall-risk",
            flagDisplay: "Fall Risk Assessment Required",
            requester: physicianId
        )
        
        // Task 3: Create blood pressure goal
        VAHospitalTasks.createGoalTask(
            patientId: patientId,
            goalDescription: "Stabilize blood pressure to < 140/90 mmHg within 7 days",
            priority: "high-priority",
            requester: physicianId
        )
        
        // Task 4: Create medication reconciliation goal
        VAHospitalTasks.createGoalTask(
            patientId: patientId,
            goalDescription: "Complete medication reconciliation and patient education",
            priority: "medium-priority",
            requester: physicianId
        )
    }
    
    print("📊 Final Statistics:")
    print("   Tasks Completed: \(finalWorld.taskStore.countTasks(status: .completed))")
    print("   Tasks Failed: \(finalWorld.taskStore.countTasks(status: .failed))")
    print("   Flags Created: \(finalWorld.resourceStore.countFlags(active: true))")
    print("   Goals Created: \(finalWorld.resourceStore.countGoals(inStatus: .inProgress))")
}

// MARK: - Usage

@main
struct VANapaTaskMain {
    static func main() async {
        do {
            try await createVANapaTaskWorkflow(
                patientId: "V123456789",
                physicianId: "P001"
            )
        } catch {
            print("❌ Workflow failed: \(error)")
        }
    }
}
