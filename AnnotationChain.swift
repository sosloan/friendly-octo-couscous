//
//  AnnotationChain.swift
//  Borrowed from Apple's https://github.com/apple/FHIRModels/issues
//
//
//  Created by Sloan Sloan on 12/28/25.
//


// ╔══════════════════════════════════════════════════════════════════════════════╗
// ║                                                                              ║
// ║   VA NAPA HOSPITAL SYSTEM - ANNOTATION-DRIVEN PROOF CHAIN                   ║
// ║                                                                              ║
// ║   "Annotation IS the Proof. Annotation.text IS the Explanation."             ║
// ║   "Annotation.author IS the Actor. Annotation.time IS the Causal Order."     ║
// ║                                                                              ║
// ║   Every morphism is annotated. Every annotation is a proof link.            ║
// ║                                                                              ║
// ╚══════════════════════════════════════════════════════════════════════════════╝

import Foundation
import FMCore

// MARK: - Annotation Chain (The Proof Trail)

/// An annotation chain that tracks the causal history of a resource
public struct AnnotationChain: Sendable {
    public let resourceReference: String
    public let annotations: ContiguousArray<Annotation>
    public let causalOrder: [UInt64] // Epochs when annotations were added
    
    public init(resourceReference: String, annotations: ContiguousArray<Annotation> = [], causalOrder: [UInt64] = []) {
        self.resourceReference = resourceReference
        self.annotations = annotations
        self.causalOrder = causalOrder
    }
    
    public func appending(_ annotation: Annotation, at epoch: UInt64) -> AnnotationChain {
        var newAnnotations = annotations
        newAnnotations.append(annotation)
        var newOrder = causalOrder
        newOrder.append(epoch)
        return AnnotationChain(
            resourceReference: resourceReference,
            annotations: newAnnotations,
            causalOrder: newOrder
        )
    }
    
    /// Get the most recent annotation
    public var latest: Annotation? {
        annotations.last
    }
    
    /// Get annotations by author
    public func annotations(by author: String) -> ContiguousArray<Annotation> {
        ContiguousArray(annotations.filter { annotation in
            switch annotation.author {
            case .reference(let ref):
                return ref.reference?.value?.string == author
            case .string(let str):
                return str.value?.string == author
            case .none:
                return false
            }
        })
    }
}

// MARK: - Annotation Store (The Proof Registry)

public final class AnnotationStore: @unchecked Sendable {
    private var _chains: [String: AnnotationChain] = [:] // resourceReference -> chain
    private var _annotationsByAuthor: [String: ContiguousArray<Annotation>] = [:]
    private var _annotationsByTime: ContiguousArray<(epoch: UInt64, annotation: Annotation, resource: String)> = []
    
    private let lock = NSLock()
    
    public init() {}
    
    public func annotate(
        resource: String,
        annotation: Annotation,
        at epoch: UInt64
    ) {
        lock.lock()
        defer { lock.unlock() }
        
        var chain = _chains[resource] ?? AnnotationChain(resourceReference: resource)
        chain = chain.appending(annotation, at: epoch)
        _chains[resource] = chain
        
        // Index by author
        if let author = extractAuthor(annotation) {
            _annotationsByAuthor[author, default: []].append(annotation)
        }
        
        // Index by time
        _annotationsByTime.append((epoch: epoch, annotation: annotation, resource: resource))
    }
    
    public func getChain(for resource: String) -> AnnotationChain? {
        lock.lock()
        defer { lock.unlock() }
        return _chains[resource]
    }
    
    public func getAnnotations(by author: String) -> ContiguousArray<Annotation> {
        lock.lock()
        defer { lock.unlock() }
        return _annotationsByAuthor[author] ?? []
    }
    
    public func getAnnotations(in epochRange: Range<UInt64>) -> ContiguousArray<Annotation> {
        lock.lock()
        defer { lock.unlock() }
        return ContiguousArray(_annotationsByTime.filter { epochRange.contains($0.epoch) }.map { $0.annotation })
    }
    
    private func extractAuthor(_ annotation: Annotation) -> String? {
        switch annotation.author {
        case .reference(let ref):
            return ref.reference?.value?.string
        case .string(let str):
            return str.value?.string
        case .none:
            return nil
        }
    }
    
    public func snapshot() -> AnnotationStore {
        lock.lock()
        defer { lock.unlock() }
        let snapshot = AnnotationStore()
        snapshot._chains = _chains
        snapshot._annotationsByAuthor = _annotationsByAuthor
        snapshot._annotationsByTime = _annotationsByTime
        return snapshot
    }
}

// MARK: - Enhanced World State (With Annotation Chains)

public struct HospitalWorld: Sendable {
    private final class Storage: @unchecked Sendable {
        var taskStore: TaskStore
        var resourceStore: FHIRResourceStore
        var annotationStore: AnnotationStore
        var epoch: UInt64
        
        init() {
            self.taskStore = TaskStore()
            self.resourceStore = FHIRResourceStore()
            self.annotationStore = AnnotationStore()
            self.epoch = 0
        }
        
        func copy() -> Storage {
            let copy = Storage()
            copy.taskStore = taskStore.snapshot()
            copy.resourceStore = resourceStore.snapshot()
            copy.annotationStore = annotationStore.snapshot()
            copy.epoch = epoch
            return copy
        }
    }
    
    private var _storage: Storage
    private let _lock = NSLock()
    
    public init() {
        self._storage = Storage()
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
    
    public var annotationStore: AnnotationStore {
        _lock.lock()
        defer { _lock.unlock() }
        return _storage.annotationStore
    }
    
    /// Annotate a resource with proof metadata
    public mutating func annotate(
        resource: String,
        text: String,
        author: String,
        time: Date = Date()
    ) {
        let epoch = evolve()
        
        let annotation = Annotation(
            text: FHIRPrimitive(FHIRString(text)),
            author: .string(FHIRPrimitive(FHIRString(author))),
            time: FHIRPrimitive(DateTime(date: time))
        )
        
        _storage.annotationStore.annotate(
            resource: resource,
            annotation: annotation,
            at: epoch
        )
    }
    
    /// Annotate a Task with execution proof
    public mutating func annotateTask(
        taskId: String,
        text: String,
        author: String
    ) {
        annotate(
            resource: "Task/\(taskId)",
            text: text,
            author: author
        )
    }
    
    /// Annotate a resource created by a Task
    public mutating func annotateResource(
        resourceType: String,
        resourceId: String,
        text: String,
        author: String,
        causedByTask: String? = nil
    ) {
        var annotationText = text
        if let taskId = causedByTask {
            annotationText += " [Caused by Task/\(taskId)]"
        }
        
        annotate(
            resource: "\(resourceType)/\(resourceId)",
            text: annotationText,
            author: author
        )
    }
    
    internal mutating func addTask(_ task: Task) {
        evolve()
        _storage.taskStore.addTask(task)
        
        // Auto-annotate task creation
        if let taskId = task.id?.value?.string {
            annotateTask(
                taskId: taskId,
                text: "Task created: \(task.description_fhir?.value?.string ?? task.code?.text?.value?.string ?? "unknown")",
                author: task.requester?.reference?.value?.string ?? "system"
            )
        }
    }
    
    internal mutating func addFlag(_ flag: Flag) {
        evolve()
        _storage.resourceStore.addFlag(flag)
        
        // Auto-annotate flag creation
        if let flagId = flag.id?.value?.string,
           let subject = flag.subject.reference?.value?.string {
            annotateResource(
                resourceType: "Flag",
                resourceId: flagId,
                text: "Flag created: \(flag.code.text?.value?.string ?? flag.code.coding?.first?.display?.value?.string ?? "unknown")",
                author: flag.author?.reference?.value?.string ?? "system"
            )
        }
    }
    
    internal mutating func addGoal(_ goal: Goal) {
        evolve()
        _storage.resourceStore.addGoal(goal)
        
        // Auto-annotate goal creation
        if let goalId = goal.id?.value?.string,
           let subject = goal.subject.reference?.value?.string {
            annotateResource(
                resourceType: "Goal",
                resourceId: goalId,
                text: "Goal created: \(goal.description_fhir.text?.value?.string ?? "unknown")",
                author: goal.recorder?.reference?.value?.string ?? goal.source?.reference?.value?.string ?? "system"
            )
        }
    }
}

// MARK: - Enhanced Task Executor (With Annotation Tracking)

extension TaskExecutor {
    /// Execute Task with full annotation/proof tracking
    public func executeWithProof(
        task: Task,
        in world: inout HospitalWorld
    ) async throws -> Task {
        guard let taskId = task.id?.value?.string else {
            throw TaskExecutionError.invalidTask("Task missing ID")
        }
        
        let startEpoch = world.epoch
        let startTime = Date()
        
        // Annotate: Task execution started
        world.annotateTask(
            taskId: taskId,
            text: "Task execution started. Inputs: \(task.input?.count ?? 0) items.",
            author: task.requester?.reference?.value?.string ?? "system"
        )
        
        // Execute the task
        var result = try await execute(task: task, in: &world)
        
        let endTime = Date()
        let duration = endTime.timeIntervalSince(startTime)
        
        // Annotate: Task execution completed
        world.annotateTask(
            taskId: taskId,
            text: "Task execution completed in \(String(format: "%.3f", duration))s. Outputs: \(result.output?.count ?? 0) items. Status: \(result.status.value.rawValue).",
            author: "task-executor"
        )
        
        // Add annotations to Task.note
        if result.note == nil {
            result.note = []
        }
        
        // Add execution proof annotation
        let executionAnnotation = Annotation(
            text: FHIRPrimitive(FHIRString("Executed at epoch \(startEpoch). Duration: \(String(format: "%.3f", duration))s.")),
            author: .string(FHIRPrimitive(FHIRString("task-executor"))),
            time: FHIRPrimitive(DateTime(date: startTime))
        )
        result.note?.append(executionAnnotation)
        
        // If task created resources, annotate them
        if let output = result.output {
            for outputItem in output {
                if case .reference(let ref) = outputItem.value,
                   let refString = ref.reference?.value?.string {
                    world.annotateResource(
                        resourceType: refString.components(separatedBy: "/").first ?? "Resource",
                        resourceId: refString.components(separatedBy: "/").last ?? "",
                        text: "Created by Task/\(taskId)",
                        author: "task-executor",
                        causedByTask: taskId
                    )
                }
            }
        }
        
        return result
    }
}

// MARK: - Annotation-Based Query System

public struct AnnotationQueries {
    /// Get the proof chain for a resource
    public static func getProofChain(
        for resource: String,
        in world: HospitalWorld
    ) -> AnnotationChain? {
        return world.annotationStore.getChain(for: resource)
    }
    
    /// Get all annotations by a specific actor
    public static func getAnnotations(
        by author: String,
        in world: HospitalWorld
    ) -> ContiguousArray<Annotation> {
        return world.annotationStore.getAnnotations(by: author)
    }
    
    /// Get the causal history of a resource (who did what, when)
    public static func getCausalHistory(
        for resource: String,
        in world: HospitalWorld
    ) -> String {
        guard let chain = world.annotationStore.getChain(for: resource) else {
            return "No annotations found for \(resource)"
        }
        
        var history: [String] = []
        history.append("📜 Causal History for \(resource):")
        history.append(String(repeating: "─", count: 60))
        
        for (index, annotation) in chain.annotations.enumerated() {
            let epoch = index < chain.causalOrder.count ? chain.causalOrder[index] : 0
            let author = extractAuthorString(annotation)
            let time = annotation.time?.value?.date.description ?? "unknown time"
            let text = annotation.text.value?.string ?? ""
            
            history.append("[Epoch \(epoch)] \(time)")
            history.append("  Author: \(author)")
            history.append("  Note: \(text)")
            history.append("")
        }
        
        return history.joined(separator: "\n")
    }
    
    private static func extractAuthorString(_ annotation: Annotation) -> String {
        switch annotation.author {
        case .reference(let ref):
            return ref.reference?.value?.string ?? "unknown"
        case .string(let str):
            return str.value?.string ?? "unknown"
        case .none:
            return "system"
        }
    }
    
    /// Verify that a resource has required proof annotations
    public static func verifyProof(
        for resource: String,
        requiredAnnotations: [String], // Required annotation text patterns
        in world: HospitalWorld
    ) -> (valid: Bool, missing: [String]) {
        guard let chain = world.annotationStore.getChain(for: resource) else {
            return (false, requiredAnnotations)
        }
        
        let annotationTexts = Set(chain.annotations.compactMap { $0.text.value?.string })
        var missing: [String] = []
        
        for required in requiredAnnotations {
            if !annotationTexts.contains(where: { $0.contains(required) }) {
                missing.append(required)
            }
        }
        
        return (missing.isEmpty, missing)
    }
}

// MARK: - Enhanced Task Factories (With Annotation Support)

extension VAHospitalTasks {
    /// Create a Task with initial annotation
    public static func createAnnotatedTask(
        code: String,
        description: String,
        patientId: String,
        requester: String,
        initialAnnotation: String
    ) -> Task {
        let task = createFlagTask(
            patientId: patientId,
            flagCode: code,
            flagDisplay: description,
            requester: requester
        )
        
        // Add initial annotation to Task.note
        var annotatedTask = task
        if annotatedTask.note == nil {
            annotatedTask.note = []
        }
        
        let annotation = Annotation(
            text: FHIRPrimitive(FHIRString(initialAnnotation)),
            author: .reference(Reference(
                reference: FHIRPrimitive(FHIRString("Practitioner/\(requester)"))
            )),
            time: FHIRPrimitive(DateTime(date: Date()))
        )
        annotatedTask.note?.append(annotation)
        
        return annotatedTask
    }
}

// MARK: - Complete Example: Annotation-Driven VA Napa System

public func createAnnotatedVANapaWorkflow(
    patientId: String,
    physicianId: String,
    physicianName: String
) async throws {
    var world = HospitalWorld()
    
    // Register handlers
    await TaskExecutor.shared.registerVANapaHandlers()
    
    // Create annotated workflow
    let finalWorld = try await TaskWorkflowEngine.shared.executePipeline(
        name: "VA_NAPA_ANNOTATED_ADMISSION",
        initialState: world
    ) {
        // Task 1: Create allergy flag with annotation
        var flagTask = VAHospitalTasks.createFlagTask(
            patientId: patientId,
            flagCode: "allergy-penicillin",
            flagDisplay: "Allergy: Penicillin",
            requester: physicianId
        )
        flagTask.note = [
            Annotation(
                text: FHIRPrimitive(FHIRString("Patient reported severe allergic reaction to penicillin in 2018. Verified in medical history.")),
                author: .reference(Reference(
                    reference: FHIRPrimitive(FHIRString("Practitioner/\(physicianId)"))
                )),
                time: FHIRPrimitive(DateTime(date: Date()))
            )
        ]
        flagTask
        
        // Task 2: Create fall risk flag
        VAHospitalTasks.createFlagTask(
            patientId: patientId,
            flagCode: "fall-risk",
            flagDisplay: "Fall Risk Assessment Required",
            requester: physicianId
        )
        
        // Task 3: Create goal with annotation
        var goalTask = VAHospitalTasks.createGoalTask(
            patientId: patientId,
            goalDescription: "Stabilize blood pressure to < 140/90 mmHg within 7 days",
            priority: "high-priority",
            requester: physicianId
        )
        goalTask.note = [
            Annotation(
                text: FHIRPrimitive(FHIRString("Current BP: 165/95. Target based on VA/DoD Clinical Practice Guidelines for Hypertension Management.")),
                author: .reference(Reference(
                    reference: FHIRPrimitive(FHIRString("Practitioner/\(physicianId)"))
                )),
                time: FHIRPrimitive(DateTime(date: Date()))
            )
        ]
        goalTask
    }
    
    // Print annotation chains
    print("\n📜 ANNOTATION PROOF CHAINS:")
    print(String(repeating: "═", count: 70))
    
    // Get all tasks and show their annotations
    let completedTasks = finalWorld.taskStore.getTasks(status: .completed)
    for task in completedTasks {
        if let taskId = task.id?.value?.string {
            print("\n📋 Task: \(taskId)")
            if let chain = finalWorld.annotationStore.getChain(for: "Task/\(taskId)") {
                for annotation in chain.annotations {
                    let author = extractAuthorString(annotation)
                    let text = annotation.text.value?.string ?? ""
                    print("   💬 [\(author)]: \(text)")
                }
            }
        }
    }
    
    // Get all flags and show their annotations
    let flags = finalWorld.resourceStore.getFlags(forPatientId: patientId)
    for flag in flags {
        if let flagId = flag.id?.value?.string {
            print("\n🚩 Flag: \(flagId)")
            if let chain = finalWorld.annotationStore.getChain(for: "Flag/\(flagId)") {
                for annotation in chain.annotations {
                    let author = extractAuthorString(annotation)
                    let text = annotation.text.value?.string ?? ""
                    print("   💬 [\(author)]: \(text)")
                }
            }
        }
    }
    
    // Get causal history for patient
    print("\n📖 Causal History for Patient/\(patientId):")
    print(AnnotationQueries.getCausalHistory(for: "Patient/\(patientId)", in: finalWorld))
    
    print("\n📊 Final Statistics:")
    print("   Tasks: \(finalWorld.taskStore.countTasks(status: .completed))")
    print("   Flags: \(finalWorld.resourceStore.countFlags(active: true))")
    print("   Goals: \(finalWorld.resourceStore.countGoals(inStatus: .inProgress))")
    print("   Total Annotations: \(finalWorld.annotationStore.getAnnotations(by: "system").count + finalWorld.annotationStore.getAnnotations(by: physicianId).count)")
}

private func extractAuthorString(_ annotation: Annotation) -> String {
    switch annotation.author {
    case .reference(let ref):
        return ref.reference?.value?.string ?? "unknown"
    case .string(let str):
        return str.value?.string ?? "unknown"
    case .none:
        return "system"
    }
}

// MARK: - Usage

@main
struct VANapaAnnotatedMain {
    static func main() async {
        do {
            try await createAnnotatedVANapaWorkflow(
                patientId: "V123456789",
                physicianId: "P001",
                physicianName: "Dr. Sarah Johnson"
            )
        } catch {
            print("❌ Workflow failed: \(error)")
        }
    }
}
