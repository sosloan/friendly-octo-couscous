//
//  KiroAgent.swift
//  Kiro-inspired autonomous coding agent with multi-day persistence
//
//  Created by Copilot on 2026-01-08.
//

import Foundation

// ╔══════════════════════════════════════════════════════════════════════════════╗
// ║                                                                              ║
// ║   KIRO-INSPIRED AUTONOMOUS CODING AGENT                                      ║
// ║                                                                              ║
// ║   "Remember for days, adapt in hours, act in seconds."                       ║
// ║                                                                              ║
// ╚══════════════════════════════════════════════════════════════════════════════╝

/// Lightweight, file-backed agent memory with rolling multi-day persistence.
public actor KiroAgent {
    // MARK: - Types
    
    public static let minimumRetentionDays = 1
    public static let defaultRecallDays = 3
    public static let defaultRecallLimit = 64
    
    public struct Config: Sendable {
        public let name: String
        public let retentionDays: Int
        public let persistenceURL: URL
        
        public init(
            name: String,
            retentionDays: Int = 14,
            persistenceURL: URL? = nil
        ) {
            self.name = name
            self.retentionDays = max(retentionDays, KiroAgent.minimumRetentionDays)
            
            if let url = persistenceURL {
                self.persistenceURL = url
            } else {
                let base = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first
                let directory = base ?? URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
                self.persistenceURL = directory.appendingPathComponent("kiro_agent_\(name).json")
            }
        }
    }
    
    public struct MemoryEntry: Codable, Sendable {
        public enum Role: String, Codable, Sendable {
            case user
            case agent
            case system
        }
        
        public let timestamp: Date
        public let role: Role
        public let content: String
        public let tags: [String]
        
        public init(
            timestamp: Date = Date(),
            role: Role,
            content: String,
            tags: [String] = []
        ) {
            self.timestamp = timestamp
            self.role = role
            self.content = content
            self.tags = tags
        }
    }
    
    public struct DayJournal: Codable, Sendable {
        public var day: String // YYYY-MM-DD
        public var entries: [MemoryEntry]
        public var summary: String?
        
        public init(day: String, entries: [MemoryEntry] = [], summary: String? = nil) {
            self.day = day
            self.entries = entries
            self.summary = summary
        }
    }
    
    private struct Snapshot: Codable, Sendable {
        var name: String
        var createdAt: Date
        var journals: [DayJournal]
        var lastCheckpoint: Date
    }
    
    // MARK: - Properties
    
    private let config: Config
    private var snapshot: Snapshot
    private let calendar: Calendar
    private static let sharedDayFormatter: DateFormatter = KiroAgent.makeDayFormatter()
    
    private static func makeDayFormatter() -> DateFormatter {
        let formatter = DateFormatter()
        formatter.calendar = Calendar(identifier: .iso8601)
        formatter.locale = Locale(identifier: "en_US_POSIX")
        formatter.timeZone = TimeZone(secondsFromGMT: 0)
        formatter.dateFormat = "yyyy-MM-dd"
        return formatter
    }
    
    // MARK: - Lifecycle
    
    public init(config: Config) {
        self.config = config
        let calendar = Calendar(identifier: .iso8601)
        self.calendar = calendar
        let loaded = Self.loadSnapshot(config: config)
        self.snapshot = Self.prune(
            snapshot: loaded,
            retentionDays: config.retentionDays,
            calendar: calendar,
            formatter: Self.sharedDayFormatter
        )
    }
    
    // MARK: - Public API
    
    public enum AgentStateError: Error {
        case missingJournalDay(String)
    }
    
    /// Record a single interaction and persist it to disk.
    public func record(
        role: MemoryEntry.Role,
        content: String,
        tags: [String] = [],
        at date: Date = Date()
    ) async throws {
        let dayKey = Self.sharedDayFormatter.string(from: date)
        ensureDayExists(dayKey)
        
        let entry = MemoryEntry(
            timestamp: date,
            role: role,
            content: content,
            tags: tags
        )
        
        try append(entry, to: dayKey)
        try persist()
    }
    
    /// Attach a short, human-readable summary for a given day.
    public func checkpoint(
        summary: String,
        for date: Date = Date()
    ) async throws {
        let dayKey = Self.sharedDayFormatter.string(from: date)
        ensureDayExists(dayKey)
        
        if let index = snapshot.journals.firstIndex(where: { $0.day == dayKey }) {
            snapshot.journals[index].summary = summary
            snapshot.lastCheckpoint = date
            try persist()
        }
    }
    
    /// Retrieve the most recent interactions across a sliding window of days.
    public func recall(
        days: Int = KiroAgent.defaultRecallDays,
        limit: Int = KiroAgent.defaultRecallLimit
    ) -> [MemoryEntry] {
        let windowDays = max(days, 1)
        let today = Date()
        guard let start = calendar.date(byAdding: .day, value: -windowDays + 1, to: today) else {
#if DEBUG
            print("KiroAgent[\(config.name)]: failed to compute recall window for \(windowDays) day(s)")
#endif
            return []
        }
        
        let startKey = Self.sharedDayFormatter.string(from: start)
        let entries = snapshot.journals
            .filter { $0.day >= startKey }
            .sorted { $0.day < $1.day }
            .flatMap { $0.entries }
        
        return Array(entries.suffix(limit))
    }
    
    /// Lightweight status for monitoring or debugging.
    public func stateSummary() -> String {
        let dayCount = snapshot.journals.count
        let entryCount = snapshot.journals.reduce(0) { $0 + $1.entries.count }
        let last = snapshot.journals.last?.day ?? "n/a"
        return "KiroAgent[\(config.name)] days=\(dayCount) entries=\(entryCount) lastDay=\(last)"
    }
    
    // MARK: - Private helpers
    
    private func ensureDayExists(_ day: String) {
        if !snapshot.journals.contains(where: { $0.day == day }) {
            snapshot.journals.append(DayJournal(day: day))
            snapshot.journals.sort { $0.day < $1.day }
        }
        snapshot = Self.prune(
            snapshot: snapshot,
            retentionDays: config.retentionDays,
            calendar: calendar,
            formatter: Self.sharedDayFormatter
        )
    }
    
    private func append(_ entry: MemoryEntry, to day: String) throws {
        guard let index = snapshot.journals.firstIndex(where: { $0.day == day }) else {
            throw AgentStateError.missingJournalDay(day)
        }
        snapshot.journals[index].entries.append(entry)
    }
    
    private func persist() throws {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        encoder.dateEncodingStrategy = .iso8601
        let data = try encoder.encode(snapshot)
        
        let directory = config.persistenceURL.deletingLastPathComponent()
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true, attributes: nil)
        try data.write(to: config.persistenceURL, options: .atomic)
    }
    
    private static func loadSnapshot(config: Config) -> Snapshot {
        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601
        
        if FileManager.default.fileExists(atPath: config.persistenceURL.path) {
            do {
                let data = try Data(contentsOf: config.persistenceURL)
                let decoded = try decoder.decode(Snapshot.self, from: data)
                return decoded
            } catch {
                #if DEBUG
                    print("KiroAgent[\(config.name)]: failed to load persisted state \(config.persistenceURL.lastPathComponent): \(error)")
                #endif
            }
        }
        
        let today = Date()
        let formatter = sharedDayFormatter
        let dayKey = formatter.string(from: today)
        
        return Snapshot(
            name: config.name,
            createdAt: today,
            journals: [DayJournal(day: dayKey)],
            lastCheckpoint: today
        )
    }
    
    private static func prune(
        snapshot: Snapshot,
        retentionDays: Int,
        calendar: Calendar,
        formatter: DateFormatter
    ) -> Snapshot {
        var snapshot = snapshot
        let today = Date()
        guard let cutoffDate = calendar.date(byAdding: .day, value: -retentionDays + 1, to: today) else {
            return snapshot
        }
        let cutoffKey = formatter.string(from: cutoffDate)
        snapshot.journals = snapshot.journals.filter { $0.day >= cutoffKey }
        return snapshot
    }
}
