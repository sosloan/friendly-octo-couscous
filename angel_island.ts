/**
 * 🛸 AÑGEL | ISLÃND 🌴
 *
 * A dynamic 3D spatiotemporal ecological graph.
 *
 * Mathematical definition:
 *   AÑGEL | ISLÃND := { G(t) }_{t ∈ 𝕋}
 *
 * where G(t) = (V, E(t), W(t)) is a time-indexed graph over unique
 * entities living in a bounded 3D world Ω ⊂ ℝ³ with time t ∈ 𝕋.
 *
 * Full world model:
 *   𝔄 = (Ω, 𝕋, V, G(t), X(t), Φ)
 *
 * Layers:
 *   1–3  : physical space  (x, y, z)
 *   4    : relational / 4th-wall meta-structure  (Φ)
 *   5    : time / history / evolution  (𝕋)
 */

// ─────────────────────────────────────────────────────────────────────────────
// 1.  GEOMETRY  —  Ω ⊂ ℝ³
// ─────────────────────────────────────────────────────────────────────────────

/** A point in 3-dimensional space. */
export interface Vec3 {
  x: number; // east–west
  y: number; // north–south
  z: number; // altitude  (land < 0 = underground, > 0 = air / sky)
}

/** Euclidean distance ‖pᵢ − pⱼ‖₂ in ℝ³. */
export function dist3(a: Vec3, b: Vec3): number {
  return Math.sqrt(
    (a.x - b.x) ** 2 +
    (a.y - b.y) ** 2 +
    (a.z - b.z) ** 2
  );
}

/** Axis-aligned bounding box for the island world Ω. */
export interface BoundingBox {
  min: Vec3;
  max: Vec3;
}

export function inBounds(p: Vec3, omega: BoundingBox): boolean {
  return (
    p.x >= omega.min.x && p.x <= omega.max.x &&
    p.y >= omega.min.y && p.y <= omega.max.y &&
    p.z >= omega.min.z && p.z <= omega.max.z
  );
}

// ─────────────────────────────────────────────────────────────────────────────
// 2.  ENTITIES  —  V = {v₁, …, vₙ}, each UNIQUE
// ─────────────────────────────────────────────────────────────────────────────

/** Every distinct kind of inhabitant on the island. */
export type EntityType =
  | "UFO"
  | "alien"
  | "plant"
  | "animal"
  | "human"
  | "landmark"
  | "sound"
  | "spirit";

/** Per-type interaction radius rτ(i),τ(j) used for proximity edges. */
export const PROXIMITY_RADIUS: Record<EntityType, number> = {
  UFO:      50,
  alien:    20,
  plant:     5,
  animal:   15,
  human:    10,
  landmark: 30,
  sound:    40,
  spirit:   25,
};

/** Ecological domain — where an entity naturally lives. */
export type Domain = "land" | "air" | "water" | "underground" | "meta";

/** Internal ecological state sᵢ(t) ∈ 𝒮. */
export interface EcoState {
  energy:    number; // [0, 1]
  health:    number; // [0, 1]
  hunger:    number; // [0, 1]
  fear:      number; // [0, 1]
  attention: number; // [0, 1]
  fertility: number; // [0, 1]
  signal:    number; // signal intensity / sound level [0, 1]
  domain:    Domain;
}

/** A unique entity vᵢ ∈ V. */
export interface Entity {
  id:       string;       // unique identifier
  name:     string;       // human-readable label (emoji encouraged)
  type:     EntityType;
  position: Vec3;         // xᵢ(t)
  velocity: Vec3;         // uᵢ(t) = dxᵢ/dt
  state:    EcoState;     // sᵢ(t)
}

/** Factory: create a new entity with sensible defaults. */
export function createEntity(
  id: string,
  name: string,
  type: EntityType,
  position: Vec3,
  overrides: Partial<EcoState> = {}
): Entity {
  return {
    id,
    name,
    type,
    position,
    velocity: { x: 0, y: 0, z: 0 },
    state: {
      energy:    1.0,
      health:    1.0,
      hunger:    0.0,
      fear:      0.0,
      attention: 0.5,
      fertility: 0.5,
      signal:    0.0,
      domain:    "land",
      ...overrides,
    },
  };
}

// ─────────────────────────────────────────────────────────────────────────────
// 3.  EDGE TYPES  —  semantic relations
// ─────────────────────────────────────────────────────────────────────────────

export type RelationType =
  | "proximity"
  | "predation"
  | "pollination"
  | "communication"
  | "co-presence"
  | "sheltering"
  | "music"
  | "observation"   // alien watching
  | "intervention"; // UFO or alien acting

/** A directed or undirected edge eᵢⱼ(t) ∈ E(t). */
export interface Edge {
  from:     string; // entity id
  to:       string; // entity id
  relation: RelationType;
  weight:   number; // wᵢⱼ(t) ∈ [0, 1]
  active:   boolean;
}

// ─────────────────────────────────────────────────────────────────────────────
// 4.  EDGE WEIGHT FUNCTION  —  wᵢⱼ(t) = f(dᵢⱼ, sᵢ, sⱼ, τᵢ, τⱼ)
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Compute the ecological edge weight between two entities.
 *
 *   wᵢⱼ(t) = f(dᵢⱼ, sᵢ, sⱼ, τᵢ, τⱼ)
 *
 * Components:
 *   dᵢⱼ  → spatial closeness: how near the two entities are relative to their
 *            interaction radius.
 *   sᵢ   → sender capacity: a healthy, energetic, signaling, non-fearful entity
 *            projects a stronger outgoing connection.
 *   sⱼ   → receiver receptivity: an attentive, healthy, non-fearful entity
 *            picks up incoming signals more strongly.
 *   τᵢ,τⱼ → type compatibility: domain-knowledge multiplier for the pair of
 *            entity kinds (e.g. alien↔UFO, sound↔spirit).
 *
 * The geometric mean of sender and receiver ecological scores ensures both
 * parties must be ecologically "live" for a strong edge to exist.
 */
export function edgeWeight(
  ei: Entity,
  ej: Entity,
  d: number,
  radius: number
): number {
  if (d > radius) return 0;

  // dᵢⱼ — spatial closeness ∈ (0, 1] (approaches 1 as d → 0)
  const spatial = 1 - d / radius;

  // sᵢ — sender's ecological capacity to project a connection:
  //   vitality (energy × health) × signal boost × fear dampening
  const si = ei.state;
  const senderCapacity =
    ((si.energy + si.health) / 2) * // vitality: must be alive to emit
    (0.5 + 0.5 * si.signal)       * // signal: baseline 0.5, boosted by active signaling
    (1 - 0.5 * si.fear);            // fear closes off outgoing links

  // sⱼ — receiver's ecological receptivity:
  //   attention × vitality × fear dampening
  const sj = ej.state;
  const receiverReceptivity =
    sj.attention                  * // openness to incoming interaction
    ((sj.energy + sj.health) / 2) * // must be alive to receive
    (1 - 0.5 * sj.fear);            // fear reduces incoming receptivity

  // geometric mean: both sides must be ecologically engaged
  const ecological = Math.sqrt(senderCapacity * receiverReceptivity);

  // τᵢ, τⱼ — type-pair compatibility multiplier ∈ [1.0, 2.0]
  const typeFactor = typeCompatibility(ei.type, ej.type);

  return Math.min(1, spatial * ecological * typeFactor);
}

/**
 * Domain-knowledge compatibility matrix between entity types.
 * Returns a multiplier in [0, 2].
 */
function typeCompatibility(a: EntityType, b: EntityType): number {
  if (a === b) return 1.2;
  const matrix: Partial<Record<EntityType, Partial<Record<EntityType, number>>>> = {
    alien:    { UFO: 1.8, human: 1.5, animal: 1.2, plant: 1.0 },
    UFO:      { alien: 1.8, human: 1.5, landmark: 1.3 },
    animal:   { plant: 1.5, animal: 1.2, human: 1.0 },
    plant:    { animal: 1.5, human: 1.2 },
    human:    { human: 1.3, animal: 1.1, landmark: 1.2, sound: 1.4 },
    sound:    { human: 1.4, animal: 1.2, spirit: 1.6 },
    spirit:   { sound: 1.6, human: 1.3 },
    landmark: { UFO: 1.3, human: 1.2 },
  };
  return matrix[a]?.[b] ?? 1.0;
}

// ─────────────────────────────────────────────────────────────────────────────
// 5.  THE GRAPH  —  G(t) = (V, E(t), W(t))
// ─────────────────────────────────────────────────────────────────────────────

export interface AngelIslandGraph {
  t:        number;   // current discrete time step
  entities: Map<string, Entity>;
  edges:    Edge[];
}

/** Build G(t) by recomputing all active edges from the current entity state. */
export function buildGraph(
  entities: Map<string, Entity>,
  t: number
): AngelIslandGraph {
  const edgeList: Edge[] = [];
  const ids = [...entities.keys()];

  for (let i = 0; i < ids.length; i++) {
    for (let j = i + 1; j < ids.length; j++) {
      const ei = entities.get(ids[i])!;
      const ej = entities.get(ids[j])!;
      const d  = dist3(ei.position, ej.position);
      const r  = Math.max(
        PROXIMITY_RADIUS[ei.type],
        PROXIMITY_RADIUS[ej.type]
      );
      const w = edgeWeight(ei, ej, d, r);
      if (w > 0) {
        edgeList.push({
          from:     ei.id,
          to:       ej.id,
          relation: inferRelation(ei, ej),
          weight:   w,
          active:   true,
        });
      }
    }
  }

  return { t, entities, edges: edgeList };
}

/** Infer the most likely relation type between two entity types. */
function inferRelation(ei: Entity, ej: Entity): RelationType {
  const pair = new Set([ei.type, ej.type]);
  if (pair.has("UFO") || pair.has("alien")) {
    if (pair.has("human") || pair.has("animal")) return "observation";
    if (pair.has("UFO")   && pair.has("alien"))  return "intervention";
  }
  if (pair.has("sound"))   return "music";
  if (pair.has("plant") && pair.has("animal"))   return "pollination";
  if (pair.has("landmark")) return "co-presence";
  if (pair.has("spirit"))   return "communication";
  return "proximity";
}

// ─────────────────────────────────────────────────────────────────────────────
// 6.  DYNAMICS  —  dx/dt = u(t)  and  ds/dt = g(s, N(t), Ω, t)
// ─────────────────────────────────────────────────────────────────────────────

/** Step motion: xᵢ(t+dt) = xᵢ(t) + uᵢ(t)·dt, clamped to Ω. */
export function stepMotion(entity: Entity, dt: number, omega: BoundingBox): void {
  entity.position = clamp3(
    {
      x: entity.position.x + entity.velocity.x * dt,
      y: entity.position.y + entity.velocity.y * dt,
      z: entity.position.z + entity.velocity.z * dt,
    },
    omega
  );
}

function clamp3(p: Vec3, omega: BoundingBox): Vec3 {
  return {
    x: Math.max(omega.min.x, Math.min(omega.max.x, p.x)),
    y: Math.max(omega.min.y, Math.min(omega.max.y, p.y)),
    z: Math.max(omega.min.z, Math.min(omega.max.z, p.z)),
  };
}

/** Neighbourhood Nᵢ(t): ids of entities connected to i in the current graph. */
function neighborhood(id: string, edges: Edge[]): string[] {
  return edges
    .filter(e => e.active && (e.from === id || e.to === id))
    .map(e => (e.from === id ? e.to : e.from));
}

/**
 * Step internal ecological state:
 *   dsᵢ/dt = gᵢ(sᵢ(t), Nᵢ(t), Ω, t)
 *
 * Simple rules:
 *  • hunger grows over time; energy falls with hunger
 *  • fear propagates from neighbours who are scared
 *  • signal decays unless the entity is a "sound" type
 *  • health recovers slowly when energy is high
 */
export function stepEcoState(
  entity: Entity,
  neighbours: Entity[],
  dt: number
): void {
  const s = entity.state;

  // hunger increases; energy decays with hunger
  s.hunger  = Math.min(1, s.hunger  + 0.01 * dt);
  s.energy  = Math.max(0, s.energy  - 0.005 * s.hunger * dt);

  // fear: mean fear of neighbours leaks in
  if (neighbours.length > 0) {
    const meanFear = neighbours.reduce((acc, n) => acc + n.state.fear, 0) / neighbours.length;
    s.fear = Math.min(1, s.fear + 0.05 * meanFear * dt);
  }
  s.fear = Math.max(0, s.fear - 0.02 * dt); // natural decay

  // signal: sound entities emit, others decay
  if (entity.type === "sound") {
    s.signal = Math.min(1, s.signal + 0.1 * dt);
  } else {
    s.signal = Math.max(0, s.signal - 0.03 * dt);
  }

  // health slowly recovers when energy is high
  if (s.energy > 0.5) {
    s.health = Math.min(1, s.health + 0.005 * dt);
  } else {
    s.health = Math.max(0, s.health - 0.01 * dt);
  }

  // attention follows health
  s.attention = 0.4 + 0.6 * s.health;
}

// ─────────────────────────────────────────────────────────────────────────────
// 7.  META-LAYER  —  Φ  (4th-wall / observer effects)
// ─────────────────────────────────────────────────────────────────────────────

export type MetaEvent =
  | { kind: "wall_break";  message: string }
  | { kind: "observation"; observerId: string; targetId: string }
  | { kind: "intervention"; actorId: string; effect: string };

export interface MetaLayer {
  events: MetaEvent[];
  log: (event: MetaEvent) => void;
  clear: () => void;
}

export function createMetaLayer(): MetaLayer {
  const events: MetaEvent[] = [];
  return {
    events,
    log(event) { events.push(event); },
    clear()    { events.length = 0; },
  };
}

// ─────────────────────────────────────────────────────────────────────────────
// 8.  FULL WORLD MODEL  —  𝔄 = (Ω, 𝕋, V, G(t), X(t), Φ)
// ─────────────────────────────────────────────────────────────────────────────

export interface AngelIslandWorld {
  /** Layer 1–3: bounded 3D island space Ω ⊂ ℝ³ */
  omega: BoundingBox;
  /** Layer 5: discrete time axis 𝕋 = ℤ */
  t: number;
  dt: number;
  /** Unique entity set V */
  entities: Map<string, Entity>;
  /** Current graph snapshot G(t) */
  graph: AngelIslandGraph;
  /** Layer 4: meta / 4th-wall events Φ */
  phi: MetaLayer;
}

/** Create the world with an initial entity population. */
export function createWorld(
  omega: BoundingBox,
  initialEntities: Entity[],
  dt = 1
): AngelIslandWorld {
  const entities = new Map(initialEntities.map(e => [e.id, e]));
  const graph = buildGraph(entities, 0);
  return { omega, t: 0, dt, entities, graph, phi: createMetaLayer() };
}

/**
 * Advance the world by one time step:
 *   X(t+dt) = step(X(t))
 */
export function stepWorld(world: AngelIslandWorld): void {
  const { entities, graph, omega, phi } = world;

  // --- motion: xᵢ(t+dt) = xᵢ(t) + uᵢ·dt
  for (const entity of entities.values()) {
    stepMotion(entity, world.dt, omega);
  }

  // --- eco-state: sᵢ(t+dt)
  for (const entity of entities.values()) {
    const nbIds = neighborhood(entity.id, graph.edges);
    const nbEntities = nbIds.map(id => entities.get(id)!).filter(Boolean);
    stepEcoState(entity, nbEntities, world.dt);
  }

  // --- 4th-wall meta events: UFO/alien observation
  for (const e of entities.values()) {
    if (e.type === "UFO" || e.type === "alien") {
      for (const target of entities.values()) {
        if (target.id === e.id) continue;
        if (dist3(e.position, target.position) < PROXIMITY_RADIUS[e.type]) {
          phi.log({ kind: "observation", observerId: e.id, targetId: target.id });
        }
      }
    }
  }

  // --- rebuild graph G(t+dt)
  world.t      += world.dt;
  world.graph   = buildGraph(entities, world.t);
}

// ─────────────────────────────────────────────────────────────────────────────
// 9.  RENDERING  —  pretty-print the world state
// ─────────────────────────────────────────────────────────────────────────────

const TYPE_EMOJI: Record<EntityType, string> = {
  UFO:      "🛸",
  alien:    "👽",
  plant:    "🌴",
  animal:   "🐎",
  human:    "🏟️",
  landmark: "🗿",
  sound:    "🎶",
  spirit:   "🦋",
};

export function printWorld(world: AngelIslandWorld): void {
  const { t, entities, graph, phi } = world;

  console.log("\n" + "═".repeat(60));
  console.log(`🌴 AÑGEL | ISLÃND — t = ${t}`);
  console.log("═".repeat(60));

  console.log("\n📍 ENTITIES");
  for (const e of entities.values()) {
    const p = e.position;
    const s = e.state;
    console.log(
      `  ${TYPE_EMOJI[e.type]} [${e.id}] ${e.name.padEnd(14)}` +
      `  pos=(${p.x.toFixed(1)}, ${p.y.toFixed(1)}, ${p.z.toFixed(1)})` +
      `  E=${s.energy.toFixed(2)} H=${s.health.toFixed(2)}` +
      `  fear=${s.fear.toFixed(2)} sig=${s.signal.toFixed(2)}`
    );
  }

  console.log(`\n🕸️  EDGES  (${graph.edges.length} active)`);
  for (const edge of graph.edges) {
    const ei = entities.get(edge.from)!;
    const ej = entities.get(edge.to)!;
    console.log(
      `  ${TYPE_EMOJI[ei.type]}${ei.name} ──[${edge.relation}, w=${edge.weight.toFixed(3)}]──` +
      ` ${TYPE_EMOJI[ej.type]}${ej.name}`
    );
  }

  if (phi.events.length > 0) {
    console.log("\n👁️  META / 4TH-WALL EVENTS");
    for (const ev of phi.events) {
      switch (ev.kind) {
        case "observation":
          console.log(`  👽 ${ev.observerId} observes ${ev.targetId}`);
          break;
        case "intervention":
          console.log(`  🛸 ${ev.actorId}: ${ev.effect}`);
          break;
        case "wall_break":
          console.log(`  🔮 [4th wall] ${ev.message}`);
          break;
      }
    }
    phi.clear();
  }
}

// ─────────────────────────────────────────────────────────────────────────────
// 10.  DEMO  —  populate the island and run a short simulation
// ─────────────────────────────────────────────────────────────────────────────

export function runDemo(): void {
  // Ω: island extends 100 units east/west, 100 north/south, −10 to +80 altitude
  const omega: BoundingBox = {
    min: { x: 0, y: 0, z: -10 },
    max: { x: 100, y: 100, z: 80 },
  };

  const entities: Entity[] = [
    // ── 🛸 UFO hovers high above the centre
    {
      ...createEntity("ufo-1", "Comet", "UFO", { x: 50, y: 50, z: 70 }),
      velocity: { x: 0.5, y: 0.2, z: 0 },
    },
    // ── 👽 Alien on the ground exploring
    {
      ...createEntity("alien-1", "Xylo", "alien", { x: 48, y: 52, z: 1 },
        { signal: 0.8, attention: 0.9 }),
      velocity: { x: -0.3, y: 0.5, z: 0 },
    },
    // ── 🌴 Palm tree (stationary landmark/plant)
    createEntity("plant-1", "BigPalm",  "plant",    { x: 45, y: 55, z: 8 }),
    createEntity("plant-2", "Fern",     "plant",    { x: 30, y: 40, z: 2 }),
    // ── 🐎 Horse galloping across the island
    {
      ...createEntity("animal-1", "Thunder", "animal", { x: 20, y: 20, z: 1 },
        { energy: 0.9, fear: 0.1 }),
      velocity: { x: 1.0, y: 0.8, z: 0 },
    },
    // ── 🐸/🐎 Ribbit — frog near the lagoon (water-edge animal)
    {
      ...createEntity("animal-2", "Ribbit", "animal", { x: 70, y: 80, z: 0 },
        { domain: "water", signal: 0.4 }),
      velocity: { x: -0.1, y: 0.2, z: 0 },
    },
    // ── 🏟️ Human performer on stage
    {
      ...createEntity("human-1", "Sol", "human", { x: 50, y: 30, z: 2 },
        { signal: 0.7, attention: 0.8 }),
      velocity: { x: 0, y: 0, z: 0 },
    },
    // ── 🗿 Ancient landmark (stationary)
    createEntity("landmark-1", "Monolith", "landmark", { x: 60, y: 60, z: 5 }),
    // ── 🎶 Sound entity (music from the stage)
    {
      ...createEntity("sound-1", "Cumbia", "sound", { x: 50, y: 30, z: 3 },
        { signal: 1.0, domain: "meta" }),
      velocity: { x: 0, y: 0, z: 0.1 },
    },
    // ── 🦋 Spirit drifting through the air
    {
      ...createEntity("spirit-1", "Mariposa", "spirit", { x: 55, y: 45, z: 20 },
        { domain: "air", attention: 1.0, signal: 0.5 }),
      velocity: { x: -0.5, y: -0.3, z: 0.2 },
    },
  ];

  // Manually add a 4th-wall event at t=0
  const world = createWorld(omega, entities);
  world.phi.log({
    kind: "wall_break",
    message: "The island awakens. Each object is unique, 3D, and alive in time.",
  });

  // Run 5 time steps and print each snapshot
  printWorld(world);
  for (let step = 0; step < 5; step++) {
    stepWorld(world);
    printWorld(world);
  }

  console.log("\n🎶 𝔄 = (Ω, 𝕋, V, G(t), X(t), Φ)  — AÑGEL | ISLÃND lives. 🌴\n");
}

// ─────────────────────────────────────────────────────────────────────────────
// Entry point
// ─────────────────────────────────────────────────────────────────────────────
if (typeof require !== "undefined" && require.main === module) {
  runDemo();
}
