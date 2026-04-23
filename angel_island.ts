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

/**
 * Signal propagation speed c — distance units per time step.
 *
 *   ‖x − sᵢ‖ = c · Δtᵢ
 *
 * A signal emitted at position sᵢ reaches point x after Δtᵢ = dᵢⱼ / c steps.
 * Set to 20 so even a 100-unit crossing takes only 5 steps.
 */
export const PROPAGATION_SPEED = 20;

/**
 * Base interaction radius in ecological profile space, per entity type.
 * Scaled by fertility βᵢ ∈ [0,1] to yield rᵢ(βᵢ) — see profileInteractionRadius().
 */
export const PROFILE_RADIUS_BASE: Record<EntityType, number> = {
  UFO:      1.5,
  alien:    1.2,
  plant:    0.7,
  animal:   0.9,
  human:    1.0,
  landmark: 0.8,
  sound:    1.4,
  spirit:   1.3,
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
  delay:    number; // Δtᵢⱼ = dᵢⱼ / c  (propagation delay in time steps)
  active:   boolean;
}

// ─────────────────────────────────────────────────────────────────────────────
// 4.  EDGE WEIGHT FUNCTION  —  wᵢⱼ(t) = f(dᵢⱼ, sᵢ, sⱼ, τᵢ, τⱼ)
// ─────────────────────────────────────────────────────────────────────────────

/** Ecological profile vector aᵢ = [energy, health, attention, signal, fertility] ∈ [0,1]⁵. */
function ecoProfile(s: EcoState): [number, number, number, number, number] {
  return [s.energy, s.health, s.attention, s.signal, s.fertility];
}

/**
 * Profile-space interaction radius rᵢ(βᵢ).
 *
 *   rᵢ(βᵢ) = base[τᵢ] · (0.5 + 0.5 · βᵢ)
 *
 * βᵢ = fertility ∈ [0,1]: a fertile entity has a larger ecological reach.
 */
function profileInteractionRadius(type: EntityType, fertility: number): number {
  return PROFILE_RADIUS_BASE[type] * (0.5 + 0.5 * fertility);
}

/**
 * Compute the ecological edge weight between two entities.
 *
 *   wᵢⱼ(t) = f(dᵢⱼ, sᵢ, sⱼ, τᵢ, τⱼ)
 *
 * Components:
 *   dᵢⱼ       → spatial closeness: how near the two entities are relative to their
 *                interaction radius.
 *   Δtᵢⱼ=dᵢⱼ/c → propagation delay: a signal from eᵢ travels at speed c and
 *                arrives at eⱼ after Δtᵢⱼ time steps (‖x − sᵢ‖ = c·Δtᵢ).
 *                Strength decays exponentially with delay: timeFactor = e^(−Δtᵢⱼ).
 *   sᵢ        → sender capacity: a healthy, energetic, signaling, non-fearful entity
 *                projects a stronger outgoing connection.
 *   sⱼ        → receiver receptivity: an attentive, healthy, non-fearful entity
 *                picks up incoming signals more strongly.
 *   profile   → ‖profileⱼ − aᵢ‖ = rᵢ(βᵢ): the Gaussian compatibility of the two
 *                entities in the 5D ecological profile space, within mutual radii
 *                rᵢ and rⱼ scaled by fertility βᵢ, βⱼ.
 *   τᵢ,τⱼ     → type compatibility: domain-knowledge multiplier for the pair of
 *                entity kinds (e.g. alien↔UFO, sound↔spirit).
 *
 * The geometric mean of sender and receiver ecological scores ensures both
 * parties must be ecologically "live" for a strong edge to exist.
 */
export function edgeWeight(
  ei: Entity,
  ej: Entity,
  d: number,
  radius: number,
  propagationSpeed = PROPAGATION_SPEED
): number {
  if (d > radius) return 0;

  // dᵢⱼ — spatial closeness ∈ (0, 1] (approaches 1 as d → 0)
  const spatial = 1 - d / radius;

  // ‖x − sᵢ‖ = c·Δtᵢ — propagation delay and its temporal attenuation.
  // A signal emitted at eᵢ takes Δtᵢⱼ = dᵢⱼ/c time steps to reach eⱼ.
  // timeFactor = e^(−Δtᵢⱼ): interactions arrive at full strength when d→0,
  // and decay toward zero for very distant or slowly propagating signals.
  const propagationDelay = d / propagationSpeed; // Δtᵢⱼ in time steps
  const timeFactor = Math.exp(-propagationDelay);

  // sᵢ — sender's ecological capacity to project a connection.
  // All EcoState fields are normalised to [0, 1] (see EcoState interface).
  const si = ei.state;
  const senderVitality  = (si.energy + si.health) / 2;  // alive-ness
  const senderFearDamp  = Math.max(0, 1 - 0.5 * si.fear); // fear ∈ [0,1] → damp ∈ [0.5,1]
  const senderCapacity  =
    senderVitality              * // must be alive to emit
    (0.5 + 0.5 * si.signal)    * // signal ∈ [0,1]: baseline 0.5, boosted by active signaling
    senderFearDamp;               // fear closes off outgoing links

  // sⱼ — receiver's ecological receptivity.
  const sj = ej.state;
  const receiverVitality    = (sj.energy + sj.health) / 2; // alive-ness
  const receiverFearDamp    = Math.max(0, 1 - 0.5 * sj.fear); // fear ∈ [0,1] → damp ∈ [0.5,1]
  const receiverReceptivity =
    sj.attention       * // attention ∈ [0,1]: openness to incoming interaction
    receiverVitality   * // must be alive to receive
    receiverFearDamp;    // fear reduces incoming receptivity

  // geometric mean: both sides must be ecologically engaged
  const ecological = Math.sqrt(senderCapacity * receiverReceptivity);

  // ‖profileⱼ − aᵢ‖ = rᵢ(βᵢ) — profile-space Gaussian compatibility.
  // The 5D ecological profile aᵢ = [energy, health, attention, signal, fertility].
  // Interaction falls off as a Gaussian with the mutual profile radius as σ.
  const pi = ecoProfile(si);
  const pj = ecoProfile(sj);
  const profileDistSq = pi.reduce((sum, v, idx) => sum + (v - pj[idx]) ** 2, 0);
  const ri = profileInteractionRadius(ei.type, si.fertility);
  const rj = profileInteractionRadius(ej.type, sj.fertility);
  const effectiveProfileRadius = (ri + rj) / 2;
  const profileFactor = Math.exp(-profileDistSq / (effectiveProfileRadius ** 2));

  // τᵢ, τⱼ — type-pair compatibility multiplier ∈ [1.0, 2.0]
  const typeFactor = typeCompatibility(ei.type, ej.type);

  return Math.min(1, spatial * timeFactor * ecological * profileFactor * typeFactor);
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
      const propagationDelay = d / PROPAGATION_SPEED;
      const w = edgeWeight(ei, ej, d, r);
      if (w > 0) {
        edgeList.push({
          from:     ei.id,
          to:       ej.id,
          relation: inferRelation(ei, ej),
          weight:   w,
          delay:    propagationDelay,
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
      `  ${TYPE_EMOJI[ei.type]}${ei.name} ──[${edge.relation}, w=${edge.weight.toFixed(3)}, Δt=${edge.delay.toFixed(2)}]──` +
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
// 10.  SIGNAL & PROFILE SPHERE
//
//   Physical (emission sphere):   ‖x − sᵢ‖ = c·Δtᵢ
//   Ecological (profile sphere):  ‖profile − aᵢ‖ = rᵢ(βᵢ)
//
//   An entity at position sᵢ emits a signal at wave-speed c.  After time Δt
//   the wavefront is a sphere of radius c·Δt centred on sᵢ.  In ecological
//   profile space the same entity's zone of influence is a sphere of radius
//   rᵢ(βᵢ) centred on its profile vector aᵢ.
//
//   🐎  (U+1F40E)  Animal entities are the primary emitters in this model.
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Emission-sphere radius: ‖x − sᵢ‖ = c·Δtᵢ
 * @param c  signal propagation speed (world units / time step)
 * @param dt elapsed time Δtᵢ since emission
 */
export function emissionRadius(c: number, dt: number): number {
  return c * dt;
}

/**
 * Extract the ecological profile vector aᵢ from an entity's EcoState.
 * aᵢ = [energy, health, hunger, fear, attention, fertility, signal]
 */
export function ecoProfile(state: EcoState): number[] {
  return [
    state.energy,
    state.health,
    state.hunger,
    state.fear,
    state.attention,
    state.fertility,
    state.signal,
  ];
}

/**
 * Distance in ecological profile space: ‖aᵢ − aⱼ‖₂
 * Used in the profile-sphere condition ‖profile − aᵢ‖ = rᵢ(βᵢ).
 */
export function profileDistance(si: EcoState, sj: EcoState): number {
  const ai = ecoProfile(si);
  const aj = ecoProfile(sj);
  let sum = 0;
  for (let k = 0; k < ai.length; k++) sum += (ai[k] - aj[k]) ** 2;
  return Math.sqrt(sum);
}

/**
 * Profile-sphere radius for entity i:
 *   rᵢ(βᵢ) = βᵢ · √(energy · health · (1 − fear))
 *
 * βᵢ is a base-scale parameter (e.g. the entity's PROXIMITY_RADIUS).
 * The radius shrinks under starvation, injury, or fear and grows when the
 * animal is vigorous and calm — matching ecological intuition for 🐎 animals.
 */
export function profileRadius(state: EcoState, beta: number): number {
  return beta * Math.sqrt(state.energy * state.health * (1 - state.fear));
}

/**
 * Check whether entity j lies inside the profile sphere of entity i.
 *   ‖profile(j) − profile(i)‖ ≤ rᵢ(βᵢ)
 */
export function inProfileSphere(
  si: EcoState,
  sj: EcoState,
  beta: number
): boolean {
  return profileDistance(si, sj) <= profileRadius(si, beta);
}

// ─────────────────────────────────────────────────────────────────────────────
// 11.  LANDSCAPE ANALYSIS  —  fitness landscape of an entity path
//
//   Given an array f of n fitness values (one per node on a linear path),
//   compute structural descriptors that characterise how rugged, symmetric,
//   and bottlenecked the landscape is.
//
//   Metric definitions
//   ──────────────────
//   σ(f)           Standard deviation of fitness values.
//   Ruggedness R   R = 1 − |r₁|  where r₁ is the lag-1 autocorrelation of f.
//                  R → 0 : perfectly smooth;  R → 1 : maximally rugged.
//   Feet           Boundary nodes {1, n}.
//   Head           Interior local maxima: f[i] > f[i−1] and f[i] > f[i+1].
//   Neck           Interior local minima: f[i] < f[i−1] and f[i] < f[i+1].
//   Wings          Interior nodes where |f[i−1] − f[i+1]| < σ · 0.3
//                  (locally symmetric axes), excluding Head/Neck.
//   Body           All remaining interior nodes.
//   Bottleneck(s)  The Neck node s* with the lowest normalised fitness
//                  f[s*] / max(f) — the tightest passage.
//   Branching(s)   For the deepest Neck node, the number of strictly uphill
//                  neighbours minus 1 (= 1 for every interior node on a
//                  linear path, reflecting a single decision fork).
// ─────────────────────────────────────────────────────────────────────────────

export interface LandscapeMetrics {
  /** σ(f): standard deviation of fitness values. */
  sigma: number;
  /** R = 1 − |r₁|: ruggedness (0 = smooth, 1 = maximally rugged). */
  ruggedness: number;
  /** Boundary / foot nodes (1-indexed). */
  feet: number[];
  /** Optima / head nodes (1-indexed): interior local maxima. */
  head: number[];
  /** Saddle / neck nodes (1-indexed): interior local minima. */
  neck: number[];
  /** Symmetric / wing nodes (1-indexed): locally symmetric axes. */
  wings: number[];
  /** Rugged / body nodes (1-indexed): remaining interior nodes. */
  body: number[];
  /** Tightest bottleneck saddle: 1-indexed label and f[s]/max(f). */
  bottleneck: { node: number; value: number } | null;
  /** Deepest saddle and its local branching factor (uphill neighbours − 1). */
  branching: { node: number; degree: number } | null;
}

/**
 * Analyse the fitness landscape of a linear path of n nodes.
 *
 * @param f  Array of n fitness values (node 1 = f[0], node n = f[n-1]).
 * @returns  LandscapeMetrics descriptor.
 */
export function analyzeLandscape(f: number[]): LandscapeMetrics {
  const n = f.length;

  // Edge cases: no nodes or a single node yield trivial metrics.
  if (n === 0) {
    return {
      sigma: 0, ruggedness: 0, feet: [], head: [], neck: [],
      wings: [], body: [], bottleneck: null, branching: null,
    };
  }
  if (n === 1) {
    return {
      sigma: 0, ruggedness: 0, feet: [1], head: [], neck: [],
      wings: [], body: [], bottleneck: null, branching: null,
    };
  }

  // ── σ(f) ──────────────────────────────────────────────────────────────────
  const mean = f.reduce((a, b) => a + b, 0) / n;
  const sigma = Math.sqrt(f.reduce((acc, v) => acc + (v - mean) ** 2, 0) / n);

  // ── Ruggedness R = 1 − |r₁| ───────────────────────────────────────────────
  let rNum = 0, rDen = 0;
  for (let i = 0; i < n - 1; i++) rNum += (f[i] - mean) * (f[i + 1] - mean);
  for (let i = 0; i < n; i++) rDen += (f[i] - mean) ** 2;
  const r1 = rDen === 0 ? 0 : rNum / rDen;
  const ruggedness = 1 - Math.abs(r1);

  // ── Feet (boundaries) ─────────────────────────────────────────────────────
  const feet: number[] = [1, n];

  // ── Head (local maxima) & Neck (local minima) ─────────────────────────────
  const head: number[] = [];
  const neck: number[] = [];
  for (let i = 1; i < n - 1; i++) {
    if (f[i] > f[i - 1] && f[i] > f[i + 1]) head.push(i + 1); // 1-indexed
    if (f[i] < f[i - 1] && f[i] < f[i + 1]) neck.push(i + 1);
  }

  // ── Wings (locally symmetric axes): |f[i−1] − f[i+1]| < σ · 0.3 ──────────
  const symThreshold = sigma * 0.3;
  const specialSet = new Set([...head, ...neck, ...feet]);
  const wings: number[] = [];
  for (let i = 1; i < n - 1; i++) {
    const node = i + 1; // 1-indexed
    if (!specialSet.has(node) && Math.abs(f[i - 1] - f[i + 1]) < symThreshold) {
      wings.push(node);
    }
  }

  // ── Body: remaining interior nodes ────────────────────────────────────────
  const allSpecial = new Set([...head, ...neck, ...wings, ...feet]);
  const body: number[] = [];
  for (let i = 1; i <= n; i++) {
    if (!allSpecial.has(i)) body.push(i);
  }

  // ── Bottleneck: saddle with lowest normalised fitness ─────────────────────
  const maxF = n > 0 ? Math.max(...f) : 0;
  let bottleneck: { node: number; value: number } | null = null;
  for (const s of neck) {
    const val = maxF === 0 ? 0 : f[s - 1] / maxF;
    if (bottleneck === null || val < bottleneck.value) {
      bottleneck = { node: s, value: parseFloat(val.toFixed(4)) };
    }
  }

  // ── Branching: uphill neighbours − 1 at the deepest saddle ───────────────
  // Neck nodes are interior local minima (f[i] < f[i−1] AND f[i] < f[i+1]),
  // so both neighbours are always strictly uphill → uphill = 2 → degree = 1.
  // The clamp guards against degenerate inputs.
  let branching: { node: number; degree: number } | null = null;
  if (neck.length > 0) {
    // Deepest saddle (lowest f value among neck nodes)
    const deepest = neck.reduce(
      (best, s) => (f[s - 1] < f[best - 1] ? s : best),
      neck[0]
    );
    const idx = deepest - 1; // 0-indexed
    let uphill = 0;
    if (idx > 0 && f[idx - 1] > f[idx]) uphill++;
    if (idx < n - 1 && f[idx + 1] > f[idx]) uphill++;
    branching = { node: deepest, degree: Math.max(0, uphill - 1) };
  }

  return {
    sigma: parseFloat(sigma.toFixed(4)),
    ruggedness: parseFloat(ruggedness.toFixed(4)),
    feet,
    head,
    neck,
    wings,
    body,
    bottleneck,
    branching,
  };
}

/**
 * Pretty-print a LandscapeMetrics report to the console.
 * The trailing 🐎 marks this as the Animal entity's landscape profile.
 */
export function printLandscape(metrics: LandscapeMetrics): void {
  console.log("\n" + "─".repeat(50));
  console.log("🐎  Fitness Landscape Analysis  (U+1F40E · animal)");
  console.log("─".repeat(50));
  console.log(`Landscape σ(f)     : ${metrics.sigma}`);
  console.log(`Ruggedness (R)     : ${metrics.ruggedness}`);
  console.log(`Feet (Boundaries)  : { ${metrics.feet.join(" ")} }`);
  console.log(`Head (Optima)      : { ${metrics.head.join(" ")} }`);
  if (metrics.bottleneck) {
    console.log(
      `Bottleneck (s=${metrics.bottleneck.node})   : ${metrics.bottleneck.value}`
    );
  }
  console.log(`Neck (Saddles)     : { ${metrics.neck.join(" ")} }`);
  console.log(`Wings (Symmetric)  : { ${metrics.wings.join(" ")} }`);
  console.log(`Body (Rugged)      : { ${metrics.body.join(" ")} }`);
  if (metrics.branching) {
    console.log(`Branching (s=${metrics.branching.node})    : ${metrics.branching.degree}`);
  }
}

// ─────────────────────────────────────────────────────────────────────────────
// 12.  DEMO  —  populate the island and run a short simulation
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

  // ── 🐎 Signal & Profile Sphere demo ────────────────────────────────────────
  const thunder = world.entities.get("animal-1")!;
  const ribbit  = world.entities.get("animal-2")!;

  // Emission sphere: Thunder emits at wave-speed 5, checked after 3 time steps
  const physRadius = emissionRadius(5, 3);
  const physDist   = dist3(thunder.position, ribbit.position);
  console.log("\n" + "─".repeat(50));
  console.log("🐎  Signal Sphere  ‖x − sᵢ‖ = c·Δtᵢ");
  console.log("─".repeat(50));
  console.log(`  c = 5, Δt = 3  →  emission radius = ${physRadius}`);
  console.log(`  ‖Thunder − Ribbit‖ = ${physDist.toFixed(3)}`);
  console.log(`  Ribbit inside emission sphere: ${physDist <= physRadius}`);

  // Profile sphere: Thunder's ecological influence in profile space
  const beta = PROXIMITY_RADIUS["animal"];
  const pRadius = profileRadius(thunder.state, beta);
  const pDist   = profileDistance(thunder.state, ribbit.state);
  console.log("\n🐎  Profile Sphere  ‖profile − aᵢ‖ = rᵢ(βᵢ)");
  console.log(`  β = ${beta} (animal proximity radius)`);
  console.log(`  rᵢ(βᵢ) = ${pRadius.toFixed(4)}`);
  console.log(`  ‖profile(Thunder) − profile(Ribbit)‖ = ${pDist.toFixed(4)}`);
  console.log(`  Ribbit inside profile sphere: ${pDist <= pRadius}`);

  // ── 🐎 Landscape Analysis demo ─────────────────────────────────────────────
  // A 20-node fitness landscape sampled along Thunder's trajectory.
  // Values derived from the entity's ecological state evolving over the path.
  const fitnessPath: number[] = [
    4.2, 3.8, 3.5, 3.1, 2.0,   //  1– 5 : approach to first saddle
    4.5, 5.1, 4.8, 4.3, 3.2,   //  6–10 : rise, second saddle
    7.8, 7.9, 6.5, 3.0, 3.9,   // 11–15 : twin optima, third saddle
    4.7, 5.0, 4.6, 4.1, 3.7,   // 16–20 : descent to boundary
  ];
  const metrics = analyzeLandscape(fitnessPath);
  printLandscape(metrics);
}

// ─────────────────────────────────────────────────────────────────────────────
// Entry point
// ─────────────────────────────────────────────────────────────────────────────
if (typeof require !== "undefined" && require.main === module) {
  runDemo();
}
