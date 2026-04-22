/// <reference path="./rummy_demo_types.d.ts" />

import {
  completedHandExists,
  isCompletedHand,
  JsIndianRummyGame,
  JsMoveType,
  JsSyndicateGame,
  score,
  type JsCard,
  type JsGameState,
  type JsMove,
  type JsMoveResult,
  type JsPlayer,
} from 'npm:indian-rummy-core'

export const sampleCompletedHand: JsCard[] = [
  { rank: 'A', suit: 'S' },
  { rank: '2', suit: 'S' },
  { rank: '3', suit: 'S' },
  { rank: '4', suit: 'H' },
  { rank: '5', suit: 'H' },
  { rank: '6', suit: 'H' },
  { rank: '7', suit: 'S' },
  { rank: '7', suit: 'H' },
  { rank: '7', suit: 'D' },
  { rank: 'K', suit: 'S' },
  { rank: 'K', suit: 'H' },
  { rank: 'K', suit: 'D' },
  { rank: 'K', suit: 'C' },
]

export const sampleJokerHand: JsCard[] = [
  { rank: 'A', suit: 'S' },
  { rank: '2', suit: 'S' },
  { rank: '3', suit: 'S' },
  { rank: '4', suit: 'H' },
  { rank: '5', suit: 'H' },
  { rank: 'J', suit: 'J' },
  { rank: '7', suit: 'S' },
  { rank: '7', suit: 'H' },
  { rank: '7', suit: 'D' },
  { rank: 'K', suit: 'S' },
  { rank: 'K', suit: 'H' },
  { rank: 'K', suit: 'D' },
  { rank: 'K', suit: 'C' },
]

export const sampleIncompleteHand: JsCard[] = [
  { rank: 'A', suit: 'S' },
  { rank: '2', suit: 'S' },
  { rank: '5', suit: 'S' },
  { rank: '9', suit: 'H' },
  { rank: '10', suit: 'H' },
  { rank: 'Q', suit: 'D' },
  { rank: '7', suit: 'S' },
  { rank: '8', suit: 'H' },
  { rank: '7', suit: 'D' },
  { rank: 'K', suit: 'S' },
  { rank: '6', suit: 'H' },
  { rank: 'K', suit: 'D' },
  { rank: '4', suit: 'C' },
]

export const sampleCandidateCards: JsCard[] = [
  ...sampleCompletedHand,
  { rank: '9', suit: 'S' },
  { rank: 'J', suit: 'C' },
]

export const sampleCosmicRunHand: JsCard[] = [
  { rank: '9', suit: 'S' },
  { rank: '10', suit: 'S' },
  { rank: 'J', suit: 'S' },
  { rank: 'Q', suit: 'S' },
  { rank: '4', suit: 'C' },
  { rank: '5', suit: 'C' },
  { rank: '6', suit: 'C' },
  { rank: 'A', suit: 'H' },
  { rank: 'A', suit: 'D' },
  { rank: 'A', suit: 'C' },
  { rank: 'K', suit: 'H' },
  { rank: 'K', suit: 'D' },
  { rank: 'K', suit: 'C' },
]

export const sampleWildlifeMeldHand: JsCard[] = [
  { rank: '3', suit: 'H' },
  { rank: '4', suit: 'H' },
  { rank: '5', suit: 'H' },
  { rank: '8', suit: 'D' },
  { rank: '9', suit: 'D' },
  { rank: '10', suit: 'D' },
  { rank: '6', suit: 'S' },
  { rank: '6', suit: 'H' },
  { rank: '6', suit: 'D' },
  { rank: 'J', suit: 'S' },
  { rank: 'J', suit: 'H' },
  { rank: 'J', suit: 'D' },
  { rank: 'J', suit: 'C' },
]

export const sampleStageShowHand: JsCard[] = [
  { rank: '5', suit: 'S' },
  { rank: '6', suit: 'S' },
  { rank: '7', suit: 'S' },
  { rank: '8', suit: 'S' },
  { rank: '3', suit: 'D' },
  { rank: '4', suit: 'D' },
  { rank: '5', suit: 'D' },
  { rank: '9', suit: 'H' },
  { rank: '9', suit: 'D' },
  { rank: '9', suit: 'C' },
  { rank: 'Q', suit: 'S' },
  { rank: 'Q', suit: 'H' },
  { rank: 'Q', suit: 'D' },
]

export const sampleGardenBloomJokerHand: JsCard[] = [
  { rank: 'A', suit: 'S' },
  { rank: '2', suit: 'S' },
  { rank: '3', suit: 'S' },
  { rank: '4', suit: 'H' },
  { rank: '5', suit: 'H' },
  { rank: 'J', suit: 'J' },
  { rank: '8', suit: 'C' },
  { rank: '8', suit: 'D' },
  { rank: '8', suit: 'H' },
  { rank: 'Q', suit: 'S' },
  { rank: 'Q', suit: 'H' },
  { rank: 'Q', suit: 'D' },
  { rank: 'Q', suit: 'C' },
]

export const sampleDragonWildcardHand: JsCard[] = [
  { rank: '6', suit: 'S' },
  { rank: '7', suit: 'S' },
  { rank: '8', suit: 'S' },
  { rank: '9', suit: 'H' },
  { rank: '10', suit: 'H' },
  { rank: 'J', suit: 'J' },
  { rank: '4', suit: 'C' },
  { rank: '4', suit: 'D' },
  { rank: '4', suit: 'H' },
  { rank: 'A', suit: 'D' },
  { rank: 'A', suit: 'H' },
  { rank: 'A', suit: 'C' },
  { rank: 'A', suit: 'S' },
]

export const sampleTreasureChestHand: JsCard[] = [
  { rank: 'K', suit: 'S' },
  { rank: 'Q', suit: 'S' },
  { rank: '10', suit: 'C' },
  { rank: '9', suit: 'H' },
  { rank: '8', suit: 'S' },
  { rank: '7', suit: 'C' },
  { rank: '6', suit: 'D' },
  { rank: '5', suit: 'H' },
  { rank: '4', suit: 'C' },
  { rank: '3', suit: 'D' },
  { rank: '2', suit: 'H' },
  { rank: 'J', suit: 'S' },
  { rank: 'A', suit: 'C' },
]

export const sampleRoadTripNearWinHand: JsCard[] = [
  { rank: 'A', suit: 'S' },
  { rank: '2', suit: 'S' },
  { rank: '3', suit: 'S' },
  { rank: '4', suit: 'H' },
  { rank: '5', suit: 'H' },
  { rank: '6', suit: 'H' },
  { rank: '7', suit: 'D' },
  { rank: '7', suit: 'H' },
  { rank: '7', suit: 'C' },
  { rank: '9', suit: 'S' },
  { rank: '10', suit: 'S' },
  { rank: 'J', suit: 'S' },
  { rank: 'K', suit: 'D' },
]

export const sampleOceanCurrentCandidates: JsCard[] = [
  ...sampleWildlifeMeldHand,
  { rank: '2', suit: 'C' },
  { rank: '8', suit: 'H' },
]

export const sampleSkylineFestivalCandidates: JsCard[] = [
  ...sampleStageShowHand,
  { rank: '2', suit: 'D' },
  { rank: '10', suit: 'C' },
]

export const sampleDesignatedJoker: JsCard = { rank: '2', suit: 'S' }

export const sampleCompletedHands = {
  classicCompleted: sampleCompletedHand,
  cosmicRun: sampleCosmicRunHand,
  wildlifeMeld: sampleWildlifeMeldHand,
  stageShow: sampleStageShowHand,
}

export const sampleJokerHands = {
  classicJoker: sampleJokerHand,
  gardenBloom: sampleGardenBloomJokerHand,
  dragonWildcard: sampleDragonWildcardHand,
}

export const sampleIncompleteHands = {
  classicIncomplete: sampleIncompleteHand,
  treasureChest: sampleTreasureChestHand,
  roadTripNearWin: sampleRoadTripNearWinHand,
}

export const sampleCandidateHandSets = {
  classicCandidates: sampleCandidateCards,
  oceanCurrent: sampleOceanCurrentCandidates,
  skylineFestival: sampleSkylineFestivalCandidates,
}

export const sampleHands = {
  completed: sampleCompletedHand,
  withJoker: sampleJokerHand,
  incomplete: sampleIncompleteHand,
  candidates: sampleCandidateCards,
  cosmicRun: sampleCosmicRunHand,
  wildlifeMeld: sampleWildlifeMeldHand,
  stageShow: sampleStageShowHand,
  gardenBloom: sampleGardenBloomJokerHand,
  dragonWildcard: sampleDragonWildcardHand,
  treasureChest: sampleTreasureChestHand,
  roadTripNearWin: sampleRoadTripNearWinHand,
  oceanCurrentCandidates: sampleOceanCurrentCandidates,
  skylineFestivalCandidates: sampleSkylineFestivalCandidates,
  completedCatalog: sampleCompletedHands,
  jokerCatalog: sampleJokerHands,
  incompleteCatalog: sampleIncompleteHands,
  candidateCatalog: sampleCandidateHandSets,
}

export interface HandEvaluation {
  hand: JsCard[]
  completed: boolean
  penaltyScore: number
}

export interface DemoGameOptions {
  playerIds?: string[]
  playerNames?: string[]
  deckCount?: number
}

export interface DemoMoveOptions {
  drawSource?: 'open' | 'close'
  discardIndex?: number
  playerId?: string
  claimWin?: boolean
}

export function evaluateHand(
  hand: JsCard[],
  designatedJoker: JsCard | null = null,
): HandEvaluation {
  return {
    hand,
    completed: isCompletedHand(hand, designatedJoker),
    penaltyScore: score(hand, designatedJoker),
  }
}

export function findBestCompletedHand(
  cards: JsCard[],
  designatedJoker: JsCard | null = null,
) {
  return completedHandExists(cards, designatedJoker)
}

export function createDemoGame(
  options: DemoGameOptions = {},
): JsIndianRummyGame {
  const {
    playerIds = ['player1', 'player2', 'player3'],
    playerNames = ['Alice', 'Bob', 'Charlie'],
    deckCount = 1,
  } = options

  return new JsIndianRummyGame(playerIds, playerNames, deckCount)
}

export function getCurrentPlayer(state: JsGameState): JsPlayer {
  const currentPlayer = state.players.find((player) =>
    player.id === state.nextTurnPlayer
  )

  if (!currentPlayer) {
    throw new Error(`Current player ${state.nextTurnPlayer} not found`)
  }

  return currentPlayer
}

export function getPlayerForMove(
  state: JsGameState,
  playerId?: string,
): JsPlayer {
  if (!playerId) {
    return getCurrentPlayer(state)
  }

  const selectedPlayer = state.players.find((player) => player.id === playerId)

  if (!selectedPlayer) {
    throw new Error(`Player ${playerId} not found`)
  }

  return selectedPlayer
}

export function buildMove(
  game: JsIndianRummyGame,
  options: DemoMoveOptions = {},
): JsMove {
  const state = game.getState()
  const {
    drawSource = 'open',
    discardIndex = 0,
    playerId,
    claimWin = false,
  } = options
  const currentPlayer = getPlayerForMove(state, playerId)
  const receivedCard = drawSource === 'open'
    ? game.getTopOpenCard()
    : game.getTopClosedCard()
  const discardCard = currentPlayer.hand[discardIndex]

  if (!receivedCard) {
    throw new Error(`Cannot build a ${drawSource}-pile move without a card`)
  }

  if (!discardCard) {
    throw new Error(
      `Player ${currentPlayer.id} has no discardable card at index ${discardIndex}`,
    )
  }

  return {
    playerId: currentPlayer.id,
    moveType: drawSource === 'open' ? JsMoveType.OpenCard : JsMoveType.CloseCard,
    cardReceived: receivedCard,
    cardDiscarded: discardCard,
    didClaimWin: claimWin,
  }
}

export function buildOpenPileMove(
  game: JsIndianRummyGame,
  options: Omit<DemoMoveOptions, 'drawSource'> = {},
): JsMove {
  return buildMove(game, { ...options, drawSource: 'open' })
}

export function buildClosePileMove(
  game: JsIndianRummyGame,
  options: Omit<DemoMoveOptions, 'drawSource'> = {},
): JsMove {
  return buildMove(game, { ...options, drawSource: 'close' })
}

export function buildFoldMove(
  game: JsIndianRummyGame,
  playerId?: string,
): JsMove {
  const state = game.getState()
  const currentPlayer = getPlayerForMove(state, playerId)

  return {
    playerId: currentPlayer.id,
    moveType: JsMoveType.Fold,
    didClaimWin: false,
  }
}

export function processDemoMove(
  game: JsIndianRummyGame,
  options: DemoMoveOptions = {},
): JsMoveResult {
  const move = buildMove(game, options)
  return game.processMove(move)
}

export function createDemoSyndicate(gameCount = 3): JsSyndicateGame {
  const playerIds = ['player1', 'player2', 'player3']
  const playerNames = ['Alice', 'Bob', 'Charlie']
  const syndicate = new JsSyndicateGame(playerIds)

  for (let index = 0; index < gameCount; index++) {
    syndicate.addRummyGame(createDemoGame({ playerIds, playerNames, deckCount: 1 }))
  }

  return syndicate
}

export function describeState(state: JsGameState): Record<string, unknown> {
  return {
    nextTurnPlayer: state.nextTurnPlayer,
    designatedJoker: `${state.designatedJoker.rank}${state.designatedJoker.suit}`,
    isComplete: state.isComplete,
    winner: state.winner ?? null,
    finalScores: state.finalScores,
    players: state.players.map((player) => ({
      id: player.id,
      name: player.name,
      handSize: player.hand.length,
    })),
  }
}

export function runExamples(): void {
  const handEvaluation = evaluateHand(sampleCompletedHand)
  console.log('Hand evaluation:', handEvaluation)

  const jokerEvaluation = evaluateHand(sampleJokerHand, sampleDesignatedJoker)
  console.log('Joker hand evaluation:', jokerEvaluation)

  const incompleteEvaluation = evaluateHand(sampleIncompleteHand)
  console.log('Incomplete hand evaluation:', incompleteEvaluation)

  const themedEvaluations = {
    cosmicRun: evaluateHand(sampleCosmicRunHand),
    wildlifeMeld: evaluateHand(sampleWildlifeMeldHand),
    gardenBloom: evaluateHand(sampleGardenBloomJokerHand, sampleDesignatedJoker),
    treasureChest: evaluateHand(sampleTreasureChestHand),
    roadTripNearWin: evaluateHand(sampleRoadTripNearWinHand),
  }
  console.log('Themed hand evaluations:', themedEvaluations)

  const completedHand = findBestCompletedHand(sampleCandidateCards)
  console.log('Best completed hand:', completedHand)

  const oceanCandidateResult = findBestCompletedHand(sampleOceanCurrentCandidates)
  console.log('Ocean candidate result:', oceanCandidateResult)

  const skylineCandidateResult = findBestCompletedHand(sampleSkylineFestivalCandidates)
  console.log('Skyline candidate result:', skylineCandidateResult)

  console.log('Creative hand catalog keys:', {
    completed: Object.keys(sampleCompletedHands),
    jokers: Object.keys(sampleJokerHands),
    incomplete: Object.keys(sampleIncompleteHands),
    candidates: Object.keys(sampleCandidateHandSets),
  })

  const game = createDemoGame()
  const initialState = game.getState()
  console.log('Initial game state:', describeState(initialState))

  const openMove = buildOpenPileMove(game)
  console.log('Open move:', openMove)

  const moveResult = game.processMove(openMove)
  console.log('Processed open move:', moveResult)

  const closeMove = buildClosePileMove(game)
  console.log('Close move option:', closeMove)

  const foldMove = buildFoldMove(game)
  console.log('Fold move option:', foldMove)

  const savedGame = game.serialize()
  const restoredGame = JsIndianRummyGame.deserialize(savedGame)
  console.log(
    'Restored game turn preserved:',
    restoredGame.getState().nextTurnPlayer === game.getState().nextTurnPlayer,
  )

  const syndicate = createDemoSyndicate()
  console.log('Syndicate game count:', syndicate.getGameCount())
  console.log('Syndicate leaderboard:', syndicate.getLeaderboard())
}

if (import.meta.main) {
  runExamples()
}
