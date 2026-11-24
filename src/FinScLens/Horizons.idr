module FinScLens.Horizons

import Control.App
import Data.DPair
import FinScBox.BoxInt
import FinScBox.Interfaces
import FinScBox.Box
import FinScBox.HereditaryMSet
import FinScBox.Views
import FinScBox.Lenses
import FinScBox.QuadraticExchange
import FinScBox.MultisetCodec
import FinScBox.BlackRedZoom
import FinScDynamics

%default total

--------------------------------------------------------------------------------
-- 1. TOPOLOGICAL RELICS, HORIZON SPECS & BOUNDARY EXCEPTIONS
--------------------------------------------------------------------------------

||| Quenched unit-weight matter skeleton observed after `GohFuel` exhaustion.
public export
TopologicalRelic : Type
TopologicalRelic = Configuration

||| Strips signed multiplicities down to the unit-weight `TopologicalRelic` skeleton (`MSetSupport`).
public export
quenchRelic : Configuration -> TopologicalRelic
quenchRelic = MSetSupport

||| External observation horizon (`Observer = Time` cutoff + UV zoom depth):
||| - `boundTime`  : Bounded compute horizon (`BoundedTime cap` or `WindowedTime win`)
||| - `retroDepth` : Retrograde UV zoom depth (`RetroTime g`)
||| - `budgetCap`  : Total stream-work fuel budget available before freeze-out
public export
record HorizonSpec where
  constructor MkHorizonSpec
  boundTime  : Time
  retroDepth : Nat
  budgetCap  : Nat

public export
Eq HorizonSpec where
  a == b = a.boundTime == b.boundTime && a.retroDepth == b.retroDepth && a.budgetCap == b.budgetCap

public export
Show HorizonSpec where
  show h =
    "HorizonSpec(bound=" ++ showTime h.boundTime ++
    ", retro=" ++ show h.retroDepth ++
    ", budget=" ++ show h.budgetCap ++ ")"

||| Typed boundary exceptions raised in the outer `App` observer shell (`MayThrow`)
||| while the inner `App1` universe remains strictly blind and `NoThrow` (`Usage = One`).
public export
data UniverseErr : Type where
  InvalidMultisetTextError   : (input : String) -> UniverseErr
  UnbalancedVacuumSplitError : (netResidual : Integer) -> UniverseErr
  HorizonFrozenError         : (horizonTime : Time) -> (reason : StreamFreezeReason) -> (cycles : Nat) -> UniverseErr

public export
Eq UniverseErr where
  (InvalidMultisetTextError a)   == (InvalidMultisetTextError b)   = a == b
  (UnbalancedVacuumSplitError a) == (UnbalancedVacuumSplitError b) = a == b
  (HorizonFrozenError t1 r1 c1)  == (HorizonFrozenError t2 r2 c2)  = t1 == t2 && r1 == r2 && c1 == c2
  _ == _ = False

public export
Show UniverseErr where
  show (InvalidMultisetTextError txt) =
    "InvalidMultisetTextError(" ++ txt ++ ")"
  show (UnbalancedVacuumSplitError net) =
    "UnbalancedVacuumSplitError(net=" ++ show net ++ " /= 0)"
  show (HorizonFrozenError t r cyc) =
    "HorizonFrozenError(time=" ++ showTime t ++ ", reason=" ++ show r ++ ", cycles=" ++ show cyc ++ ")"

||| Readout returned when probing a live black-box universe through a `HorizonSpec`.
public export
record HorizonObservationReport where
  constructor MkHorizonObservationReport
  horizonTime     : Time
  retroDepth      : Nat
  cyclesCompleted : Nat
  streamWorkUsed  : Nat
  freezeReason    : StreamFreezeReason

public export
Show HorizonObservationReport where
  show r =
    "HorizonObservationReport(time=" ++ showTime r.horizonTime ++
    ", retro=" ++ show r.retroDepth ++
    ", cycles=" ++ show r.cyclesCompleted ++
    ", workUsed=" ++ show r.streamWorkUsed ++
    ", reason=" ++ show r.freezeReason ++ ")"

||| Freezes a constant-work-per-cycle observer stream via `freezeStateStreamWith`.
public export
observeHorizonFreeze : HorizonSpec -> (workPerCycle : Nat) -> HorizonObservationReport
observeHorizonFreeze spec workPerCycle =
  let (relic, _) =
        freezeStateStreamWith
          spec.retroDepth
          spec.budgetCap
          (MkGohFuel spec.budgetCap)
          20
          (\u => if workPerCycle == 0 then (StreamSkip, 0, 1) else (StreamYield u, workPerCycle, 0))
          ()
  in MkHorizonObservationReport
       spec.boundTime
       spec.retroDepth
       (if workPerCycle == 0 then 0 else relic.stepsConsumed)
       relic.yieldsConsumed
       relic.freezeReason

--------------------------------------------------------------------------------
-- 2. INITIAL PROBE CONFIGURATIONS & SERIALIZED BOUNDARY VALIDATORS
--------------------------------------------------------------------------------

public export
vacuumMSet : Configuration
vacuumMSet = emptyPureMSet

||| Canonical 1D photon dipole `A_1 = alpha^{+1} - alpha^{-1}`.
public export
photonMSet : Configuration
photonMSet =
  subHMSet (MkHMSet (singletonBox (scalarKeyHMSet 1) (intToBoxInt 1)))
           (MkHMSet (singletonBox (scalarKeyHMSet (-1)) (intToBoxInt 1)))

||| Canonical Proton triad `uud = (+2, +2, -1)` at axes `1, 2, 3`.
public export
protonMSet : Configuration
protonMSet =
  MkHMSet $ setBox (scalarKeyHMSet 3) (intToBoxInt (-1))
          $ setBox (scalarKeyHMSet 2) (intToBoxInt 2)
          $ setBox (scalarKeyHMSet 1) (intToBoxInt 2) BoxLeaf

||| Canonical Neutron triad `udd = (+2, -1, -1)` at axes `1, 2, 3`.
public export
neutronMSet : Configuration
neutronMSet =
  MkHMSet $ setBox (scalarKeyHMSet 3) (intToBoxInt (-1))
          $ setBox (scalarKeyHMSet 2) (intToBoxInt (-1))
          $ setBox (scalarKeyHMSet 1) (intToBoxInt 2) BoxLeaf

||| Dense massive probe configuration (`10 * protonMSet`).
public export
heavyMassMSet : Configuration
heavyMassMSet = scaleHMSet (intToBoxInt 10) protonMSet

public export
vacuumSettingText : String
vacuumSettingText = "[]"

public export
photonSettingText : String
photonSettingText = "{{[]:[-1]}:[-1], {[]:[1]}:[1]}"

public export
protonSettingText : String
protonSettingText = "{{[]:[1]}:[2], {[]:[2]}:[2], {[]:[3]}:[-1]}"

public export
neutronSettingText : String
neutronSettingText = "{{[]:[1]}:[2], {[]:[2]}:[-1], {[]:[3]}:[-1]}"

public export
heavyMassSettingText : String
heavyMassSettingText = "{{[]:[1]}:[20], {[]:[2]}:[20], {[]:[3]}:[-10]}"

||| Validates serialized multiset configuration text via total `Dec` recognizer before entering `App1`.
export
validateAndDecodeSetting : Has [Exception UniverseErr] e => String -> App e Configuration
validateAndDecodeSetting txt =
  case recognizePureMultisetText txt of
    No _ => throw (InvalidMultisetTextError txt)
    Yes _ =>
      case decodeCanonicalMultiset txt of
        Just (Element m _) => pure m
        Nothing            => throw (InvalidMultisetTextError txt)

||| Enforces global 0D zero-sum neutrality (`wA + wB == 0`) at the black-box boundary.
export
requireBalancedSplit : Has [Exception UniverseErr] e => Integer -> Integer -> App e ()
requireBalancedSplit wA wB =
  let net = wA + wB
  in if net == 0
       then pure ()
       else throw (UnbalancedVacuumSplitError net)

--------------------------------------------------------------------------------
-- 3. POLYNUMBER HORIZON CUTOFFS & GOH SPREAD STREAM FREEZE-OUT
--------------------------------------------------------------------------------

||| Derives an observer `HorizonSpec` intrinsically from `selfFuelOfHMSet (spreadZHMSet polyOrder)`.
public export
mkPolyFuelHorizonSpec : (polyOrder, retroDepth : Nat) -> HorizonSpec
mkPolyFuelHorizonSpec polyOrder retroDepth =
  let (_, stepBudget) = selfFuelOfHMSet (spreadZHMSet polyOrder)
  in MkHorizonSpec (BoundedTime stepBudget) retroDepth stepBudget

||| Coarse observation horizon (`RetroTime 0`) budgeted from `spreadZHMSet 10`.
public export
coarseHorizonSpec : HorizonSpec
coarseHorizonSpec = mkPolyFuelHorizonSpec 10 0

||| Deep UV observation horizon (`RetroTime 2`) budgeted from `spreadZHMSet 10`.
public export
deepUVHorizonSpec : HorizonSpec
deepUVHorizonSpec = mkPolyFuelHorizonSpec 10 2

||| Streams irreducible Cigler-Herbig spread polynomial factors `Psi_d(x)` across ticks `d = 1..n`.
public export
streamGohDivisorFactors : (n : Nat) -> TimeStream OpenFuture Configuration
streamGohDivisorFactors n =
  MkTimeStream 1 (\d =>
    if d == 0 || d > n
      then StreamDone
      else if snd (divModIntegerPos (natToInteger n) (natToInteger d)) == 0
        then StreamYield (gohPsiFactor d)
        else StreamSkip)

||| Freezes `streamGohDivisorFactors n` via Cauchy multiplication (`mulHMSet`) under `selfFuelOfHMSet (spreadZHMSet n)`.
public export
freezeGohSpreadStream : (n : Nat) -> (Configuration, Nat, Nat, Nat, StreamFreezeReason)
freezeGohSpreadStream n =
  let (MkAnyGohFuel cap yFuel, sBudget) = selfFuelOfHMSet (spreadZHMSet n)
      relic = freezeTimeStreamWith 0 cap yFuel sBudget mulHMSet 1 (streamGohDivisorFactors n)
  in (relic.relicState, gohSteps yFuel, relic.yieldsConsumed, relic.stepsConsumed, relic.freezeReason)

--------------------------------------------------------------------------------
-- 4. OUTER `App` BOUNDARY GUARDS & EXTERNAL HORIZON PROBE RUNNERS
--------------------------------------------------------------------------------

export
runFlatGuardedSplitBoundary :
  Has [Exception UniverseErr] e =>
  String -> Integer -> Integer -> App e CloseOutcome
runFlatGuardedSplitBoundary settingTxt wA wB = do
  _ <- validateAndDecodeSetting settingTxt
  requireBalancedSplit wA wB
  app1 $ App1.do
    u0 <- bootFlat0D
    u1 <- splitFlatFromVacuum (5, 0, 0) (5, 0, 0) (intToBoxInt wA) u0
    closeFlat0D u1

export
runFlatHorizonProbe :
  Has [Exception UniverseErr] e =>
  HorizonSpec -> App e HorizonObservationReport
runFlatHorizonProbe spec = do
  rep <- app1 $ App1.do
    u0 <- bootFlat0D
    u1 <- splitFlatFromVacuum (5, 0, 0) (30, 30, 0) (intToBoxInt 14) u0
    obs <- observeFlatDirect
             (\st =>
                let p50          = mkCoord2D coord2Basis 5 0
                    baseWork     = weightMagnitude (lookupMaxel p50 p50 st.polarityMaxel)
                    workPerCycle = if baseWork == 0 then 0 else baseWork + 2 * spec.retroDepth
                in observeHorizonFreeze spec workPerCycle)
             u1
    let MkPureObs u2 rep _ = obs
    _ <- closeFlat0D u2
    App1.pure rep
  case rep.freezeReason of
    YieldExhausted =>
      throw (HorizonFrozenError rep.horizonTime rep.freezeReason rep.cyclesCompleted)
    StepHorizonReached =>
      throw (HorizonFrozenError rep.horizonTime rep.freezeReason rep.cyclesCompleted)
    NaturalCompletion =>
      pure rep

export
runHereditaryGuardedSplitBoundary :
  Has [Exception UniverseErr] e =>
  String -> Integer -> Integer -> App e CloseOutcome
runHereditaryGuardedSplitBoundary settingTxt wA wB = do
  payload <- validateAndDecodeSetting settingTxt
  requireBalancedSplit wA wB
  app1 $ App1.do
    u0 <- bootHereditary0D
    u1 <- splitHereditaryFromVacuum (5, 0, 0) (5, 0, 0) payload u0
    closeHereditary0D u1

export
runHereditaryHorizonProbe :
  Has [Exception UniverseErr] e =>
  HorizonSpec -> Configuration -> App e HorizonObservationReport
runHereditaryHorizonProbe spec crossingPayload = do
  rep <- app1 $ App1.do
    u0 <- bootHereditary0D
    u1 <- splitHereditaryFromVacuum (5, 0, 0) (30, 30, 0) crossingPayload u0
    let lens = zoomedExternalLens
                 (findMatterPayloadAt (5, 0, 0))
                 (blackShift spec.retroDepth)
                 zeroShift
                 spec.boundTime
                 (\zoomed =>
                    let workPerCycle = (streamPeelWorkHMSet zoomed).workSteps
                    in observeHorizonFreeze spec workPerCycle)
    obs <- observeHereditaryPure lens u1
    let MkPureObs u2 rep _ = obs
    _ <- closeHereditary0D u2
    App1.pure rep
  case rep.freezeReason of
    YieldExhausted =>
      throw (HorizonFrozenError rep.horizonTime rep.freezeReason rep.cyclesCompleted)
    StepHorizonReached =>
      throw (HorizonFrozenError rep.horizonTime rep.freezeReason rep.cyclesCompleted)
    NaturalCompletion =>
      pure rep

--------------------------------------------------------------------------------
-- 5. COMPILE-TIME QTT 0 PROBE, CODEC & GOH SPREAD THEOREMS
--------------------------------------------------------------------------------

public export
checkSettingDecode : String -> Configuration -> Bool
checkSettingDecode txt expected =
  case parsePureMultiset txt of
    Just m  => m == expected
    Nothing => False

public export
0 prfVacuumZeroStreamWork :
  (FinScBox.Views.streamPeelWorkHMSet FinScLens.Horizons.vacuumMSet).workSteps = 0
prfVacuumZeroStreamWork = Refl

public export
0 prfQuenchRelicIdempotent :
  FinScLens.Horizons.quenchRelic (FinScLens.Horizons.quenchRelic FinScLens.Horizons.protonMSet)
  = FinScLens.Horizons.quenchRelic FinScLens.Horizons.protonMSet
prfQuenchRelicIdempotent = Refl

public export
0 prfLoadThrottleMatterRoundTrips :
  (let kL = mkLoadKey FinScLens.Horizons.photonMSet
       kT = mkThrottleKey FinScLens.Horizons.photonMSet
       kM = mkMatterKey (natSiteKey (8, 7, 0)) FinScLens.Horizons.protonMSet
   in (unpackLoadKey kL == Just FinScLens.Horizons.photonMSet) &&
      (unpackThrottleKey kT == Just FinScLens.Horizons.photonMSet) &&
      (unpackMatterKey kM == Just (natSiteKey (8, 7, 0), FinScLens.Horizons.protonMSet))) = True
prfLoadThrottleMatterRoundTrips = Refl

public export
0 prfStreamWorkMonotonic :
  (let wVac   = (streamPeelWorkHMSet FinScLens.Horizons.vacuumMSet).workSteps
       wPhot  = (streamPeelWorkHMSet FinScLens.Horizons.photonMSet).workSteps
       wProt  = (streamPeelWorkHMSet FinScLens.Horizons.protonMSet).workSteps
       wHeavy = (streamPeelWorkHMSet FinScLens.Horizons.heavyMassMSet).workSteps
   in (wVac == 0) && (wVac < wPhot) && (wPhot < wProt) && (wProt < wHeavy)) = True
prfStreamWorkMonotonic = Refl

public export
0 prfFastPeelMatchesProton :
  FinScBox.Views.fastPeelWorkHMSet FinScLens.Horizons.protonMSet
  = (FinScBox.Views.streamPeelWorkHMSet FinScLens.Horizons.protonMSet).workSteps
prfFastPeelMatchesProton = Refl

public export
0 prfAllSettingsParityAndDecode :
  ((FinScLens.Horizons.vacuumSettingText    == renderPureMultiset FinScLens.Horizons.vacuumMSet) &&
   (FinScLens.Horizons.photonSettingText    == renderPureMultiset FinScLens.Horizons.photonMSet) &&
   (FinScLens.Horizons.protonSettingText    == renderPureMultiset FinScLens.Horizons.protonMSet) &&
   (FinScLens.Horizons.neutronSettingText   == renderPureMultiset FinScLens.Horizons.neutronMSet) &&
   (FinScLens.Horizons.heavyMassSettingText == renderPureMultiset FinScLens.Horizons.heavyMassMSet) &&
   checkSettingDecode FinScLens.Horizons.vacuumSettingText    FinScLens.Horizons.vacuumMSet &&
   checkSettingDecode FinScLens.Horizons.photonSettingText    FinScLens.Horizons.photonMSet &&
   checkSettingDecode FinScLens.Horizons.protonSettingText    FinScLens.Horizons.protonMSet &&
   checkSettingDecode FinScLens.Horizons.neutronSettingText   FinScLens.Horizons.neutronMSet &&
   checkSettingDecode FinScLens.Horizons.heavyMassSettingText FinScLens.Horizons.heavyMassMSet) = True
prfAllSettingsParityAndDecode = Refl

public export
0 prfPythPowerMatchesSpreadZ28 :
  (let (_, b1, r1) = FinScBox.QuadraticExchange.pythPowerTriple 1 (MkBoxInt 28)
       (_, b2, _)  = FinScBox.QuadraticExchange.pythPowerTriple 2 (MkBoxInt 28)
       (_, b3, _)  = FinScBox.QuadraticExchange.pythPowerTriple 3 (MkBoxInt 28)
       (_, b4, _)  = FinScBox.QuadraticExchange.pythPowerTriple 4 (MkBoxInt 28)
       num         = 4 * b1 * b1
       den         = r1 * r1
   in (evalHomogPolyHMSet 1 (spreadZHMSet 1) num den == 4 * b1 * b1) &&
      (evalHomogPolyHMSet 2 (spreadZHMSet 2) num den == 4 * b2 * b2) &&
      (evalHomogPolyHMSet 3 (spreadZHMSet 3) num den == 4 * b3 * b3) &&
      (evalHomogPolyHMSet 4 (spreadZHMSet 4) num den == 4 * b4 * b4)) = True
prfPythPowerMatchesSpreadZ28 = Refl
