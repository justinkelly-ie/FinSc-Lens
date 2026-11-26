module FinScLens.Horizons

import Control.App
import Data.DPair
import Data.List
import Data.Maybe
import FinScBox.BoxInt
import FinScBox.Interfaces
import FinScBox.Box
import FinScBox.HereditaryMSet
import FinScBox.Views
import FinScBox.QuadraticExchange
import FinScBox.MultisetCodec
import FinScBox.BlackRedZoom
import FinScSpace

%default total

--------------------------------------------------------------------------------
-- 1. TOPOLOGICAL RELICS, CLOSE OUTCOMES, HORIZON SPECS & BOUNDARY EXCEPTIONS
--------------------------------------------------------------------------------

||| Quenched unsigned `MultisetBit` skeleton observed after `GohBound` exhaustion.
public export
TopologicalRelic : Type
TopologicalRelic = HereditaryMSet MultisetBit

||| Observer classification of a quenched final state after closing an `App1` universe handle.
public export
data CloseOutcome : Type where
  ||| All opposing polarities annihilated back to the 0D vacuum root `[]`.
  CleanVacuumCollapse : CloseOutcome
  ||| System froze at a static relic with exact global signed multiplicity sum and active work.
  FrozenAtHorizon     : (staticTime : Time) -> (canonicalText : String) -> (workSteps : Nat) -> (globalNetSum : Integer) -> CloseOutcome

public export
Eq CloseOutcome where
  CleanVacuumCollapse           == CleanVacuumCollapse           = True
  (FrozenAtHorizon t1 s1 w1 n1) == (FrozenAtHorizon t2 s2 w2 n2) =
    t1 == t2 && s1 == s2 && w1 == w2 && n1 == n2
  _ == _ = False

public export
Show CloseOutcome where
  show CleanVacuumCollapse = "CleanVacuumCollapse([])"
  show (FrozenAtHorizon t txt work net) =
    "FrozenAtHorizon(time=" ++ show t ++ ", work=" ++ show work ++ ", net=" ++ show net ++ ", text=" ++ txt ++ ")"

public export
closeNetSum : CloseOutcome -> Integer
closeNetSum CleanVacuumCollapse       = 0
closeNetSum (FrozenAtHorizon _ _ _ g) = g

public export
closeOutcomeText : CloseOutcome -> String
closeOutcomeText CleanVacuumCollapse         = "[]"
closeOutcomeText (FrozenAtHorizon _ txt _ _) = txt

||| Counts active streaming peel work on `root : Configuration`:
||| causal load dipoles and throttle dipoles charge ONLY their outer boundary quanta (`1` per pole).
public export
rootActivePeelWork : Configuration -> Nat
rootActivePeelWork (MkHMSet b) =
  foldBox (\k, w, acc =>
             let mag = weightMagnitude w
             in if isJust (unpackLoadKey k) || isJust (unpackThrottleKey k)
                  then acc + mag
                  else acc + mag * S (fastPeelWorkHMSet k))
          0 b

||| Classifies a closed `EvolutionState k` into a `CloseOutcome`.
public export
classifyFlatClose : Ord k => EvolutionState k -> CloseOutcome
classifyFlatClose state =
  if isEmptyMaxel state.polarityMaxel &&
     isEmptyMaxel state.loadMaxel && isEmptyMaxel state.throttleMaxel
    then CleanVacuumCollapse
    else let work = stateActiveWork state
             net  = statePolarityNet state + stateFluxNet state
             stT  = StaticTime 0 (cast work)
         in FrozenAtHorizon stT ("FlatSparse(" ++ show work ++ " active)") work net

||| Classifies a closed hereditary `Configuration` into a `CloseOutcome`.
public export
classifyHereditaryClose : Configuration -> CloseOutcome
classifyHereditaryClose root =
  if root == emptyHMSet
    then CleanVacuumCollapse
    else let work = rootActivePeelWork root
             net  = unwrapBox (sumHMSet root)
             txt  = renderPureMultiset root
             stT  = StaticTime 0 (cast work)
         in FrozenAtHorizon stT txt work net

||| External observation horizon (`Observer = Time` cutoff + UV zoom depth).
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
    "HorizonSpec(bound=" ++ show h.boundTime ++
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
    "HorizonFrozenError(time=" ++ show t ++ ", reason=" ++ show r ++ ", cycles=" ++ show cyc ++ ")"

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
Eq HorizonObservationReport where
  a == b =
    a.horizonTime == b.horizonTime &&
    a.retroDepth == b.retroDepth &&
    a.cyclesCompleted == b.cyclesCompleted &&
    a.streamWorkUsed == b.streamWorkUsed &&
    a.freezeReason == b.freezeReason

public export
Show HorizonObservationReport where
  show r =
    "HorizonObservationReport(time=" ++ show r.horizonTime ++
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
          (MkGohBound {capacity = spec.budgetCap} spec.budgetCap)
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

||| Canonical Proton triad `uud = (+2, +2, -1)` at axes `1, 2, 3`.
public export
protonMSet : Configuration
protonMSet =
  MkHMSet $ setBox (scalarKeyHMSet 3) (MkBoxInt (-1))
          $ setBox (scalarKeyHMSet 2) (MkBoxInt 2)
          $ setBox (scalarKeyHMSet 1) (MkBoxInt 2) BoxLeaf

||| Canonical Neutron triad `udd = (+2, -1, -1)` at axes `1, 2, 3`.
public export
neutronMSet : Configuration
neutronMSet =
  MkHMSet $ setBox (scalarKeyHMSet 3) (MkBoxInt (-1))
          $ setBox (scalarKeyHMSet 2) (MkBoxInt (-1))
          $ setBox (scalarKeyHMSet 1) (MkBoxInt 2) BoxLeaf

||| Dense massive probe configuration (`10 * protonMSet`).
public export
heavyMassMSet : Configuration
heavyMassMSet = scaleHMSet (MkBoxInt 10) protonMSet

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

||| Derives an observer `HorizonSpec` intrinsically from `selfBoundOfHMSet (spreadZHMSet polyOrder)`.
public export
mkPolyBoundHorizonSpec : (polyOrder, retroDepth : Nat) -> HorizonSpec
mkPolyBoundHorizonSpec polyOrder retroDepth =
  let (_, stepBudget) = selfBoundOfHMSet (spreadZHMSet polyOrder)
  in MkHorizonSpec (BoundedTime stepBudget) retroDepth stepBudget

||| Streams irreducible Cigler-Herbig spread polynomial factors `Psi_d(x)` across ticks `d = 1..n`.
public export
streamGohDivisorFactors : (n : Nat) -> TimeStream OpenFuture Configuration
streamGohDivisorFactors n =
  let tbl = gohPsiTableUpTo n
  in MkTimeStream 1 (\d =>
       if d == 0 || d > n
         then StreamDone
         else if dividesNatPos d n
           then StreamYield (fromMaybe 1 (lookup d tbl))
           else StreamSkip)

||| Freezes `streamGohDivisorFactors n` via Cauchy multiplication (`*`) under `selfBoundOfHMSet (spreadZHMSet n)`.
public export
freezeGohSpreadStream : (n : Nat) -> (Configuration, Nat, Nat, Nat, StreamFreezeReason)
freezeGohSpreadStream n =
  let (MkAnyGohBound _ yBound, sBudget) = selfBoundOfHMSet (spreadZHMSet n)
      relic = freezeTimeStreamWith 0 yBound sBudget (*) 1 (streamGohDivisorFactors n)
  in (relic.relicState, yBound.boundSteps, relic.yieldsConsumed, relic.stepsConsumed, relic.freezeReason)

--------------------------------------------------------------------------------
-- 4. COMPILE-TIME QTT 0 PROBE, CODEC & GOH SPREAD THEOREMS
--------------------------------------------------------------------------------

public export
checkSettingDecode : String -> Configuration -> Bool
checkSettingDecode txt expected =
  case parsePureMultiset txt of
    Just m  => m == expected
    Nothing => False

public export
0 prfVacuumZeroStreamWork :
  (FinScBox.Views.streamPeelWorkHMSet (the Configuration FinScBox.HereditaryMSet.emptyHMSet)).workSteps = 0
prfVacuumZeroStreamWork = Refl

public export
0 prfQuenchRelicIdempotent :
  (let r0 : TopologicalRelic
       r0 = FinScBox.HereditaryMSet.castSupportHMSet FinScLens.Horizons.protonMSet
       c0 : Configuration
       c0 = FinScBox.HereditaryMSet.castSupportHMSet r0
   in the TopologicalRelic (FinScBox.HereditaryMSet.castSupportHMSet c0) == r0) = True
prfQuenchRelicIdempotent = Refl

public export
0 prfLoadThrottleLocalizedRoundTrips :
  (let pos = mkPureCoord [-8, 7, -3]
       kL  = mkLoadKey FinScBox.BlackRedZoom.sampleUnitPairMSet
       kT  = mkThrottleKey FinScBox.BlackRedZoom.sampleUnitPairMSet
       kM  = mkLocalizedKey pos FinScLens.Horizons.protonMSet
   in (unpackLoadKey kL == Just FinScBox.BlackRedZoom.sampleUnitPairMSet) &&
      (unpackThrottleKey kT == Just FinScBox.BlackRedZoom.sampleUnitPairMSet) &&
      (lookupLocalizedAt pos (wrapBox kM) == FinScLens.Horizons.protonMSet)) = True
prfLoadThrottleLocalizedRoundTrips = Refl

public export
0 prfStreamWorkMonotonic :
  (let wVac   = (streamPeelWorkHMSet (the Configuration FinScBox.HereditaryMSet.emptyHMSet)).workSteps
       wPhot  = (streamPeelWorkHMSet FinScBox.BlackRedZoom.sampleUnitPairMSet).workSteps
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
  ((FinScLens.Horizons.vacuumSettingText    == renderPureMultiset FinScBox.HereditaryMSet.emptyHMSet) &&
   (FinScLens.Horizons.photonSettingText    == renderPureMultiset FinScBox.BlackRedZoom.sampleUnitPairMSet) &&
   (FinScLens.Horizons.protonSettingText    == renderPureMultiset FinScLens.Horizons.protonMSet) &&
   (FinScLens.Horizons.neutronSettingText   == renderPureMultiset FinScLens.Horizons.neutronMSet) &&
   (FinScLens.Horizons.heavyMassSettingText == renderPureMultiset FinScLens.Horizons.heavyMassMSet) &&
   checkSettingDecode FinScLens.Horizons.vacuumSettingText    FinScBox.HereditaryMSet.emptyHMSet &&
   checkSettingDecode FinScLens.Horizons.photonSettingText    FinScBox.BlackRedZoom.sampleUnitPairMSet &&
   checkSettingDecode FinScLens.Horizons.protonSettingText    FinScLens.Horizons.protonMSet &&
   checkSettingDecode FinScLens.Horizons.neutronSettingText   FinScLens.Horizons.neutronMSet &&
   checkSettingDecode FinScLens.Horizons.heavyMassSettingText FinScLens.Horizons.heavyMassMSet) = True
prfAllSettingsParityAndDecode = Refl

public export
0 prfPythPowerMatchesSpreadZ28 :
  (let MkQuadraticTriple _ b1 r1 = FinScBox.QuadraticExchange.pythPowerTriple 1 (MkBoxInt 28)
       MkQuadraticTriple _ b2 _  = FinScBox.QuadraticExchange.pythPowerTriple 2 (MkBoxInt 28)
       MkQuadraticTriple _ b3 _  = FinScBox.QuadraticExchange.pythPowerTriple 3 (MkBoxInt 28)
       MkQuadraticTriple _ b4 _  = FinScBox.QuadraticExchange.pythPowerTriple 4 (MkBoxInt 28)
       num                       = 4 * b1 * b1
       den                       = r1 * r1
   in (evalHomogPolyHMSet 1 (spreadZHMSet 1) num den == 4 * b1 * b1) &&
      (evalHomogPolyHMSet 2 (spreadZHMSet 2) num den == 4 * b2 * b2) &&
      (evalHomogPolyHMSet 3 (spreadZHMSet 3) num den == 4 * b3 * b3) &&
      (evalHomogPolyHMSet 4 (spreadZHMSet 4) num den == 4 * b4 * b4)) = True
prfPythPowerMatchesSpreadZ28 = Refl
