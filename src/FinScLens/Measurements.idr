module FinScLens.Measurements

import FinScBox.BoxInt
import FinScBox.Interfaces
import FinScBox.Box
import FinScBox.HereditaryMSet
import FinScSpace
import FinScLens.Horizons

%default total

--------------------------------------------------------------------------------
-- 1. EXTERNAL MEASUREMENT READOUT RECORDS
--------------------------------------------------------------------------------

||| Trajectory & quadrance readout for a localized excitation evolving under quadratic link exchanges.
public export
record ExcitationReport where
  constructor MkExcitationReport
  meanX           : Proportion
  meanY           : Proportion
  fluxYAccum      : BoxInt
  initQuadrance   : BoxInt
  finalQuadrance  : BoxInt
  maxQuadrance    : BoxInt
  polarityNetSum  : BoxInt
  activeWorkSteps : Nat

||| Sterling recognition predicate verifying finite affine `Proportion` centroids (`den /= 0`).
public export
isValidExcitationReport : ExcitationReport -> Bool
isValidExcitationReport r = isAffineRatio r.meanX && isAffineRatio r.meanY

public export
Eq ExcitationReport where
  a == b =
    a.meanX == b.meanX &&
    a.meanY == b.meanY &&
    a.fluxYAccum == b.fluxYAccum &&
    a.initQuadrance == b.initQuadrance &&
    a.finalQuadrance == b.finalQuadrance &&
    a.maxQuadrance == b.maxQuadrance &&
    a.polarityNetSum == b.polarityNetSum &&
    a.activeWorkSteps == b.activeWorkSteps

public export
Show ExcitationReport where
  show r =
    "ExcitationReport(meanX=" ++ show r.meanX ++
    ", meanY=" ++ show r.meanY ++
    ", fluxYAccum=" ++ show r.fluxYAccum ++
    ", initQ=" ++ show r.initQuadrance ++
    ", finalQ=" ++ show r.finalQuadrance ++
    ", maxQ=" ++ show r.maxQuadrance ++
    ", net=" ++ show r.polarityNetSum ++
    ", work=" ++ show r.activeWorkSteps ++ ")"

||| Readout comparing bound-dipole self-trapping under interaction load vs free linear dispersion.
public export
record SelfTrapReport where
  constructor MkSelfTrapReport
  initQuadrance  : BoxInt
  finalQuadrance : BoxInt
  maxQuadrance   : BoxInt
  coreShare      : Proportion
  netSignedSum   : BoxInt
  workSteps      : Nat

||| Sterling recognition predicate verifying finite affine `coreShare` (`den /= 0`).
public export
isValidSelfTrapReport : SelfTrapReport -> Bool
isValidSelfTrapReport r = isAffineRatio r.coreShare

public export
Eq SelfTrapReport where
  a == b =
    a.initQuadrance == b.initQuadrance &&
    a.finalQuadrance == b.finalQuadrance &&
    a.maxQuadrance == b.maxQuadrance &&
    a.coreShare == b.coreShare &&
    a.netSignedSum == b.netSignedSum &&
    a.workSteps == b.workSteps

public export
Show SelfTrapReport where
  show r =
    "SelfTrapReport(Q=" ++ show r.initQuadrance ++ "->" ++ show r.finalQuadrance ++
    ", maxQ=" ++ show r.maxQuadrance ++
    ", coreShare=" ++ show r.coreShare ++
    ", net=" ++ show r.netSignedSum ++
    ", work=" ++ show r.workSteps ++ ")"

||| Readout for equal-quadrance 2-body mutual deflection (with zero background dipole).
public export
record TwoBodyReport where
  constructor MkTwoBodyReport
  tbBotY        : Proportion
  tbTopY        : Proportion
  tbSingleBotY  : Proportion
  tbBotSy       : BoxInt
  tbTopSy       : BoxInt
  tbSingleBotSy : BoxInt
  tbLinearBotSy : BoxInt
  tbInitQ       : BoxInt
  tbFinalQ      : BoxInt
  tbMaxQ        : BoxInt
  tbNetSum      : BoxInt

||| Sterling recognition predicate verifying finite affine centroid `Proportion` fields (`den /= 0`).
public export
isValidTwoBodyReport : TwoBodyReport -> Bool
isValidTwoBodyReport r = isAffineRatio r.tbBotY && isAffineRatio r.tbTopY && isAffineRatio r.tbSingleBotY

public export
Eq TwoBodyReport where
  a == b =
    a.tbBotY == b.tbBotY &&
    a.tbTopY == b.tbTopY &&
    a.tbSingleBotY == b.tbSingleBotY &&
    a.tbBotSy == b.tbBotSy &&
    a.tbTopSy == b.tbTopSy &&
    a.tbSingleBotSy == b.tbSingleBotSy &&
    a.tbLinearBotSy == b.tbLinearBotSy &&
    a.tbInitQ == b.tbInitQ &&
    a.tbFinalQ == b.tbFinalQ &&
    a.tbMaxQ == b.tbMaxQ &&
    a.tbNetSum == b.tbNetSum

public export
Show TwoBodyReport where
  show r =
    "TwoBodyReport(dy=" ++ show (subProportion r.tbBotY r.tbSingleBotY) ++
    ", yBot=" ++ show r.tbBotY ++ ", yTop=" ++ show r.tbTopY ++
    ", SyBot=" ++ show r.tbBotSy ++ ", SyTop=" ++ show r.tbTopSy ++
    " vs 1exc=" ++ show r.tbSingleBotSy ++ ", lin=" ++ show r.tbLinearBotSy ++
    ", Q=" ++ show r.tbInitQ ++ "->" ++ show r.tbFinalQ ++
    ", maxQ=" ++ show r.tbMaxQ ++ ", net=" ++ show r.tbNetSum ++ ")"

||| Readout proving finite-speed `c = 1` causal cone propagation:
||| exact zero perturbation at spacelike separation (`t = 2`) followed by deflection at `t = 18`.
public export
record CausalLightConeReport where
  constructor MkCausalLightConeReport
  clSpacelikeExact : Bool
  clEarlyVacX      : Proportion
  clEarlyDpX       : Proportion
  clEarlyDpSy      : BoxInt
  clLateVacX       : Proportion
  clLateDpX        : Proportion
  clLateDpY        : Proportion
  clLateDpSy       : BoxInt

||| Sterling recognition predicate verifying finite affine cone centroid `Proportion`s (`den /= 0`).
public export
isValidCausalLightConeReport : CausalLightConeReport -> Bool
isValidCausalLightConeReport r =
  isAffineRatio r.clEarlyVacX && isAffineRatio r.clEarlyDpX &&
  isAffineRatio r.clLateVacX && isAffineRatio r.clLateDpX && isAffineRatio r.clLateDpY

public export
Eq CausalLightConeReport where
  a == b =
    a.clSpacelikeExact == b.clSpacelikeExact &&
    a.clEarlyVacX == b.clEarlyVacX &&
    a.clEarlyDpX == b.clEarlyDpX &&
    a.clEarlyDpSy == b.clEarlyDpSy &&
    a.clLateVacX == b.clLateVacX &&
    a.clLateDpX == b.clLateDpX &&
    a.clLateDpY == b.clLateDpY &&
    a.clLateDpSy == b.clLateDpSy

public export
Show CausalLightConeReport where
  show r =
    "CausalLightCone(spacelikeExact=" ++ show r.clSpacelikeExact ++
    ", t2: VacX=" ++ show r.clEarlyVacX ++ " vs DpX=" ++ show r.clEarlyDpX ++ ", DpSy=" ++ show r.clEarlyDpSy ++
    " -> t18: VacX=" ++ show r.clLateVacX ++ " vs DpX=" ++ show r.clLateDpX ++ ", DpY=" ++ show r.clLateDpY ++ ", DpSy=" ++ show r.clLateDpSy ++ ")"

||| Readout for anisotropic directional load stress and quadrupole polarization (`sum_X L - sum_Y L`).
public export
record AnisotropicStressReport where
  constructor MkAnisotropicStressReport
  asQuadHoriz    : BoxInt
  asQuadVert     : BoxInt
  asProbeHorizX  : Proportion
  asProbeHorizY  : Proportion
  asProbeHorizSy : BoxInt
  asProbeVertX   : Proportion
  asProbeVertY   : Proportion
  asProbeVertSy  : BoxInt
  asSymLoadOk    : Bool
  asNetSum       : BoxInt

||| Sterling recognition predicate verifying finite affine probe centroid `Proportion`s (`den /= 0`).
public export
isValidAnisotropicStressReport : AnisotropicStressReport -> Bool
isValidAnisotropicStressReport r =
  isAffineRatio r.asProbeHorizX && isAffineRatio r.asProbeHorizY &&
  isAffineRatio r.asProbeVertX && isAffineRatio r.asProbeVertY

public export
Eq AnisotropicStressReport where
  a == b =
    a.asQuadHoriz == b.asQuadHoriz &&
    a.asQuadVert == b.asQuadVert &&
    a.asProbeHorizX == b.asProbeHorizX &&
    a.asProbeHorizY == b.asProbeHorizY &&
    a.asProbeHorizSy == b.asProbeHorizSy &&
    a.asProbeVertX == b.asProbeVertX &&
    a.asProbeVertY == b.asProbeVertY &&
    a.asProbeVertSy == b.asProbeVertSy &&
    a.asSymLoadOk == b.asSymLoadOk &&
    a.asNetSum == b.asNetSum

public export
Show AnisotropicStressReport where
  show r =
    "AnisotropicStress(Q_H=" ++ show r.asQuadHoriz ++
    ", Q_V=" ++ show r.asQuadVert ++
    ", probeH=(" ++ show r.asProbeHorizX ++ "," ++ show r.asProbeHorizY ++ ",Sy=" ++ show r.asProbeHorizSy ++ ")" ++
    ", probeV=(" ++ show r.asProbeVertX ++ "," ++ show r.asProbeVertY ++ ",Sy=" ++ show r.asProbeVertSy ++ ")" ++
    ", dX=" ++ show (subProportion r.asProbeHorizX r.asProbeVertX) ++
    ", sym=" ++ show r.asSymLoadOk ++
    ", net=" ++ show r.asNetSum ++ ")"

--------------------------------------------------------------------------------
-- 2. EXTERNAL SPATIAL MOMENTS, CORE QUADRANCE & LOAD QUADRUPOLE REDUCTIONS
--------------------------------------------------------------------------------

public export
inSpaceBox : {n : Nat} -> AxisBasis k -> (lo, hi : Space n) -> k -> Bool
inSpaceBox ax lo hi pos = go Z lo hi (ax.toSiteMSet pos)
  where
    go : {0 m : Nat} -> Nat -> Space m -> Space m -> PureMultiset -> Bool
    go _   []        []        _ = True
    go idx (l :: ls) (h :: hs) s =
      let c : SignedCoord
          c = cast (lookupKeyHMSet (axisAtom idx) s)
      in c >= l && c <= h && go (S idx) ls hs s

public export
canonAnyLinkStart : GroupCoord k => AxisBasis k -> Maxel BoxInt k k DiagAntisymForm -> k -> k -> Maybe k
canonAnyLinkStart ax pol a b =
  findBox (\axKey, _ => canonLinkStart (ax.axisStep axKey) a b)
          (activeAxisAtomsOf ax pol)

||| Exact rational share `[coreQ : totQ]` of `quadranceOf st.polarityMaxel` localized inside `[lo..hi]`.
public export
fieldCoreQuadranceShare : {n : Nat} -> GroupCoord k => AxisBasis k -> (lo, hi : Space n) -> EvolutionState k -> Proportion
fieldCoreQuadranceShare ax lo hi st =
  let inCore = inSpaceBox ax lo hi
      qM     = quadranceOf st.polarityMaxel
      totQ   = sumStoredMaxel qM
      cM     = filterMaxel (\a, b =>
                             if a == b
                               then inCore a
                               else case canonAnyLinkStart ax st.polarityMaxel a b of
                                      Just s  => inCore s
                                      Nothing => False)
                           qM
      cTot   = sumStoredMaxel cM
  in if totQ <= 0 then MkProportion 0 1 else MkProportion cTot totQ

public export
bandMomentsWithPol :
  GroupCoord k =>
  AxisBasis k ->
  (axisT : PureMultiset) ->
  (tLo, tHi : BoxInt) ->
  Maxel BoxInt k k DiagAntisymForm ->
  (Box BoxInt k, BoxInt, BoxInt, BoxInt)
bandMomentsWithPol ax axisT tLo tHi dPol =
  let bandPol    = filterBox (\a => let t = coordAlong ax axisT a in t >= tLo && t <= tHi) (diagBox dPol)
      qpBox      = bandPol .*. bandPol
      accQPol    = totalMassBox qpBox
      momentMSet = flattenHMSetBox (mapBox ax.toSiteMSet qpBox)
  in (bandPol, lookupKeyHMSet (axisAtom 0) momentMSet, lookupKeyHMSet axisT momentMSet, accQPol)

||| Transverse-axis (`axisT`, `tLo <= t <= tHi`) background-subtracted excitation readout,
||| composing `diagBox`, `offDiagMaxel`, `flattenHMSetBox`, and `<.> (contractRMaxel dFluxT <+> contractLMaxel dFluxT)`.
public export
measureBandAxis :
  GroupCoord k =>
  AxisBasis k ->
  (axisT : PureMultiset) ->
  (tLo, tHi : BoxInt) ->
  (st, bgSt : EvolutionState k) ->
  (initQ, finalQ, maxQ : BoxInt) ->
  (workSteps : Nat) ->
  ExcitationReport
measureBandAxis ax axisT tLo tHi st bgSt initQ finalQ maxQ workSteps =
  let dPol                   = subM st.polarityMaxel bgSt.polarityMaxel
      stepT                  = ax.axisStep axisT
      (bandPol, wX, wT, accQPol) = bandMomentsWithPol ax axisT tLo tHi dPol
      dFluxT                 = filterMaxel (\x, y => y == combine x stepT) (toGeneralMaxel (offDiagMaxel dPol))
      accFlux                = bandPol <.> (contractRMaxel dFluxT <+> contractLMaxel dFluxT)
      mx                     = if accQPol == 0 then MkProportion 0 1 else MkProportion wX accQPol
      mt                     = if accQPol == 0 then MkProportion 0 1 else MkProportion wT accQPol
      net                    = statePolarityNet st + stateFluxNet st
  in MkExcitationReport mx mt accFlux initQ finalQ maxQ net workSteps

||| Bit-for-bit check that `(st - bgSt)` matches `vacSt` identically on every polarity entry
||| inside the hyperbox `[lo..hi]` (confirming zero action-at-a-distance outside the `c = 1` cone).
public export
bandsEqualInBox :
  {n : Nat} ->
  Ord k =>
  AxisBasis k ->
  (lo, hi : Space n) ->
  (st, bgSt, vacSt : EvolutionState k) ->
  Bool
bandsEqualInBox ax lo hi st bgSt vacSt =
  let fltP = filterMaxel (\a, b => inSpaceBox ax lo hi a && inSpaceBox ax lo hi b)
  in fltP (subM st.polarityMaxel bgSt.polarityMaxel) == fltP vacSt.polarityMaxel

||| Quadrupole polarization between axes `axA` and `axB`:
||| `sum_{a} L(a, a + step_A) - sum_{a} L(a, a + step_B)`.
public export
loadQuadrupoleAxes : GroupCoord k => AxisBasis k -> (axA, axB : PureMultiset) -> Maxel BoxInt k k SymForm -> BoxInt
loadQuadrupoleAxes ax axA axB loadM =
  let sA = ax.axisStep axA
      sB = ax.axisStep axB
  in foldStoredMaxel
       (\a, b, w, acc =>
          case canonLinkStart sA a b of
            Just _  => acc + w
            Nothing => case canonLinkStart sB a b of
                         Just _  => acc - w
                         Nothing => acc)
       0 loadM

--------------------------------------------------------------------------------
-- 3. PROJECTIVE PROPORTIONS, DEFLECTION SPREAD & EXTERNAL ROTOR-FRAME READOUTS
--------------------------------------------------------------------------------

||| Exact rational deflection spread between the `+X` beam direction and shifted centroid `(wX, wT - t0 * accQ)`.
public export
measureBandDeflectionSpread :
  GroupCoord k =>
  AxisBasis k ->
  (axisT : PureMultiset) ->
  (tLo, tHi : BoxInt) ->
  (t0 : BoxInt) ->
  (st, bgSt : EvolutionState k) ->
  Proportion
measureBandDeflectionSpread ax axisT tLo tHi t0 st bgSt =
  let (_, wX, wT, accQ) = bandMomentsWithPol ax axisT tLo tHi (subM st.polarityMaxel bgSt.polarityMaxel)
      dW                = wT - t0 * accQ
  in spreadBetween blueMetric (vec2Box 1 0) (vec2Box wX dW)

||| Extracts the 3D quadrance-weighted spatial moment vector `(M0, M1, M2)` over `Fin 3` externally.
public export
observeStateMoment3D : Ord k => AxisBasis k -> EvolutionState k -> Box BoxInt (Fin 3)
observeStateMoment3D ax st =
  let qDiag      = diagBox (quadranceOf st.polarityMaxel)
      momentMSet = flattenHMSetBox (mapBox ax.toSiteMSet qDiag)
  in vec3Box (lookupKeyHMSet (axisAtom 0) momentMSet)
             (lookupKeyHMSet (axisAtom 1) momentMSet)
             (lookupKeyHMSet (axisAtom 2) momentMSet)

||| External rotor-frame observation `actMaxelR (rotorActionMaxel g r)` on the 3D state moment (`st` untouched).
public export
observeRotatedMoment3D :
  Ord k =>
  AxisBasis k ->
  Maxel BoxInt (Fin 3) (Fin 3) SymForm ->
  Rotor ->
  EvolutionState k ->
  Box BoxInt (Fin 3)
observeRotatedMoment3D ax g r st = actMaxelR (rotorActionMaxel g r) (observeStateMoment3D ax st)
