module FinScLens.Measurements

import FinScBox.BoxInt
import FinScBox.Interfaces
import FinScBox.Box
import FinScBox.HereditaryMSet
import FinScDynamics
import FinScLens.Horizons

%default total

--------------------------------------------------------------------------------
-- 1. EXTERNAL MEASUREMENT READOUT RECORDS
--------------------------------------------------------------------------------

||| Trajectory & quadrance readout for a localized excitation evolving under quadratic link exchanges.
public export
record ExcitationReport where
  constructor MkExcitationReport
  meanXMilli      : Integer
  meanYMilli      : Integer
  fluxYAccum      : Integer
  initQuadrance   : Integer
  finalQuadrance  : Integer
  maxQuadrance    : Integer
  polarityNetSum  : Integer
  activeWorkSteps : Nat

public export
Eq ExcitationReport where
  a == b =
    a.meanXMilli == b.meanXMilli &&
    a.meanYMilli == b.meanYMilli &&
    a.fluxYAccum == b.fluxYAccum &&
    a.initQuadrance == b.initQuadrance &&
    a.finalQuadrance == b.finalQuadrance &&
    a.maxQuadrance == b.maxQuadrance &&
    a.polarityNetSum == b.polarityNetSum &&
    a.activeWorkSteps == b.activeWorkSteps

public export
Show ExcitationReport where
  show r =
    "ExcitationReport(meanXMilli=" ++ show r.meanXMilli ++
    ", meanYMilli=" ++ show r.meanYMilli ++
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
  initQuadrance  : Integer
  finalQuadrance : Integer
  maxQuadrance   : Integer
  coreFracPct    : Integer
  netSignedSum   : Integer
  workSteps      : Nat

public export
Eq SelfTrapReport where
  a == b =
    a.initQuadrance == b.initQuadrance &&
    a.finalQuadrance == b.finalQuadrance &&
    a.maxQuadrance == b.maxQuadrance &&
    a.coreFracPct == b.coreFracPct &&
    a.netSignedSum == b.netSignedSum &&
    a.workSteps == b.workSteps

public export
Show SelfTrapReport where
  show r =
    "SelfTrapReport(Q=" ++ show r.initQuadrance ++ "->" ++ show r.finalQuadrance ++
    ", maxQ=" ++ show r.maxQuadrance ++
    ", coreFrac=" ++ show r.coreFracPct ++ "%" ++
    ", net=" ++ show r.netSignedSum ++
    ", work=" ++ show r.workSteps ++ ")"

||| Readout for equal-quadrance 2-body mutual deflection (with zero background dipole).
public export
record TwoBodyReport where
  constructor MkTwoBodyReport
  tbBotYMilli       : Integer
  tbTopYMilli       : Integer
  tbSingleBotYMilli : Integer
  tbBotSy           : Integer
  tbTopSy           : Integer
  tbSingleBotSy     : Integer
  tbLinearBotSy     : Integer
  tbInitQ           : Integer
  tbFinalQ          : Integer
  tbMaxQ            : Integer
  tbNetSum          : Integer

public export
Eq TwoBodyReport where
  a == b =
    a.tbBotYMilli == b.tbBotYMilli &&
    a.tbTopYMilli == b.tbTopYMilli &&
    a.tbSingleBotYMilli == b.tbSingleBotYMilli &&
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
    "TwoBodyReport(dy=" ++ show (r.tbBotYMilli - r.tbSingleBotYMilli) ++
    ", yBot=" ++ show r.tbBotYMilli ++ ", yTop=" ++ show r.tbTopYMilli ++
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
  clEarlyVacX      : Integer
  clEarlyDpX       : Integer
  clEarlyDpSy      : Integer
  clLateVacX       : Integer
  clLateDpX        : Integer
  clLateDpY        : Integer
  clLateDpSy       : Integer

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
  asQuadHoriz    : Integer
  asQuadVert     : Integer
  asProbeHorizX  : Integer
  asProbeHorizY  : Integer
  asProbeHorizSy : Integer
  asProbeVertX   : Integer
  asProbeVertY   : Integer
  asProbeVertSy  : Integer
  asSymLoadOk    : Bool
  asNetSum       : Integer

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
    ", dX=" ++ show (r.asProbeHorizX - r.asProbeVertX) ++
    ", sym=" ++ show r.asSymLoadOk ++
    ", net=" ++ show r.asNetSum ++ ")"

--------------------------------------------------------------------------------
-- 2. EXTERNAL SPATIAL MOMENTS, CORE QUADRANCE & LOAD QUADRUPOLE REDUCTIONS
--------------------------------------------------------------------------------

public export
isCoreSite : AxisBasis k -> k -> Bool
isCoreSite ax pos =
  let x = coordAlong ax (axisNat 0) pos
      y = coordAlong ax (axisNat 1) pos
  in x >= (-1) && x <= 1 && y >= (-1) && y <= 2

public export
canonAnyLinkStart : Ord k => AxisBasis k -> Maxel BoxInt k k DiagAntisymForm -> k -> k -> Maybe k
canonAnyLinkStart ax pol a b =
  findBox (\axKey, _ => canonLinkStart ax (ax.axisStep axKey) a b)
          (activeAxisAtomsOf ax pol)

||| Percentage of `quadranceOf st.polarityMaxel` localized inside `[-1..1] x [-1..2]`.
public export
fieldCoreQuadranceFrac : Ord k => AxisBasis k -> EvolutionState k -> Integer
fieldCoreQuadranceFrac ax st =
  let qM   = quadranceOf st.polarityMaxel
      totQ = unwrapBox (sumStoredMaxel qM)
      cM   = filterMaxel (\a, b =>
                           if a == b
                             then isCoreSite ax a
                             else case canonAnyLinkStart ax st.polarityMaxel a b of
                                    Just s  => isCoreSite ax s
                                    Nothing => False)
                         qM
      cTot = unwrapBox (sumStoredMaxel cM)
  in if totQ <= 0 then 0 else divIntegerPos (cTot * 100) totQ

public export
extractBandPolarityAndFluxAxis :
  Ord k =>
  AxisBasis k ->
  (axisT : PureMultiset) ->
  (tLo, tHi : Integer) ->
  Maxel BoxInt k k DiagAntisymForm ->
  (Box BoxInt k, Box BoxInt (k, k))
extractBandPolarityAndFluxAxis ax axisT tLo tHi pol =
  let stepT = ax.axisStep axisT
  in foldStoredMaxel
       (\a, b, w, (bp, df) =>
          if a == b
            then let t = coordAlong ax axisT a
                 in if t >= tLo && t <= tHi then (setBox a w bp, df) else (bp, df)
            else if b == ax.shiftAdd a stepT
              then (bp, setBox (a, b) w df)
              else if a == ax.shiftAdd b stepT
                then (bp, setBox (b, a) (negWeight w) df)
                else (bp, df))
       (BoxLeaf, BoxLeaf) pol

||| Transverse-axis (`axisT`, `tLo <= t <= tHi`) background-subtracted excitation readout,
||| extracting spatial moments via `flattenHMSetBox (mapBox ax.toSiteMSet qpBox)`
||| and transverse flux correlation via `bandPol <.> (contractRBox dFluxT <+> contractLBox dFluxT)`.
public export
measureBandAxis :
  Ord k =>
  AxisBasis k ->
  (axisT : PureMultiset) ->
  (tLo, tHi : Integer) ->
  (st, bgSt : EvolutionState k) ->
  (initQ, finalQ, maxQ : Integer) ->
  (workSteps : Nat) ->
  ExcitationReport
measureBandAxis ax axisT tLo tHi st bgSt initQ finalQ maxQ workSteps =
  let dPol              = subMaxel st.polarityMaxel bgSt.polarityMaxel
      (bandPol, dFluxT) = extractBandPolarityAndFluxAxis ax axisT tLo tHi dPol
      qpBox             = bandPol .*. bandPol
      accQPol           = unwrapBox (totalMassBox qpBox)
      momentMSet        = flattenHMSetBox (mapBox ax.toSiteMSet qpBox)
      wX                = lookupKeyHMSet (axisNat 0) momentMSet
      wT                = lookupKeyHMSet axisT momentMSet
      accFlux           = unwrapBox (bandPol <.> (contractRBox dFluxT <+> contractLBox dFluxT))
      mxM               = if accQPol == 0 then 0 else unwrapBox (tdivPosBox (wX * 1000) accQPol)
      mtM               = if accQPol == 0 then 0 else unwrapBox (tdivPosBox (wT * 1000) accQPol)
      net               = statePolarityNet st + stateFluxNet st
  in MkExcitationReport mxM mtM accFlux initQ finalQ maxQ net workSteps

public export
inBoxRange : AxisBasis k -> (xLo, xHi, yLo, yHi : Integer) -> k -> Bool
inBoxRange ax xLo xHi yLo yHi pos =
  let x = coordAlong ax (axisNat 0) pos
      y = coordAlong ax (axisNat 1) pos
  in x >= xLo && x <= xHi && y >= yLo && y <= yHi

||| Bit-for-bit check that `(st - bgSt)` matches `vacSt` identically on every polarity entry
||| inside `[xLo..xHi] x [yLo..yHi]` (confirming zero action-at-a-distance outside the `c = 1` cone).
public export
bandsEqualInBox :
  Ord k =>
  AxisBasis k ->
  (xLo, xHi, yLo, yHi : Integer) ->
  (st, bgSt, vacSt : EvolutionState k) ->
  Bool
bandsEqualInBox ax xLo xHi yLo yHi st bgSt vacSt =
  let fltP = filterMaxel (\a, b => inBoxRange ax xLo xHi yLo yHi a &&
                                   inBoxRange ax xLo xHi yLo yHi b)
  in fltP (subMaxel st.polarityMaxel bgSt.polarityMaxel) == fltP vacSt.polarityMaxel

||| Plus-mode quadrupole polarization of `loadM`:
||| `sum_{a} L(a, a + step_X) - sum_{a} L(a, a + step_Y)`.
public export
loadQuadrupolePlus : Ord k => AxisBasis k -> Maxel BoxInt k k SymForm -> Integer
loadQuadrupolePlus ax loadM =
  let sX = ax.axisStep (axisNat 0)
      sY = ax.axisStep (axisNat 1)
  in foldStoredMaxel
       (\a, b, (MkBoxInt w), acc =>
          case canonLinkStart ax sX a b of
            Just _  => acc + w
            Nothing => case canonLinkStart ax sY a b of
                         Just _  => acc - w
                         Nothing => acc)
       0 loadM
