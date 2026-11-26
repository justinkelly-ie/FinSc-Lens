# FinSc-Lens

**Pure installable Layer-3 observer library providing non-perturbing horizons, quenched `TopologicalRelic` skeletons, projective/frame readout reductions, and the polymorphic `BlackBoxLab` harness over [`FinSc-Space`](../FinSc-Space) and [`FinSc-Box`](../FinSc-Box).**

`FinSc-Lens` sits at **Layer 3**, strictly between `FinSc-Space` and [`Finite-Science`](../Finite-Science):

```text
FinSc-Box (Layer 1: Pure Box, HereditaryMSet, Maxel, Space n, GohBound, QuadraticTriple)
    │
    ▼
FinSc-Space (Layer 2: Geometry.{Affine, Projective} + Dynamics)
    │
    ▼
FinSc-Lens (Layer 3: Non-Perturbing Horizons, Projective & Frame Readouts, BlackBoxLab)
    │
    ▼
Finite-Science (Layer 4: Top-of-Stack Application & Interface Shell)
```

## Modules

- [`Horizons.idr`](src/FinScLens/Horizons.idr) — Observer `Time` / UV cutoffs (`HorizonSpec`, `mkPolyBoundHorizonSpec`, `observeHorizonFreeze`), `CloseOutcome` classification & `rootActivePeelWork`, quenched `TopologicalRelic` (`castSupportHMSet`), Goh spread stream freeze-out, and `Dec`/`Subset` boundary validators.
- [`Measurements.idr`](src/FinScLens/Measurements.idr) — Readout records (`ExcitationReport`, `SelfTrapReport`, `TwoBodyReport`, `CausalLightConeReport`, `AnisotropicStressReport`, `ReggeVertexReport`), spatial moment/flux/quadrupole reductions (`measureBandAxis`, `fieldCoreQuadranceShare`, `bandsEqualInBox`, `loadQuadrupoleAxes`), division-free projective/rotor-frame readouts (`measureBandDeflectionSpread`, `observeStateMoment3D`, `observeRotatedMoment3D`), and emergent dynamical Regge vertex curvature (`opticalEdgeQuadrance`, `reggeStar2D`, `reggeStar3D`, `observeReggeVertexStar`).
- [`Lab.idr`](src/FinScLens/Lab.idr) — Polymorphic multi-observer harness (`BlackBoxLab`, `StepTel`, `EndAudit`, `isValidStepTel`, `isValidEndAudit`, `flatLab`, `hereditaryLab`, `bootInjectAndRun`, `driveLabStepsTel`) comparing `FlatBlackBox (Space n)` and `HereditaryBlackBox` (`PureMultiset`) over strict `App1` linearity.

## Building & Installing

```bash
cd FinSc-Box && idris2 --build FinSc-Box.ipkg && idris2 --install FinSc-Box.ipkg
cd ../FinSc-Space && idris2 --build FinSc-Space.ipkg && idris2 --install FinSc-Space.ipkg
cd ../FinSc-Lens && idris2 --build FinSc-Lens.ipkg && idris2 --install FinSc-Lens.ipkg
```

## License

Released under the MIT License.
