# FinSc-Lens

**Pure installable Layer-3 observer library providing non-perturbing horizons, initial probe configurations, quenched `TopologicalRelic` skeletons, and projective/frame readout reductions over [`FinSc-Space`](FinSc-Space) and [`FinSc-Box`](FinSc-Box).**

`FinSc-Lens` sits at **Layer 3**, strictly between `FinSc-Space` and [`Finite-Science`](Finite-Science):

```text
FinSc-Box (Layer 1: Pure Box, HereditaryMSet, Maxel, Space n, GohBound, QuadraticTriple)
    │
    ▼
FinSc-Space (Layer 2: Geometry.{Affine, Projective} + Dynamics)
    │
    ▼
FinSc-Lens (Layer 3: Non-Perturbing Horizons, Projective & Frame Readouts)
    │
    ▼
Finite-Science (Layer 4: Top-of-Stack Lab & Audits)
```

## Modules

- [`Horizons.idr`](src/FinScLens/Horizons.idr) — Observer `Time` / UV cutoffs (`HorizonSpec`, `mkPolyBoundHorizonSpec`, `observeHorizonFreeze`), `CloseOutcome` classification & `rootActivePeelWork`, quenched `TopologicalRelic` (`castSupportHMSet`), Goh spread stream freeze-out, and `Dec` boundary validators.
- [`Measurements.idr`](src/FinScLens/Measurements.idr) — Readout records (`ExcitationReport`, `SelfTrapReport`, `TwoBodyReport`, `CausalLightConeReport`, `AnisotropicStressReport`), spatial moment/flux/quadrupole reductions (`measureBandAxis`, `fieldCoreQuadranceFrac`, `bandsEqualInBox`, `loadQuadrupoleAxes`), and division-free projective/rotor-frame readouts (`measureBandDeflectionSpread`, `observeStateMoment3D`, `observeRotatedMoment3D`).

## Building & Installing

```bash
cd FinSc-Box && idris2 --build FinSc-Box.ipkg && idris2 --install FinSc-Box.ipkg
cd ../FinSc-Space && idris2 --build FinSc-Space.ipkg && idris2 --install FinSc-Space.ipkg
cd ../FinSc-Lens && idris2 --build FinSc-Lens.ipkg && idris2 --install FinSc-Lens.ipkg
```

## License

Released under the MIT License.
