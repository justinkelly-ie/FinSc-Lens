# FinSc-Lens

**Pure installable Layer-3 observer library providing non-perturbing horizons, initial probe configurations, quenched `TopologicalRelic` skeletons, and readout reductions over [`FinSc-Dynamics`](https://github.com/justinkelly-ie/FinSc-Dynamics) and [`FinSc-Box`](https://github.com/justinkelly-ie/FinSc-Box).**

`FinSc-Lens` sits at **Layer 3** alongside peer observer libraries (such as `FinSc-Dihedron`), consumed at the top of the stack by [`Finite-Science`](https://github.com/justinkelly-ie/Finite-Science):

```text
                FinSc-Box               (Layer 1: Core Multiset & Maxel Algebra)
                    │
                    ▼
             FinSc-Dynamics             (Layer 2: Blind Local Evolution — App1, NoThrow)
               ╱         ╲
              ▼           ▼
        FinSc-Lens     FinSc-Dihedron   (Layer 3: Independent Non-Perturbing Observers)
              ╲           ╱
               ▼         ▼
             Finite-Science             (Layer 4: Top-of-Stack Lab, Audits & Visualizer)
```

## Modules

- [`Horizons.idr`](src/FinScLens/Horizons.idr) — Observer `Time` / UV cutoffs (`HorizonSpec`, `mkPolyBoundHorizonSpec`, `observeHorizonFreeze`), `CloseOutcome` classification & `rootActivePeelWork`, initial probe configurations (`protonMSet`, `neutronMSet`, `heavyMassMSet`), quenched `TopologicalRelic` (`castSupportHMSet`), Goh spread stream freeze-out, and `Dec` boundary validators.
- [`Measurements.idr`](src/FinScLens/Measurements.idr) — Readout records (`ExcitationReport`, `SelfTrapReport`, `TwoBodyReport`, `CausalLightConeReport`, `AnisotropicStressReport`) and external spatial moment, band-flux, core-quadrance, and load-quadrupole reductions (`measureBandAxis`, `fieldCoreQuadranceFrac`, `bandsEqualInBox`, `loadQuadrupoleAxes`).

## Building & Installing

```bash
cd FinSc-Box && idris2 --build FinSc-Box.ipkg && idris2 --install FinSc-Box.ipkg
cd ../FinSc-Dynamics && idris2 --build FinSc-Dynamics.ipkg && idris2 --install FinSc-Dynamics.ipkg
cd ../FinSc-Lens && idris2 --build FinSc-Lens.ipkg && idris2 --install FinSc-Lens.ipkg
```

## License

Released under the MIT License.
