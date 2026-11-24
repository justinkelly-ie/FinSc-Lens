# FinSc-Lens

**Non-perturbing external horizons, initial probe configurations, quenched `TopologicalRelic` skeletons, and readout audits over [`Finite-Science`](https://github.com/justinkelly-ie/Finite-Science) and [`FinSc-Box`](https://github.com/justinkelly-ie/FinSc-Box).**

`FinSc-Lens` implements **Layer 3 (Observation & Readouts)** of the three-layer architecture:

1. **`FinSc-Box` (Core Ontology):** `HereditaryMSet`, `[⬛, 🟥] = []`, `Maxel`, `GohFuel`, `QuadraticExchange`, `Views`, `BlackRedZoom`, and `Lenses`.
2. **`Finite-Science` (Blind Local Dynamics):** `Configuration`, `EvolutionState`, `PolarityMaxel`, `QuadranceMaxel`, `loadMaxel`, `throttleMaxel`, `FlatBlackBox`, and `HereditaryBlackBox` — evolving locally with zero observer knowledge.
3. **`FinSc-Lens` (Observation & Readouts):**
   - [`Horizons.idr`](src/FinScLens/Horizons.idr) — Observer `Time` / UV cutoffs (`HorizonSpec`, `coarseHorizonSpec`, `deepUVHorizonSpec`), initial probe configurations (`photonMSet`, `protonMSet`, `neutronMSet`, `heavyMassMSet`), quenched `TopologicalRelic` (`quenchRelic = MSetSupport`), and `Control.App` boundary guards.
   - [`Measurements.idr`](src/FinScLens/Measurements.idr) — External spatial moment, band-flux, core-quadrance, and load-quadrupole readouts (`ExcitationReport`, `SelfTrapReport`, `TwoBodyReport`, `CausalLightConeReport`, `AnisotropicStressReport`) plus all 6 automated audits (`runAudit1..6`).

*(Rational `SO(3; Q)` projective rotation proportions and `SL(2; Q)` Dihedron frames live separately in `FinSc-Dihedron` for relativistic observer development.)*

---

## Building & Running the Audit Suite

Ensure both `FinSc-Box` and `Finite-Science` are built and installed first:

```bash
cd FinSc-Box && idris2 --build FinSc-Box.ipkg && idris2 --install FinSc-Box.ipkg
cd ../Finite-Science && idris2 --build Finite-Science.ipkg && idris2 --install Finite-Science.ipkg
cd ../FinSc-Lens
idris2 --build FinSc-Lens.ipkg
./build/exec/finsc-lens-audits
```

A passing run prints all 6 audit reports, writes `audit_trace.txt` (git-ignored), and concludes with:

```text
ALL 6 FINSC-LENS AUDITS PASSED (Blind Dynamics + External Lenses + TopologicalRelic)
Boundary telemetry written to audit_trace.txt: True
```

---

## License

Released under the MIT License.
