# shieldR 0.6.0

- Reframe the public model as abstract logistics-chain simulation with changing item specifications.
- Rename condition-model functions, observation fields, viability endpoints and workbench labels consistently.
- Preserve the simulation equations and random-draw schedules; this is a terminology and interface revision, not a newly calibrated model.
- Earlier serialized objects and scripts require explicit migration to the renamed interface.

# shieldR 0.5.0

* Introduce the public MIT-licensed academic edition with abstract network stages
  and stochastic disruption terminology. Public identifiers use `stage` and
  `disruption`; earlier analysis environments require explicit migration.

# shieldR 0.4.0

* Add an opt-in degradation-trauma item model with actual servicing events,
  finite procedural capacity and overall viability at 30 days from damage.
* Add aggregate calibration and independent stochastic admission-batch replay.
* Preserve legacy execution by default and support separately pinned item
  comparisons with frozen planning assumptions and observed information.

# shieldR 0.3.0

- Add matched initial-observation and updated-observation policy controls.
- Separate reactive current-risk planning from the P3 response forecast while
  retaining the same decision architecture.
- Preserve uncontrolled simulation behavior and document the servicing-free
  endpoint and separate-experiment requirement.

# shieldR 0.2.0

- Added explicit replication-seed and bootstrap primitives for external study
  schedulers while preserving caller RNG state and original numerical methods.
- Added a local Shiny scenario workbench with independent and paired designs,
  diagnostics, validated graph editing, provenance and replayable exports.
- PDF exports recover when Cairo is advertised but its runtime cannot load;
  the portable fallback preserves existing graphics devices.
- Expanded the README, reference help and three executable tutorials.
- Added a reciprocal analysis/package synchronization contract and local gate.
- Expanded package checks to Linux, macOS, Windows and three R release tracks;
  documentation and coverage artifacts remain within private CI.

# shieldR 0.1.0

- Initial private package extraction of the five-leg SHIELD simulation.
- Explicit validated scenario inputs and two neutral teaching examples.
- Reproducible batch simulation with preserved caller RNG state.
- Independent-arm, multi-arm and paired-intervention comparison functions.
- Documented endpoint and information-timing assumptions.
- Stable log-ratio forecasts for extreme response parameters outside the
  original study range, with unchanged arithmetic within that range.
