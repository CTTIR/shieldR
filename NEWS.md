# shieldR 0.7.0

* Fix the coupled P3 policy of the item-condition extension for a zero
  softmax parameter (`beta = 0`). Excluded route choices produced `NaN`
  weights and stopped the simulation; they now receive zero weight, so the
  policy draws uniformly among the admissible choices. Results for positive
  `beta` are unchanged.
* Label the workbench departure-history window in departures and document
  `atk_window` in `shield_parameters()` as a count of recent departures. The
  workbench previously described it in minutes.
* Report clearer input errors in `with_item_model()` and
  `calibrate_item_model()`. `calibrate_item_model()` now ignores additional
  named entries in `targets` and rejects non-numeric calibration batches;
  `adjust_fdr()` accepts an all-missing logical vector.
* Add tests for `calibrate_item_model()` and `simulate_item_reference()`.
* Cite Cliff (1993), Benjamini and Hochberg (1995) and the Romano et al.
  (2006) magnitude thresholds in the help pages of `compare_policies()`,
  `adjust_fdr()` and `effect_magnitude()`.
* Correct wording in help pages, tutorials and the workbench, give the
  item-condition functions their own reference section, and drop the unused
  `digest` suggestion.
* Prevent integer overflow in large-sample Mann-Whitney variance and Cliff's
  delta calculations. Comparisons with more than 2^31 - 1 cross-arm pairs
  now retain their effect size and log-tail probability.

* Prepare the package for CRAN: every exported function now has a runnable
  example, the `Description` mentions the optional 'shiny' workbench, the
  maintainer ORCID and `Language` are declared, and `cran-comments.md` is added.
* Use British English (`Language: en-GB`) consistently in help pages,
  tutorials, NEWS and workbench text, and fix the parameter label
  "destructiveness" (previously misspelled) for `q` in `shield_parameters()`
  and the workbench. Function, argument and column names are unchanged.
* Run the GitHub Actions R CMD check with `--as-cran`.

# shieldR 0.6.0

* Standardise the named v5 package logo across README, documentation pages
  and available application headers, with matching light and dark artwork.


- Reframe the public model as abstract logistics-chain simulation with changing item specifications.
- Rename condition-model functions, observation fields, viability endpoints and workbench labels consistently.
- Preserve the simulation equations and random-draw schedules; this is a terminology and interface revision, not a newly calibrated model.
- Earlier serialised objects and scripts require explicit migration to the renamed interface.

# shieldR 0.5.0

* Introduce the public MIT-licensed academic edition with abstract network stages
  and stochastic disruption terminology. Public identifiers use `stage` and
  `disruption`; earlier analysis environments require explicit migration.

# shieldR 0.4.0

* Add an opt-in degradation item model with actual servicing events,
  finite procedural capacity and overall viability at 30 days from item onset.
* Add aggregate calibration and independent stochastic admission-batch replay.
* Preserve legacy execution by default and support separately pinned item
  comparisons with frozen planning assumptions and observed information.

# shieldR 0.3.0

- Add matched initial-observation and updated-observation policy controls.
- Separate reactive current-risk planning from the P3 response forecast while
  retaining the same decision architecture.
- Preserve uncontrolled simulation behaviour and document the servicing-free
  endpoint and separate-experiment requirement.

# shieldR 0.2.0

- Added explicit replication-seed and bootstrap primitives for external study
  schedulers while preserving caller RNG state and original numerical methods.
- Added a local Shiny scenario workbench with independent and paired designs,
  diagnostics, validated graph editing, provenance and replayable exports.
- PDF exports recover when Cairo is advertised but its runtime cannot load;
  the portable fallback preserves existing graphics devices.
- Expanded the README, reference help and three executable tutorials.
- Added a reciprocal analysis/package synchronisation contract and local gate.
- Expanded package checks to Linux, macOS, Windows and three R release tracks;
  documentation and coverage artefacts remain within private CI.

# shieldR 0.1.0

- Initial package extraction of the five-leg simulation.
- Explicit validated scenario inputs and two neutral teaching examples.
- Reproducible batch simulation with preserved caller RNG state.
- Independent-arm, multi-arm and paired-intervention comparison functions.
- Documented endpoint and information-timing assumptions.
- Stable log-ratio forecasts for extreme response parameters outside the
  original study range, with unchanged arithmetic within that range.
