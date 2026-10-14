#' shieldR: Simulation of Evacuation Policies under Uncertainty
#'
#' A academic research framework for reproducible policy evaluation in a fixed
#' five-leg evacuation chain. Scenario inputs are explicit; the included
#' examples have neutral physiological and disruption_risk settings.
#'
#' @section Start here:
#' Create a teaching scenario with [example_scenario()] or validate your own
#' inputs with [shield_parameters()]. Run independent cohort replications using
#' [simulate_shield()] and inspect median/IQR summaries with [summarize_shield()].
#' The optional [shield_app()] workbench provides interactive configuration,
#' simulation, inspection and export through the same package functions.
#'
#' @section Design and interpretation:
#' One row is one cohort replication. The primary endpoint `surv_stage2` is the
#' proportion of the original cohort arriving alive at Stage 2 by the
#' administrative horizon. Physical arrival is recorded before unloading and
#' stops physiological hazard. Live nonarrivals are endpoint failures.
#' Arrival-time medians are conditional on arrival and may be missing.
#'
#' [compare_policies()] applies a Mann-Whitney U comparison to independent
#' cohorts and reports Cliff's delta and percentile bootstrap intervals.
#' [compare_multiple()] reports a Kruskal-Wallis omnibus comparison.
#' [compare_intervention()] estimates a paired disruptions-disabled contrast.
#' [adjust_fdr()] applies Benjamini-Hochberg adjustment to one explicitly
#' supplied family. Bootstrap uncertainty is conditional on the model.
#'
#' @section Reproducibility:
#' Simulation and bootstrap functions preserve the caller's RNG kind and state.
#' Seed 20260512 is the reproducible master default. Use distinct, recorded seeds
#' for independent arms and matching replication schedules for paired
#' interventions. Preserve full run objects as RDS to retain their parameters
#' and software attributes; record `sessionInfo()` in the analysis output.
#'
#' @section Scope:
#' The coupled P3 policy is a heuristic with known disruption_model-model assumptions;
#' the package does not solve an equilibrium. Internal verification and replay
#' do not establish clinical or operational validity. Controlled study inputs,
#' scenario grids and publication products remain in the originating project.
#' The package and its documentation remain private pending release decisions.
#'
#' @seealso [example_scenario()], [shield_parameters()], [simulate_shield()],
#'   [compare_policies()], [compare_intervention()], [shield_app()]
#' @keywords internal
"_PACKAGE"
