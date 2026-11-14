## Submission

This is a new submission of shieldR 0.7.0.

## Test environments

* local: macOS (aarch64-apple-darwin), R 4.6.1, full `R CMD check --as-cran`
  on the built tarball, including the PDF manual with TinyTeX and all Suggests.
  The 2026-11-14 candidate passed 456 test assertions with no test skips.
* Configured GitHub Actions matrix (r-lib/actions v2, `--as-cran`);
  the exact-commit results must be verified before submission:
  macos-latest (release), windows-latest (release),
  ubuntu-latest (devel, release, oldrel-1).

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'R. Heller <raban.heller@uni-ulm.de>'
  New submission

  This is the first submission of the package.

## Notes for the CRAN team

* All examples run in a few seconds. `shield_app()` starts a 'shiny'
  application and is therefore only run when `interactive()` is `TRUE`.
* Simulation and bootstrap functions restore the caller's RNG kind and state.
