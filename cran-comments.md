## R CMD check results

### Local release R

* Version: R 4.2.3
* Platform: x86_64-apple-darwin17.0
* `R CMD check --no-manual`: OK
* `R CMD check --as-cran`: package checks completed; final PDF-manual check
  requires `pdflatex`, which is not installed on this machine. Incoming URL
  checks could not run because the sandbox has no DNS access.

### R-devel

* Platform: pending
* `R CMD check --as-cran`: pending

### Win-builder

* R-devel: pending

### R-hub

* Linux: pending
* Windows: pending
* macOS: pending

## Notes

The package has no external runtime or network requirement in its installed
code, examples, or tests. ML and PPO integrations are intentionally deferred
to separate extensions.
