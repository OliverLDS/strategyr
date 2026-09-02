## R CMD check results

### Local release R

* Version: R 4.2.3
* Platform: x86_64-apple-darwin17.0
* `R CMD check --no-manual`: OK
* `R CMD check --as-cran`: package checks completed; final PDF-manual check
  requires `pdflatex`, which is not installed on this machine. Incoming URL
  checks could not run because the sandbox has no DNS access.

### R-devel

* R-hub GitHub Actions checks: passed on Linux, Windows, and macOS
* Workflow: https://github.com/OliverLDS/strategyr/actions/runs/32561102017

### Win-builder

* R-devel: passed with one expected NOTE: new submission
* Log: https://win-builder.r-project.org/j47ll1eFOZn4

### R-hub

* Linux R-devel: passed
* Windows R-devel: passed
* macOS R-devel: passed
* Workflow: https://github.com/OliverLDS/strategyr/actions/runs/32561102017

## Notes

The package has no external runtime or network requirement in its installed
code, examples, or tests. ML and PPO integrations are intentionally deferred
to separate extensions.
