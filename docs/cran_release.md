# CRAN Release Checklist

Use this checklist only after the package version, NEWS entry, and public API
are final.

## Local Gate

1. Install the current R release and current R-devel.
2. Regenerate documentation with the repository-pinned roxygen version.
3. Build the source package with `R CMD build .`.
4. Run `R CMD check --as-cran strategyr_<version>.tar.gz` on R-devel.
5. Resolve every warning and significant note. Do not suppress checks merely
   to obtain a clean result.

## Cross-Platform Gate

1. Submit the source tarball to Win-builder using R-devel.
2. Run R-hub checks for Linux, Windows, macOS, and R-devel.
3. Check all URLs in DESCRIPTION, README, Rd files, and NEWS.
4. Confirm the source tarball has no generated binaries, large data, or local
   operating-system artifacts.

## Submission Gate

1. Confirm the package name is available on CRAN and Bioconductor.
2. Confirm copyright and licensing for all source files and derived code.
3. Record local, Win-builder, and R-hub results in `cran-comments.md`; that
   file is intentionally excluded from the source tarball.
4. Submit the `R CMD build` source tarball through the CRAN submission form
   and confirm the maintainer email.
5. Do not resubmit while the current upload is pending review.
