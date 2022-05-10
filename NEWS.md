# nbc4va 1.2

* Remove `stringsAsFactors = FALSE` as it is now the default for R v4.0.0+
* Set R version to 4.0.0 or higher
* Fixed broken urls, missing tags, and un-exported functions in documentation
* Replace http urls with https urls
* Fix error/warning "In xtfrm.data.frame(x) : cannot xtfrm data frames"
* Function `plot.nbc` now restores user settings even if it fails
* Package `shiny` is now a required dependency and is not installed on usage of `nbc4vaGUI`

# nbc4va 1.1

* Added a `NEWS.md` file to track changes to the package
* Fixed if() conditions for lengths greater than 1
* Moved help documentation to vignettes and github pages
* Added github repository
* Added authors and contributors to `DESCRIPTION` file

# nbc4va 1.0

* Initial release
