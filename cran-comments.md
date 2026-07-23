## Resubmission

This is a resubmission. In this version we have:

* Appropriate citation of our methodology is now included in the DESCRIPTION file.

* Added @returns to the roxygen2 comments of merge_entries, and re-generated the .Rd documentation with the \value{} attribute filled.

* Changed error handling in DHScale (in DHScale.R) and Enzyme (enzyme.R) so that when an error is thrown due to multiple matches, the relevant data is encapsulated in an errorCondition object instead of being directly printed out.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

  The package has been tested on Windows, MacOS, and Linux, both locally and via Github Action, and all checks return with no errors, warnings, or notes.
