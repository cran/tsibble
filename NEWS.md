# tsibble 0.1.2

This release marks the complete support of dplyr key verbs.

## Reexported functions

* `tidyr::fill()` fills `NA` backward or forward in tsibble.
* Implement `tbl_ts` support for `dplyr::*_join()`.
* No `tbl_ts` support for `dplyr::transmute()` and `dplyr::distinct()` and return an error. 

## New functions

* `inform_duplicates()` informs which row has duplicated elements of key and index variables.

## Bug fixes

* Fix bug in `summarise.tbl_ts()` and `tsummarise.tbl_ts()`, when calling functions with no parameters like `dplyr::n()`.
* In `summarise.tbl_ts()` and `tsummarise.tbl_ts()`, one grouping level should be dropped for the consistency with `dplyr::summarise()` for a grouped `tbl_ts`.
* Fix incorrect group and key indices.
* `NULL` and `tbl_ts` are supported in `as_tsibble()`. An empty tsibble is not allowed.
* `group_by.tbl_ts(.data, ..., add = TRUE)` works as expected now.

## Internal changes

* Better handling `grouped_ts` and `grouped_df`.
* More informative error messages.

# tsibble 0.1.0

* Initial release on CRAN.

# tsibble 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Pre-release on Github


