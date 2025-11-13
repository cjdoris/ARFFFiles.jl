# Changelog

## 1.5.2 (2025-11-13)
* Bug fixes.

## 1.5.1 (2024-10-24)
* Fix bug saving `NaN`.

## 1.5.0 (2024-10-24)
* Can now load files with relational fields.
* Fix bug loading bare strings with leading/trailing whitespace.

## 1.4.1 (2021-11-22)
* Fix bugs handling zeros in sparse format.

## 1.4.0 (2021-10-13)
* `missingcols = :auto` argument to automatically narrow the eltype of columns not
  containing missing values.

## 1.3.0 (2021-08-05)
* Can now load files in sparse format.
* Nominal columns are stored as `CategoricalVector`.
* Loading is now much faster thanks to Parsers.jl.

## 1.2.0 (2021-07-27)
* Reading now supports UTF-8.

## 1.1.0 (2021-05-12)
* Add `save()`.

## 1.0.0 (2021-05-11)
* Initial release.
