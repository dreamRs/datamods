# datamods 1.1.5

* `import_*_server()` added reset argument to clear the data.
* `import_copypaste_server()` also return a `reactive` function "name" like the others.
* New function `i18n()` to add internationalization in shiny apps.


# datamods 1.1.4

* `filter_data_server`: convert data to `data.frame` (fix [esquisse #149](https://github.com/dreamRs/esquisse/issues/149)).
* `filter_data_server`: fixed bug with timezone if POSIXct.
* Import data from package: use `pkg::data` notation for data's name.


# datamods 1.1.3

* Preserve class `sf` in output.


# datamods 1.1.2

* Fixed a bug when retrieving data from package with parenthesis in name.
* Fixed test on R-oldrel


# datamods 1.1.0

* Added internationalization to translate labels used in modules, see corresponding vignette.


# datamods 1.0.1

* First release on CRAN: Shiny modules import, to update, validate and filter data in interactive applications
* Added a `NEWS.md` file to track changes to the package.
