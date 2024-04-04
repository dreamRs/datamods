# datamods 1.4.6

* New module `create_column_ui()` / `create_column_server()` to add new column based on an expression to a `data.frame`.
* New module `cut_variable_ui()` / `cut_variable_server()` to cut a numeric factor into several interval.


# datamods 1.4.5

* `edit_data_server()` : fixed default variable labels when `var_labels = NULL`.


# datamods 1.4.4

* `edit_data_server()` : added argument `add_default_values = list(...)` to specify default value for input widget when adding a new entry in the table.


# datamods 1.4.3

* `edit_data_server()` : added the ability to specify callbacks functions to be executed before performing an action on the table (add, update or delete).
* `edit_data_server()` : pass reactable option + selection to the table [#82](https://github.com/dreamRs/datamods/pull/82)
* `edit-data` module : use factor levels and sort theme in edit input form for factors (sorting also applies for characters), thanks to [@Felixmil](https://github.com/Felixmil).
* `import-file` module : allow to specify string used to identify `NA`, thanks to [@DrFabach](https://github.com/DrFabach).
* `filter_data_server()` : argument `drop_ids` can now be set via option `datamods.filter.drop_ids` and can be a list like `list(p = 0.9, n = 50)` to specify threshold values to remove IDs columns.


# datamods 1.4.2

* i18n: japanese translations added, thanks to [@nissinbo](https://github.com/nissinbo).
* `select_group_server()` : output value now have an `inputs` attribute with a named list of selected inputs values.


# datamods 1.4.1

* i18n: polish translations added, thanks to [@jakub-jedrusiak](https://github.com/jakub-jedrusiak).

### Bug fixes
* Fixed displaying variable class in View tab (fix [#64](https://github.com/dreamRs/datamods/issues/64)).
* `select_group_server()` : fix update inputs when `multiple = FALSE`.
* `filter_data_server()` : sorting choices in select menus (select, picker and virtual) (fix [#66](https://github.com/dreamRs/datamods/issues/64))).
* `filter_data_server()` : don't use `<`/`>` for empty field to not confuse to an HTML tag (fix [#65](https://github.com/dreamRs/datamods/issues/65))).


# datamods 1.4.0

* New module : `edit_data_ui()` / `edit_data_server()` to interactively edit a `data.frame`, thanks to [@ggsamra](https://github.com/ggsamra).
* New module : `sample_ui()` / `sample_server()` to take a sample from a table, thanks to [@ggsamra](https://github.com/ggsamra).



# datamods 1.3.4

* i18n: korean translations added, thanks to [@ChangwooLim](https://github.com/ChangwooLim) (migrated from esquisse package).
* `import_ui()` / `import_modal()`: added `file_extensions` argument passed to `import_file_ui()` (fix [#51](https://github.com/dreamRs/datamods/issues/51)).



# datamods 1.3.3

* i18n: turkish translations added, thanks to [@sbalci](https://github.com/sbalci).
* `filter_data` module now support getting and setting filter values, thanks to [@bellma-lilly](https://github.com/bellma-lilly).



# datamods 1.3.2

* Fix bad link in NEWS.



# datamods 1.3.1

* Fixed a bug in `update_variables` module.



# datamods 1.3.0

* New module to read flat data from URLs `import_url_*()`.
* Error messages displayed to the user are more informative on the actual error.
* `filter_data_server()`: new argument `value_na` to set default value for NA's filters widgets.
* `import_copypaste_ui()`: new argument `name_field` to show or not name field.
* `import_copypaste_server()`: new argument `fread_args` to pass arguments to `data.table::fread`.
* i18n: chinese translations added, thanks to [@xmusphlkg](https://github.com/xmusphlkg).
* i18n: spanish translations added, thanks to [@dnldelarosa](https://github.com/dnldelarosa).
* i18n: german translations added, thanks to [@SteEcker](https://github.com/SteEcker) and joerghenkebuero.



# datamods 1.2.0

* Switch to [{phosphoricons}](https://github.com/dreamRs/phosphoricons) for icons.
* `import_file_ui()` has a new argument `file_extensions` to select the files that the user can import.
* `import_file_server()` has a new argument `read_fns` to define custom function(s) to read data.

### Translations
* i18n: :macedonia: macedonian translations added, thanks to [@novica](https://github.com/novica).
* i18n: :albania: albanian translations added, thanks to [@novica](https://github.com/novica).
* i18n: :portugal: :brazil: brazilian portuguese translations added, thanks to [@gabrielteotonio](https://github.com/gabrielteotonio).



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
