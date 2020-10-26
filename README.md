
# datamods

> Shiny modules to import and manipulate data into an application or addin.

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R build status](https://github.com/dreamRs/datamods/workflows/R-CMD-check/badge.svg)](https://github.com/dreamRs/datamods/actions)
[![codecov](https://codecov.io/gh/dreamRs/datamods/branch/master/graphs/badge.svg)](https://codecov.io/gh/dreamRs/datamods)
<!-- badges: end -->


## Overview

This package provides custom shiny modules to import data from various sources and
update variables in the dataset.  
The modules can be used in any standard shiny app.


### Installation

You can install the development version of datamods from [GitHub](https://github.com/dreamRs/datamods) with:

``` r
remotes::install_github("dreamRs/datamods")
```


### Usage

There are two functions associated with each module, `import_*_ui` and `import_*_server`  
Just plug them in the correct place with an `id` and they should be up and running.  

Let us take the `import-file` module as an example.  



#### Steps:  

1. Add `import_file_ui` in the ui definition of the app.  

```r
library(datamods)
library(shiny)

ui <- fluidPage(
  tags$h3("Import data from a file"),
  fluidRow(
    column(
      width = 4,
      import_file_ui("myid")  ## <---
    ),
    column(
      width = 8,
      tags$b("Imported data:"),
      tableOutput(outputId = "result")
    )
  )
)
```

2. Add the `import_file_server` to the server definition.  
Note that the `id` should be the same.  

```r
server <- function(input, output, session) {
  
  imported <- import_file_server("myid") ## <---
  
  output$result <- renderTable({
    imported$data()
  })
  
}
```

3. The call to server returns a `reactiveValues` object, so we assign it in a variable `imported`  
We can now call `imported$data()` to access the data returned.

4. Run the app!

```r
if (interactive())
  shinyApp(ui, server)
```



## Modules available :

  - [import-globalenv](#import-globalenv)
  - [import-file](#import-file)
  - [import-copypaste](#import-copypaste)
  - [import-googlesheets](#import-googlesheets)


### Import from Environment

This module imports data from the global environment or from a package.


### Import from file

One can upload files of format supported by `rio::import()` (text, csv, Excel, SAS, ...).


### Import by copying/pasting data

Copy and paste data from anywhere and it will be read as a `data.frame`.


### Import from GoogleSheet

Just paste the link to a GoogleSheet file and it will be read.  


#### Final Notes :

 - Please find example apps of each of the modules in the `/examples` folder.
 - For a detailed description, please see the vignette.
 - Read more about shiny modules [here](https://shiny.rstudio.com/articles/modules.html)
