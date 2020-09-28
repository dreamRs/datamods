
# datamods

> Shiny modules to import data into an application or addin.

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Overview

This package provides custom shiny modules to import data from various sources and
update variables in the dataset.  
The modules can be used in any standard shiny app.

### Installation

You can install the development version of datamods from [GitHub](https://github.com/dreamRs/datamods) with:

``` r
devtools::install_github("dreamRs/datamods")
```

### Usage

There are two functions associated with each module, `import_ui` and `import_server`  
Just plug them in the correct place with an `id` and they should be up and running.  

Let us take the `import-file` module as an example.  

#### Steps:  

1. Add `import_file_ui` in the ui definition of the app.  

``` r
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
      verbatimTextOutput(outputId = "result")
    )
  )
)
```

2. Add the `import_file_server` to the server definition.  
Note that the `id` should be the same.  

``` r
server <- function(input, output, session) {
  
  imported <- import_file_server("myid") ## <---
  
  output$result <- renderPrint({
    imported$data()
  })
  
}
```

3. The call to server returns a `reactiveValues` object, so we assign it in a variable `imported`  
We can now call `imported$data()` to access the data returned.

4. Run the app!

``` r
if (interactive())
  shinyApp(ui, server)

```



## Modules available :

  - [import-globalenv](#import-globalenv)
  - [import-file](#import-file)
  - [import-copypaste](#import-copypaste)
  - [import-googlesheets](#import-googlesheets)

### `import-globalenv`  

This module imports data from the global environment or from a package specified.

### `import-file`

One can upload files of format supported by `rio::import()`

### `import-copypaste`

Copy and paste data from anywhere and it will be read as a `data.table` with 
`data.table::fread()`

### `import-googlesheets`

Just paste the link to to a googlesheets file and it will be read
