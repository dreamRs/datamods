
# -------------------------------------------------------------------------
# Edit data only with callbacks -------------------------------------------
# -------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library(data.table)
library(datamods)
library(shiny)
library(bslib)
library(shinybusy)
library(reactable)



# Fake data ---------------------------------------------------------------

mydata <- charlatan::ch_generate()
setDT(mydata)
mydata[, ID := seq_len(.N)] # internal ID
mydata[, user := paste0("user", seq_len(.N))]
setcolorder(mydata, c("ID", "user"))

path_to_data <- tempfile(pattern = "mydata", fileext = ".rds")
saveRDS(mydata, file = path_to_data)



# Utilities to update data ------------------------------------------------

update_data <- function(modified) {
  res <- try({
    base <- readRDS(path_to_data)
    setDT(base)
    base[ID == modified$ID, (names(modified)) := as.list(modified)]
    saveRDS(base, path_to_data)
  })
  inherits(res, "try-error")
}

delete_data <- function(deleted) {
  res <- try({
    base <- readRDS(path_to_data)
    setDT(base)
    base <- base[ID != deleted$ID]
    saveRDS(base, path_to_data)
  })
  inherits(res, "try-error")
}

add_data <- function(added) {
  res <- try({
    base <- readRDS(path_to_data)
    setDT(base)
    if (added$user %in% base$user) {
      shinybusy::report_warning("Warning", "User already exist.")
      return(FALSE)
    }
    added$ID <- max(base$ID) + 1
    base <- rbind(base, added[, .SD, .SDcols = !is.na(added)], use.names = TRUE, fill = TRUE)
    saveRDS(base, path_to_data)
  })
  inherits(res, "try-error")
}




# App ---------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(
    version = 5
  ),
  edit_data_ui(id = "EDIT"),
  verbatimTextOutput("result")
)

server <- function(input, output, session) {

  data_r <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = path_to_data,
    readFunc = readRDS
  )

  # Tableau
  edited_gestion_utilisateurs_r <- edit_data_server(
    id = "EDIT",
    data_r = data_r,
    add = TRUE,
    update = TRUE,
    delete = TRUE,
    download_csv = FALSE,
    download_excel = TRUE,
    modal_size = "xl",
    n_column = 2,
    var_edit = c("user", "name", "job", "phone_number"),
    callback_update = function(data, row) {
      result <- update_data(row)
      if (isTRUE(result))
        shinybusy::report_failure("Error", "Failed to update data")
      removeModal()
      return(TRUE)
    },
    callback_delete = function(data, row) {
      result <- delete_data(row)
      if (isTRUE(result))
        shinybusy::report_failure("Error", "Failed to delete data")
      removeModal()
      return(TRUE)
    },
    callback_add = function(data, add) {
      result <- add_data(add)
      if (isTRUE(result))
        shinybusy::report_failure("Error", "Failed to add data")
      removeModal()
      return(TRUE)
    },
    only_callback = TRUE,
    use_notify = FALSE,
    reactable_options = list(
      pagination = FALSE,
      outline = TRUE,
      striped = TRUE,
      columns = list(
        ID = colDef(show = FALSE)
      )
    )
  )

}


if (interactive())
  shinyApp(ui, server)
