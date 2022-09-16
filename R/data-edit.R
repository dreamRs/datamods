
data_edit_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # Ajouter une ligne --
    uiOutput(outputId = ns("bouton_ajouter")),

    # Table --
    reactableOutput(outputId = ns("table"))
  )
}

data_edit_server <- function(id,
                             data_r = reactive(NULL), # fonction reactive avec un data.frame
                             add = TRUE, # si vrai, permet d'ajouter une ligne dans la table via un bouton en haut à droite
                             update = TRUE, # si vrai, permet de modifier une ligne de la table via un bouton situé dans le tableau sur la ligne que l'on veut éditer
                             delete = TRUE # si vrai, permet de supprimer une ligne de la table via un bouton dans la table
) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # Donnees data_r() avec ajout des colonnes "ID" et "ID_2" ---
      data_calc_r <- reactive({
        req(data_r())
        data <- data_r()
        data <- as.data.table(data)
        # Ajout de la colonne "ID" et "ID_2"
        data <- data[, ID := 1:nrow(data)]
        data <- data[, ID_2 := 1:nrow(data)]
        return(data)
      })

      # Table ---
      output$table <- renderReactable({
        req(data_calc_r())
        data <- data_calc_r()
        table(data = data,
              modifierInputId = if (isTRUE(update)) ns("modifier"),
              supprimerInputId = if (isTRUE(delete)) ns("supprimer"))
      })

      # Ajouter une ligne  ---
      output$bouton_ajouter <- renderUI({
        if (isTRUE(add)) {
          actionButton(
            inputId = ns("ajouter"),
            label = tagList(ph("plus"), "Ajouter une ligne"),
            class = "btn-outline-primary float-end"
          )
        }
      })

      observeEvent(input$ajouter, {
        req(data_calc_r())
        fenetre_saisie(id_valider = "ajouter_ligne",
                       donnees = data_calc_r())
      })

      observeEvent(input$ajouter_ligne, {
        req(data_calc_r())
        data <- data_calc_r()
        data <- as.data.table(data)
        removeModal()
        list_inputs <- reactiveValuesToList(input)
        resultat_ajout <- try({
          resultats_inputs <- lapply(
            X = seq_len(ncol(data)),
            FUN = function(i) {
              inputs <- list()
              input_name <- names(data)[i]
              inputs[[i]] <- list_inputs[[input_name]]
            }
          )
          resultats_inputs[[ncol(data) - 1]] <- max(data$ID) + 1
          resultats_inputs[[ncol(data)]] <- max(data$ID_2) + 1

          new <- data.frame(resultats_inputs)
          colnames(new) <- names(data)
          new <- data.table(new)
          data <- rbind(data, new, fill = TRUE)
          #print(data)
          #saveRDS(data, file = "datamods/data.rds") #changer nom
        })
        if (inherits(resultat_ajout, "try-error")) {
          shinybusy::report_failure(
            title = "Erreur",
            text = "Impossible d\'ajouter l\'\u00e9l\u00e9ment, contactez l\'administrateur de la plateforme.",
            button = "Fermer"
          )
        } else {
          shinybusy::report_success(
            title = "Enregistr\u00e9",
            text = "L\'\u00e9l\u00e9ment a \u00e9t\u00e9 enregistr\u00e9",
            button = "Fermer"
          )
        }
      })


      # Modifier une ligne ---
      observeEvent(input$modifier, {
        req(data_calc_r())
        data <- data_calc_r()
        data <- as.data.table(data)
        ligne <- data[ID == input$modifier]
        fenetre_saisie(
          .data = ligne,
          titre = "Modifier la ligne",
          id_valider = "modifier_ligne",
          donnees = data_calc_r()
        )
      })

      observeEvent(input$modifier_ligne, {
        req(data_calc_r())
        data <- data_calc_r()
        data <- as.data.table(data)
        removeModal()
        list_inputs <- reactiveValuesToList(input)
          resultat_modifier <- try({
            resultats_inputs <- lapply(
              X = seq_len(ncol(data)),
              FUN = function(i) {
                inputs <- list()
                input_name <- names(data)[i]
                inputs[[i]] <- list_inputs[[input_name]]
              }
            )
            resultats_inputs[[ncol(data) - 1]] <- data[ID == input$modifier, ID]
            resultats_inputs[[ncol(data)]] <- data[ID_2 == input$modifier, ID_2]

            modification <- data.frame(resultats_inputs)
            colnames(modification) <- names(data)
            modification <- data.table(modification)

            data <- rbind(data[ID != input$modifier], modification, fill = TRUE)
            data <- data[order(ID)]
            #print(data)
            #saveRDS(data, file = "datamods/data.rds") #changer nom
          })
          if (inherits(resultat_modifier, "try-error")) {
            shinybusy::report_failure(
              title = "Erreur",
              text = "Impossible de modifier l\'\u00e9l\u00e9ment, contactez l\'administrateur de la plateforme.",
              button = "Fermer"
            )
          } else {
            shinybusy::report_success(
              title = "Enregistr\u00e9",
              text = "L\'\u00e9l\u00e9ment a \u00e9t\u00e9 modifi\u00e9",
              button = "Fermer"
            )
          }
        })


      # Supprimer une ligne ---
      observeEvent(input$supprimer, {
        req(data_calc_r())
        data <- data_calc_r()
        data <- as.data.table(data)
        ligne <- data[ID_2 == input$supprimer]
        removeModal()
        showModal(fenetre_confirmation(
          inputId = ns("confirmation_supprimer_ligne"),
          titre = "Supprimer",
          "Souhaitez-vous supprimer l\'\u00e9l\u00e9ment s\u00e9lectionn\u00e9 ?"
        ))
      })
      observeEvent(input$confirmation_supprimer_ligne_oui, {
        req(data_calc_r())
        data <- data_calc_r()
        data <- as.data.table(data)
        ligne <- data[ID_2 == input$supprimer]
        resultat_supprimer <- try({
          data <- data[ID_2 != input$supprimer]
          data <- data[order(ID)]
          #print(data)
          #saveRDS(data, file = "datamods/data.rds") #changer nom
        })
        if (inherits(resultat_supprimer, "try-error")) {
          shinybusy::report_failure(
            title = "Erreur",
            text = "Impossible de supprimer l\'\u00e9l\u00e9ment, contactez l\'administrateur de la plateforme.",
            button = "Fermer"
          )
        } else {
          shinybusy::report_success(
            title = "Enregistr\u00e9",
            text = "L\'\u00e9l\u00e9ment a \u00e9t\u00e9 supprim\u00e9",
            button = "Fermer"
          )
        }
        removeModal()
      })
      observeEvent(input$confirmation_supprimer_ligne_non, {
        shinybusy::report_info(
          title = "Information",
          text = "L\'\u00e9l\u00e9ment n\'a pas \u00e9t\u00e9 supprim\u00e9",
          button = "Fermer"
        )
        removeModal()
      })

    }
  )
}



# Fonctions ---------------------------------------------------------------

fenetre_saisie <- function(.data = list(),
                            id_valider = "ajouter_ligne",
                            titre = "Ajouter une ligne",
                            donnees,
                            session = getDefaultReactiveDomain()) {
  ns <- session$ns
  showModal(modalDialog(
    title = tagList(
      titre,
      tags$button(
        phosphoricons::ph("x", title = "Fermer", height = "2em"),
        class = "btn btn-link",
        style = css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
        `data-bs-dismiss` = "modal",
        `aria-label` = "Fermer"
      )
    ),
    footer = NULL,
    size = "xl",
    easyClose = TRUE,
    saisie_ligne(.data = .data, donnees, session = session),
    actionButton(
      inputId = ns(id_valider),
      label = "Valider la saisie",
      class = "btn-outline-primary float-end"
    )
  ))
}


saisie_ligne <- function(.data = list(), donnees, session = getDefaultReactiveDomain()) {

  ns <- session$ns

  tagList(
    fluidRow(
      style = css(fontSize = "smaller"),
      column(
        width = 6,
        lapply(
          X = seq_len(ncol(donnees)),
          FUN = function(i) {
            variable_name <- names(donnees)[i]
            variable <- donnees[[i]]

            if (isTRUE((inherits(x = variable, what = "numeric")))) {
              numericInput(
                inputId = ns(variable_name),
                label = paste0(variable_name, " : "),
                value = .data$variable_name %||% 0,
                width = "100%"
              )
            } else if (isTRUE((inherits(x = variable, what = "factor")))) {
              virtualSelectInput(
                inputId = ns(variable_name),
                label = paste0(variable_name, " : "),
                choices = unique(variable),
                selected = .data$variable_name %||% unique(variable)[[1]],
                width = "100%",
                allowNewOption = TRUE
              )
            } else if (isTRUE((inherits(x = variable, what = "character")))) {
              textInput(
                inputId = ns(variable_name),
                label = paste0(variable_name, " : "),
                value = .data$variable_name %||% "",
                width = "100%"
              )
            } else if (isTRUE((inherits(x = variable, what = "logical")))) {
              prettyCheckbox(
                inputId = ns("variable_name"),
                label = paste0(variable_name, " : "),
                value = .data$variable_name %||% FALSE,
                icon = icon("check"),
                status = "primary"
              )
            } else if (isTRUE((inherits(x = variable, what = "Date")))) {
              dateInput(
                inputId = ns("variable_name"),
                label = paste0(variable_name, " : "),
                value = .data$variable_name %||% Sys.Date()
              )
            } else {
              return(NULL)
            }
          }
        )
      )
    )
  )
}


table <- function(data, modifierInputId = NULL, supprimerInputId = NULL) {
  reactable(
    data = data,
    columns = list(
      ID = col_def_modifier(modifierInputId),
      ID_2 = col_def_supprimer(supprimerInputId)
      )
    )
}

col_def_modifier <- function(inputId) {
  if (is.null(inputId))
    return(reactable::colDef(show = FALSE))
  reactable::colDef(
    name = "Modifier",
    width = 82,
    sortable = FALSE,
    html = TRUE,
    filterable = FALSE,
    cell = function(value) {
      tags$button(
        class = "btn btn-outline-primary rounded-circle",
        style = htmltools::css(
          height = "40px",
          width = "40px",
          padding = 0
        ),
        onClick = sprintf(
          "Shiny.setInputValue(\'%s\', %s,  {priority: \'event\'})",
          inputId, value
        ),
        title = "cliquez \u00e9diter",
        ph("pencil-simple-line", height = "1.2em")
      ) %>%
        htmltools::doRenderTags()
    }
  )
}

col_def_supprimer <- function(inputId) {
  if (is.null(inputId))
    return(reactable::colDef(show = FALSE))
  reactable::colDef(
    name = "Supprimer",
    width = 96,
    sortable = FALSE,
    html = TRUE,
    filterable = FALSE,
    cell = function(value) {
      tags$button(
        class = "btn btn-outline-danger rounded-circle",
        style = htmltools::css(
          height = "40px",
          width = "40px",
          padding = 0
        ),
        onClick = sprintf(
          "Shiny.setInputValue(\'%s\', %s,  {priority: \'event\'})",
          inputId, value
        ),
        title = "cliquez pour supprimer",
        ph("x", height = "1.2em")
      ) %>%
        htmltools::doRenderTags()
    }
  )
}


fenetre_confirmation <- function(inputId, ..., titre = NULL) {
  modalDialog(
    title = tagList(
      tags$button(
        phosphoricons::ph("x", title = "Fermer", height = "2em"),
        class = "btn btn-link",
        style = css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
        `data-bs-dismiss` = "modal",
        `aria-label` = "Fermer"
      ),
      titre
    ),
    ...,
    size = "m",
    footer = tagList(
      tags$button(
        "Annuler",
        class = "btn btn-outline-secondary",
        `data-bs-dismiss` = "modal"
      ),
      actionButton(
        inputId = paste0(inputId, "_non"),
        label = "Non",
        class = "btn-outline-danger",
        `data-bs-dismiss` = "modal"
      ),
      actionButton(
        inputId = paste0(inputId, "_oui"),
        label = "Oui",
        class = "btn-outline-primary"
      )
    )
  )
}
