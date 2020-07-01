# eeb_poc Shiny server
# Written by Mairin Deith (mdeith@zoology.ubc.ca)
# First created June, 2020

# https://deanattali.com/2015/06/14/mimicking-google-form-shiny/

# Load libraries ----------------------------------------------------------
# devtools::install_github('rstudio/DT')
library(DT)
library(shiny)
library(shinybusy)
library(shinyjs)
library(digest)
library(tidyr)
library(rorcid)

# Definition of mandatory fields and helper function ---------------------

# Save outputs - setup fields to be tracked
fieldsAll <- c("input_type", "author_info")

### THIS NEEDS TO BE CHANGED TO GOOGLE SHEETS SYNC
responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}

# UI ----------------------------------------------------------------------
shinyApp(
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    # titlePanel("POC Authors in Ecology, Evolution, and Biological Sciences"),
    tags$div(class = "h2",
      "POC Authors in Ecology, Evolution, and Biological Sciences: Author Submission Portal"
    ),
    tabsetPanel(
      id = "tabs",
      tabPanel(title="Submit author information to build our database",
      sidebarPanel(
        helpText("Please select what type of author information you'd like to use for looking up associated works. Then, the app will look up references associated with that identifier."),
        tags$b("If you have a common name, it is best to input your ORCID directly as the author search does not provide other identifying informtion."),
        tags$hr(),
        selectInput("input_type", "I would like to submit author information using my...", c("...choose one..."="","Publishing Name", "ORCID"), ""),
        uiOutput("ui_input_text"),
        ### OLD:
        ### DT::dataTableOutput("author_select")
        actionButton("authsearch", "Search for author", icon = icon("address-book"), class = "btn-primary", width = "100%"),
        tags$hr(),
        uiOutput("auth_select"),
        tags$hr(),
        uiOutput("worksearch_ui")
      ),
      mainPanel(
        # Author selection scroll box, paper selection scroll box
        checkboxInput("dt_sel", "Select/deselect all"),
        DT::DTOutput("works_dt")
      )
      )
    )
  ),
  server = function(input, output, session) {
    # Initially disable/hide some buttons
    shinyjs::hide("ui_input_text")
    shinyjs::hide("authsearch")
    shinyjs::hide("worksearch")
    # Figure this out #
    shinyjs::hide("dt_sel")
    shinyjs::disable("dt_sel")

    # Global variable
    author_dt <<- data.frame("Given name" = "",
                             "Family name" = "",
                             "ORCID" = "",
                             "Alternative names" = "")

    ### Support code to show author information when searched
    shinyInput <- function(FUN,id,num,...){
      inputs <- character(num)
      for (i in seq_len(num)) {
        inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
      }
      inputs
    }

    # Implement row selection
    rowSelect <- reactive({

      rows=names(input)[grepl(pattern = "srows_",names(input))]
      paste(unlist(lapply(rows,function(i){
        if(input[[i]]==T){
          return(substr(i,gregexpr(pattern = "_",i)[[1]]+1,nchar(i)))
        }
      })))

    })

    # Save inputs
    # formData <- reactive({
    #   data <- sapply(fieldsAll, function(x) input[[x]])
    #   data <- c(data, timestamp = epochTime())
    #   data <- t(data)
    #   data
    # })

    # If input type selected:
    observeEvent(
      input$input_type, {
      if(input$input_type == "ORCID"){
#        shinyjs::enable("author_info")
        shinyjs::show("ui_input_text")
        shinyjs::show("worksearch_ui")
        shinyjs::show("author_info")
        # Create a global variable, orc_input, to be used later
        orc_input <<- input$author_info
      } else if(input$input_type == "Publishing Name"){
#        shinyjs::enable("authsearch")
#        shinyjs::enable("author_info")
        shinyjs::show("ui_input_text")
        shinyjs::show("authsearch")
        shinyjs::show("author_info")
      }
    }, ignoreInit = T)

    # Search box
    output$ui_input_text <- renderUI({
      textInput("author_info", ifelse(input$input_type == "ORCID", "Enter your ORCID to search for your publications:", "Search the ORCID database for your name, which we will use to search for your publications:"))
    })

    observeEvent(input$authsearch, {
        show_modal_spinner()
        # if(input$authsearch){
        q = rorcid::orcid(query = tolower(input$author_info))
        o = apply(q[,2], 1, rorcid::as.orcid)
        orc_vec = c()
        given_vec = c()
        family_vec = c()
        alts_vec = c()
        for(i in 1:length(o)){
          orc_tmp = names(o[[i]])
          given_tmp = o[[i]][[orc_tmp]]$name$`given-names`$value
          given_tmp = ifelse(is.null(given_tmp), NA, given_tmp)
          family_tmp = o[[i]][[orc_tmp]]$name$`family-name`$value
          family_tmp = ifelse(is.null(family_tmp), NA, family_tmp)
          alts = o[[i]][[orc_tmp]]$`other-names`$`other-name`
          if(!identical(alts, list())){
            # Unlist the alternative names, put into parentheses
            alts_vec = c(alts_vec, paste(o[[i]][[orc_tmp]]$`other-names`$`other-name`$content, collapse = ", "))
          } else {
            alts_vec = c(alts_vec, NA)
          }
          given_vec = c(given_vec, given_tmp)
          family_vec = c(family_vec, family_tmp)
          orc_vec = c(orc_vec, orc_tmp)
        }
        author_dt <<- data.frame("Given name" = given_vec,
          "Family name" = family_vec,
          "ORCID" = orc_vec,
          "Alternative names" = alts_vec)
        # Create display names
        author_displays <<- paste0(given_vec, " ", family_vec, ". ORCID: ", orc_vec, " (alternative names: ", alts_vec, ")")
        # print(author_displays)
        remove_modal_spinner()

        output$auth_select = renderUI({
          selectInput(
            inputId = "auth_choice", label = "Which of these search results match you?", choices = c("...choose one...", paste0(given_vec, " ", family_vec, ". ORCID: ", orc_vec, " (alternative names: ", alts_vec, ")")),
            selected = NULL, multiple = F
          )
        })

        observeEvent(input$auth_choice, {
          choice_idx = which(author_displays == input$auth_choice)
          orc_input <<- author_dt[choice_idx, "ORCID"]
          print(paste0("Input = ", orc_input))
          output$worksearch_ui <- renderUI({
            actionButton("worksearch", paste0("Find works by ORCID ", orc_input), icon = icon("search"), class = "btn-primary", width = "100%")
          })
          shinyjs::show("worksearch_ui")
        }, ignoreInit = T)
        # print(input$auth_choice)
        # shinyjs::show("worksearch")
      })

      observeEvent(input$worksearch, {
        show_modal_spinner()
        q = data.frame(rorcid::orcid_works(orcid = orc_input)[[1]][[1]])
        print(head(q))
        workstable <- q %>%
          dplyr::select("title.title.value",
            "publication.date.year.value",
            "publication.date.day.value",
            "publication.date.month.value",
            "journal.title.value",
            "external.ids.external.id",
            "path")

        doi_fetcher <- q$`external.ids.external.id`

        doi_vec <- c()
        html_vec <- c()

        for(d in doi_fetcher){
          rowidx <- which(d$`external-id-type` == "doi")
          doi_tmp <- d$`external-id-value`[rowidx]
          if(!identical(rowidx, integer(0))){
            doi_vec <- c(doi_vec, doi_tmp)
            html_vec <- c(html_vec, paste0("https://doi.org/", doi_tmp))
          } else {
            doi_vec <- c(doi_vec, "No DOI found")
            html_vec <- c(html_vec, NULL)
          }
        }

        prettytable <- workstable %>%
            dplyr::transmute(Title = title.title.value,
               Journal = journal.title.value,
               Year = publication.date.year.value,
               Date = paste0(tidyr::replace_na(month.abb[as.numeric(publication.date.month.value)], ""), " ", tidyr::replace_na(publication.date.day.value, "")),
               DOI = paste0("<a href='", html_vec, "'>", doi_vec, "</a>"),
               ORCID.Path = path
             )
        print(nrow(prettytable))

        output$works_dt <- DT::renderDT({
          DT::datatable(
            cbind(
            `Submit?`=shinyInput(
              checkboxInput,"srows_",nrow(prettytable),value=NULL,width=1),
            prettytable),
            options = list(orderClasses = TRUE,
              lengthMenu = c(10, 25, 50),
              pageLength = 10,
            drawCallback= JS(
            'function(settings) {
              Shiny.bindAll(this.api().table().node());}'),
            dom = 't', searching=TRUE),
          selection='multiple',escape=F)
        })
        remove_modal_spinner()
      }, ignoreInit = T)

#      observeEvent(input$dt_sel, {
#        dt_proxy <<- DT::dataTableProxy("works_dt")
#        if(isTRUE(input$dt_sel)) {
#          DT::selectRows(dt_proxy, input$dt_rows_all)
#        } else {
#          DT::selectRows(dt_proxy, NULL)
#        }
#      })
})
