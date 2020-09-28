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
# library(scholar)

# UI ----------------------------------------------------------------------
shinyApp(
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    # titlePanel("POC Authors in Ecology, Evolution, and Biological Sciences"),
    tags$div(class = "h2",
      "POC Authors in Ecology, Evolution, and Behavioural Sciences: Author Submission Portal"
    ),
    tabsetPanel(
      id = "tabs",

    tabPanel(title="Researcher Information",
    sidebarPanel(
      helpText("The Graduate Diversity Council in the Department of Environmental Science, Policy, & Management at UC Berkeley and a group of collaborators from the Zoology Department at the University of British Columbia are seeking to increase visibility of scholars with underrepresented racial backgrounds in our seminar series, course syllabuses, and citation practices. To that end, we are assembling a list of BIPOC (Black, Indigenous, Person of Color) scholars in fields related to environmental sciences (including natural, social, and/or physical sciences)."),
      br(),
      helpText("If you identify as a scholar in environmental sciences from an underrepresented racial or ethnic background, we would love to include you on a list that will be used for future seminar series and revising course syllabuses. Please take a few minutes to fill out this form and share it with others in your network!"),
      br(),
      helpText("All fields except your name are optional - please only fill in what you are comfortable being accessible online.")
    ),
  mainPanel(
    column(4, br(),
      textInput("name", label = "Name"),
      textInput("institution", label = "Affiliated institution"),
      textInput("email", label = "Email address (will be anonymized?)"),
      textInput("site", label = "Affiliatedilliated website (may include lab/department page or personal page)"),
      textInput("country", label = "Country of current residence"),
      textInput("scholar", label = "Google Scholar or other research page (ORCID entry possible on the next tab)"),
      textInput("twitter", label = "Twitter handle")
  ),
  column(4,br(),
    selectizeInput("careerstage", label = "Career stage", choices = c("", "Graduate student", "Post-doctoral Scholar", "Research Scientist", 'Pre-Tenure Faculty', "Post-Tenure Faculty", "Emeritus Faculty")),
    selectizeInput("gender", label = "Gender", choices = c("", "Nonbinary", "Female", "Male", "Prefer not to say", "Prefer another identity (indicate below)")),
    textInput("another_gender", label = "Preferred identity"),
    selectInput("bipoc", label = "Do you identify as a BIPOC (Black, Indigenous, Person of Color) scholar?", choices = c("", "Yes", "No")),
    textInput("bipoc_specify", label = "Underpresented racial/ethnic minotirty identity"),
    selectInput("disability", label = "Do you identify as a person with a disability?", choices = c("", "Yes", "No")),
    selectInput("other_underrep", label = "Do you identify as an other underrepresented group not listed above? (e.g. LGBTQ+, First Generation College, or others)", choices = c("", "Yes", "No")),
    textInput("other_specify", label = "Feel free to specify here:")
  ),
  column(4, br(),
    selectInput("subdisc", label = "Subdiscipline", choices = c("", "Biogeochemistry","Entomology","Evolutionary Biology","Food Systems & Agroecology","Forestry","Freshwater Ecology","Political Ecology","Sustainability Studies","Wildlife Ecology","Conservation Science","Environmental Social Sciences","Other...")),
    textInput("disc_specify", label = "Please specify your subdiscipline"),
    textInput("keywords", label = "Please provide keywords for your research, separated with a semicolon (;)"),
    textInput("refers", label = "Please provide the names of other BIPOC scholars in your fiend that you would recommend we reach out to.")
  )
)
),
tabPanel(title="Search for papers using your ORCID",
sidebarPanel(
  helpText("Please select what type of author information you'd like to use for looking up associated works. Then, the app will look up references associated with that identifier."),
  tags$b("If you have a common name, it is best to input your ORCID directly as the author search does not provide other identifying informtion."),
  tags$hr(),
  ### textInput("orcid_token", "[testing only] Please enter your ORCID API Token here:", "NULL"),
  ### tags$hr(),
  selectInput("input_type", "I would like to submit author information using my...", c("...choose one..."="","Publishing Name", "ORCID"), ""),
  uiOutput("ui_input_text"),
  # textOutput("restart_prompt"),
  uiOutput("restart_prompt"),
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
  helpText("When your works appear below, please click on the works you would like be submitted. Click again to remove. Any highlighted works will be submitted to the database."),
  DT::DTOutput("works_dt"),
  checkboxInput("dt_sel", "Select/deselect all"),
  h4("selected_rows:"),
  verbatimTextOutput("selected_rows", TRUE),
  # actionButton("submitall", "Submit all works above"),
  actionButton("submitselected", "Submit selected works")
)
)
)
),

  server = function(input, output, session) {
    show_modal_spinner()
    # Initially disable/hide some buttons
    ### shinyjs::hide("input_type")
    shinyjs::hide("another_gender")
    shinyjs::hide("bipoc_specify")
    shinyjs::hide("other_specify")
    shinyjs::hide("disc_specify")

    shinyjs::hide("ui_input_text")
    shinyjs::hide("restart_prompt")
    shinyjs::hide("authsearch")
    shinyjs::hide("worksearch")
    # Figure this out?
    shinyjs::hide("dt_sel")
    shinyjs::hide("selected_rows")
    shinyjs::hide("submitselected")
    remove_modal_spinner()
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
    # "Other" boxes appearances controlled here
    observeEvent(input$gender, {
      if(input$gender == "Prefer another identity (indicate below)"){
        shinyjs::show("another_gender")
      } else {
        shinyjs::hide("another_gender")
      }
    })
    observeEvent(input$bipoc, {
      if(input$bipoc == "Yes"){
        shinyjs::show("bipoc_specify")
      } else {
        shinyjs::hide("bipoc_specify")
      }
    })
    observeEvent(input$other_underrep, {
      if(input$other_underrep == "Yes"){
        shinyjs::show("other_specify")
      } else {
        shinyjs::hide("other_specify")
      }
    })
    observeEvent(input$subdisc, {
      if(input$subdisc == "Other..."){
        shinyjs::show("disc_specify")
      } else {
        shinyjs::hide("disc_specify")
      }
    })

    # Save inputs
    # formData <- reactive({
    #   data <- sapply(fieldsAll, function(x) input[[x]])
    #   data <- c(data, timestamp = epochTime())
    #   data <- t(data)
    #   data
    # })

    ### observeEvent(
    ###   input$orcid_token, {
    ###     if(!is.null(input$orcid_token)){
    ###       orcid_token <<-
    ###       shinyjs::show("input_type")
    ###     }
    ###   }
    ### )

    # If input type selected:
    observeEvent(
      input$input_type, {
      if(input$input_type == "ORCID"){
#        shinyjs::enable("author_info")
        shinyjs::show("ui_input_text")
        shinyjs::show("worksearch_ui")
        shinyjs::show("author_info")
        shinyjs::hide("authsearch")
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

    # When an author has been searched, require a restart.
    output$restart_prompt <- renderUI(HTML(paste0(
      em("Please restart the page to search for a new author name/ORCID.")
    )))

    # Search box
    output$ui_input_text <- renderUI({
      textInput("author_info", ifelse(input$input_type == "ORCID", "Enter your ORCID to search for your publications:", "Search the ORCID database for your name, which we will use to search for your publications:"), width="100%")
    })

    observeEvent(input$author_info, {
      shinyjs::show("worksearch_ui")
    })

    observeEvent(input$authsearch, {
      author_dt <<- NULL
      shinyjs::disable("author_info")
      shinyjs::disable("authsearch")
      shinyjs::disable("worksearch_ui")
      shinyjs::disable("ui_input_text")
      shinyjs::disable("input_type")
      shinyjs::show("restart_prompt")
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
          inputId = "auth_choice", label = "Which of these search results match you?", choices = c("", "...choose one...", paste0(given_vec, " ", family_vec, ". ORCID: ", orc_vec, " (alternative names: ", alts_vec, ")")),
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
        shinyjs::disable("ui_input_text")
        shinyjs::show("dt_sel")
        shinyjs::show("selected_rows")
        shinyjs::show("submitselected")
        shinyjs::show("restart_prompt")
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

        prettytable <- reactive({DT::datatable(workstable %>%
            dplyr::transmute(Title = title.title.value,
               Journal = journal.title.value,
               Year = publication.date.year.value,
               Date = paste0(tidyr::replace_na(month.abb[as.numeric(publication.date.month.value)], ""), " ", tidyr::replace_na(publication.date.day.value, "")),
               DOI = paste0("<a href='", html_vec, "'>", doi_vec, "</a>"),
               ORCID.Path = path
             ))})
        print(nrow(prettytable))

        output$works_dt <- DT::renderDT(
          #DT::datatable(
#            cbind(
#            `Submit?`=shinyInput(checkboxInput,"srows_",nrow(prettytable),value=NULL,width=1),
            prettytable(), server = T)
#            options = list(orderClasses = TRUE,
#              lengthMenu = c(10, 25, 50),
#              pageLength = 10,
#            drawCallback= JS(
#            'function(settings) {
#              Shiny.bindAll(this.api().table().node());}'),
#            dom = 't', searching=TRUE),
#          selection='multiple',escape=F)
#        )
        remove_modal_spinner()
      }, ignoreInit = T)

      dt_proxy <- DT::dataTableProxy("works_dt")
      observeEvent(input$dt_sel, {
        if (isTRUE(input$dt_sel)) {
          DT::selectRows(dt_proxy, input$works_dt_rows_all)
        } else {
          DT::selectRows(dt_proxy, NULL)
        }
      })

#      observeEvent(input$dt_sel, {
#        dt_proxy <<- DT::dataTableProxy("works_dt")
#        if(isTRUE(input$dt_sel)) {
#          DT::selectRows(dt_proxy, input$dt_rows_all)
#        } else {
#          DT::selectRows(dt_proxy, NULL)
#        }
#      })
      output$selected_rows <- renderPrint(print(input$works_dt_rows_selected))
})
