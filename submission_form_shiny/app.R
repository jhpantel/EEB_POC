# eeb_poc Shiny server
# Written by Mairin Deith (mdeith@zoology.ubc.ca)
# First created June, 2020
# Last modified Nov 18, 2020

# Load libraries ----------------------------------------------------------
# devtools::install_github('rstudio/DT')
library(DT)
library(shiny)
library(shinybusy)
library(shinyjs)
library(digest)
library(tidyr)
library(rorcid)
library(googledrive)
library(googlesheets4)
library(shinythemes)

# library(scholar)

### SET UP AUTHENTICATION
# Designate project-specific cache
#   To be hosted in shinyapps.io designated folder
options(gargle_oauth_cache = ".cache")

# Run once in an interactive session to create the auth cache.
drive_auth() 

# Authorize Google Sheets to use this token
# sheets_auth(token = drive_token())

# In subsequent runs, use this cache
drive_auth(cache = ".cache", email = "eebpocdatabase@gmail.com")
gs4_auth(token = drive_token())

# UI ----------------------------------------------------------------------
shinyApp(
  ui <- fluidPage(theme=shinytheme("yeti"),
    shinyjs::useShinyjs(),
    # titlePanel("POC Authors in Ecology, Evolution, and Biological Sciences"),
    tags$div(class = "h2",
      "BIPOC Scholars in Behavioral Sciences, Ecology, Evolution, and Environmental Studies (BEEES): Author Submission Portal"
    ),
    tags$div(class = "h4",
    "App created by Mairin Deith, mdeith@zoology[.]ubc[.]ca"),
    tabsetPanel(
      id = "tabs",

    tabPanel(title = "Author Information",
    sidebarPanel(
      helpText("The Graduate Diversity Council in the Department of Environmental Science, Policy, & Management at UC Berkeley and collaborators from UBC (Vancouver) are seeking to increase visibility of scholars with underrepresented racial backgrounds in our seminar series, course syllabuses, and citation practices. To that end, we are assembling a list of BIPOC (Black, Indigenous, Person of Color) scholars in fields related to environmental studies (including natural, social, physical sciences, and/or environmental humanities)."),
      br(),
      helpText("If you identify as a scholar in environmental sciences from an underrepresented racial or ethnic background, we would love to include you on a list that will be used for future seminar series and revising course syllabuses. Please take a few minutes to fill out this form and share it with others in your network!"),
      br(),
      helpText("All fields except your name are optional - please only fill in what you are comfortable being accessible online.")
    ),
  mainPanel(
    tags$h3("Scholar information"),
    column(4, 
      textInput("name", label = "Name (required)", value=""),
      textInput("email", label = "Email address", value=""),
      textInput("country", label = "Country of current residence", value=""),
      textInput("institution", label = "Affiliated institution", value=""),
      selectizeInput("careerstage", label = "Career stage", choices = c("", "Graduate student", "Post-doctoral Scholar", "Research Scientist", "Pre-Tenure Faculty", "Post-Tenure Faculty", "Emeritus Faculty")),
      textInput("twitter", label = "Twitter handle", value=""),
      helpText("We are also interested in highlighting some of your research contributions associated with your ORCID. See below if you would like to also contribute to this database."),
      textInput("orcid_form", label = "ORCID (format: xxxx-xxxx-xxxx-xxxx)", value=""),
      actionButton("orcid_nav", label = "Look-up and submit ORCID-associated works", class="btn-primary"),
      br(),
      br()
  ),
  column(4,
    textInput("site", label = "Affiliated website (including lab/department webpages or personal webpages)", value=""),
    textInput("scholar", label = "Google Scholar or other research page", value=""),
    tags$hr(),
    selectizeInput("gender", label = "Gender", choices = c("", "Nonbinary", "Female", "Male", "Prefer not to say", "Prefer another identity (indicate below)")),
    textInput("gender_openended", label = "Preferred identity", value=""),
    selectInput("bipoc", label = "Do you identify as a BIPOC (Black, Indigenous, Person of Color) scholar?", choices = c("", "Yes", "No")),
    textInput("bipoc_specify", label = "Underpresented racial/ethnic minotirty identity", value=""),
    selectInput("disability", label = "Do you identify as a person with a disability?", choices = c("", "Yes", "No")),
    selectInput("other_underrep", label = "Do you identify as an other underrepresented group not listed above? (e.g. LGBTQ+, First Generation College, or others)", choices = c("", "Yes", "No")),
    textInput("other_specify", label = "Feel free to specify here:", value="")
  ),
  column(4, 
    selectInput("subdisc", label = "Subdiscipline", choices = c("", "Biogeochemistry","Entomology","Evolutionary Biology","Food Systems & Agroecology","Forestry","Freshwater Ecology","Political Ecology","Sustainability Studies","Wildlife Ecology","Conservation Science","Environmental Social Sciences","Other...")),
    textInput("disc_specify", label = "Please specify your subdiscipline", value=""),
    textInput("keywords", label = "Please provide keywords for your research, separated with a semicolon (;)", value=""),
    helpText("One of the purposes of this database is to connect those looking for more representative speakers at academic events. Our intention is that your time will be compensated for these events; however we cannot ensure that your contact information will be used exclusively by paying hosts."), 
    selectInput("speaking_ops", label = "Are you open to being contacted for speaking opportunities?", choices = c("", "Yes", "No")),
    textInput("refers", label = "Please provide the names of other BIPOC scholars in your field that you would recommend we reach out to."),
    br()
  ),
  # Continue in the main panel
  actionButton("submitauth", label = "Submit author information to our database", icon = icon("archive"), class = "btn-success", width = "100%")
)),
tabPanel(title="ORCID Lookup",
sidebarPanel(
  helpText("Please veryify your ORCID, then search for associated works. Once they are loaded, select those you would like to be submitted to our database and press 'Submit'."),
  textOutput("orcid_fromform"),
  hr(),
  actionButton("orcid_lookup", "Search for associated works", icon = icon("search"), class = "btn-primary", width = "100%"),
),

mainPanel(
  # Author selection scroll box, paper selection scroll box
  DT::DTOutput("works_dt"),
  checkboxInput("dt_sel", "Select/deselect all"),
  # h4("selected_rows:"),
  # verbatimTextOutput("selected_rows", TRUE),
  # actionButton("submitall", "Submit all works above"),
  uiOutput("orcid_search_error"),
  helpText("WARNING: You can only submit works once, so please ensure you have selected all works you wish to be entered in the database."),
  actionButton("submitselected", "Submit selected works", class = "btn-success", width = "50%"),
  br(),
  hr(),
  br(),
  actionButton("author_nav", "Return to Author Information page", class="btn-primary", width = "50%")
)
)
)
),

  server = function(input, output, session){
      workstable <<- data.frame()
    show_modal_spinner(spin = "spring", text = "Connecting to database...")
    # Setup Google Sheets and global ID parameter

    # Initially disable/hide some buttons
    ### shinyjs::hide("input_type")
    shinyjs::hide("gender_openended")
    shinyjs::hide("bipoc_specify")
    shinyjs::hide("other_specify")
    shinyjs::hide("disc_specify")
    shinyjs::hide("orcid_lookup")
    shinyjs::hide("submitselected")
    shinyjs::disable("submitauth")
    shinyjs::hide("dt_sel")
    
    wb <<- googledrive::drive_get("nov10_shinytest_authors")
    # Get a unique fid for that author - first column
    newid <<- tryCatch({
          na.omit(googlesheets4::range_speedread(ss=wb, sheet = 1, range = "Sheet1!A2:A"))+1
    }, error=function(cond){
      return(0)
    })
    remove_modal_spinner()
    message(paste0("ID: ", newid))
    
    # "Other" boxes appearances controlled here
    #   If authors choose any field with "indicate below" options, have those
    #   appear
    observeEvent(input$gender, {
      if(input$gender == "Prefer another identity (indicate below)"){
        shinyjs::show("gender_openended")
      } else {
        shinyjs::hide("gender_openended")
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

    observeEvent(input$orcid_nav, {
      updateTabsetPanel(session = session, inputId = "tabs", selected = "ORCID Lookup")
    })

    observeEvent(input$author_nav, {
      updateTabsetPanel(session = session, inputId = "tabs", selected = "Author Information")
    })

    observeEvent(input$orcid_form, {
      if(input$orcid_form!=""){
        shinyjs::show("orcid_lookup")
        shinyjs::enable("orcid_nav")
      } else {
        shinyjs::hide("orcid_lookup")
        shinyjs::disable("orcid_nav")
      }
    })

    observeEvent(input$name, {
      if(input$orcid_form!=""){
#        shinyjs::show("submit_auth")
        shinyjs::enable("submitauth")
      } else {
#        shinyjs::hide("orcid_lookup")
        shinyjs::disable("submitauth")
      }
    })

    output$orcid_fromform <- renderText({paste0("Your ORCID: ", input$orcid_form)})

    # Submit author data to GSheet
    observeEvent(input$submitauth, {
        show_modal_spinner(spin = "spring", "Submitting to Google Sheet database...")
        # Create a dataframe based on user inputs - this will be saved to the GSheet
        author_df <- reactive({data.frame(
            submitter_unique_id = newid,
            name = input$name,
            institution = input$institution,
            email = gsub("@", "[at]", input$email),
            site = input$site,
            country = input$country,
            scholar = input$scholar,
            orcid = input$orcid_form,
            twitter = input$twitter,
            careerstage = input$careerstage,
            gender = ifelse(input$gender=="Prefer another identity (indicate below)", input$gender_openended, input$gender),
            bipoc = input$bipoc,
            bipoc_specify = input$bipoc_specify,
            disability = input$disability,
            other_underrep_minority = input$other_underrep,
            other_underrep_minority_specify = input$other_specify,
            subdisc = input$subdisc,
            disc_specify = input$disc_specify,
            keywords = input$keywords,
            refers = input$refers,
            speaking_ops = input$speaking_ops,
            upload_date = strptime(Sys.time(), "%m/%d/%y %H:%M:%S")
        )
        # shinyjs::disable("submitauth")
        # shinyjs::disable("author_nav")
    })
        shinyjs::disable("submitauth")
        googlesheets4::sheet_append(ss=wb, data=author_df(), sheet=1)
        remove_modal_spinner()
    }, ignoreInit=T) # once = T)

      observeEvent(input$orcid_lookup, {
        shinyjs::disable("ui_input_text")
        shinyjs::show("dt_sel")
        shinyjs::show("selected_rows")
        shinyjs::show("restart_prompt")
        show_modal_spinner()
        
        
        # q0 = rorcid::orcid_works(orcid = orc_input)
        q0 <- tryCatch({
          rorcid::orcid_works(orcid = input$orcid_form)
        }, error=function(cond){
            output$orcid_search_error <- renderUI(HTML(paste0(
            h4(paste0("ORCID Lookup Error: '", cond, "'. Please try again."))
        )))
        remove_modal_spinner()
        return(NULL)
      }, warning=function(cond){
        output$orcid_search_error <- renderUI(HTML(paste0(
            h4(paste0("ORCID Lookup warning: '", cond, "'."))
        )))
      })
      if(length(q0) > 0){
        shinyjs::hide("orcid_search_error")
        cat(length(q0))
        q <- data.frame(q0[[1]][[1]])
        print(head(q))
        
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
        #!# workstable <<- q %>%
        #!#   dplyr::select("title.title.value",
        #!#   "publication-date.year.value",
        #!#   "publication-date.day.value",
        #!#   "publication-date.month.value",
        #!#   "journal-title.value",
        #!#   "path") %>% 
        #!#   dplyr::transmute(Title = `title.title.value`,
        #!#      Journal = `journal-title.value`,
        #!#      Year = `publication-date.year.value`,
        #!#      Date = paste0(tidyr::replace_na(month.abb[as.numeric(`publication-date.month.value`)], ""), " ", tidyr::replace_na(`publication-date.day.value`, "")),
        #!#      ORCID.Path = path, 
        #!#      DOI = html_vec)
        workstable <<- q %>%
          dplyr::select("title.title.value",
            "publication.date.year.value",
            "publication.date.day.value",
            "publication.date.month.value",
            "journal.title.value",
            "path") %>% 
            dplyr::transmute(
              # submitter_unique_id = newid,
              Title = title.title.value,
               Journal = journal.title.value,
               Year = publication.date.year.value,
               Date = paste0(tidyr::replace_na(month.abb[as.numeric(publication.date.month.value)], ""), " ", tidyr::replace_na(publication.date.day.value, "")),
               ORCID.Path = path, 
               DOI = html_vec
               )

        remove_modal_spinner()
        message("Search complete")

        prettytable <<- reactive({DT::datatable(workstable)})
     
        output$works_dt <- DT::renderDT(
            prettytable(), server = T)

        remove_modal_spinner()
      }
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
      
      observeEvent(input$works_dt_rows_selected, {
        if(is.null(input$works_dt_rows_selected) || length(input$works_dt_rows_selected) == 0){
          shinyjs::disable("submitselected")
      } else {
        shinyjs::show("submitselected")
        shinyjs::enable("submitselected")
      }
      })

      observeEvent(input$submitselected, {
        show_modal_spinner(spin = "spring", "Submitting to Google Sheet database...")
        outtable <- workstable[input$works_dt_rows_selected,] %>%
          dplyr::mutate(submitter_unique_id = newid) %>%
          dplyr::select(submitter_unique_id, everything())
        wb2 <- drive_get("nov10_shinytest_works")
        saveRDS(workstable, "workstable.Rds")
        # googlesheets4::gs4_create("shiny_workstable_output_testy_test", sheets = list(workstable[input$works_dt_rows_selected,]))
        googlesheets4::sheet_append(data = outtable, ss=wb2)
        shinyjs::disable("submitselected")
        shinyjs::disable("orcid_nav")
        remove_modal_spinner()
    }, once = T)
})
