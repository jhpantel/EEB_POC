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
library(googledrive)
library(googlesheets4) 
library(shinythemes)

### To do - ORCID to first page - lookups there?
### Link to Google sheet/database here

### SET UP AUTHENTICATION
# Designate project-specific cache
#   To be hosted in shinyapps.io designated folder
# options(gargle_oauth_cache = ".cache")

# Run once in an interactive session to create the auth cache.
# drive_auth()

# Authorize Google Sheets to use this token
# gs4_auth(token = drive_token())

# In subsequent runs, use this cache
drive_auth(cache = ".cache", email = "eebpocdatabase@gmail.com")
gs4_auth(token = drive_token())
# UI ----------------------------------------------------------------------
shinyApp(
  ui <- fluidPage(theme=shinytheme("yeti"),
    shinyjs::useShinyjs(),
    tags$div(class = "h1",
      "POC Authors in BEES* - Submission portal"),  
    tags$div(class = "h2", "*Behavioural, ecological, evolutionary, and social sciences"),

    sidebarLayout(
    sidebarPanel(
      helpText("The Graduate Diversity Council in the Department of Environmental Science, Policy, & Management at UC Berkeley and a group of collaborators from the Zoology Department at the University of British Columbia are seeking to increase visibility of scholars with underrepresented racial backgrounds in our seminar series, course syllabuses, and citation practices. To that end, we are assembling a list of BIPOC (Black, Indigenous, Person of Color) scholars in fields related to environmental sciences (including natural, social, and/or physical sciences)."),
      br(),
      helpText("If you identify as a scholar in environmental sciences from an underrepresented racial or ethnic background, we would love to include you on a list that will be used for future seminar series and revising course syllabuses. Please take a few minutes to fill out this form and share it with others in your network!"),
      br(),
      helpText("All fields except your name are optional - please only fill in what you are comfortable being accessible online.")
    ),
  mainPanel(
    tags$h3("Scholar information"),
    column(4, 

      textInput("name", label = "Name (required)"),
      textInput("email", label = "Email address"),
      textInput("country", label = "Country of current residence"),
      textInput("institution", label = "Affiliated institution"),
      selectizeInput("careerstage", label = "Career stage", choices = c("", "Graduate student", "Post-doctoral Scholar", "Research Scientist", "Pre-Tenure Faculty", "Post-Tenure Faculty", "Emeritus Faculty")),
      textInput("twitter", label = "Twitter handle"),
      helpText("We are also interested in highlighting some of your research contributions associated with your ORCID. See below if you would like to also contribute to this database."),
      textInput("orcid_form", label = "ORCID (format: xxxx-xxxx-xxxx-xxxx)"),
  ),
  column(4,
    textInput("site", label = "Affiliated website (including lab/department webpages or personal webpages)"),
    textInput("scholar", label = "Google Scholar or other research page"),
    tags$hr(),
    selectizeInput("gender", label = "Gender", choices = c("", "Nonbinary", "Female", "Male", "Prefer not to say", "Prefer another identity (indicate below)")),
    textInput("gender_openended", label = "Preferred identity"),
    selectInput("bipoc", label = "Do you identify as a BIPOC (Black, Indigenous, Person of Color) scholar?", choices = c("", "Yes", "No")),
    textInput("bipoc_specify", label = "Underpresented racial/ethnic minotirty identity"),
    selectInput("disability", label = "Do you identify as a person with a disability?", choices = c("", "Yes", "No")),
    selectInput("other_underrep", label = "Do you identify as an other underrepresented group not listed above? (e.g. LGBTQ+, First Generation College, or others)", choices = c("", "Yes", "No")),
    textInput("other_specify", label = "Feel free to specify here:")
  ),
  column(4, 
    selectInput("subdisc", label = "Subdiscipline", choices = c("", "Biogeochemistry","Entomology","Evolutionary Biology","Food Systems & Agroecology","Forestry","Freshwater Ecology","Political Ecology","Sustainability Studies","Wildlife Ecology","Conservation Science","Environmental Social Sciences","Other...")),
    textInput("disc_specify", label = "Please specify your subdiscipline"),
    textInput("keywords", label = "Please provide keywords for your research, separated with a semicolon (;)"),
    helpText("One of the purposes of this database is to connect those looking for more representative speakers at academic events. Our intention is that your time will be compensated for these events; however we cannot ensure that your contact information will be used exclusively by paying hosts."), 
    selectInput("speaking_ops", label = "Are you open to being contacted for speaking opportunities?", choices = c("", "Yes", "No"), ),
    textInput("refers", label = "Please provide the names of other BIPOC scholars in your field that you would recommend we reach out to.")
  ),
  # Continue in the main panel
  actionButton("submitauth", label = "Submit author information to our database", icon = icon("archive"), class = "btn-success", width = "100%"),
  tags$hr(),
  tags$h4("Use your ORCID (if provided) to lookup research works"),
  column(6, 
    actionButton("orcid_lookup", "Find works associated with your ORCID", icon = icon("search"), class = "btn-primary", width = "100%"),
    uiOutput("orcid_search_error"),
    uiOutput("orcid_search_restart")
    ), 
   column(6,
     actionButton("submitselected", "Submit selected works", icon = icon("archive"), class = "btn-success", width = "100%"),
     checkboxInput("dt_sel", "Select/deselect all")
  ),
  DT::DTOutput("works_dt")
) # end main panel
) # sidebar layout
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
    shinyjs::hide("dt_sel")
    
    wb <<- googledrive::drive_get("nov10_shinytest_authors")
    # Get a unique fid for that author - first column
    newid <<- max(
          na.omit(range_speedread(ss=wb, sheet = 1, range = "Sheet1!A:A")
        ), 0) + 1
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
            keywords = input$keyworcols,
            submitter_unique_id = newid,
            refers = input$refers,
            speaking_ops = input$speaking_ops,
            upload_date = strptime(Sys.time(), "%m/%d/%y %H:%M:%S")
        )
    })
        googlesheets4::sheet_append(ss=wb, data=author_df(), sheet=1)
        remove_modal_spinner()
    }, ignoreInit=T) # once = T)

    # Only show the "search ORCID" button If an ORCID is provided
    #   otherwise disable (greyed out)
    observeEvent(input$orcid_form, {
       if(input$orcid_form != ""){
        shinyjs::show("orcid_lookup")
        shinyjs::show("dt_sel")
        shinyjs::show("submitselected")
       } else {
        shinyjs::hide("orcid_lookup")
        shinyjs::hide("dt_sel")
        shinyjs::hide("submitselected")
       }
    })

    observeEvent(input$orcid_lookup, {        
      show_modal_spinner(spin = "spring", text = "Looking up works...")
      message(paste0("...searching for ORCID: ", input$orcid_form))
      q0 <- tryCatch({
          rorcid::orcid_works(orcid = input$orcid_form, warn = F)
      }, error=function(cond){
      output$orcid_search_error <- renderUI(HTML(paste0(
          h2(paste0("ORCID Lookup Error: '", cond, "'. Please try again."))
        )))
        remove_modal_spinner()
        return(NULL)
      }, warning=function(cond){
          message(paste0("Warning with lookup: ",cond)) 
          remove_modal_spinner()
          return(NULL)
      })
      if(!is.null(q0)){
        shinyjs::hide("orcid_search_error")
        # Eval/parse with ID to get the DF of works
        q <- eval(parse(text=paste0("q0$'", input$orcid_form, "'$works")))
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
        # message(colnames(q))
        workstable <<- q %>%
          dplyr::select("title.title.value",
          "publication-date.year.value",
          "publication-date.day.value",
          "publication-date.month.value",
          "journal-title.value",
          "path") %>% 
          dplyr::transmute(Title = `title.title.value`,
             Journal = `journal-title.value`,
             Year = `publication-date.year.value`,
             Date = paste0(tidyr::replace_na(month.abb[as.numeric(`publication-date.month.value`)], ""), " ", tidyr::replace_na(`publication-date.day.value`, "")),
             ORCID.Path = path, 
             DOI = html_vec)
        remove_modal_spinner()
        message("Search complete")
      }
      }, ignoreInit=T)

      prettytable <<- reactive({DT::datatable(workstable)}) 

      output$works_dt <- DT::renderDT(
        prettytable())
      
      dt_proxy <<- DT::dataTableProxy("works_dt")
      
      observeEvent(input$dt_sel, {
        if (isTRUE(input$dt_sel)) {
          message("...select all")
          DT::selectRows(dt_proxy, input$works_dt_rows_all)
        } else {
          DT::selectRows(dt_proxy, NULL)
        }
      })
      
#      observeEvent(input$works_dt_rows_selected, {
#        if(is.null(input$works_dt_rows_selected) || length(input$works_dt_rows_selected) == 0){
#      } else {
#        shinyjs::enable("submitselected")
#      }
#      })

    observeEvent(input$submitselected, {
      show_modal_spinner(spin = "spring", text = "Submitting to works database...")
      submitted_data <- isolate(save_df())
      submitted_data$submitter_unique_id <- newid
      worksdb <- googledrive::drive_get("nov10_shinytest_works")
      googlesheets4::sheet_append(ss=worksdb, data=submitted_data, sheet=1)
      remove_modal_spinner()
    }
  )    
}
)
