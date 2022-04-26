##heading##################################################
# Ecorules RShiny interface                               #
#                                                         #
# Auteurs: Willem Stolte, Marc Weeber                     #
#                                                         #
#                                                         #
#                                                         #
# Datum : 2019-04-23                                      #
# Bedrijf: Deltares                                       #
# Licentie: GNU General Public License                    #
#                                                         #           
# Contact : Marc Weeber                                   #
# Email : Marc Weeber@deltares.nl                         #
#                                                         #
########################################################### 


##=PREPARATION========

##==legend=====

## COMMENTS
## ##      = comment
## #       = outcommented code
## #!#     = should be checked if location of script changes 
## #?#     = to do
## ##==    = block separator

## OBJECTS
## fnXXX   = filename
## dirXXX  = directory (path) / foldername
## oXXX    = loaded object
## dfXXX   = dataframe      
## dtXXX   = datatable 

##==install packages and open libraries ====

# install.packages("shiny")          ## shiny
# install.packages("shinydashboard") ## layout for shiny
# install.packages("data.table")     ## working with data.frames the data.table way
# install.packages("ggplot2")        ## plotting
# install.packages("openxlsx")       ## open excel files
# install.packages("digest")         ## needed for dev version ggplot2
# install.packages("devtools")
# devtools::install_github('hadley/ggplot2')
# install.packages("leaflet")
# install.packages("plotly")
# install.packages("lubridate")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("rgdal")
# install.packages("shinyjs")
# install.packages("rmapshaper")
# install.packages("shinycssloaders")
# install.packages("plyr")
# install.packages("V8")
# install.packages("htmlwidgets")
# install.packages("mapview")
# install.packages("XML")

library(shiny)         
library(shinydashboard)
library(leaflet)
library(plotly)
library(lubridate)
library(shinyjs)
library(shinycssloaders)
library(plyr)
library(rgdal)
library(V8)
library(htmlwidgets)
library(mapview)
library(xml2)
library(tidyverse)


##==settings=======

options(shiny.display.mode="showcase")
options(scipen = 999)  ## no scientific numbering
options(shiny.reactlog = T)
options(shiny.maxRequestSize = 30*1024^2) ## maximum upload size is now 30Mb, standard for R shiny is 5Mb. 
## rshiny shinyapps.io max limit size = 32MB. 

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page



##== load functions ====

source("../ae.R")
#source('AqMaD_inleesfuncties.R')
#source('AqMaD_plotFunctions.R')
#source('AqMaD_rekenkern.R')



##==set paths ========

#!# directories

knowldedgeRuleDir <- "../data/_knowledgerules"
# knowldedgeRuleDir <- "n:/Projects/11203500/11203758/B. Measurements and calculations/005- habitat modeling/EcologischeDatabase/github_setup/_knowledgerules"
#!# files


##== set variables =======

# variables = reactiveValues(soortgroep = NULL, soortgroepnaam = NULL, watertype = NULL)



##==load files=========

## no files to load yet


##==make catalogue=========

#?# read all knowledge rule types from xml files in directory

knowledgeRuleTypeList <- list.files(knowldedgeRuleDir, include.dirs = T)



# test
# dd <- "n:/Projects/11203500/11203758/B. Measurements and calculations/005- habitat modeling/EcologischeDatabase/github_setup/_knowledgerules/species/Fish"
# extract_names(dd)
# eind test


#?# remove attributes ?


##== set additional functions=====




##==USER INTERFACE==================

header  <- dashboardHeader(title = "EcoRules")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "side_tabs",
              menuItem(text = "Start page", tabName = "start", icon = icon("home")),
              menuItem(text = "Read data", tabName = "readdata", icon = icon("upload")),
              menuItem(text = "Visualize", tabName = "visualize", icon = icon("list-alt", lib = "glyphicon"))
  ))
## rows have a grid width of 12, so a box with width = 4, takes up one third of the space
## tops will be lined out, bottoms not
## heights are in pixels.. 

body    <- dashboardBody(
  
  tabItems(
    
    ##===start page==============
    tabItem(tabName = "start",
            fluidRow(
              box(title = "Welkom to EcoRules",
                  solidHeader = T,
                  status = "success",
                  collapsible = F,
                  p("Introduction to EcoRules"),
                  p("This app"),
                  p("Credits/acknowledgements"),
                  # img(src='stowa_logo.png'),
                  img(src='deltares_logo.png'),
                  width = 12))),
    
    ##== readdata page ==============
    tabItem(tabName = "readdata", 
            fluidRow(
              box(title = "Read data",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = F,
                  width = 6,
                  p("Choose the knowledge rule topic and element"),
                  # tags$ol(tags$li("Select species name type (scientific or common"),
                  #         tags$li("Select species from dropdown")),
                  selectInput("knowledgeRuleTypeLevel1", "Knowledge rule type level 1: ", knowledgeRuleTypeList),
                  uiOutput(outputId = "knowledgeRuleTypeLevel2ui"),
                  uiOutput(outputId = "knowledgeRuleTypeLevel3ui"),
                  actionButton("read_actual_ae", "read knowledge rule"),
                  textOutput("selected_var"),
                  textOutput("speciesName")
              ),
              box(title = "select elements",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = F,
                  width = 6,
                  p("Choose the elements that you want to visualize."),
                  uiOutput(outputId = "modelType"),
                  uiOutput(outputId = "systems"),
                  p("recognized knowledge rules:"),
                  tableOutput(outputId = "knowledgeRuleNames")
              ),
              
              
              
              ##==== choose stuff =====
              # box(title = "2. Selecteer stilstaand of stromend water",
              #     solidHeader = T,
              #     status = "success",
              #     collapsible = T,
              #     collapsed = T,
              #     width = 12,
              #     p("Selecteer hier uw watertype. Op basis van deze keuze worden de resultaten gepresenteerd volgens de ESF-systematiek van resp. stilstaand of stromend water."),
              #     radioButtons(inputId = "rb_watertype", 
              #                  label = NULL, 
              #                  choices = c("Stilstaand water", "Stromend water"))),
              
              ##==== reset application ========
              box(title = "Restart application",
                  solidHeader = T,
                  status = "warning",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  p("Press to restart, All read information will be lost"),
                  useShinyjs(),                                           # Include shinyjs in the UI
                  #extendShinyjs(text = jsResetCode),                      # Add the js code to the page
                  actionButton("reset_button", "Restart application")
              )
              
              
              
            )
    ),
    
    ##========= visualization ============================
    tabItem(tabName = "visualize",
            fluidRow(
              tabBox(title = NULL,
                     width = 12, 
                     side = "left",
                     # selected = "Huidige monitordata",
                     tabPanel("Species",  
                              p("Presentation of species"),
                              htmlOutput("speciesDescription")
                     ),
                     tabPanel("System",  
                              p("Presentation of system"),
                              htmlOutput("systemDescription")
                     ),
                     tabPanel("Knowledge rules",  
                              p("Presentation of knowledge rules"),
                              fluidRow(
                                uiOutput("rulesBoxes")
                              )
                              
                     ),
                     tabPanel("something else",
                              # tableOutput("dummy"),
                              p("Something is presented here")
                     )))),
    
    
    
    ##====achtergrondinformatie pagina ====
    tabItem(tabName = "info",
            fluidRow(
              box(title = "Download achtergrondinformatie",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = F,
                  width = 12,
                  # h3("Handleiding AqMaD"),
                  # downloadButton("handleiding_aqmad", "Download handleiding voor AqMaD"),
                  p("De R-code voor deze tool is op aanvraag beschikbaar bij Willem Stolte (Deltares) via:"),
                  img(src='contact.png')
              )))))



ui <- dashboardPage(skin = "green", header, sidebar, body,  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), tags$style(".leaflet-top {z-index:999!important;}"))) ## tags$head etc is noodzakelijk om de zoombuttons van leaflet achter de dropdown box te krijgen. 
# ui <- dashboardPage(header, sidebar, body,  tags$head(tags$style(".leaflet-top {z-index:999!important;}"))) ## tags$head etc is noodzakelijk om de zoombuttons van leaflet achter de dropdown box te krijgen. 


##==SET SERVER=========


server <- function(input, output, session) {
  
  
  
  ##========= reset shiny app ==================
  observeEvent(input$reset_button, {js$reset()}) 
  
  ##========= toekennen Variabelen ==================
  # observeEvent(input$rb_ecologische_groep, {
  #   
  #   if(input$rb_ecologische_groep == 'Diatomeeen'){
  #   variables$soortgroep = "Diatomeeen"
  #   variables$soortgroepnaam = c("DIATM", "DIAB", "Diatomeeen")
  #   } else if(input$rb_ecologische_groep == 'Macrofyten'){
  #   variables$soortgroep = "Macrofyten"
  #   variables$soortgroepnaam = c('MAFY','MACFT','MACFY','mafy','macft','macfy','MFYT', 'Macrofyten')
  #   } else if(input$rb_ecologische_groep == 'Macrofauna'){
  #   variables$soortgroep = "Macrofauna_V2"
  #   variables$soortgroepnaam = c('Macrofauna','MFA')
  #   #} else if(input$rb_ecologische_groep == 'Macrofauna'){
  #   #  variables$soortgroep = "Macrofauna"
  #   #  variables$soortgroepnaam = c("MACEV", "MFA$", "MFAT", "Macrofauna")
  #   } else if(input$rb_ecologische_groep == 'Vissen'){
  #   variables$soortgroep = "Vissen"
  #   variables$soortgroepnaam = c("VISSN", "Vissen")
  #   }})
  # 
  # observeEvent(input$rb_watertype, {
  #   if(input$rb_watertype == "Stilstaand water"){
  #     variables$watertype = "stilstaand"
  #   } else if(input$rb_watertype == "Stromend water"){
  #     variables$watertype = "stromend"
  #   }})
  
  
  ##========= weergave soortgroep ==================
  # output$selected_soortgroep_abio <- renderText({req(input$rb_ecologische_groep); paste("Geselecteerde soortgroep: ", input$rb_ecologische_groep, sep = "")})
  # output$selected_soortgroep_z    <- renderText({req(input$rb_ecologische_groep); paste("Geselecteerde soortgroep: ", input$rb_ecologische_groep, sep = "")})
  
  
  ## output 
  
  ##==ACTIONS=====
  
  actual_ae <- eventReactive(input$read_actual_ae, {
    read_ae_xml(file.path(knowldedgeRuleDir, input$knowledgeRuleTypeLevel1, input$knowledgeRuleTypeLevel2, input$knowledgeRuleTypeLevel3))
  })
  
  ##==UI OPBOUW=========================================
  
  output$knowledgeRuleTypeLevel2ui <- renderUI({
    req(input$knowledgeRuleTypeLevel1)
    choice_of_knowledgeRuleTypeLevel2 <- list.files(file.path(knowldedgeRuleDir, input$knowledgeRuleTypeLevel1), include.dirs = T)
    selectInput("knowledgeRuleTypeLevel2", "knowledgeRule type level 2:", choice_of_knowledgeRuleTypeLevel2)
  })
  
  output$knowledgeRuleTypeLevel3ui <- renderUI({
    req(input$knowledgeRuleTypeLevel2)
    choice_of_knowledgeRuleTypeLevel3 <- list.files(file.path(knowldedgeRuleDir, input$knowledgeRuleTypeLevel1, input$knowledgeRuleTypeLevel2), include.dirs = T)
    selectInput("knowledgeRuleTypeLevel3", "knowledgeRule type level 3:", choice_of_knowledgeRuleTypeLevel3)
  })
  
  # output$catalogue <- renderUI({
  #   req(input$knowledgeRuleTypeLevel3)
  #   choice_of_knowledgeRuleTypeLevel4 <- list.files(file.path(knowldedgeRuleDir, input$knowledgeRuleTypeLevel1, input$knowledgeRuleTypeLevel2), include.dirs = T)
  #   selectInput("knowledgeRuleTypeLevel2", "knowledgeRule type level 3:", choice_of_knowledgeRuleTypeLevel3)
  # })
  
  output$modelType <- renderUI({
    req(actual_ae())
    # choice_of_modelTypes <- get_element_ModelType(actual_ae()) %>% xml2::xml_attrs() %>% map_chr("name")
    choice_of_modelTypes <- get_modeltypes(actual_ae())
    selectInput("modeltype", "choose model type:", choice_of_modelTypes)
  })
  
  output$systems <- renderUI({
    req(actual_ae(), input$modeltype)
    choice_of_systems <- get_element_ModelType(actual_ae())[which(get_element_ModelType(actual_ae()) %>% xml2::xml_attrs() %>% map_chr("name") == input$modeltype)] %>% 
      xml2::xml_find_all(xpath = ".//System") %>%
      xml2::xml_attrs() %>% map_chr("name")
    selectInput("system", "choose system:", choice_of_systems)
  })
  # system
  # knowledgerule
  # formulabased OR responsecurve
  
  output$knowledgeRuleNames <- renderTable({
    req(actual_ae(), input$system, input$modeltype)
    get_type_knowledgeruleNames(ae = actual_ae(), modeltype = input$modeltype, system = input$system)
    
  })
  
  # output$relation <- renderUI({
  #   req(actual_ae(), input$system, input$rule)
  #   choice_of_relation <- get_element_knowledgerules(actual_ae()) %>%  
  #     xml2::xml_children() %>% 
  #     xml2::xml_attrs() %>% unlist() %>% unname()
  # })
  
  
  ##== OUTPUT =========================================
  
  
  ## Levert namen en types van alle regels
  actual_knowledge_rules_df <- reactive({
    req(actual_ae(), input$system, input$modeltype)
    get_type_knowledgeruleNames(ae = actual_ae(), modeltype = input$modeltype, system = input$system)
    
  })
  
  
  ## text output chosen species in current ae
  output$speciesName <- renderText({
    req(actual_ae())
    species <- get_element_species(actual_ae()) %>% xml2::xml_find_first("LatName") %>%xml2::as_list() %>% unlist()
    paste("you have selected", species)
  })
  
  
  output$speciesDescription <- renderUI({
    req(actual_ae())
    get_element_contentdescription(actual_ae()) %>%xml2::as_list() %>% rlist::list.flatten() %>% 
      as.character() %>%
      htmltools::HTML()
  })
  
  output$systemDescription <- renderUI({
    req(actual_ae(), input$modeltype)
    get_system_description(actual_ae(), input$modeltype) %>%
      htmltools::HTML()
  })
  
  
  all_knowledgerules_data <- reactive({
    req(actual_ae(), input$system, input$modeltype)
    mySystem <- get_element_knowledgerules(ae = actual_ae(), modeltype = input$modeltype, system = input$system) %>% xml2::xml_parent()
    response_curve = mySystem %>% xml2::xml_find_all(xpath = ".//KnowledgeRules/ResponseCurve")
    formula_based = mySystem %>% xml2::xml_find_all(xpath = ".//KnowledgeRules/FormulaBased")
    
    if(length(response_curve) == 0){
      all_data_response_curve <- NULL} else {
        all_data_response_curve = lapply(response_curve, get_data_response_curve)
      }
    if(length(formula_based) == 0){
      all_data_formula_based <- NULL} else {
        all_data_formula_based = lapply(formula_based, get_data_formula_based)
      }
    data = c(all_data_response_curve, all_data_formula_based)
    return(data)
  })
  
  
  # output$dummy <- renderTable({
  #   map(all_knowledgerules_data(), list("rule")) %>% as.data.frame()
  # })
  
  theme_categorical <- reactive({
    theme(axis.text.x = element_text(angle = 45))
  })
  
  output$rulesBoxes <- renderUI({
    req(all_knowledgerules_data()) # is al list in goede format

    b <- lapply(all_knowledgerules_data(), function(a) {
      
      category = a$KnowledgeruleCategorie
      
      if(category ==  "ResponseCurve"){
        name = a$name
        type = a$type
        data = a$rule
        box(width = 12,
            title = paste0(name, " - ", type),
            fluidRow(
              column(width = 6, offset = 0,
                     if(category ==  "ResponseCurve" & type == "scalar"){
                       renderPlot(ggplot(data, aes(input, output)) + geom_line(color = "lightblue", size = 1) + theme_minimal(), width = 400, height = 300)},
                     if(category ==  "ResponseCurve" & type == "categorical"){
                       renderPlot(ggplot(data, aes(input_cat, output)) + geom_bar(fill = "lightblue", stat="identity") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)), width = 400, height = 300)},
                     if(category ==  "ResponseCurve" & type == "range / categorical"){
                       data$cat = reorder(data$cat, data$rangemin)
                       renderPlot(ggplot(data, aes(input, output)) + 
                                    geom_bar(fill = "lightblue", stat="identity") + 
                                    geom_text(aes(cat, -0.1, label = paste(rangemin, rangemax, sep = " - "))) +
                                    theme_minimal() + 
                                    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)), width = 400, height = 300)},
                     if(category ==  "ResponseCurve" & type == "ranges"){
                       data$cat <- paste(data$rangemin, data$rangemax, sep = " - ")
                       data$cat = reorder(data$cat, data$rangemin)
                       renderPlot(ggplot(data, aes(cat, HSI)) + 
                                    geom_bar(fill = "lightblue", stat="identity") + 
                                    # geom_text(aes(cat, -0.1, label = paste(rangemin, rangemax, sep = " - "))) +
                                    theme_minimal() + 
                                    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)), width = 400, height = 300)
                     }
              ),
              column(width = 6, offset = 0,
                     if(category ==  "ResponseCurve"){
                       renderTable(data, width = 400)
                     }
              )
            ),
            collapsible = T,
            collapsed = T
        )
        
      } else
      
      if(category == "FormulaBased"){
        
        name = a$name
        unit = a$unit
        statistic = a$statistic
        output = a$output
        equationText = a$equation_text
        parameter_datanames = map(a$parameters, "dataname")
        parameter_types = map(a$parameters, "type")
        parameter_units = map(a$parameters, "unit")
        parameter_data = map(a$parameters, "data")
        scalars = parameter_datanames[parameter_types == "scalar"]
        constants = parameter_datanames[parameter_types == "constant"]
        
        box(width = 12,
            title = paste0(name, " - ", category),
            collapsible = T,
            collapsed = T,
            ## choose variable to show up on x-axis
            renderUI(
              selectInput("FB_x", "choose parameter:", unlist(parameter_datanames))
            ),
            ## select other variables
            ## for scalars, a slideInput UI should be made
            ## for constants, a selectInput should be made (or radiobuttons)
            FB_params = parameter_datanames[-input$FB_x],
            FB_scalars = FB_params[FB_params %>% scalars],
            FB_constants = FB_params[FB_params %>% constants],
            
            # lapply(FB_constants, renderUI, selectInput(.......)),
            # lapply(FB_scalars, renderUI, sliderInput(........))
          
            renderPlot(
              # X = seq(parameter_data$),  # define X variable
              X = seq(1,10,by = 0.1),
              Y = eval(parse(text = equationText)),
              plot(X,Y, type = "l")
            )
        )
      }
      
      
    })
    tagList(b)
  })
  
  
  
  # working backup
  # output$rulesBoxes <- renderUI({
  #   req(actual_knowledge_rules_df())
  #   ruleList <- split(actual_knowledge_rules_df(), actual_knowledge_rules_df()$name)
  # 
  #   b <- lapply(ruleList, function(a) {
  #     x = 1:100
  #     box(
  #       title = paste0(a$name, " - ", a$type),
  #       renderPlot(plot(x = x, y = x^2)),
  #       collapsible = T,
  #       collapsed = T
  #     )
  #   })
  #   tagList(b)
  # })
  
  
  
  # output$AE_summary <- renderText({
  #     get_element_species(actual_ae) %>% xml2::xml_find_first("LatName") %>%xml2::as_list() %>% unlist()
  # })
  
  
  ##==== download data prep ====
  
  
  
  ##=downloads======================
  
  
  
  # output$zwaarden_map <- downloadHandler(
  #   filename = function(){paste(input$zwaardenRuimteJaar_ui, "_", input$zwaardenRuimteParameter_ui, "_", input$zwaardenRuimtePtype_ui, ".html", sep = "")},
  #   content = function(file){
  #     saveWidget(widget = zWaardenLocatiesDownload(), file = file)
  #   }
  # )
  
  # output$downloadHuidigAbiotiek <- downloadHandler(
  #   filename = function(){paste(gsub(".txt", "", input$huidigInvoerSoortenBestand), ".csv", sep = "")},
  #   content = function(file){ write.table(downloadAbiotiekHuidigRuw(), file, sep = ";", dec = ".", row.names = FALSE)
  #     write.csv},
  #   contentType = "text/csv"
  #   )
  
  
}

shinyApp(ui, server)
