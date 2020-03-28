#####
# shiny01_appPlot
# Creation date: 26/03/2020
# Version date: 28/03/2020
# Author: Leonardo Stincone
# R 3.6.3
#####

# Libraries ####
library(tidyverse)
library(forcats)
library(RCurl)
library(rgdal)
library(plotly)
library(leaflet)
library(ggdark)
library(RColorBrewer)
library(viridis)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)

# Data wrangling ####
# Set plot theme
# theme_set(dark_theme_gray())
theme_set(theme_bw())

# Prepare data ####

# Read data
url <- getURL("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
covid_prov <- read_csv(url, na = "")

# Remove summary rows
covid_prov <- covid_prov %>% 
  filter(!is.na(sigla_provincia)) %>% 
  rename(casi_tot = totale_casi)

# Mutate date-time into date
covid_prov <- covid_prov %>% 
  mutate(data = as.Date(data))


# Create daily cases
covid_prov <- covid_prov %>% 
  left_join(covid_prov %>% 
              select(data, sigla_provincia, casi_tot_previous_day = casi_tot) %>% 
              mutate(data = data + 1),
            by = c("data", "sigla_provincia")) %>% 
  mutate(casi_tot_previous_day = ifelse(is.na(casi_tot_previous_day), 0, casi_tot_previous_day),
         casi_new = casi_tot - casi_tot_previous_day) %>% 
  select(-casi_tot_previous_day)


# Data on population
pop_prov <- read_csv2("data/pop_prov_italia.csv",
                      na = "")

covid_prov <- covid_prov %>% 
  left_join(pop_prov %>% 
              select(sigla, popolazione, superficie, densita),
            by = c("sigla_provincia" = "sigla")) %>% 
  mutate(casi_tot_onpop = casi_tot / popolazione * 1e5,
         casi_new_onpop = casi_new / popolazione * 1e5)

covid_prov <- covid_prov %>% 
  mutate(popup = str_c("<b>", denominazione_provincia, "</b>",
                       "<br>Data: ", data,
                       "<br>Totale casi: ", casi_tot,
                       "<br>Totale casi / pop: ", round(casi_tot_onpop, 2), " /100 000",
                       "<br>Nuovi casi: ", casi_new,
                       "<br>Nuovi casi: / pop: ", round(casi_new_onpop, 2), " /100 000"))

province <- sort(unique(covid_prov$denominazione_provincia))

n <- 5

province_init <- covid_prov %>% 
  filter(data == max(data)) %>% 
  top_n(n = n, wt = casi_tot) %>% 
  arrange(-casi_tot) %>% 
  .$denominazione_provincia

regioni <- sort(unique(covid_prov$denominazione_regione))



# User interface ####
ui <- fluidPage(
  # theme = shinytheme("darkly"),
  titlePanel("Diffusione covid-19 nelle province italiane"),
  sidebarLayout(
    sidebarPanel(
      tags$h4("Seleziona province"),
      inputPanel(
      # wellPanel(
      # flowLayout(
        pickerInput(
          inputId = "province_displayed",
          label = "Seleziona province", 
          choices = province,
          selected = province_init,
          multiple = TRUE,
          options = list(
            `live-search` = TRUE)),
        pickerInput(
          inputId = "regione_displayed",
          label = "Seleziona regione", 
          choices = regioni,
          # selected = "",
          multiple = TRUE,
          options = list(
            `live-search` = TRUE)
      ),
      actionButton("deselect_button",
                  "Deseleziona tutte le province"),
      actionButton("select_top_n_button",
                   "Seleziona le prime n province")#,
      # materialSwitch("multiple_region_switch",
      #                "Seleziona più regioni",
      #                value = TRUE,
      #                right = TRUE),
    # )),
    ),
    br(),
    tags$h4("Opzioni"),
    inputPanel(
      tags$div(title = "Seleziona questo checkbox per vedere il numero di casi ogni 100000 abitanti al posto del numero di casi totali",
        awesomeCheckbox(
          inputId = "rescalePop_check",
          label = "Riscala sulla popolazione", 
          value = FALSE#,
          # status = "danger"
        )
      )
    )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Casi cumulati",
                 plotly::plotlyOutput("plot_cases_tot")),
        tabPanel("Casi giornalieri",
                 plotly::plotlyOutput("plot_cases_new"))
      )
    )
  )
)


# Server ####
server <- function(input, output, session){
  
  # Plot total cases ####
  output$plot_cases_tot <- plotly::renderPlotly({
    
    if(is.null(input$province_displayed)){
      p <- tibble(data = 0, casi_tot = 0,
             denominazione_provincia = "", popup = "") %>% 
        ggplot(aes(x = data, y = casi_tot,
                   color = denominazione_provincia,
                   text = popup,
                   group = 1))
      
      ggplotly(p)
    } else {
      if(input$rescalePop_check){
        p <- covid_prov %>% 
          filter(denominazione_provincia %in% input$province_displayed) %>% 
          mutate(denominazione_provincia = fct_relevel(denominazione_provincia,
                                                       province)) %>% 
          ggplot(aes(x = data, y = casi_tot_onpop,
                     color = denominazione_provincia,
                     text = popup,
                     group = 1)) +
          geom_line() +
          geom_point() +
          labs(color = "Provincia",
               y = "Casi ogni 100 000 abitanti")
      } else {
        p <- covid_prov %>% 
          filter(denominazione_provincia %in% input$province_displayed) %>% 
          mutate(denominazione_provincia = fct_relevel(denominazione_provincia,
                                                       province)) %>% 
          ggplot(aes(x = data, y = casi_tot,
                     color = denominazione_provincia,
                     text = popup,
                     group = 1)) +
          geom_line() +
          geom_point() +
          labs(color = "Provincia",
               y = "Casi totali")
      }
    
      ggplotly(p, tooltip = "text")  
    }
    
  })
  
  
  # Plot new cases ####
  output$plot_cases_new <- plotly::renderPlotly({
    
    if(is.null(input$province_displayed)){
      p <- tibble(data = 0, casi_tot = 0,
                  denominazione_provincia = "", popup = "") %>% 
        ggplot(aes(x = data, y = casi_tot,
                   color = denominazione_provincia,
                   text = popup,
                   group = 1))
      
      ggplotly(p)
    } else {
      if(input$rescalePop_check){
        p <- covid_prov %>% 
          filter(denominazione_provincia %in% input$province_displayed) %>% 
          mutate(denominazione_provincia = fct_relevel(denominazione_provincia,
                                                       province)) %>% 
          ggplot(aes(x = data, y = casi_new_onpop,
                     color = denominazione_provincia,
                     text = popup,
                     group = 1)) +
          geom_line() +
          geom_point() +
          labs(color = "Provincia",
               y = "Casi ogni 100 000 abitanti")
      } else {
        p <- covid_prov %>% 
          filter(denominazione_provincia %in% input$province_displayed) %>% 
          mutate(denominazione_provincia = fct_relevel(denominazione_provincia,
                                                       province)) %>% 
          ggplot(aes(x = data, y = casi_new,
                     color = denominazione_provincia,
                     text = popup,
                     group = 1)) +
          geom_line() +
          geom_point() +
          labs(color = "Provincia",
               y = "Casi totali")
      }
      
      ggplotly(p, tooltip = "text")  
    }
    
  })
  
  
  # Choose region ####
  observeEvent(input$regione_displayed, {
    province_init <- covid_prov %>% 
      filter(denominazione_regione %in% input$regione_displayed) %>% 
      .$denominazione_provincia %>% 
      unique() %>% 
      sort()
    
    updatePickerInput(session = session, inputId = "province_displayed",
                      selected = province_init)
    
  }, ignoreInit = TRUE)
  
  
  # Deselect everything ####
  observeEvent(input$deselect_button, {
    updatePickerInput(session = session, inputId = "regione_displayed",
                      selected = "")
    updatePickerInput(session = session, inputId = "province_displayed",
                      selected = "")
  }, ignoreInit = TRUE)
  
  
  # Select one region at the time ####
  # print(input$multiple_region_switch)
  # observeEvent(input$multiple_region_switch, {
  #   updatePickerInput(session = session, inputId = "regione_displayed",
  #                     choices = input$regione_displayed[1],
  #                     multiple = input$multiple_region_switch)
  #   print(input$province_displayed)
  # }, ignoreInit = TRUE)
  
  # Select top n provinces ####
  observeEvent(input$select_top_n_button, {
    showModal(modalDialog(
      title = "Seleziona le prime n province",
      numericInput("top_n", "Scegli quante province vuoi visualizzare",
                   value = 5, min = 1, max = 20, step = 1),
      selectInput("criteria", "Seleziona il criterio",
                  choices = c("Casi totali",
                              "Casi totali / popolazione",
                              "Nuovi casi",
                              "Nuovi casi / popolazione"),
                  selected = ""),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("select_top_n_button_ok", "Ok")
        # modalButton("select_top_n_button_ok", "Ok")
      )
    ))
    # shinyalert(
    #   title = "Seleziona le prime n province",
    #   numericInput("top_n", "Scegli quante province vuoi visualizzare",
    #                value = 5, min = 1, max = 20, step = 1),
    #   easyClose = TRUE,
    #   footer = tagList(
    #     modalButton("Cancel"),
    #     actionButton("select_top_n_button_ok", "Ok")
    #     # modalButton("select_top_n_button_ok", "Ok")
    #   )
    # )
  })
  
  observeEvent(input$select_top_n_button_ok, {
    print(input$top_n)
    
    if(input$criteria == "Casi totali / popolazione"){
      province_toSelect <- covid_prov %>% 
        filter(data == max(data)) %>% 
        top_n(n = input$top_n, wt = casi_tot_onpop) %>%
        arrange(-casi_tot) %>% 
        .$denominazione_provincia  
    } else if(input$criteria == "Casi totali") {
      province_toSelect <- covid_prov %>% 
        filter(data == max(data)) %>% 
        top_n(n = input$top_n, wt = casi_tot) %>%
        arrange(-casi_tot) %>% 
        .$denominazione_provincia
    } else if(input$criteria == "Nuovi casi / popolazione") {
      province_toSelect <- covid_prov %>% 
        filter(data == max(data)) %>% 
        top_n(n = input$top_n, wt = casi_new_onpop) %>%
        arrange(-casi_tot) %>% 
        .$denominazione_provincia
    } else {
      province_toSelect <- covid_prov %>% 
        filter(data == max(data)) %>% 
        top_n(n = input$top_n, wt = casi_new) %>%
        arrange(-casi_tot) %>% 
        .$denominazione_provincia
    }
    
    updatePickerInput(session = session, inputId = "regione_displayed",
                      selected = "")
    updatePickerInput(session = session, inputId = "province_displayed",
                      selected = province_toSelect)
    
    removeModal()
  }, ignoreInit = TRUE)
  
  
}


# App ####
shinyApp(ui = ui, server = server)










