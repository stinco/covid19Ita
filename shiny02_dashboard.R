#####
# shiny02_dashboard
# Creation date: 28/03/2020
# Version date: 28/03/2020
# Author: Leonardo Stincone
# R 3.6.3
#####

# Libraries ####
library(tidyverse)
library(forcats)
library(RCurl)
library(rgdal)
library(rmapshaper)
library(plotly)
library(leaflet)
library(ggdark)
library(RColorBrewer)
library(viridis)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
# library(fontawesome)

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


# # Read map data
# map_prov <- readOGR(dsn = "data/Limiti01012020_g/ProvCM01012020_g",
#                     layer = "ProvCM01012020_g_WGS84",
#                     use_iconv = TRUE, encoding = "UTF-8")
# 
# map_prov@data <- map_prov@data %>% 
#   mutate_if(.predicate = is.factor,
#             .funs = as.character)
# 
# # Change reference system
# map_prov <- spTransform(map_prov, CRS("+init=epsg:4326"))
# 
# map_prov <- ms_simplify(map_prov, keep = .01)
# 
# save(map_prov, file = "data/map_prov_simply.RData")
load("data/map_prov_simply.RData")



# Ad info on covid19
map_prov_data <- map_prov@data


# Create funcion for maximum value on map color scale
ceil10 <- function(x){
  10^ceiling(log10(max(x)))
}



# User interface ####
ui <- dashboardPage(
  dashboardHeader(title = "Diffusione del Covid-19 nelle province italiane", titleWidth = 400,
                  tags$li(class = "dropdown", tags$a(href = "https://www.linkedin.com/in/leonardo-stincone/",
                                                     icon("linkedin"), "My Profile", target = "_blank")
                          ),
                  tags$li(class = "dropdown", tags$a(href = "https://github.com/stinco/covid19ItaProv",
                                                     icon("github"), "Source Code", target ="_blank"))
  ),
  dashboardSidebar({
    sidebarMenu(
      menuItem("Grafici", tabName = "tab_plots", icon = icon("chart-line", lib = "font-awesome")),
      menuItem("Mappe", tabName = "tab_maps", icon = icon("map", lib = "font-awesome")),
      menuItem("Tabelle", tabName = "tab_tables", icon = icon("table", lib = "font-awesome"))
    )
  }),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "headerStyle.css.css")
    ),
    {
    # Tab plots ####
    tabItems(
      tabItem(tabName = "tab_plots",
              fluidRow(
                shinyjs::useShinyjs(),
                # box(
                  # sidebarLayout(
                  # column(
                    box(
                      width = 5,
                      tags$h4("Seleziona province"),
                      # inputPanel(
                      wellPanel(
                        # verticalLayout(
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
                            # tags$br(),
                            actionButton("deselect_button",
                                         "Deseleziona tutte le province"),
                            actionButton("select_top_n_button",
                                         "Seleziona le prime n province")#,
                        # materialSwitch("multiple_region_switch",
                        #                "Seleziona più regioni",
                        #                value = TRUE,
                        #                right = TRUE),
                        # )),
                      # )
                      # )
                      ),
                      tags$br(),
                      tags$h4("Opzioni"),
                      # inputPanel(
                      wellPanel(
                        # width = 5,
                        tags$b("Seleziona se vuoi riscalare i dati sulla popolazione"),
                        tags$br(),
                        tags$div(title = "Seleziona questo checkbox per visualizzare il numero di casi ogni 100000 abitanti al posto del numero di casi totali",
                                 awesomeCheckbox(
                                   inputId = "rescalePop_check",
                                   label = "Riscala sulla popolazione", 
                                   value = FALSE#,
                                  )
                        ),
                        tags$br(),
                        tags$div(title = "La curva approssimante è calcolata tramite una Local Polynomial Regression (LOESS) calcolata sui dati empirici",
                          checkboxGroupButtons(
                            inputId = "element_plot_check",
                            label = "Seleziona gli elementi che vuoi visualizzare",
                            choices = c("Dati osservati", 
                                        "Curva approssimante"),
                            selected = "Dati osservati",
                            # direction = "vertical",
                            checkIcon = list(
                              yes = tags$i(class = "fa fa-check-square", 
                                          style = "color: steelblue"),
                              no = tags$i(class = "fa fa-square-o", 
                                          style = "color: steelblue"))
                        )),
                        tags$br(),
                        tags$b("Seleziona se vuoi visualizzare gli intervalli di confidenza per la curva approssimante"),
                        tags$br(),
                        tags$div(title = "Le opzioni sulla curva approssimante sono disponibili solo se è selezionata la curva approssimante",
                          awesomeCheckbox(
                            inputId = "showSE_check",
                            label = "Intervalli di confidenza", 
                            value = FALSE#,
                          ),
                          tags$br(),
                          sliderInput("span_slider", "Seleziona il livello di smoothness della curva approssimante",
                                      min = .1, max = 2, step = .1,
                                      value = .8
                          )
                        )
                      )
                    ),
                    tags$head(tags$script('
                      // Define function to set height of "map" and "map_container"
                      setHeight = function() {
                        var window_height = $(window).height();
                        var header_height = $(".main-header").height();
                
                        var boxHeight = window_height - header_height - 50;
                
                        $("#map_container").height(boxHeight);
                        $("#map").height(boxHeight - 20);
                      };
                
                      // Set input$box_height when the connection is established
                      $(document).on("shiny:connected", function(event) {
                        setHeight();
                      });
                
                      // Refresh the box height on every window resize event    
                      $(window).on("resize", function(){
                        setHeight();
                      });
                    ')),
                    box(#fid = "map_container",
                        width = 7,
                        tabsetPanel(
                          tabPanel("Casi cumulati",
                                   plotly::plotlyOutput("plot_cases_tot",
                                                        width = "100%", height = "100%")),
                          tabPanel("Casi giornalieri",
                                  plotly::plotlyOutput("plot_cases_new",
                                                       width = "100%", height = "100%"))
                        )
                    )
                  # )
                # )
              )
                
      ),
      # Tab tables ####
      tabItem(tabName = "tab_tables",
              fluidRow(
                # box(
                  # width = 8, height = 6,
                  DT::dataTableOutput("table_provinces")
                # )
              )
      ),
      # Tab maps ####
      tabItem(tabName = "tab_maps",
              fluidRow(
                box(width = 5,
                    tags$h4("Opzioni"),
                    wellPanel(
                      tags$div(title = "Premi il tasto play per visualizzare l'evoluzione nel tempo",
                        # sliderInput("day_slider", "Giorno di riferimento",
                        #             min = min(covid_prov$data), max = max(covid_prov$data), step = 1,
                        #             value = max(covid_prov$data),
                        #             animate = T
                        # )
                        uiOutput("day_slider")
                      ),
                      tags$b("Seleziona se vuoi riscalare i dati sulla popolazione"),
                      tags$br(),
                      tags$div(title = "Seleziona questo checkbox per visualizzare il numero di casi ogni 100000 abitanti al posto del numero di casi totali",
                               awesomeCheckbox(
                                 inputId = "rescalePop_map_check",
                                 label = "Riscala sulla popolazione", 
                                 value = FALSE#,
                               )
                      ),
                      radioGroupButtons(
                        inputId = "variable_map_radio",
                        label = "Seleziona che dati vuoi visualizzare",
                        choices = c("Casi cumulati totali", "Casi per periodo"),
                        individual = TRUE,
                        checkIcon = list(
                          yes = tags$i(class = "fa fa-circle", 
                                       style = "color: steelblue"),
                          no = tags$i(class = "fa fa-circle-o", 
                                      style = "color: steelblue"))
                      )
                    )
                ),
                box(id = "map_container",
                    width = 7,
                    leaflet::leafletOutput("map_leaflet",
                                           width = "100%", height = "100%")
                )
              )
      )
    )
  
  })
)




# Server ####
# server <- function(input, output, session){}
server <- function(input, output, session){
  
  covid_prov_filtered <- reactive({
    covid_prov %>% 
      filter(denominazione_provincia %in% input$province_displayed)
  })
  
  
  # Plot total cases ####
  plot_cases_tot <- reactive({
    if(input$rescalePop_check){
      covid_prov_filtered() %>% 
        ggplot(aes(x = data, y = casi_tot_onpop,
                   color = denominazione_provincia,
                   text = popup,
                   group = denominazione_provincia)) +
        labs(color = "Provincia",
             y = "Casi ogni 100 000 abitanti")
    } else {
      covid_prov_filtered() %>% 
        ggplot(aes(x = data, y = casi_tot,
                   color = denominazione_provincia,
                   text = popup,
                   group = denominazione_provincia)) +
        labs(color = "Provincia",
             y = "Casi")
    }
  })
  
  output$plot_cases_tot <- plotly::renderPlotly({
    
    if(is.null(input$province_displayed)){
      p <- tibble(data = 0, casi_tot = 0,
                  denominazione_provincia = "", popup = "") %>% 
        ggplot(aes(x = data, y = casi_tot,
                   color = denominazione_provincia,
                   text = popup,
                   group = denominazione_provincia))
      
      if(input$rescalePop_check){
        p <- p +
          labs(color = "Provincia",
               y = "Casi ogni 100 000 abitanti")
      } else {
        p <- p +
          labs(color = "Provincia",
               y = "Casi")
      }
      
    } else {
      
      p <- plot_cases_tot()
      
      if("Dati osservati" %in% input$element_plot_check){
        p <- p +
          geom_line() +
          geom_point()
      }
      
      if("Curva approssimante" %in% input$element_plot_check){
        p <- p +
          geom_smooth(method = "loess",
                      formula = "y ~ x",
                      se = input$showSE_check,
                      span = input$span_slider)
      }
      
    }
    
    ggplotly(p, tooltip = "text")
    
  })
  
  
  # Plot new cases ####
  plot_cases_new <- reactive({
    if(input$rescalePop_check){
      covid_prov_filtered() %>% 
        ggplot(aes(x = data, y = casi_new_onpop,
                   color = denominazione_provincia,
                   text = popup,
                   group = denominazione_provincia)) +
        labs(color = "Provincia",
             y = "Casi ogni 100 000 abitanti")
    } else {
      covid_prov_filtered() %>% 
        ggplot(aes(x = data, y = casi_new,
                   color = denominazione_provincia,
                   text = popup,
                   group = denominazione_provincia)) +
        labs(color = "Provincia",
             y = "Casi")
    }
  })
  
  
  output$plot_cases_new <- plotly::renderPlotly({
    
    if(is.null(input$province_displayed)){
      p <- tibble(data = 0, casi_tot = 0,
                  denominazione_provincia = "", popup = "") %>% 
        ggplot(aes(x = data, y = casi_tot,
                   color = denominazione_provincia,
                   text = popup,
                   group = denominazione_provincia))
      
      if(input$rescalePop_check){
        p <- p +
          labs(color = "Provincia",
               y = "Casi ogni 100 000 abitanti")
      } else {
        p <- p +
          labs(color = "Provincia",
               y = "Casi")
      }
    } else {
      
      p <- plot_cases_new()
      
      if("Dati osservati" %in% input$element_plot_check){
        p <- p +
          geom_line() +
          geom_point()
      }
      
      if("Curva approssimante" %in% input$element_plot_check){
        p <- p +
          geom_smooth(method = "loess",
                      formula = "y ~ x",
                      se = input$showSE_check,
                      span = input$span_slider)
      }
    
    }
    
    ggplotly(p, tooltip = "text")
    
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
      )
    ))
  })
  
  observeEvent(input$select_top_n_button_ok, {
    # print(input$top_n)
    
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
  
  
  # Disable input elements for Confidence intervals ####
  observeEvent(input$element_plot_check, {
    if("Curva approssimante" %in% input$element_plot_check){
      shinyjs::enable("showSE_check")
      shinyjs::enable("span_slider")
    } else {
      shinyjs::disable("showSE_check")
      shinyjs::disable("span_slider")
    }
  })
  
  
  # Create table ####
  output$table_provinces <- DT::renderDataTable({
    covid_prov %>% 
      select(data,
             denominazione_regione,
             denominazione_provincia,
             casi_tot, casi_tot_onpop,
             casi_new, casi_new_onpop
             ) %>% 
      mutate(casi_tot_onpop = round(casi_tot_onpop, 2),
             casi_new_onpop = round(casi_new_onpop, 2)) %>% 
      arrange(desc(data), desc(casi_tot))
  })
  
  
  # Create leaflet map ####
  pal_casi_tot <- reactive({colorNumeric(
    # palette = "RdYlGn",
    palette = c("#FFFFFFFF", rev(inferno(256))),
    domain = c(0, log(ceil10(max(covid_prov$casi_tot + 1)))),
    reverse = F
  )})
  
  pal_casi_tot_onpop <- reactive({colorNumeric(
    # palette = "RdYlGn",
    palette = c("#FFFFFFFF", rev(inferno(256))),
    domain = c(0, log(ceil10(max(covid_prov$casi_tot_onpop + 1)))),
    reverse = F
  )})
  
  
  output$map_leaflet <- leaflet::renderLeaflet({
    map_prov@data <- map_prov_data %>% 
      left_join(covid_prov %>% 
                  filter(data == max(data)),
                by = c("SIGLA" = "sigla_provincia"))
    
    leaflet(data = map_prov) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = 12, lat = 42, zoom = 5.5) %>%
      addPolygons(layerId = ~SIGLA,
                  color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.8,
                  fillColor = ~pal_casi_tot()(log(casi_tot + 1)),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = ~popup) %>%
      addLegend(position = "bottomright",
                pal = pal_casi_tot(), opacity = 1,
                bins = log(10^(0:log10(ceil10(max(covid_prov$casi_tot + 1))))),
                value = log(1:10^(log10(ceil10(max(covid_prov$casi_tot + 1))))),
                data = log(10^(0:log10(ceil10(max(covid_prov$casi_tot + 1))))),
                labFormat = labelFormat(transform = exp))
  })
  
  observeEvent(input$rescalePop_map_check, {
    if(input$rescalePop_map_check){
      leafletProxy("map_leaflet", data = map_prov) %>%
        clearControls() %>% 
        addLegend(position = "bottomright",
                  pal = pal_casi_tot_onpop(), opacity = 1,
                  bins = log(10^(0:log10(ceil10(max(covid_prov$casi_tot_onpop + 1))))),
                  value = log(1:10^(log10(ceil10(max(covid_prov$casi_tot_onpop + 1))))),
                  data = log(10^(0:log10(ceil10(max(covid_prov$casi_tot_onpop + 1))))),
                  labFormat = labelFormat(transform = exp,
                                          suffix = " /100 000"))
    } else {
      leafletProxy("map_leaflet", data = map_prov) %>%
        clearControls() %>% 
        addLegend(position = "bottomright",
                  pal = pal_casi_tot(), opacity = 1,
                  bins = log(10^(0:log10(ceil10(max(covid_prov$casi_tot + 1))))),
                  value = log(1:10^(log10(ceil10(max(covid_prov$casi_tot + 1))))),
                  data = log(10^(0:log10(ceil10(max(covid_prov$casi_tot + 1))))),
                  labFormat = labelFormat(transform = exp))
    }
  })
  
  
  observe({
    
    # print("Day slider")
    # print(input$day_slider)
    # print("Day slider 1")
    # print(input$day_slider1)
    # print("Day slider 2")
    # print(input$day_slider2)
    
    
    
    # if(length(input$day_slider == 1)){
    if(input$variable_map_radio == "Casi cumulati totali" & !is.null(input$day_slider1)){
      # Visualize total cumulated cases
      map_prov@data <- map_prov_data %>%
        left_join(covid_prov %>%
                    filter(data == input$day_slider1),
                  by = c("SIGLA" = "sigla_provincia"))

      if(input$rescalePop_map_check){
        leafletProxy("map_leaflet", data = map_prov) %>%
          addPolygons(layerId = ~SIGLA,
                      color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.8,
                      fillColor = ~pal_casi_tot_onpop()(log(casi_tot_onpop + 1)),
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE),
                      popup = ~popup)
      } else {
        leafletProxy("map_leaflet", data = map_prov) %>%
          addPolygons(layerId = ~SIGLA,
                      color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.8,
                      fillColor = ~pal_casi_tot()(log(casi_tot + 1)),
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE),
                      popup = ~popup)
      }
    } else if(!is.null(input$day_slider2)) {
      #  Visualize cases over period
      
      map_prov@data <- map_prov_data %>%
        left_join(covid_prov %>%
                    filter(data >= input$day_slider2[1] &
                             data <= input$day_slider2[2]) %>% 
                    group_by(sigla_provincia) %>% 
                    summarize(denominazione_provincia = max(denominazione_provincia),
                              casi_tot = sum(casi_new),
                              casi_tot_onpop = sum(casi_new_onpop)) %>% 
                    mutate(casi_tot = ifelse(casi_tot < 0, 0, casi_tot),
                           casi_tot_onpop = ifelse(casi_tot_onpop < 0, 0, casi_tot_onpop),
                           popup = str_c("<b>", denominazione_provincia, "</b>",
                                         "<br>Periodo: ", input$day_slider2[1], ", ", input$day_slider2[2],
                                         "<br>Totale casi nel periodo: ", casi_tot,
                                         "<br>Totale casi nel periodo / pop: ", round(casi_tot_onpop, 2), " /100 000")),
                  by = c("SIGLA" = "sigla_provincia"))
      
      if(input$rescalePop_map_check){
        leafletProxy("map_leaflet", data = map_prov) %>%
          addPolygons(layerId = ~SIGLA,
                      color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.8,
                      fillColor = ~pal_casi_tot_onpop()(log(casi_tot_onpop + 1)),
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE),
                      popup = ~popup)
      } else {
        leafletProxy("map_leaflet", data = map_prov) %>%
          addPolygons(layerId = ~SIGLA,
                      color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.8,
                      fillColor = ~pal_casi_tot()(log(casi_tot + 1)),
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE),
                      popup = ~popup)
      }
    }
    
    
    
  })
  
  
  output$day_slider <- renderUI({
    if(input$variable_map_radio == "Casi cumulati totali"){
      sliderInput("day_slider1", "Giorno di riferimento",
                  min = min(covid_prov$data), max = max(covid_prov$data), step = 1,
                  value = max(covid_prov$data),
                  animate = T)
    } else {
      sliderInput("day_slider2", "Periodo di riferimento",
                  min = min(covid_prov$data), max = max(covid_prov$data), step = 1,
                  value = c(max(covid_prov$data) - 6, max(covid_prov$data)),
                  animate = T)
    }
  })
  
    
  
}


# App ####
shinyApp(ui = ui, server = server)










