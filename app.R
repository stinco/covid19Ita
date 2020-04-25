#####
# shiny02_dashboard
# Creation date: 28/03/2020
# Version date: 25/04/2020
# Author: Leonardo Stincone
# R 3.6.3
#####

# Libraries ####
library(tidyverse)
library(forcats)
library(RCurl)
library(rgdal)
library(rmapshaper)
library(plotly)            # For interactive plots
library(ggiraph)           # For interactive plots
library(leaflet)
library(ggdark)
library(RColorBrewer)
library(viridis)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
# library(fontawesome)
library(mgcv)             # For GAMs

# Data wrangling ####
# Set plot theme
# theme_set(dark_theme_gray())
theme_set(theme_bw(base_family = "sans"))

# Prepare provinces data ####

# Read data
url_prov <- getURL("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
covid_prov <- read_csv(url_prov, na = "",
                       col_types = list(data = col_datetime(format = ""),
                                        stato = col_character(),
                                        codice_regione = col_character(),
                                        denominazione_regione = col_character(),
                                        codice_provincia = col_character(),
                                        denominazione_provincia = col_character(),
                                        sigla_provincia = col_character(),
                                        lat = col_double(),
                                        long = col_double(),
                                        totale_casi = col_double(),
                                        note_it = col_character(),
                                        note_en = col_character()))
# covid_prov <- read_csv("www/data/dpc-covid19-ita-province.csv", na = "")

# Remove summary rows
covid_prov <- covid_prov %>% 
  filter(!is.na(sigla_provincia)) %>% 
  rename(casi_tot = totale_casi)

# Mutate date-time into date
covid_prov <- covid_prov %>% 
  mutate(data = as.Date(data)) %>% 
  # fix the name of the province Trientino Alto Adige
  mutate(denominazione_regione = if_else(codice_regione == "04",
                                        "Trentino Alto Adige",
                                        denominazione_regione))

# Create daily cases
covid_prov <- covid_prov %>% 
  left_join(covid_prov %>% 
              select(data, sigla_provincia, casi_tot_previous_day = casi_tot) %>% 
              mutate(data = data + 1),
            by = c("data", "sigla_provincia")) %>% 
  mutate(casi_tot_previous_day = if_else(is.na(casi_tot_previous_day), 0, casi_tot_previous_day),
         casi_new = casi_tot - casi_tot_previous_day) %>%
  # Fix the problem of negative data in daily cases
  mutate(casi_new = if_else(casi_new < 0, 0, casi_new)) %>% 
  select(-casi_tot_previous_day)


# Data on population
pop_prov <- read_csv2("www/data/pop_prov_italia.csv",
                      na = "",
                      col_types = list(sigla = col_character(),
                                       provincia = col_character(),
                                       regione = col_character(),
                                       popolazione = col_number(),
                                       superficie = col_number(),
                                       densita = col_number(),
                                       numero_comuni = col_double()))

# TO DO: Unisci Trento e Bolzano

covid_prov <- covid_prov %>% 
  left_join(pop_prov %>% 
              select(sigla, popolazione, superficie, densita),
            by = c("sigla_provincia" = "sigla")) %>% 
  mutate(casi_tot_onpop = casi_tot / popolazione * 1e5,
         casi_new_onpop = casi_new / popolazione * 1e5)

covid_prov <- covid_prov %>% 
  mutate(popup_casi = str_c("<b>", denominazione_provincia, "</b>",
                       "<br>Data: ", data,
                       "<br>Totale casi: ", casi_tot,
                       "<br>Totale casi / pop: ", round(casi_tot_onpop, 2), " /100 000",
                       "<br>Nuovi casi: ", casi_new,
                       "<br>Nuovi casi: / pop: ", round(casi_new_onpop, 2), " /100 000"))

split_tibble <- function(tibble, column = 'col') {
  tibble %>%
    split(., .[,column]) %>%
    lapply(., function(x) x[,setdiff(names(x),column)])
}

province_list <- covid_prov %>% 
  select(denominazione_regione, denominazione_provincia) %>% 
  unique() %>% 
  split_tibble(column = "denominazione_regione") %>% 
  lapply(function(x){x[["denominazione_provincia"]]})
  

province <- sort(unique(covid_prov$denominazione_provincia))

n <- 5

province_init <- covid_prov %>% 
  filter(data == max(data)) %>% 
  top_n(n = n, wt = casi_tot) %>% 
  arrange(-casi_tot) %>% 
  .$denominazione_provincia

regioni <- sort(unique(covid_prov$denominazione_regione))


# Prepare regions data ####

# Read data
url_reg <- getURL("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
covid_reg <- read_csv(url_reg, na = "",
                      col_types = list(data = col_datetime(format = ""),
                                       stato = col_character(),
                                       codice_regione = col_character(),
                                       denominazione_regione = col_character(),
                                       lat = col_double(),
                                       long = col_double(),
                                       ricoverati_con_sintomi = col_double(),
                                       terapia_intensiva = col_double(),
                                       totale_ospedalizzati = col_double(),
                                       isolamento_domiciliare = col_double(),
                                       totale_positivi = col_double(),
                                       variazione_totale_positivi = col_double(),
                                       nuovi_positivi = col_double(),
                                       dimessi_guariti = col_double(),
                                       deceduti = col_double(),
                                       totale_casi = col_double(),
                                       tamponi = col_double(),
                                       casi_testati = col_double(),
                                       note_it = col_character(),
                                       note_en = col_character()))
# covid_reg <- read_csv("www/data/dpc-covid19-ita-regioni.csv", na = "")

# Mutate date-time into date
covid_reg <- covid_reg %>% 
  mutate(data = as.Date(data)) %>% 
  select(data, stato, codice_regione, denominazione_regione, lat, long,
         casi_tot = totale_casi,
         deced_tot = deceduti,
         tamponi_tot = tamponi,
         positivi_tot = totale_positivi,
         osped_tot = totale_ospedalizzati,
         terap_tot = terapia_intensiva
         ) %>% 
  # fix the name of the province Trientino Alto Adige
  mutate(denominazione_regione = if_else(codice_regione == "04",
                                        "Trentino Alto Adige",
                                        denominazione_regione)) %>% 
  group_by(data, stato, codice_regione, denominazione_regione) %>% 
  summarize(casi_tot = sum(casi_tot),
            deced_tot = sum(deced_tot),
            tamponi_tot = sum(tamponi_tot),
            positivi_tot = sum(positivi_tot),
            osped_tot = sum(osped_tot),
            terap_tot = sum(terap_tot)) %>% 
  ungroup()


# Create national data
covid_ita <- covid_reg %>% 
  group_by(data, stato) %>% 
  summarize(casi_tot = sum(casi_tot),
            deced_tot = sum(deced_tot),
            tamponi_tot = sum(tamponi_tot),
            positivi_tot = sum(positivi_tot),
            osped_tot = sum(osped_tot),
            terap_tot = sum(terap_tot)) %>%  
  ungroup()


# Create daily cases for regional data
covid_reg <- covid_reg %>% 
  left_join(covid_reg %>% 
              select(data, codice_regione,
                     casi_tot_previous_day = casi_tot,
                     deced_tot_previous_day = deced_tot,
                     tamponi_tot_previous_day = tamponi_tot,
                     positivi_tot_previous_day = positivi_tot,
                     osped_tot_previous_day = osped_tot,
                     terap_tot_previous_day = terap_tot) %>% 
              mutate(data = data + 1),
            by = c("data", "codice_regione")) %>% 
  mutate(casi_tot_previous_day = if_else(is.na(casi_tot_previous_day), 0, casi_tot_previous_day),
         deced_tot_previous_day = if_else(is.na(deced_tot_previous_day), 0, deced_tot_previous_day),
         tamponi_tot_previous_day = if_else(is.na(tamponi_tot_previous_day), 0, tamponi_tot_previous_day),
         positivi_tot_previous_day = if_else(is.na(positivi_tot_previous_day), 0, positivi_tot_previous_day),
         osped_tot_previous_day = if_else(is.na(osped_tot_previous_day), 0, osped_tot_previous_day),
         terap_tot_previous_day = if_else(is.na(terap_tot_previous_day), 0, terap_tot_previous_day)
         ) %>% 
  mutate(casi_new = casi_tot - casi_tot_previous_day,
         deced_new = deced_tot - deced_tot_previous_day,
         tamponi_new = tamponi_tot - tamponi_tot_previous_day,
         positivi_new = positivi_tot - positivi_tot_previous_day,
         osped_new = osped_tot - osped_tot_previous_day,
         terap_new = terap_tot - terap_tot_previous_day
         ) %>%
  # Fix the problem of negative data in daily cases
  mutate(casi_new = if_else(casi_new < 0, 0, casi_new),
         deced_new = if_else(deced_new < 0, 0, deced_new),
         tamponi_new = if_else(tamponi_new < 0, 0, tamponi_new)
         ) %>% 
  select(-matches("_previous_day"))


covid_reg <- covid_reg %>% 
  left_join(covid_prov %>% 
              select(codice_regione, codice_provincia, popolazione) %>% 
              unique() %>% 
              group_by(codice_regione) %>% 
              summarize(popolazione = sum(popolazione)) %>% 
              ungroup(),
            by = "codice_regione") %>% 
  mutate(
    casi_tot_onpop = casi_tot / popolazione * 1e5,
    deced_tot_onpop = deced_tot / popolazione * 1e5,
    tamponi_tot_onpop = tamponi_tot / popolazione * 1e5,
    positivi_tot_onpop = positivi_tot / popolazione * 1e5,
    osped_tot_onpop = osped_tot / popolazione * 1e5,
    terap_tot_onpop = terap_tot / popolazione * 1e5,
    casi_new_onpop = casi_new / popolazione * 1e5,
    deced_new_onpop = deced_new / popolazione * 1e5,
    tamponi_new_onpop = tamponi_new / popolazione * 1e5,
    positivi_new_onpop = positivi_new / popolazione * 1e5,
    osped_new_onpop = osped_new / popolazione * 1e5,
    terap_new_onpop = terap_new / popolazione * 1e5
  ) %>% 
  # Add tooltip
  mutate(popup_casi = str_c("<b>", denominazione_regione, "</b>",
                            "<br>Data: ", data,
                            "<br>Totale casi: ", casi_tot,
                            "<br>Totale casi / pop: ", round(casi_tot_onpop, 2), " /100 000",
                            "<br>Nuovi casi: ", casi_new,
                            "<br>Nuovi casi: / pop: ", round(casi_new_onpop, 2), " /100 000"),
         popup_deced = str_c("<b>", denominazione_regione, "</b>",
                             "<br>Data: ", data,
                             "<br>Totale decessi: ", deced_tot,
                             "<br>Totale decessi / pop: ", round(deced_tot_onpop, 2), " /100 000",
                             "<br>Nuovi decessi: ", deced_new,
                             "<br>Nuovi decessi: / pop: ", round(deced_new_onpop, 2), " /100 000"),
         popup_tamponi = str_c("<b>", denominazione_regione, "</b>",
                               "<br>Data: ", data,
                               "<br>Totale tamponi: ", tamponi_tot,
                               "<br>Totale tamponi / pop: ", round(tamponi_tot_onpop, 2), " /100 000",
                               "<br>Nuovi tamponi: ", tamponi_new,
                               "<br>Nuovi tamponi: / pop: ", round(tamponi_new_onpop, 2), " /100 000"),
         popup_positivi = str_c("<b>", denominazione_regione, "</b>",
                                "<br>Data: ", data,
                                "<br>Attualmente positivi: ", positivi_tot,
                                "<br>Attualmente positivi / pop: ", round(positivi_tot_onpop, 2), " /100 000"),
         popup_osped = str_c("<b>", denominazione_regione, "</b>",
                             "<br>Data: ", data,
                             "<br>Attualmente ospedalizzati: ", osped_tot,
                             "<br>Attualmente ospedalizzati / pop: ", round(osped_tot_onpop, 2), " /100 000"),
         popup_terap = str_c("<b>", denominazione_regione, "</b>",
                             "<br>Data: ", data,
                             "<br>Attualmente in terapia intensiva: ", terap_tot,
                             "<br>Attualmente in terapia intensiva / pop: ", round(terap_tot_onpop, 2), " /100 000")
         )
                            


# Create daily cases for national data
covid_ita <- covid_ita %>% 
  left_join(covid_ita %>% 
              select(data,
                     casi_tot_previous_day = casi_tot,
                     deced_tot_previous_day = deced_tot,
                     tamponi_tot_previous_day = tamponi_tot,
                     positivi_tot_previous_day = positivi_tot,
                     osped_tot_previous_day = osped_tot,
                     terap_tot_previous_day = terap_tot
                     ) %>% 
              mutate(data = data + 1),
            by = "data") %>% 
  mutate(casi_tot_previous_day = if_else(is.na(casi_tot_previous_day), 0, casi_tot_previous_day),
         deced_tot_previous_day = if_else(is.na(deced_tot_previous_day), 0, deced_tot_previous_day),
         tamponi_tot_previous_day = if_else(is.na(tamponi_tot_previous_day), 0, tamponi_tot_previous_day),
         positivi_tot_previous_day = if_else(is.na(positivi_tot_previous_day), 0, positivi_tot_previous_day),
         osped_tot_previous_day = if_else(is.na(osped_tot_previous_day), 0, osped_tot_previous_day),
         terap_tot_previous_day = if_else(is.na(terap_tot_previous_day), 0, terap_tot_previous_day)
         ) %>% 
  mutate(casi_new = casi_tot - casi_tot_previous_day,
         deced_new = deced_tot - deced_tot_previous_day,
         tamponi_new = tamponi_tot - tamponi_tot_previous_day,
         positivi_new = positivi_tot - positivi_tot_previous_day,
         osped_new = osped_tot - osped_tot_previous_day,
         terap_new = terap_tot - terap_tot_previous_day
         ) %>%
  # Fix the problem of negative data in daily cases
  mutate(casi_new = if_else(casi_new < 0, 0, casi_new),
         deced_new = if_else(deced_new < 0, 0, deced_new),
         tamponi_new = if_else(tamponi_new < 0, 0, tamponi_new)
  ) %>% 
  select(-matches("_previous_day"))



covid_ita <- covid_ita %>% 
  mutate(popolazione = sum(pop_prov$popolazione)) %>% 
  mutate(
    casi_tot_onpop = casi_tot / popolazione * 1e5,
    deced_tot_onpop = deced_tot / popolazione * 1e5,
    tamponi_tot_onpop = tamponi_tot / popolazione * 1e5,
    positivi_tot_onpop = positivi_tot / popolazione * 1e5,
    osped_tot_onpop = osped_tot / popolazione * 1e5,
    terap_tot_onpop = terap_tot / popolazione * 1e5,
    casi_new_onpop = casi_new / popolazione * 1e5,
    deced_new_onpop = deced_new / popolazione * 1e5,
    tamponi_new_onpop = tamponi_new / popolazione * 1e5,
    positivi_new_onpop = positivi_new / popolazione * 1e5,
    osped_new_onpop = osped_new / popolazione * 1e5,
    terap_new_onpop = terap_new / popolazione * 1e5
  ) %>% 
  # Add tooltip
  mutate(popup_casi = str_c("<b>", "Italia", "</b>",
                            "<br>Data: ", data,
                            "<br>Totale casi: ", casi_tot,
                            "<br>Totale casi / pop: ", round(casi_tot_onpop, 2), " /100 000",
                            "<br>Nuovi casi: ", casi_new,
                            "<br>Nuovi casi: / pop: ", round(casi_new_onpop, 2), " /100 000"),
         popup_deced = str_c("<b>", "Italia", "</b>",
                             "<br>Data: ", data,
                             "<br>Totale decessi: ", deced_tot,
                             "<br>Totale decessi / pop: ", round(deced_tot_onpop, 2), " /100 000",
                             "<br>Nuovi decessi: ", deced_new,
                             "<br>Nuovi decessi: / pop: ", round(deced_new_onpop, 2), " /100 000"),
         popup_tamponi = str_c("<b>", "Italia", "</b>",
                               "<br>Data: ", data,
                               "<br>Totale tamponi: ", tamponi_tot,
                               "<br>Totale tamponi / pop: ", round(tamponi_tot_onpop, 2), " /100 000",
                               "<br>Nuovi tamponi: ", tamponi_new,
                               "<br>Nuovi tamponi: / pop: ", round(tamponi_new_onpop, 2), " /100 000"),
         popup_positivi = str_c("<b>", "Italia", "</b>",
                                "<br>Data: ", data,
                                "<br>Attualmente positivi: ", positivi_tot,
                                "<br>Attualmente positivi / pop: ", round(positivi_tot_onpop, 2), " /100 000"),
         popup_osped = str_c("<b>", "Italia", "</b>",
                             "<br>Data: ", data,
                             "<br>Attualmente ospedalizzati: ", osped_tot,
                             "<br>Attualmente ospedalizzati / pop: ", round(osped_tot_onpop, 2), " /100 000"),
         popup_terap = str_c("<b>", "Italia", "</b>",
                             "<br>Data: ", data,
                             "<br>Attualmente in terapia intensiva: ", terap_tot,
                             "<br>Attualmente in terapia intensiva / pop: ", round(terap_tot_onpop, 2), " /100 000")
         )



# Prepare map data ####


# # Read map data
# map_prov <- readOGR(dsn = "maps_data/Limiti01012020_g/ProvCM01012020_g",
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
# save(map_prov, file = "www/data/map_prov_simply.RData")
load("www/data/map_prov_simply.RData")


# Ad info on covid19
map_prov_data <- map_prov@data




# Define models for trend approximations ####

model <- function(variable, version, df, sp) {
  gam(formula = as.formula(str_c(variable, "_", version, " ~ s(as.numeric(data), k = 30) + offset(log(popolazione))")),
      sp = sp,
      data = df,
      family = poisson(link = "log"))
}

# model_new <- function(df, sp) {
#   gam(formula = casi_new ~ s(as.numeric(data), k = 30) + offset(log(popolazione)),
#       sp = sp,
#       data = df,
#       family = poisson(link = "log"))
# }
# 
# model_tot <- function(df, sp) {
#   gam(formula = casi_tot ~ s(as.numeric(data), k = 30) + offset(log(popolazione)),
#       sp = sp,
#       data = df,
#       family = poisson(link = "log"))
# }

# Create funcion for maximum value on map color scale
ceil10 <- function(x){
  10^ceiling(log10(max(x)))
}


reg_prov <- covid_prov %>% 
  select(stato, codice_regione, denominazione_regione,
         codice_provincia, denominazione_provincia, sigla_provincia) %>% 
  unique()


# # Prova
# choosen_dataset <- "covid_prov"
# selecting_detail <- "denominazione_provincia"
# selected_detail <- "input$province_displayed"
# # selected_detail_label <- "Provincia"


# Plots options ####
# tooltip_css <- "font-family: sans-serif; padding: 10px; border-radius: 5px; font-size: x-small"
tooltip_css <- "font-family: sans-serif; padding: 10px; border-radius: 5px; font-size: 0.9em"
hoven_css <- "r: 2.2pt"


# User interface ####
ui <- dashboardPage(
  dashboardHeader(title = "Diffusione del Covid-19 in Italia", titleWidth = 400,
                  tags$li(class = "dropdown", tags$a(href = "https://www.linkedin.com/in/leonardo-stincone/",
                                                     icon("linkedin"), "My Profile", target = "_blank")
                          ),
                  tags$li(class = "dropdown", tags$a(href = "https://github.com/stinco/covid19Ita",
                                                     icon("github"), "Source Code", target ="_blank"))
  ),
  dashboardSidebar({
    sidebarMenu(
      menuItem("Mappe", tabName = "tab_maps", icon = icon("map", lib = "font-awesome")),
      menuItem("Grafici", tabName = "tab_plots", icon = icon("chart-line", lib = "font-awesome"))#,
      # menuItem("Tabelle", tabName = "tab_tables", icon = icon("table", lib = "font-awesome"))
    )
  }),
  dashboardBody(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "headerStyle.css.css")
    # ),
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
                        radioGroupButtons(
                          inputId = "regionalDetail_radio",
                          label = "Seleziona il dettaglio",
                          choices = c("Italia", "Regioni", "Province"),
                          individual = TRUE,
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-circle", 
                                         style = "color: steelblue"),
                            no = tags$i(class = "fa fa-circle-o", 
                                        style = "color: steelblue"))
                        ),
                        selectInput("variable", "Seleziona la grandezza da visualizzare",
                                    choices = c("Casi confermati" = "casi",
                                                "Decessi" = "deced",
                                                "Tamponi" = "tamponi",
                                                "Attualmente positivi" = "positivi",
                                                "Attualmente ospedalizzati" = "osped",
                                                "Attualmente in terapia intensiva" = "terap"),
                                    selected = "casi"),
                        pickerInput(
                          inputId = "province_displayed",
                          label = "Seleziona province", 
                          # choices = province,
                          choices = province_list,
                          selected = province_init,
                          multiple = TRUE,
                          options = list(
                            `live-search` = TRUE)),
                        pickerInput(
                          inputId = "regioni_displayed",
                          label = "Seleziona regione", 
                          choices = regioni,
                          # selected = "",
                          multiple = TRUE,
                          options = list(
                            `live-search` = TRUE)
                        ),
                        pickerInput(
                          inputId = "regioni_displayed_2",
                          label = "Seleziona regione", 
                          choices = regioni,
                          # selected = "",
                          multiple = TRUE,
                          options = list(
                            `live-search` = TRUE)
                        ),
                        # tags$br(),
                        actionButton("deselect_button",
                                     "Deseleziona tutto"),
                        actionButton("select_top_n_reg_button",
                                     "Seleziona le prime n regioni"),
                        actionButton("select_top_n_prov_button",
                                     "Seleziona le prime n province")
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
                        tags$div(# title = "La curva approssimante è calcolata tramite una Local Polynomial Regression (LOESS) fittata sui dati empirici",
                          title = "La curva approssimante è calcolata tramite un Generalized Additive Model (GAM) fittato sui dati empirici",
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
                        hidden(
                          tags$br(),
                          tags$b(id = "showSE_text", "Seleziona se vuoi visualizzare gli intervalli di confidenza per la curva approssimante"),
                          tags$br()
                        )
                      ,
                        tags$div(id = "tooltip_check",
                                   title = "Le opzioni sulla curva approssimante sono disponibili solo se è selezionata la curva approssimante",
                                   hidden(
                                   awesomeCheckbox(
                              inputId = "showSE_check",
                              label = "Intervalli di confidenza", 
                              value = FALSE#,
                            )
                          )
                      ,
                          tags$br()
                        ),
                        hidden(
                            sliderInput("span_slider", "Seleziona il livello di smoothness della curva approssimante",
                                        min = 0, max = 10, step = .01,
                                        value = 2
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
                        tabsetPanel(id = "plots_tabPanel",
                          tabPanel(title = "Casi totali",
                                   value = "plot_cases_tot_panel",
                                   # plotly::plotlyOutput("plot_cases_tot",
                                   #                      width = "100%", height = "100%") %>% 
                                   ggiraph::girafeOutput("plot_cases_tot",
                                                         width = "100%", height = "100%") %>%
                                     withSpinner()
                                   ),
                          tabPanel(title = "Casi giornalieri",
                                   value = "plot_cases_new_panel",
                                  # plotly::plotlyOutput("plot_cases_new",
                                  #                      width = "100%", height = "100%") %>% 
                                  ggiraph::girafeOutput("plot_cases_new",
                                                        width = "100%", height = "100%") %>%
                                    withSpinner())
                        )
                    )
                  # )
                # )
              )
                
      ),
      # # Tab tables ####
      # tabItem(tabName = "tab_tables",
      #         fluidRow(
      #           # box(
      #             # width = 8, height = 6,
      #             DT::dataTableOutput("table_provinces")
      #           # )
      #         )
      # ),
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
  
  
  # Create reactive components ####
  
  choosen_dataset <- reactive({
    if(input$regionalDetail_radio == "Italia"){
      covid_ita
    } else if(input$regionalDetail_radio == "Regioni") {
      covid_reg
    } else {
      covid_prov
    }
  })
  
  selecting_detail <- reactive({
    if(input$regionalDetail_radio == "Italia"){
      "stato"
    } else if(input$regionalDetail_radio == "Regioni") {
      "denominazione_regione"
    } else {
      "denominazione_provincia"
    }
  })
  
  selected_detail <- reactive({
    if(input$regionalDetail_radio == "Italia"){
      "ITA"
    } else if(input$regionalDetail_radio == "Regioni"){
      # input$regioni_displayed
      input$regioni_displayed_2
    } else {
      input$province_displayed
    }
  })
  
  selected_detail_label <- reactive({
    if(input$regionalDetail_radio == "Italia"){
      "Nazione"
    } else if(input$regionalDetail_radio == "Regioni") {
      "Regione"
    } else {
      "Provincia"
    }
  })
  
  onpop <- reactive({
    if(input$rescalePop_check){
      "_onpop"
    } else {
      ""
    }
  })
  
  onpop_description <- reactive({
    if(input$rescalePop_check){
      " ogni 100 000 abitanti"
    } else {
      ""
    }
  })
  
  variable <- reactive({
    if(input$regionalDetail_radio == "Provincia"){
      "casi"
    } else {
      input$variable
    }
  })
  
  variable_description <- reactive({
    if(input$variable == "casi"){
      "Casi"
    } else if(input$variable == "deced"){
      "Decessi"
    } else if(input$variable == "tamponi"){
      "Tamponi"
    } else if(input$variable == "positivi"){
      "Attualmente positivi"
    } else if(input$variable == "osped"){
      "Attualmente ospedalizzati"
    } else {
      "Attualmente in terapia intensiva"
    }
  })
  
  
  
  # Dataset ####
  covid_dataset_filtered <- reactive({
    choosen_dataset() %>% 
      filter(get(selecting_detail()) %in% selected_detail())
  })
  
  
  # Models ####
  
  # Compute model for total data
  covid_fitted_tot <- reactive({
    covid_dataset_filtered() %>% 
      group_by(get(selecting_detail())) %>%
      nest() %>%
      # mutate(model = map(data, function(x){model_tot(x, sp = input$span_slider)})) %>%
      mutate(model = map(data, function(x){model(variable = variable(), version = "tot", df = x, sp = exp(input$span_slider + .01))})) %>%
      mutate(
        data = map2(data, model,
                    function(data, model){
                      pred <- predict(object = model, newdata = data, type = "response", se.fit = T)
                      data %>% 
                        mutate(fit = pred$fit,
                               se = pred$se.fit,
                               lower = fit - 2 * se,
                               upper = fit + 2 * se,
                               fit_onpop = fit / popolazione * 1e5,
                               lower_onpop = lower / popolazione * 1e5,
                               upper_onpop = upper / popolazione * 1e5)
                    }
        )
      ) %>%
      unnest(data)
  })
  
  # Compute model for new data
  covid_fitted_new <- reactive({
    covid_dataset_filtered() %>% 
      group_by(get(selecting_detail())) %>%
      nest() %>%
      # mutate(model = map(data, function(x){model_new(x, sp = input$span_slider)})) %>%
      mutate(model = map(data, function(x){model(variable = variable(), version = "new", df = x, sp = exp(input$span_slider + .01))})) %>%
      mutate(
        data = map2(data, model,
                    function(data, model){
                      pred <- predict(object = model, newdata = data, type = "response", se.fit = T)
                      data %>% 
                        mutate(fit = pred$fit,
                               se = pred$se.fit,
                               lower = fit - 2 * se,
                               upper = fit + 2 * se,
                               fit_onpop = fit / popolazione * 1e5,
                               lower_onpop = lower / popolazione * 1e5,
                               upper_onpop = upper / popolazione * 1e5)
                    }
        )
      ) %>%
      unnest(data)
  })
    
  
  
  # Plots ####
  plot_cases_tot <- reactive({
    
    # variable <- if_else(input$regionalDetail_radio == "Provincia",
    #                     "casi", input$variable)
    
    covid_dataset_filtered() %>% 
      # ggplot(aes(x = data, y = get(str_c("casi", "_tot", onpop())),
      ggplot(aes(x = data, y = get(str_c(variable(), "_tot", onpop())),
                 color = get(selecting_detail()),
                 # text = get(str_c("popup_", variable())),
                 tooltip = get(str_c("popup_", variable())),
                 data_id = get(str_c("popup_", variable())),
                 group = get(selecting_detail()))) +
      labs(color = selected_detail_label(),
           y = str_c(variable_description(), onpop_description()))
  
  })
  
  plot_cases_new <- reactive({
    covid_dataset_filtered() %>% 
      ggplot(aes(x = data, y = get(str_c(variable(), "_new", onpop())),
                 color = get(selecting_detail()),
                 # text = get(str_c("popup_", variable())),
                 tooltip = get(str_c("popup_", variable())),
                 data_id = get(str_c("popup_", variable())),
                 group = get(selecting_detail()))) +
      labs(color = selected_detail_label(),
           y = str_c(variable_description(), onpop_description()))
    
  })
  
  
  plot_cases <- function(version){
    if(is.null(selected_detail())){
      p <- tibble(data = 0, casi_tot = 0,
                  denominazione_provincia = "", popup_casi = "") %>% 
        ggplot(aes(x = data, y = casi_tot,
                   color = denominazione_provincia,
                   # text = popup_casi,
                   group = denominazione_provincia)) +
        labs(color = selected_detail_label(),
             y = str_c(variable_description(), onpop_description()))
      
    } else {
      
      if(version == "tot"){
        p <- plot_cases_tot()
      } else {
        p <- plot_cases_new()
      }
      
      
      if("Dati osservati" %in% input$element_plot_check){
        p <- p +
          geom_line() +
          # geom_point
          geom_point_interactive()
      }
      
      if("Curva approssimante" %in% input$element_plot_check){
        
        if(version == "tot"){
          if(input$showSE_check){
            p <- p + geom_ribbon(data = covid_fitted_tot(),
                                 aes(ymin = get(str_c("lower", onpop())),
                                     ymax = get(str_c("upper", onpop())),
                                     # y = get(str_c("fit", onpop())),
                                     fill = get(selecting_detail())),
                                 color = NA,
                                 alpha = .4,
                                 show.legend = FALSE)
          }
            
          p <- p +
            geom_line(data = covid_fitted_tot(),
                      aes(y = get(str_c("fit", onpop()))),
                      size = 1) +
            geom_point_interactive(data = covid_fitted_tot(),
                                   aes(y = get(str_c("fit", onpop()))),
                                   size = .1)
          
        } else {
          
          if(input$showSE_check){
            p <- p + geom_ribbon(data = covid_fitted_new(),
                                 aes(ymin = get(str_c("lower", onpop())),
                                     ymax = get(str_c("upper", onpop())),
                                     # y = get(str_c("fit", onpop())),
                                     fill = get(selecting_detail())),
                                 color = NA,
                                 alpha = .4,
                                 show.legend = FALSE)
          }
          
          p <- p +
            geom_line(data = covid_fitted_new(),
                      aes(y = get(str_c("fit", onpop()))),
                      size = 1) +
            geom_point_interactive(data = covid_fitted_new(),
                                   aes(y = get(str_c("fit", onpop()))),
                                   size = .1)
          
        }
        
      }
      
    }
    
    # ggplotly(p, tooltip = "text")
    girafe(ggobj = p,
           options = list(opts_tooltip(#offx = 80, offy = 20, use_cursor_pos = FALSE,
                                       use_cursor_pos = TRUE,
                                       use_fill = TRUE,
                                       css = tooltip_css),
                          opts_hover(css = hoven_css),
                          # opts_sizing(rescale = TRUE, width = 1),
                          opts_sizing(rescale = FALSE),
                          opts_selection(type = "none"),
                          opts_toolbar(saveaspng = FALSE),
                          opts_zoom(max = 2)),
           # fonts = list(sans = "Roboto"),
           width_svg = 5.5, height_svg = 4.5)
  }
  
  
  # Plot total cases ####
  
  # output$plot_cases_tot <- plotly::renderPlotly({
  output$plot_cases_tot <- ggiraph::renderGirafe({
    plot_cases(version = "tot")
  })
  
  
  # Plot new cases ####
  
  # output$plot_cases_new <- plotly::renderPlotly({
  output$plot_cases_new <- ggiraph::renderGirafe({
    plot_cases(version = "new")
  })
  

  # Choose province by region ####
  observeEvent(input$regioni_displayed, {
    province_init <- reg_prov %>% 
      filter(denominazione_regione %in% input$regioni_displayed) %>% 
      .$denominazione_provincia %>% 
      unique() %>% 
      sort()
    
    updatePickerInput(session = session, inputId = "province_displayed",
                      selected = province_init)
    
  }, ignoreInit = TRUE)
  
  
  # Choose region by provinces ####
  
  observeEvent(input$province_displayed, {
    updatePickerInput(session = session, inputId = "regioni_displayed_2",
                      selected = reg_prov %>% 
                        filter(denominazione_provincia %in% input$province_displayed) %>% 
                        .$denominazione_regione %>% 
                        unique())
  })
  
  
  
  # Deselect everything ####
  observeEvent(input$deselect_button, {
    updatePickerInput(session = session, inputId = "regioni_displayed",
                      selected = "")
    updatePickerInput(session = session, inputId = "province_displayed",
                      selected = "")
    updatePickerInput(session = session, inputId = "regioni_displayed_2",
                      selected = "")
  }, ignoreInit = TRUE)

  
  # Select top n regions ####
  observeEvent(input$select_top_n_reg_button, {
    showModal(modalDialog(
      title = "Seleziona le prime n regioni",
      numericInput("top_n", "Scegli quante regioni vuoi visualizzare",
                   value = if_else(input$regionalDetail_radio == "Regioni", 5, 2),
                   min = 1, max = 20, step = 1),
      selectInput("criteria", "Seleziona il criterio",
                  choices = c("Casi totali" = "casi_tot",
                              "Casi totali / popolazione" = "casi_tot_onpop",
                              "Nuovi casi" = "casi_new",
                              "Nuovi casi / popolazione" = "casi_new_onpop"),
                  selected = ""),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("select_top_n_reg_button_ok", "Ok")
      )
    ))
  })
  
  observeEvent(input$select_top_n_reg_button_ok, {
    # print(input$top_n)

    if(input$criteria %in% c("casi_tot", "casi_tot_onpop")){
      regioni_toSelect <- covid_reg %>%
        filter(data == max(data)) %>% 
        top_n(n = input$top_n, wt = get(input$criteria)) %>%
        arrange(-casi_tot) %>% 
        .$denominazione_regione
    } else {
      regioni_toSelect <- covid_reg %>%
        filter(data >= max(data) - 2) %>%
        group_by(denominazione_regione) %>%
        summarize(criteria = sum(get(input$criteria)),
                  casi_tot = max(casi_tot)) %>%
        top_n(n = input$top_n, wt = criteria) %>%
        arrange(-casi_tot) %>% 
        .$denominazione_regione
    }
    
    if(input$regionalDetail_radio == "Regioni"){
      updatePickerInput(session = session, inputId = "regioni_displayed_2",
                        selected = regioni_toSelect)  
    } else {
      updatePickerInput(session = session, inputId = "regioni_displayed",
                        selected = regioni_toSelect)  
    }

    removeModal()
  }, ignoreInit = TRUE)
  
  
  # Select top n provinces ####
  observeEvent(input$select_top_n_prov_button, {
    showModal(modalDialog(
      title = "Seleziona le prime n province",
      numericInput("top_n", "Scegli quante province vuoi visualizzare",
                   value = 5, min = 1, max = 20, step = 1),
      selectInput("criteria", "Seleziona il criterio",
                  choices = c("Casi totali" = "casi_tot",
                              "Casi totali / popolazione" = "casi_tot_onpop",
                              "Nuovi casi" = "casi_new",
                              "Nuovi casi / popolazione" = "casi_new_onpop"),
                  selected = ""),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("select_top_n_prov_button_ok", "Ok")
      )
    ))
  })
  
  observeEvent(input$select_top_n_prov_button_ok, {
    # print(input$top_n)
    
    if(input$criteria %in% c("casi_tot", "casi_tot_onpop")){
      province_toSelect <- covid_prov %>% 
        filter(data == max(data)) %>% 
        top_n(n = input$top_n, wt = get(input$criteria)) %>%
        arrange(-casi_tot) %>% 
        .$denominazione_provincia
    } else {
      province_toSelect <- covid_prov %>% 
        filter(data >= max(data) - 2) %>%
        group_by(denominazione_provincia) %>%
        summarize(criteria = sum(get(input$criteria)),
                  casi_tot = max(casi_tot)) %>%
        top_n(n = input$top_n, wt = criteria) %>%
        arrange(-casi_tot) %>% 
        .$denominazione_provincia
    }
    
    updatePickerInput(session = session, inputId = "regioni_displayed",
                      selected = "")
    updatePickerInput(session = session, inputId = "province_displayed",
                      selected = province_toSelect)
    
    removeModal()
  }, ignoreInit = TRUE)
  
  
  
  # Disable input elements for Confidence intervals ####
  observeEvent(input$element_plot_check, {
    if("Curva approssimante" %in% input$element_plot_check){
      # shinyjs::enable("showSE_check")
      # shinyjs::enable("span_slider")
      # shynyjs::show("tooltip_check", anim = T)
      shinyjs::show("showSE_text", anim = T)
      shinyjs::show("showSE_check", anim = T)
      shinyjs::show("span_slider", anim = T)
    } else {
      # shynyjs::hide("tooltip_check", anim = T)
      shinyjs::hide("showSE_text", anim = T)
      shinyjs::hide("showSE_check", anim = T)
      shinyjs::hide("span_slider", anim = T)
      # shinyjs::disable("showSE_check")
      # shinyjs::disable("span_slider")
    }
  })
  
  
  # Select regional detail ####
  observeEvent(input$regionalDetail_radio, {
    
    if(input$regionalDetail_radio == "Italia"){
      
      shinyjs::enable("variable")
      
      shinyjs::hide("province_displayed", anim = T)
      shinyjs::hide("regioni_displayed", anim = F)
      shinyjs::hide("regioni_displayed_2", anim = F)
      shinyjs::hide("deselect_button", anim = T)
      shinyjs::hide("select_top_n_prov_button", anim = T)
      shinyjs::hide("select_top_n_reg_button", anim = T)
      
    } else if (input$regionalDetail_radio == "Regioni") {
      
      shinyjs::enable("variable")
      
      shinyjs::hide("province_displayed", anim = T)
      shinyjs::hide("regioni_displayed", anim = F)
      shinyjs::show("regioni_displayed_2", anim = F)
      shinyjs::show("deselect_button", anim = T)
      shinyjs::hide("select_top_n_prov_button", anim = T)
      shinyjs::show("select_top_n_reg_button", anim = T)
      
    } else {
      
      updatePickerInput(session = session, inputId = "variable",
                        selected = "casi")
      shinyjs::disable("variable")
      
      shinyjs::show("province_displayed", anim = T)
      shinyjs::show("regioni_displayed", anim = F)
      shinyjs::hide("regioni_displayed_2", anim = F)
      shinyjs::show("deselect_button", anim = T)
      shinyjs::show("select_top_n_prov_button", anim = T)
      shinyjs::hide("select_top_n_reg_button", anim = T)
      
    }
  })
  
  
  # Select variable ####
  observeEvent(input$variable, {
    if(input$variable %in% c("positivi", "osped", "terap")){
      updateTabsetPanel(session = session, inputId = "plots_tabPanel", selected = "plot_cases_tot_panel")
      hideTab(inputId = "plots_tabPanel", target = "plot_cases_new_panel")
    } else {
      showTab(inputId = "plots_tabPanel", target = "plot_cases_new_panel")
    }
  })
  
  # output$plot_cases_tot_panel_title = renderText({
  #   if(input$variable %in% c("positivi", "osped")){
  #     "Casi attuali"
  #   } else {
  #     "Casi totali"
  #   }
  # })
  
  
  
  # # Create table ####
  # output$table_provinces <- DT::renderDataTable({
  #   covid_prov %>% 
  #     select(data,
  #            denominazione_regione,
  #            denominazione_provincia,
  #            casi_tot, casi_tot_onpop,
  #            casi_new, casi_new_onpop
  #            ) %>% 
  #     mutate(casi_tot_onpop = round(casi_tot_onpop, 2),
  #            casi_new_onpop = round(casi_new_onpop, 2)) %>% 
  #     arrange(desc(data), desc(casi_tot))
  # })
  
  
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
                  popup = ~popup_casi) %>%
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
                      popup = ~popup_casi)
      } else {
        leafletProxy("map_leaflet", data = map_prov) %>%
          addPolygons(layerId = ~SIGLA,
                      color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.8,
                      fillColor = ~pal_casi_tot()(log(casi_tot + 1)),
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE),
                      popup = ~popup_casi)
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
                    mutate(casi_tot = if_else(casi_tot < 0, 0, casi_tot),
                           casi_tot_onpop = if_else(casi_tot_onpop < 0, 0, casi_tot_onpop),
                           popup_casi = str_c("<b>", denominazione_provincia, "</b>",
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
                      popup = ~popup_casi)
      } else {
        leafletProxy("map_leaflet", data = map_prov) %>%
          addPolygons(layerId = ~SIGLA,
                      color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.8,
                      fillColor = ~pal_casi_tot()(log(casi_tot + 1)),
                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                          bringToFront = TRUE),
                      popup = ~popup_casi)
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
  
  # Close server session when the browser tab get closed
  session$onSessionEnded(stopApp)
    
}


# App ####
shinyApp(ui = ui, server = server)










