#####
# 01. Analisi esplorativa dati
# 25/03/2020
# R 3.6.3
#####

# Libraries ####
library(tidyverse)
library(forcats)
library(RCurl)
library(rgdal)
library(plotly)
library(leaflet)
library(RColorBrewer)
library(viridis)

theme_set(theme_bw())
theme_set(theme_dark())



# Read data ####

# covid_prov <- read_csv("C:/Users/leona/Documents/universita - pc/studioPersonale/covid19/COVID-19/dati-province/dpc-covid19-ita-province.csv",
#                        na = "")

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


# Read data on population ####

pop_prov <- read_csv2("data/pop_prov_italia.csv",
                      na = "")

covid_prov <- covid_prov %>% 
  left_join(pop_prov %>% 
              select(sigla, popolazione, superficie, densita),
            by = c("sigla_provincia" = "sigla")) %>% 
  mutate(casi_tot_onpop = casi_tot / popolazione * 1e5)

covid_prov <- covid_prov %>% 
  mutate(popup = str_c("<b>", denominazione_provincia, "</b>",
                       "<br>Data: ", data,
                       "<br>Totale casi: ", casi_tot,
                       "<br>Totale casi / pop: ", round(casi_tot_onpop, 2), " /100 000",
                       "<br>Nuovi casi: ", casi_new))


# Explore data ####
covid_prov %>% 
  count(data)

summary(covid_prov$data)

summary(covid_prov)

covid_prov %>% 
  count(sigla_provincia)


# Plot number of cumulative cases ####

p <- covid_prov %>% 
  mutate(denominazione_provincia = fct_reorder2(denominazione_provincia,
                                                         data, casi_tot)) %>% 
  ggplot(aes(x = data, y = casi_tot,
             color = denominazione_provincia)) +
  geom_line() +
  geom_point() +
  theme(legend.position = "none")
  
ggplotly(p)


# Filter by region
covid_prov %>% 
  count(denominazione_regione)

regione <- "Friuli Venezia Giulia"

p <- covid_prov %>% 
  filter(denominazione_regione == regione) %>% 
  mutate(denominazione_provincia = fct_reorder2(denominazione_provincia,
                                                   data, casi_tot)) %>% 
  ggplot(aes(x = data, y = casi_tot,
             color = denominazione_provincia,
             text = popup,
             group = 1)) +
  geom_line() +
  geom_point() +
  theme(
    legend.box.background = element_rect(),
    legend.box.margin = margin(6, 6, 6, 6)
  )

ggplotly(p,
         tooltip = "text")



# Filter first n province by cases
n <- 10

province <- covid_prov %>% 
  filter(data == max(data)) %>% 
  top_n(n = n, wt = casi_tot) %>% 
  arrange(-casi_tot) %>% 
  .$denominazione_provincia

p <- covid_prov %>% 
  filter(denominazione_provincia %in% province) %>% 
  mutate(denominazione_provincia = fct_relevel(denominazione_provincia,
                                               province)) %>% 
  ggplot(aes(x = data, y = casi_tot,
             color = denominazione_provincia,
             text = popup,
             group = 1)) +
  geom_line() +
  geom_point()

ggplotly(p)




# Plot number of daily new cases ####

# covid_prov %>% 
#   group_by(data) %>% 
#   summarize(casi_tot = sum(casi_tot)) %>% 
#   print(n = 100)
# 
# covid_prov %>% 
#   filter(as.Date(data) == as.Date("2020-03-24")) %>% 
#   group_by(denominazione_regione) %>% 
#   summarize(casi_tot = sum(casi_tot)) %>% 
#   arrange(-casi_tot)
# 
# 
# head(covid_prov$data) + 1
# 
# covid_prov <- covid_prov %>% 
#   left_join(covid_prov %>% 
#               select(data, sigla_provincia, casi_tot_previous_day = casi_tot) %>% 
#               mutate(data = data + 1),
#             by = c("data", "sigla_provincia")) %>% 
#   mutate(casi_tot_previous_day = ifelse(is.na(casi_tot_previous_day), 0, casi_tot_previous_day),
#          casi_new = casi_tot - casi_tot_previous_day)



# Plot data
p <- covid_prov %>% 
  ggplot(aes(x = data, y = casi_new,
             color = denominazione_provincia,
             fill = denominazione_provincia)) +
  # geom_line() +
  # geom_point()
  geom_col(position = "dodge") +
  theme(legend.position = "none")

ggplotly(p)


# Filter by region
covid_prov %>% 
  count(denominazione_regione)

regione <- "Friuli Venezia Giulia"

p <- covid_prov %>% 
  filter(denominazione_regione == regione) %>% 
  ggplot(aes(x = data, y = casi_new,
             color = denominazione_provincia,
             fill = denominazione_provincia)) +
  # geom_line() +
  # geom_point()
  geom_col(position = "dodge")

ggplotly(p)




# Animations ####

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}



# Scelgo una provincia
regione <- "Friuli Venezia Giulia"
regione <- "Lombardia"

df <- covid_prov %>% 
  mutate(data_num = as.numeric(data) - min(as.numeric(data))) %>% 
  filter(denominazione_regione == regione) %>% 
  mutate(denominazione_provincia = fct_reorder2(denominazione_provincia,
                                                data, casi_tot)) %>% 
  accumulate_by(~data_num)
  

# Scelgo le prime n province
n <- 10

province <- covid_prov %>% 
  filter(data == max(data)) %>% 
  top_n(n = n, wt = casi_tot) %>% 
  arrange(-casi_tot) %>% 
  .$denominazione_provincia

df <- covid_prov %>% 
  mutate(data_num = as.numeric(data) - min(as.numeric(data))) %>% 
  filter(denominazione_provincia %in% province) %>% 
  mutate(denominazione_provincia = fct_reorder2(denominazione_provincia,
                                                data, casi_tot)) %>% 
  accumulate_by(~data_num)



df %>%   
  plot_ly(
    x = ~data_num,
    y = ~casi_tot,
    # color = ~denominazione_provincia,
    split = ~denominazione_provincia,
    frame = ~frame,
    # text = ~denominazione_provincia,
    text = ~popup,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'lines',
    line = list(simplyfy = F)
  ) %>% 
  layout(
    xaxis = list(
      title = "Giorni dal 24/02/2020",
      zeroline = F
    ),
    yaxis = list(
      title = "Casi cumulati",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    frame = 200, 
    transition = 0, 
    redraw = FALSE
  )



df %>%   
  plot_ly(
    x = ~data_num,
    y = ~casi_tot_onpop,
    # color = ~denominazione_provincia,
    split = ~denominazione_provincia,
    frame = ~frame,
    # text = ~denominazione_provincia,
    text = ~popup,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'lines',
    line = list(simplyfy = F)
  ) %>% 
  layout(
    xaxis = list(
      title = "Giorni dal 24/02/2020",
      zeroline = F
    ),
    yaxis = list(
      title = "Casi cumulati / popolazione",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    frame = 200, 
    transition = 0, 
    redraw = FALSE
  )


df %>%   
  plot_ly(
    x = ~data_num,
    y = ~casi_new,
    # color = ~denominazione_provincia,
    split = ~denominazione_provincia,
    frame = ~frame,
    # text = ~denominazione_provincia,
    text = ~popup,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'lines',
    line = list(simplyfy = F)
  ) %>% 
  layout(
    xaxis = list(
      title = "Giorni dal 24/02/2020",
      zeroline = F
    ),
    yaxis = list(
      title = "Casi cumulati / popolazione",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    frame = 200, 
    transition = 0, 
    redraw = FALSE
  )


