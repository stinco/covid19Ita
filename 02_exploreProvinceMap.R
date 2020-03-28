#####
# 02. Mappa province
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


# Read map ####

map_prov <- readOGR(dsn = "data/Limiti01012020_g/ProvCM01012020_g",
                    layer = "ProvCM01012020_g_WGS84",
                    use_iconv = TRUE, encoding = "UTF-8")

map_prov@data <- map_prov@data %>% 
  mutate_if(.predicate = is.factor,
            .funs = as.character)


# Special caraters are correctly displayed
map_prov@data$TIPO_UTS

plot(map_prov)


map_prov@data %>% 
  as_tibble()



# Change reference system

map_prov <- spTransform(map_prov, CRS("+init=epsg:4326"))



# Ad info on covid19
map_prov@data <- map_prov@data %>% 
  left_join(covid_prov %>% 
              filter(data == max(data)),
            by = c("SIGLA" = "sigla_provincia"))



# Visualize maps ####

ceil10 <- function(x){
  10^ceiling(log10(max(x)))
}


# Casi totali

max(covid_prov$casi_tot + 1)

pal_casi_tot <- colorNumeric(
  # palette = "RdYlGn",
  palette = c("#FFFFFFFF", rev(inferno(256))),
  domain = c(0, log(ceil10(max(covid_prov$casi_tot + 1)))),
  reverse = F
)

leaflet(map_prov) %>% 
  # addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~pal_casi_tot(log(casi_tot + 1)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~popup) %>% 
  addLegend(position = "bottomright",
            pal = pal_casi_tot, opacity = 1,
            bins = log(10^(0:log10(ceil10(max(covid_prov$casi_tot + 1))))),
            value = log(1:10^(log10(ceil10(max(covid_prov$casi_tot + 1))))),
            data = log(10^(0:log10(ceil10(max(covid_prov$casi_tot + 1))))),
            labFormat = labelFormat(transform = exp))


# Casi totali pro capite

max(covid_prov$casi_tot_onpop + 1)

pal_casi_tot_onpop <- colorNumeric(
  # palette = "RdYlGn",
  palette = c("#FFFFFFFF", rev(inferno(256))),
  domain = c(0, log(ceil10(max(covid_prov$casi_tot_onpop + 1)))),
  reverse = F
)

leaflet(map_prov) %>% 
  # addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~pal_casi_tot_onpop(log(casi_tot_onpop + 1)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~popup) %>% 
  addLegend(position = "bottomright",
            pal = pal_casi_tot_onpop, opacity = 1,
            bins = log(10^(seq(0, log10(ceil10(max(covid_prov$casi_tot_onpop + 1))), .5))),
            value = log(1:10^(log10(ceil10(max(covid_prov$casi_tot_onpop + 1))))),
            data = log(10^(0:log10(ceil10(max(covid_prov$casi_tot_onpop + 1))))),
            labFormat = labelFormat(transform = function(x) round(exp(x)) ,suffix = " /100 000"))




# Read data on population ####

# pop_prov <- read_csv2("data/pop_prov_italia.csv",
#                       na = "")
# 
# pop_prov %>% 
#   mutate(diff = popolazione/superficie - densita) %>% 
#   arrange(-abs(diff))
# 
# pop_prov <- pop_prov %>% 
#   mutate(densita = popolazione/superficie)


# # Check if the keys are the same
# map_prov@data %>% 
#   as_tibble() %>% 
#   anti_join(pop_prov,
#             by = c("SIGLA" = "sigla"))
# 
# pop_prov %>% 
#   anti_join(map_prov@data,
#             by = c("sigla" = "SIGLA"))
# 
# 
# map_prov@data <- map_prov@data %>% 
#   left_join(pop_prov %>% 
#               select(sigla, popolazione, superficie, densita),
#             by = c("SIGLA" = "sigla"))



# covid_prov %>% 
#   anti_join(pop_prov,
#             by = c("sigla_provincia" = "sigla"))
# 
# pop_prov %>% 
#   anti_join(covid_prov,
#             by = c("sigla" = "sigla_provincia"))
# 
# covid_prov <- covid_prov %>% 
#   left_join(pop_prov %>% 
#               select(sigla, popolazione, superficie, densita),
#             by = c("sigla_provincia" = "sigla")) %>% 
#   mutate(totale_casi_onpop = totale_casi / popolazione * 1e3)
















