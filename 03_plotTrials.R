#####
# 03. Prove grafici
# 29/03/2020
# R 3.6.3
#####


province <- c("Udine", "Trieste")


p <- covid_prov %>% 
  filter(denominazione_provincia %in% province) %>% 
  # mutate(denominazione_provincia = fct_relevel(denominazione_provincia,
  #                                              province)) %>% 
  ggplot(aes(x = data, y = casi_tot,
             color = denominazione_provincia,
             text = popup,
             group = denominazione_provincia)) +
  labs(color = "Provincia",
       y = "Casi")


library(splines)
library(mgcv)

p +
  geom_line() +
  geom_point() +
  # geom_smooth(method = "loess",
  #             formula = "y ~ x",
  #             se = F,
  #             span = .8) +
  stat_smooth(method = mgcv::gam#Ï,
              # formula = y ~ ns(x, 10),
              # family = "quasipoisson"
              # family = "quasipoisson"
              )




?gam
?mgcv::gam
?stats::loess
?stats::lowess




# Maps location ####
leaflet(map_prov) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = 12, lat = 42, zoom = 5.5)



# Simplify poligon

library(rmapshaper)

map_prov_simp1 <- ms_simplify(map_prov)

map_prov_simp2 <- ms_simplify(map_prov, keep = .01)

map_prov_simp3 <- ms_simplify(map_prov, keep = .005)


map_prov@data %>% 
  as_tibble()

map_prov_simp1@data %>% 
  as_tibble()


plot(map_prov)
plot(map_prov_simp1)
plot(map_prov_simp2)
plot(map_prov_simp3)



leaflet(map_prov) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = 12, lat = 42, zoom = 5.5) %>%
  addPolygons(layerId = ~SIGLA, color = "#444444",
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              # fillColor = ~pal_casi_tot()(log(casi_tot + 1)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

leaflet(map_prov_simp1) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = 12, lat = 42, zoom = 5.5) %>%
  addPolygons(layerId = ~SIGLA, color = "#444444",
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              # fillColor = ~pal_casi_tot()(log(casi_tot + 1)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

leaflet(map_prov_simp2) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = 12, lat = 42, zoom = 5.5) %>%
  addPolygons(layerId = ~SIGLA, color = "#444444",
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              # fillColor = ~pal_casi_tot()(log(casi_tot + 1)),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
























