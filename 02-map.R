library(ojodb)

zip_shp <- rgdal::readOGR(dsn = "okzip", layer = "okzip", verbose = FALSE) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

shp <- zip_shp %>%
  sp::merge(d %>% rename(name = court), all.x = T)

pal <-  colorBin(palette = "RdBu",
                 domain = c(min(d$n_cases), max(d$n_cases)),
                 reverse = FALSE)

leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = shp,
              fillColor = ~pal(shp@data$n_cases),
              weight = 1,
              opacity = .9,
              color = "black",
              fillOpacity = 0.7,
              label = paste("County:", shp@data$name),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))
