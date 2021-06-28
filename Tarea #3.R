library(dplyr)
library(ggplot2)
library(sf)
library(DT)
library(plotly)
library(leaflet)
library(jsonlite)


junco_vulcani <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/junco_vulcani-cr-registros.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )

# Asignación de CRS
st_crs(junco_vulcani) = 4326

# Capa geespacial de cantones
cantones <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_cantones_simp_wgs84.geojson",
    quiet = TRUE
  )

# Cruce espacial con la tabla de cantones, para obtener el nombre del cantón
junco_vulcani <- 
  junco_vulcani %>%
  st_join(cantones["canton"])


# Tabla de registros de presencia
junco_vulcani %>%
  st_drop_geometry() %>%
  select(stateProvince, canton, species, family, eventDate) %>%
  datatable(
    colnames = c("Provincia", "Cantón", "Especies", "Familia", "Fecha"),
    options = list(
      searchHighlight = TRUE,
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
      pageLength = 5
    )
  )

options = list(
  searchHighlight = TRUE,
  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
)


# Gráfico de estacionalidad
  junco_vulcani %>%
    st_drop_geometry() %>%
    group_by(mes = format(as.Date(eventDate, "%Y-%m-%d"), "%m")) %>%
    summarize(suma_registros = n()) %>%
    filter(!is.na(mes))  %>%
    plot_ly(x = ~ mes,
            y = ~ suma_registros,
            type="scatter", mode="markers", fill = "tozeroy", fillcolor = "green") %>%
    layout(title = "Estacionalidad",
           xaxis = list(title = "Mes"),
           yaxis = list(title = "Cantidad de registros"))

  
  
  
  
  
  
  
# Gráfico Pastel
View(junco_vulcani)
ex_primates_cr <- data.frame("Categoria"=rownames(junco_vulcani), junco_vulcani) 
primates_cr_data <- ex_junco_vulcani[,c('Categoria','species')]

fig <-
  plot_ly(
    labels = ~ c("Ateles geoffroyi", "Cebus capucinus", "", ""),
    values = ~ c(1994, 599, 453, 1463),
    type = 'pie') %>%
    config(locale = "es") %>% layout(
    title = 'Especies de Junco Volcanico',
    
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    )
  )
    
fig

  
# Mapa de registros de presencia
junco_vulcani %>%
  select(stateProvince,
         canton,
         locality,
         eventDate,
         decimalLongitude,
         decimalLatitude) %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
  addCircleMarkers(
    stroke = F,
    radius = 4,
    fillColor = 'gray',
    fillOpacity = 1,
    popup = paste(
      junco_vulcani$stateProvince,
      junco_vulcani$canton,
      junco_vulcani$locality,
      junco_vulcani$eventDate,
      junco_vulcani$decimalLongitude,
      junco_vulcani$decimalLatitude,
      sep = '<br/>'
    ),
    group = "Junco vulcani"
  ) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Stamen Toner Lite", "Imágenes de ESRI"),
    overlayGroups = c("Junco vulcani")
  ) %>%
  addMiniMap(
    tiles = providers$Stamen.OpenStreetMap.Mapnik,
    position = "bottomleft",
    toggleDisplay = TRUE
  )

  
  
  
  
  
  # Mapa de registros de presencia
  primates_cr %>%
    select(stateProvince,
           canton,
           locality,
           eventDate,
           decimalLongitude,
           decimalLatitude) %>%
    leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
    addCircleMarkers(
      stroke = F,
      radius = 4,
      fillColor = 'gray',
      fillOpacity = 1,
      popup = paste(
        primates_cr$stateProvince,
        primates_cr$canton,
        primates_cr$locality,
        primates_cr$eventDate,
        primates_cr$decimalLongitude,
        primates_cr$decimalLatitude,
        sep = '<br/>'
      ),
      group = "Primates"
    ) %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Stamen Toner Lite", "Imágenes de ESRI"),
      overlayGroups = c("Primates")
    ) %>%
    addMiniMap(
      tiles = providers$Stamen.OpenStreetMap.Mapnik,
      position = "bottomleft",
      toggleDisplay = TRUE
    )
  
