# =============================================================================
# SHINY APP: MAPA DE SUPERFICIE COSECHADA (P217_SUP_ha) EN PERÚ
# =============================================================================

library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(DT)

# -------------------------------------------------------------------
# DATOS SIMULADOS CON PROVINCIAS Y DISTRITOS REALES
# -------------------------------------------------------------------
set.seed(123)

# Estructura jerárquica: Departamento -> Provincia -> Distrito con coordenadas reales
estructura_peru <- data.frame(
  NOMBREDD = c(
    # PUNO
    rep("PUNO", 15),
    # CUSCO
    rep("CUSCO", 15),
    # AREQUIPA
    rep("AREQUIPA", 15),
    # LIMA
    rep("LIMA", 15),
    # JUNÍN
    rep("JUNÍN", 15),
    # CAJAMARCA
    rep("CAJAMARCA", 15),
    # LA LIBERTAD
    rep("LA LIBERTAD", 15),
    # PIURA
    rep("PIURA", 15),
    # ÁNCASH
    rep("ÁNCASH", 15),
    # AYACUCHO
    rep("AYACUCHO", 15)
  ),
  NOMBREPV = c(
    # PUNO
    rep("PUNO", 3), rep("AZÁNGARO", 3), rep("SAN ROMÁN", 3), rep("MELGAR", 3), rep("CHUCUITO", 3),
    # CUSCO
    rep("CUSCO", 3), rep("ANTA", 3), rep("CALCA", 3), rep("CANCHIS", 3), rep("URUBAMBA", 3),
    # AREQUIPA
    rep("AREQUIPA", 3), rep("CAYLLOMA", 3), rep("CAMANÁ", 3), rep("CASTILLA", 3), rep("ISLAY", 3),
    # LIMA
    rep("LIMA", 3), rep("HUARAL", 3), rep("CAÑETE", 3), rep("BARRANCA", 3), rep("HUAURA", 3),
    # JUNÍN
    rep("HUANCAYO", 3), rep("JAUJA", 3), rep("CONCEPCIÓN", 3), rep("TARMA", 3), rep("SATIPO", 3),
    # CAJAMARCA
    rep("CAJAMARCA", 3), rep("JAÉN", 3), rep("SAN IGNACIO", 3), rep("CHOTA", 3), rep("CUTERVO", 3),
    # LA LIBERTAD
    rep("TRUJILLO", 3), rep("ASCOPE", 3), rep("PACASMAYO", 3), rep("OTUZCO", 3), rep("SANTIAGO DE CHUCO", 3),
    # PIURA
    rep("PIURA", 3), rep("SULLANA", 3), rep("TALARA", 3), rep("PAITA", 3), rep("MORROPÓN", 3),
    # ÁNCASH
    rep("HUARAZ", 3), rep("CARHUAZ", 3), rep("HUAYLAS", 3), rep("YUNGAY", 3), rep("SANTA", 3),
    # AYACUCHO
    rep("HUAMANGA", 3), rep("HUANTA", 3), rep("LA MAR", 3), rep("CANGALLO", 3), rep("LUCANAS", 3)
  ),
  NOMBREDI = c(
    # PUNO (15)
    "Puno", "Acora", "Ilave", "Azángaro", "Asillo", "San José", 
    "Juliaca", "Cabana", "Cabanillas", "Ayaviri", "Nuñoa", "Santa Rosa",
    "Juli", "Pomata", "Zepita",
    # CUSCO (15)
    "Cusco", "Santiago", "Wanchaq", "Anta", "Ancahuasi", "Chinchaypujio",
    "Calca", "Pisac", "Lamay", "Sicuani", "Combapata", "Marangani",
    "Urubamba", "Ollantaytambo", "Maras",
    # AREQUIPA (15)
    "Arequipa", "Cayma", "Cerro Colorado", "Chivay", "Cabanaconde", "Maca",
    "Camaná", "Samuel Pastor", "Ocoña", "Aplao", "Viraco", "Uraca",
    "Mollendo", "Mejía", "Cocachacra",
    # LIMA (15)
    "Lima", "San Juan de Lurigancho", "San Martin de Porres", "Huaral", "Aucallama", "Chancay",
    "San Vicente de Cañete", "Imperial", "Mala", "Barranca", "Paramonga", "Supe",
    "Huacho", "Santa María", "Végueta",
    # JUNÍN (15)
    "Huancayo", "El Tambo", "Chilca", "Jauja", "Apata", "Yauyos",
    "Concepción", "Santa Rosa de Ocopa", "Matahuasi", "Tarma", "Acobamba", "Palca",
    "Satipo", "Mazamari", "Río Tambo",
    # CAJAMARCA (15)
    "Cajamarca", "Baños del Inca", "Llacanora", "Jaén", "Bellavista", "Chontali",
    "San Ignacio", "Chirinos", "Huarango", "Chota", "Lajas", "Chalamarca",
    "Cutervo", "Santo Domingo de la Capilla", "Pimpingos",
    # LA LIBERTAD (15)
    "Trujillo", "Víctor Larco", "La Esperanza", "Ascope", "Chicama", "Casa Grande",
    "Pacasmayo", "San Pedro de Lloc", "Guadalupe", "Otuzco", "Usquil", "Mache",
    "Santiago de Chuco", "Angasmarca", "Cachicadán",
    # PIURA (15)
    "Piura", "Castilla", "Catacaos", "Sullana", "Bellavista", "Marcavelica",
    "Talara", "Pariñas", "El Alto", "Paita", "Colán", "La Huaca",
    "Morropón", "Chulucanas", "Buenos Aires",
    # ÁNCASH (15)
    "Huaraz", "Independencia", "Pampas", "Carhuaz", "Marcará", "Ataquero",
    "Caraz", "Huaylas", "Pueblo Libre", "Yungay", "Ranrahirca", "Mancos",
    "Chimbote", "Nuevo Chimbote", "Santa",
    # AYACUCHO (15)
    "Ayacucho", "Carmen Alto", "San Juan Bautista", "Huanta", "Luricocha", "Santillana",
    "San Miguel", "Anco", "Ayna", "Cangallo", "Los Morochucos", "Paras",
    "Puquio", "Lucanas", "Sancos"
  ),
  latitud = c(
    # PUNO
    -15.8402, -15.9724, -16.0833, -14.9055, -14.6949, -14.3456,
    -15.5000, -15.6389, -15.6500, -14.8827, -14.4789, -14.6234,
    -16.2089, -16.0667, -16.5000,
    # CUSCO
    -13.5319, -13.5225, -13.5356, -13.4728, -13.6234, -13.5678,
    -13.3267, -13.4208, -13.3892, -14.2686, -14.1034, -14.3567,
    -13.3056, -13.2589, -13.3456,
    # AREQUIPA
    -16.4090, -16.3700, -16.3456, -15.5067, -15.6178, -15.6500,
    -16.6217, -16.5789, -16.4234, -16.0644, -15.7234, -15.8567,
    -17.0233, -16.9876, -17.0567,
    # LIMA
    -12.0464, -11.9956, -12.0234, -11.4956, -11.3789, -11.4567,
    -13.0781, -13.0567, -12.6589, -10.7567, -10.6734, -10.7989,
    -11.1067, -11.0867, -11.0234,
    # JUNÍN
    -12.0656, -12.0389, -12.0856, -11.7758, -11.7234, -11.7989,
    -11.9189, -11.8567, -11.9456, -11.4189, -11.3456, -11.4789,
    -11.2522, -11.1234, -11.4156,
    # CAJAMARCA
    -7.1611, -7.1456, -7.1889, -5.7078, -5.6234, -5.7456,
    -5.1456, -5.0678, -5.2134, -6.5567, -6.4789, -6.6234,
    -6.3700, -6.2456, -6.4567,
    # LA LIBERTAD
    -8.1116, -8.1345, -8.0889, -7.7144, -7.8456, -7.6734,
    -7.4006, -7.3234, -7.4789, -7.9039, -7.8234, -7.9567,
    -8.1478, -8.0567, -8.2234,
    # PIURA
    -5.1945, -5.1789, -5.2667, -4.9033, -4.8234, -4.9789,
    -4.5772, -4.6234, -4.5034, -5.0892, -5.0234, -5.1567,
    -5.1722, -5.1034, -5.2456,
    # ÁNCASH
    -9.5278, -9.5089, -9.5567, -9.2856, -9.2234, -9.3456,
    -9.0400, -8.9567, -9.1234, -9.1378, -9.0234, -9.2067,
    -9.0853, -9.1234, -8.9789,
    # AYACUCHO
    -13.1631, -13.1456, -13.1889, -12.9356, -12.8567, -12.9989,
    -13.0122, -12.8456, -12.6234, -13.6267, -13.5234, -13.7089,
    -14.7050, -14.6234, -14.7789
  ),
  longitud = c(
    # PUNO
    -70.0219, -69.7889, -69.6456, -70.1967, -70.3456, -70.5234,
    -70.1322, -70.3789, -70.3456, -70.4861, -70.6234, -70.7567,
    -69.4589, -69.2734, -69.1234,
    # CUSCO
    -71.9675, -71.9456, -71.9889, -72.1378, -72.2567, -72.0789,
    -71.9578, -71.8456, -71.8234, -71.2286, -71.3456, -71.1789,
    -72.1167, -72.2634, -72.1456,
    # AREQUIPA
    -71.5375, -71.5089, -71.4567, -71.5983, -71.7234, -71.6456,
    -72.7111, -72.6234, -73.0456, -72.6283, -72.7789, -72.5234,
    -71.9456, -72.0567, -71.8234,
    # LIMA
    -77.0428, -76.9956, -77.0567, -77.2078, -77.1234, -77.2789,
    -76.3842, -76.4567, -76.6234, -77.7614, -77.8234, -77.7089,
    -77.6111, -77.5456, -77.6789,
    # JUNÍN
    -75.2144, -75.2567, -75.1789, -75.4972, -75.5234, -75.4567,
    -75.3147, -75.3789, -75.2456, -75.6906, -75.7567, -75.6234,
    -74.6378, -74.5234, -74.7789,
    # CAJAMARCA
    -78.5126, -78.4789, -78.5567, -78.8078, -78.8456, -78.7234,
    -79.0017, -79.0789, -78.9234, -78.6489, -78.7234, -78.5789,
    -78.8156, -78.8789, -78.7456,
    # LA LIBERTAD
    -79.0292, -79.0456, -79.0089, -79.1058, -79.1567, -79.0734,
    -79.5714, -79.6234, -79.5089, -78.5644, -78.6234, -78.5089,
    -78.1778, -78.2567, -78.1234,
    # PIURA
    -80.6328, -80.6089, -80.6789, -80.6850, -80.7234, -80.6456,
    -81.2717, -81.3234, -81.2089, -81.1139, -81.0567, -81.1789,
    -80.0728, -80.1234, -80.0089,
    # ÁNCASH
    -77.5278, -77.5089, -77.5567, -77.6439, -77.6789, -77.6234,
    -77.8106, -77.8567, -77.7789, -77.7481, -77.7089, -77.7956,
    -78.5778, -78.5234, -78.6456,
    # AYACUCHO
    -74.2236, -74.2456, -74.1889, -74.2478, -74.3234, -74.1789,
    -73.9800, -73.8567, -73.5234, -74.1378, -74.0234, -74.2567,
    -74.1300, -74.0567, -74.2234
  )
)

# Generar múltiples observaciones por distrito
base <- estructura_peru %>%
  slice(rep(1:n(), each = 3)) %>%  # 3 observaciones por distrito
  mutate(
    ANIO = sample(2020:2023, n(), replace = TRUE),
    P204_NOM = sample(c("Papa", "Maíz", "Quinua", "Trigo", "Café", 
                        "Cacao", "Arroz", "Plátano", "Yuca", "Frijol"), 
                      n(), replace = TRUE),
    P217_SUP_ha = round(runif(n(), 0.5, 50), 2),
    # Añadir pequeña variación aleatoria a las coordenadas (±0.05 grados)
    latitud = latitud + runif(n(), -0.05, 0.05),
    longitud = longitud + runif(n(), -0.05, 0.05)
  )

# -------------------------------------------------------------------
# INTERFAZ DE USUARIO
# -------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Mapa de Superficie Cosechada (P217_SUP_ha) - ENA 2023"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtros de visualización"),
      selectInput("region", "Departamento:",
                  choices = c("Todos", sort(unique(base$NOMBREDD))),
                  selected = "Todos"),
      selectInput("provincia", "Provincia:",
                  choices = c("Todas"),
                  selected = "Todas"),
      selectInput("cultivo", "Cultivo:",
                  choices = c("Todos", sort(unique(base$P204_NOM))),
                  selected = "Todos"),
      sliderInput("superficie_min", "Superficie mínima (ha):",
                  min = 0, max = 50, value = 0, step = 1),
      hr(),
      h5("Variable mapeada:"),
      p(strong("P217_SUP_ha:"), "Superficie cosechada en hectáreas"),
      p("Fuente: CAP200AB - ENA 2023"),
      hr(),
      h5("Total de registros:", textOutput("n_registros", inline = TRUE))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Mapa interactivo", 
                 leafletOutput("mapa", height = "650px")),
        tabPanel("Estadísticas", 
                 h4("Resumen por distrito"),
                 DTOutput("tabla"),
                 hr(),
                 plotOutput("histograma", height = "400px"))
      )
    )
  )
)

# -------------------------------------------------------------------
# SERVIDOR
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Actualizar provincias según departamento seleccionado
  observe({
    if (input$region == "Todos") {
      provincias <- c("Todas", sort(unique(base$NOMBREPV)))
    } else {
      provincias <- c("Todas", sort(unique(base$NOMBREPV[base$NOMBREDD == input$region])))
    }
    updateSelectInput(session, "provincia", choices = provincias)
  })
  
  # Datos filtrados según selección
  datos_filtrados <- reactive({
    datos <- base
    
    # Filtrar por departamento
    if (input$region != "Todos") {
      datos <- datos %>% filter(NOMBREDD == input$region)
    }
    
    # Filtrar por provincia
    if (input$provincia != "Todas") {
      datos <- datos %>% filter(NOMBREPV == input$provincia)
    }
    
    # Filtrar por cultivo
    if (input$cultivo != "Todos") {
      datos <- datos %>% filter(P204_NOM == input$cultivo)
    }
    
    # Filtrar por superficie mínima
    datos <- datos %>% filter(P217_SUP_ha >= input$superficie_min)
    
    return(datos)
  })
  
  # Número de registros
  output$n_registros <- renderText({
    nrow(datos_filtrados())
  })
  
  # Mapa interactivo con Leaflet
  output$mapa <- renderLeaflet({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(leaflet() %>% 
               addProviderTiles("CartoDB.Positron") %>%
               setView(lng = -75.0152, lat = -9.19, zoom = 5))
    }
    
    # Paleta de colores
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = datos$P217_SUP_ha
    )
    
    leaflet(datos) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~longitud, 
        lat = ~latitud,
        radius = 4,  # Tamaño fijo pequeño
        color = ~pal(P217_SUP_ha),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        opacity = 0.8,
        popup = ~paste(
          "<b>Departamento:</b>", NOMBREDD, "<br>",
          "<b>Provincia:</b>", NOMBREPV, "<br>",
          "<b>Distrito:</b>", NOMBREDI, "<br>",
          "<b>Cultivo:</b>", P204_NOM, "<br>",
          "<b>Superficie:</b>", P217_SUP_ha, "ha", "<br>",
          "<b>Año:</b>", ANIO
        )
      ) %>%
      addLegend(
        "bottomright", 
        pal = pal,
        values = ~P217_SUP_ha, 
        title = "Superficie<br>cosechada (ha)",
        opacity = 1
      )
  })
  
  # Tabla descriptiva por distrito
  output$tabla <- renderDT({
    datos <- datos_filtrados() %>%
      group_by(NOMBREDD, NOMBREPV, NOMBREDI) %>%
      summarise(
        `N° parcelas` = n(),
        `Sup. total (ha)` = round(sum(P217_SUP_ha), 2),
        `Sup. promedio (ha)` = round(mean(P217_SUP_ha), 2),
        `Sup. mínima (ha)` = round(min(P217_SUP_ha), 2),
        `Sup. máxima (ha)` = round(max(P217_SUP_ha), 2),
        .groups = "drop"
      ) %>%
      arrange(NOMBREDD, NOMBREPV, desc(`Sup. total (ha)`))
    
    datatable(datos, 
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Histograma de superficie cosechada
  output$histograma <- renderPlot({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(ggplot() + theme_void() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No hay datos para mostrar", size = 6))
    }
    
    ggplot(datos, aes(x = P217_SUP_ha)) +
      geom_histogram(bins = 30, fill = "#E74C3C", color = "white", alpha = 0.8) +
      geom_vline(aes(xintercept = mean(P217_SUP_ha)), 
                 color = "blue", linetype = "dashed", size = 1) +
      annotate("text", 
               x = mean(datos$P217_SUP_ha), 
               y = Inf, 
               label = paste("Media:", round(mean(datos$P217_SUP_ha), 2), "ha"),
               vjust = 2, color = "blue", size = 4) +
      labs(
        title = "Distribución de Superficie Cosechada (P217_SUP_ha)",
        x = "Superficie cosechada (hectáreas)",
        y = "Frecuencia"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
}

# -------------------------------------------------------------------
# EJECUTAR APP
# -------------------------------------------------------------------
shinyApp(ui, server)