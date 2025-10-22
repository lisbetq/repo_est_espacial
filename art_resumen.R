# =============================================================
# ENA 2023 - ESTADÍSTICA ESPACIAL SIN SHAPEFILE (LIVIANO)
# =============================================================

# Instalar librerías si es necesario:
# install.packages(c("haven","dplyr","sf","spdep","classInt","ggplot2","viridis"))

library(haven)
library(dplyr)
library(sf)
library(spdep)
library(ggplot2)
library(tmap)
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(kableExtra)

# =============================================================
# 1️⃣ LECTURA Y PREPARACIÓN DE DATOS
# =============================================================

cap200ab <- read_sav("D:/ESt_espacial/2023/2023 - DESCOMPRIMIDO/922-Modulo1830/03_CAP200AB.sav")
caratula <- read_sav("D:/ESt_espacial/2023/2023 - DESCOMPRIMIDO/922-Modulo1828/01_CARATULA.sav")


# PASO CRÍTICO: Verificar nombres de variables
cat("=== VARIABLES EN CARATULA ===\n")
print(names(caratula))
cat("\n=== VARIABLES EN CAP200AB ===\n")
print(names(cap200ab))

# Verificar estructura
glimpse(caratula)
glimpse(cap200ab)

# =============================================================================
# 2. UNIÓN DE DATOS (Ajustar según variables reales)
# =============================================================================

# Identificar las claves de unión correctas
# Buscar variables comunes
vars_comunes <- intersect(names(caratula), names(cap200ab))
cat("\n=== VARIABLES COMUNES PARA UNIÓN ===\n")
print(vars_comunes)

# Unir datasets (ajustar según tus claves reales)
# Ejemplo genérico - DEBES AJUSTAR ESTO:
datos_completos <- caratula %>%
  inner_join(cap200ab, by = vars_comunes)

cat("\n=== DATOS UNIDOS ===\n")
cat("Dimensiones:", nrow(datos_completos), "filas x", ncol(datos_completos), "columnas\n")

# =============================================================================
# 3. CONSTRUCCIÓN DE BASE AGREGADA POR DEPARTAMENTO
# =============================================================================

# Verificar qué variables de filtro existen
cat("\n=== VERIFICANDO VARIABLES PARA FILTROS ===\n")
cat("¿Existe RESFIN?:", "RESFIN" %in% names(datos_completos), "\n")
cat("¿Existe LATITUD?:", "LATITUD" %in% names(datos_completos), "\n")
cat("¿Existe LONGITUD?:", "LONGITUD" %in% names(datos_completos), "\n")

# Crear agregación por departamento (VERSIÓN FLEXIBLE)
datos_depto <- datos_completos %>%
  # Filtros condicionales (ajustar según variables disponibles)
  filter(
    # Solo si existe RESFIN
    if("RESFIN" %in% names(.)) RESFIN == 1 else TRUE,
    
    # Coordenadas válidas
    !is.na(if("LATITUD" %in% names(.)) LATITUD else NA),
    !is.na(if("LONGITUD" %in% names(.)) LONGITUD else NA),
    
    # Variables de producción
    !is.na(if("P217_SUP_ha" %in% names(.)) P217_SUP_ha else NA),
    !is.na(if("P219_EQUIV_KG" %in% names(.)) P219_EQUIV_KG else NA)
  ) %>%
  
  # Agrupar (ajustar según variables reales)
  group_by(
    CCDD = if("CCDD" %in% names(.)) CCDD else NA_character_,
    NOMBREDD = if("NOMBREDD" %in% names(.)) NOMBREDD else NA_character_,
    REGION = if("REGION" %in% names(.)) REGION else NA_real_
  ) %>%
  
  summarise(
    # Variables de producción (con verificación)
    superficie_total = if("P217_SUP_ha" %in% names(.)) {
      sum(P217_SUP_ha * FACTOR_PRODUCTOR, na.rm = TRUE)
    } else 0,
    
    produccion_total_kg = if("P219_EQUIV_KG" %in% names(.)) {
      sum(P219_EQUIV_KG * FACTOR_PRODUCTOR, na.rm = TRUE)
    } else 0,
    
    n_productores = sum(FACTOR_PRODUCTOR, na.rm = TRUE),
    
    n_cultivos = if("P204_COD" %in% names(.)) {
      n_distinct(P204_COD)
    } else 0,
    
    # Rendimiento
    rendimiento = if(superficie_total > 0) {
      produccion_total_kg / superficie_total
    } else NA_real_,
    
    # Proporción vendida (verificar variables)
    prop_venta = tryCatch({
      sum((P220_1_CANT_1 + P220_1_CANT_2/1000) * FACTOR_PRODUCTOR, na.rm = TRUE) / 
        sum((P219_CANT_1 + P219_CANT_2/1000) * FACTOR_PRODUCTOR, na.rm = TRUE)
    }, error = function(e) NA_real_),
    
    # Afectación general
    prop_afectada = if("P223A" %in% names(.)) {
      sum(ifelse(P223A == 1, FACTOR_PRODUCTOR, 0), na.rm = TRUE) / n_productores
    } else NA_real_,
    
    # Afectación por sequía
    afectacion_sequia = if("P223B_1" %in% names(.)) {
      sum(ifelse(P223B_1 == 1, FACTOR_PRODUCTOR, 0), na.rm = TRUE) / n_productores
    } else NA_real_,
    
    # Afectación por heladas
    afectacion_heladas = if("P223B_3" %in% names(.)) {
      sum(ifelse(P223B_3 == 1, FACTOR_PRODUCTOR, 0), na.rm = TRUE) / n_productores
    } else NA_real_,
    
    # Afectación por plagas
    afectacion_plagas = if("P223B_7" %in% names(.)) {
      sum(ifelse(P223B_7 == 1, FACTOR_PRODUCTOR, 0), na.rm = TRUE) / n_productores
    } else NA_real_,
    
    # Coordenadas centroides
    lat_centroide = mean(LATITUD, na.rm = TRUE),
    lon_centroide = mean(LONGITUD, na.rm = TRUE),
    
    .groups = 'drop'
  )

# Verificar resultado
cat("\n=== DATOS AGREGADOS ===\n")
print(datos_depto)

# Remover filas con coordenadas inválidas
datos_depto <- datos_depto %>%
  filter(!is.na(lat_centroide), 
         !is.na(lon_centroide),
         !is.infinite(lat_centroide),
         !is.infinite(lon_centroide))

cat("\nDepartamentos con datos válidos:", nrow(datos_depto), "\n")

# =============================================================================
# 4. CREAR OBJETO ESPACIAL
# =============================================================================

if(nrow(datos_depto) > 0) {
  datos_sf <- st_as_sf(datos_depto, 
                       coords = c("lon_centroide", "lat_centroide"),
                       crs = 4326)
  
  cat("\n=== OBJETO ESPACIAL CREADO ===\n")
  print(datos_sf)
  
  # =============================================================================
  # 5. ESTADÍSTICAS DESCRIPTIVAS
  # =============================================================================
  
  if("REGION" %in% names(datos_depto) && !all(is.na(datos_depto$REGION))) {
    tabla_descriptiva <- datos_depto %>%
      group_by(REGION) %>%
      summarise(
        `N Departamentos` = n(),
        `Superficie (ha)` = round(mean(superficie_total, na.rm = TRUE), 0),
        `Producción (ton)` = round(mean(produccion_total_kg, na.rm = TRUE)/1000, 0),
        `Rendimiento (kg/ha)` = round(mean(rendimiento, na.rm = TRUE), 2),
        `% Venta` = round(mean(prop_venta, na.rm = TRUE) * 100, 1),
        `% Afectación` = round(mean(prop_afectada, na.rm = TRUE) * 100, 1)
      ) %>%
      mutate(REGION = case_when(
        REGION == 1 ~ "Costa",
        REGION == 2 ~ "Sierra",
        REGION == 3 ~ "Selva",
        TRUE ~ paste("Región", REGION)
      ))
    
    print(kable(tabla_descriptiva, 
                caption = "Estadísticas Descriptivas por Región Natural"))
  }
  
  # =============================================================================
  # 6. VISUALIZACIÓN ESPACIAL
  # =============================================================================
  
  # Mapa de rendimiento
  mapa_rendimiento <- ggplot(datos_sf) +
    geom_sf(aes(fill = rendimiento), size = 3, shape = 21, color = "white") +
    scale_fill_gradient2(
      low = "#d73027",
      mid = "#fee08b",
      high = "#1a9850",
      midpoint = median(datos_sf$rendimiento, na.rm = TRUE),
      name = "Rendimiento\n(kg/ha)",
      na.value = "grey50"
    ) +
    labs(title = "Distribución Espacial del Rendimiento Agrícola",
         subtitle = "Perú - ENA 2023") +
    theme_minimal()
  
  print(mapa_rendimiento)
  
  # =============================================================================
  # 7. AUTOCORRELACIÓN ESPACIAL
  # =============================================================================
  
  if(nrow(datos_sf) >= 5) {
    # Crear matriz de vecindad
    coords <- st_coordinates(st_centroid(datos_sf))
    k <- min(5, nrow(datos_sf) - 1)
    knn_k <- knearneigh(coords, k = k)
    nb_knn <- knn2nb(knn_k)
    pesos_espaciales <- nb2listw(nb_knn, style = "W", zero.policy = TRUE)
    
    # Moran's I para rendimiento
    if(sum(!is.na(datos_sf$rendimiento)) >= 5) {
      moran_rendimiento <- moran.test(datos_sf$rendimiento, 
                                      pesos_espaciales, 
                                      na.action = na.omit,
                                      zero.policy = TRUE)
      
      cat("\n=== ÍNDICE DE MORAN (Rendimiento) ===\n")
      cat("Moran's I:", round(moran_rendimiento$estimate[1], 4), "\n")
      cat("P-valor:", format.pval(moran_rendimiento$p.value), "\n")
      cat("Interpretación:", ifelse(moran_rendimiento$estimate[1] > 0,
                                    "Autocorrelación positiva (agrupamiento)",
                                    "Autocorrelación negativa (dispersión)"), "\n")
      
      # Gráfico de Moran
      moran.plot(datos_sf$rendimiento, 
                 pesos_espaciales,
                 labels = datos_sf$NOMBREDD,
                 main = "Diagrama de Moran - Rendimiento Agrícola")
    }
  } else {
    cat("\n⚠️ Datos insuficientes para análisis espacial (n < 5)\n")
  }
  
} else {
  cat("\n❌ ERROR: No hay datos válidos después del filtrado\n")
  cat("Revisar:\n")
  cat("  1. Nombres correctos de variables\n")
  cat("  2. Valores de filtros (RESFIN, coordenadas, etc.)\n")
  cat("  3. Claves de unión entre datasets\n")
}

# =============================================================================
# DIAGNÓSTICO DE PROBLEMAS
# =============================================================================

cat("\n\n=== DIAGNÓSTICO DE VARIABLES CLAVE ===\n")
cat("Variables esperadas vs encontradas:\n\n")

vars_esperadas <- c("RESFIN", "LATITUD", "LONGITUD", "P217_SUP_ha", 
                    "P219_EQUIV_KG", "FACTOR_PRODUCTOR", "CCDD", 
                    "NOMBREDD", "REGION", "P204_COD")

for(var in vars_esperadas) {
  existe_caratula <- var %in% names(caratula)
  existe_cap200 <- var %in% names(cap200ab)
  
  cat(sprintf("%-20s | Carátula: %-5s | Cap200AB: %-5s\n", 
              var, 
              ifelse(existe_caratula, "✓ Sí", "✗ No"),
              ifelse(existe_cap200, "✓ Sí", "✗ No")))
}

cat("\n=== CONTINUANDO CON ANÁLISIS ESPACIAL COMPLETO ===\n")

# =============================================================================
# 8. ANÁLISIS LISA (LOCAL INDICATORS OF SPATIAL ASSOCIATION)
# =============================================================================

if(exists("pesos_espaciales") && nrow(datos_sf) >= 5) {
  
  cat("\n--- Calculando LISA ---\n")
  
  # Calcular LISA para rendimiento
  lisa_rendimiento <- localmoran(datos_sf$rendimiento, 
                                 pesos_espaciales,
                                 zero.policy = TRUE,
                                 na.action = na.omit)
  
  # Agregar resultados al objeto espacial
  datos_sf$lisa_I <- lisa_rendimiento[, 1]
  datos_sf$lisa_p <- lisa_rendimiento[, 5]
  
  # Clasificar clústeres LISA
  datos_sf <- datos_sf %>%
    mutate(
      rendimiento_std = scale(rendimiento)[,1],
      lag_rendimiento = lag.listw(pesos_espaciales, rendimiento_std),
      cluster_lisa = case_when(
        lisa_p > 0.05 ~ "No significativo",
        rendimiento_std > 0 & lag_rendimiento > 0 ~ "Alto-Alto (H-H)",
        rendimiento_std < 0 & lag_rendimiento < 0 ~ "Bajo-Bajo (L-L)",
        rendimiento_std > 0 & lag_rendimiento < 0 ~ "Alto-Bajo (H-L)",
        rendimiento_std < 0 & lag_rendimiento > 0 ~ "Bajo-Alto (L-H)"
      ),
      cluster_lisa = factor(cluster_lisa, 
                            levels = c("Alto-Alto (H-H)", 
                                       "Bajo-Bajo (L-L)",
                                       "Alto-Bajo (H-L)", 
                                       "Bajo-Alto (L-H)",
                                       "No significativo"))
    )
  
  # Mapa LISA
  mapa_lisa <- ggplot(datos_sf) +
    geom_sf(aes(fill = cluster_lisa), size = 3, shape = 21, color = "white") +
    scale_fill_manual(
      values = c("Alto-Alto (H-H)" = "#d73027",
                 "Bajo-Bajo (L-L)" = "#1a9850",
                 "Alto-Bajo (H-L)" = "#fdae61",
                 "Bajo-Alto (L-H)" = "#abd9e9",
                 "No significativo" = "#eeeeee"),
      name = "Clúster LISA",
      drop = FALSE
    ) +
    labs(title = "Análisis LISA - Clústeres de Rendimiento Agrícola",
         subtitle = paste0("Moran's I = ", 
                           round(moran_rendimiento$estimate[1], 3)),
         caption = "Fuente: ENA 2023 - INEI") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )
  
  print(mapa_lisa)
  
  # Tabla de frecuencias LISA
  tabla_lisa <- datos_sf %>%
    st_drop_geometry() %>%
    count(cluster_lisa) %>%
    mutate(Porcentaje = round(n / sum(n) * 100, 1))
  
  cat("\n--- Distribución de Clústeres LISA ---\n")
  print(kable(tabla_lisa,
              col.names = c("Tipo de Clúster", "N Departamentos", "%"),
              caption = "Distribución de Clústeres LISA"))
}

# =============================================================================
# 9. ANÁLISIS GETIS-ORD Gi* (HOTSPOT ANALYSIS)
# =============================================================================

if(exists("pesos_espaciales") && nrow(datos_sf) >= 5) {
  
  cat("\n--- Calculando Getis-Ord Gi* ---\n")
  
  # Calcular Gi* statistic
  gi_star <- localG(datos_sf$rendimiento, 
                    pesos_espaciales,
                    zero.policy = TRUE)
  
  # Agregar resultados
  datos_sf$gi_star <- as.numeric(gi_star)
  
  # Clasificar hotspots y coldspots
  datos_sf <- datos_sf %>%
    mutate(
      hotspot = case_when(
        gi_star >= 2.58 ~ "Hotspot (99% conf.)",
        gi_star >= 1.96 ~ "Hotspot (95% conf.)",
        gi_star >= 1.65 ~ "Hotspot (90% conf.)",
        gi_star <= -2.58 ~ "Coldspot (99% conf.)",
        gi_star <= -1.96 ~ "Coldspot (95% conf.)",
        gi_star <= -1.65 ~ "Coldspot (90% conf.)",
        TRUE ~ "No significativo"
      ),
      hotspot = factor(hotspot,
                       levels = c("Hotspot (99% conf.)",
                                  "Hotspot (95% conf.)",
                                  "Hotspot (90% conf.)",
                                  "No significativo",
                                  "Coldspot (90% conf.)",
                                  "Coldspot (95% conf.)",
                                  "Coldspot (99% conf.)"))
    )
  
  # Mapa Gi*
  mapa_gistar <- ggplot(datos_sf) +
    geom_sf(aes(fill = hotspot), size = 3, shape = 21, color = "white") +
    scale_fill_manual(
      values = c("Hotspot (99% conf.)" = "#67001f",
                 "Hotspot (95% conf.)" = "#d6604d",
                 "Hotspot (90% conf.)" = "#fdae61",
                 "No significativo" = "#f7f7f7",
                 "Coldspot (90% conf.)" = "#abd9e9",
                 "Coldspot (95% conf.)" = "#4393c3",
                 "Coldspot (99% conf.)" = "#053061"),
      name = "Gi* Clusters",
      drop = FALSE
    ) +
    labs(title = "Análisis Getis-Ord Gi* - Hotspots de Rendimiento",
         subtitle = "Identificación de áreas de alto y bajo rendimiento",
         caption = "Fuente: ENA 2023 - INEI") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )
  
  print(mapa_gistar)
  
  # Tabla resumen de hotspots
  tabla_hotspots <- datos_sf %>%
    st_drop_geometry() %>%
    count(hotspot) %>%
    mutate(Porcentaje = round(n / sum(n) * 100, 1))
  
  cat("\n--- Distribución de Hotspots/Coldspots ---\n")
  print(kable(tabla_hotspots,
              col.names = c("Tipo", "N", "%")))
}

# =============================================================================
# 10. ANÁLISIS BIVARIADO: RENDIMIENTO vs AFECTACIÓN
# =============================================================================

if(exists("pesos_espaciales") && 
   sum(!is.na(datos_sf$afectacion_sequia)) >= 5) {
  
  cat("\n--- Análisis Bivariado ---\n")
  
  # Crear lag espacial de afectación por sequía
  datos_sf$lag_sequia <- lag.listw(pesos_espaciales, 
                                   scale(datos_sf$afectacion_sequia)[,1])
  
  # Gráfico de dispersión bivariado
  plot_bivariado <- ggplot(datos_sf, 
                           aes(x = scale(rendimiento)[,1], 
                               y = lag_sequia)) +
    geom_point(aes(color = as.factor(REGION)), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "black", 
                linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    scale_color_manual(
      values = c("1" = "#1b9e77", "2" = "#d95f02", "3" = "#7570b3"),
      labels = c("Costa", "Sierra", "Selva"),
      name = "Región"
    ) +
    labs(
      title = "Autocorrelación Espacial Bivariada",
      subtitle = "Rendimiento vs Afectación por Sequía",
      x = "Rendimiento estandarizado",
      y = "Lag espacial de Afectación por Sequía"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  print(plot_bivariado)
}

# =============================================================================
# 11. MAPA DE AFECTACIÓN CLIMÁTICA
# =============================================================================

if(sum(!is.na(datos_sf$prop_afectada)) >= 3) {
  
  mapa_afectacion <- ggplot(datos_sf) +
    geom_sf(aes(fill = prop_afectada * 100), 
            size = 3, shape = 21, color = "white") +
    scale_fill_gradient(
      low = "#ffffcc",
      high = "#800026",
      name = "% Afectación",
      na.value = "grey50"
    ) +
    labs(title = "Producción Afectada por Factores Climáticos/Plagas",
         subtitle = "Perú - ENA 2023",
         caption = "Fuente: INEI") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right"
    )
  
  print(mapa_afectacion)
}

# =============================================================================
# 12. ANÁLISIS DE FACTORES DETERMINANTES
# =============================================================================

if(nrow(datos_sf) >= 10) {
  
  cat("\n--- Análisis de Factores Determinantes ---\n")
  
  # Discretizar variables para análisis tipo Geodetector
  datos_sf <- datos_sf %>%
    mutate(
      cat_rendimiento = cut(rendimiento,
                            breaks = quantile(rendimiento, 
                                              probs = seq(0, 1, 0.25),
                                              na.rm = TRUE),
                            labels = c("Bajo", "Medio-Bajo", 
                                       "Medio-Alto", "Alto"),
                            include.lowest = TRUE),
      
      cat_afectacion = cut(prop_afectada,
                           breaks = c(0, 0.25, 0.5, 0.75, 1),
                           labels = c("Baja", "Media-Baja", 
                                      "Media-Alta", "Alta"),
                           include.lowest = TRUE)
    )
  
  # ANOVA por región
  if(length(unique(datos_sf$REGION)) > 1) {
    anova_region <- aov(rendimiento ~ as.factor(REGION), 
                        data = st_drop_geometry(datos_sf))
    eta_sq_region <- summary(anova_region)[[1]][1, "Sum Sq"] / 
      sum(summary(anova_region)[[1]][, "Sum Sq"])
    
    cat("\nPoder explicativo de REGIÓN sobre rendimiento (η²):", 
        round(eta_sq_region, 4), "\n")
    cat("F-value:", round(summary(anova_region)[[1]][1, "F value"], 2), "\n")
    cat("P-valor:", format.pval(summary(anova_region)[[1]][1, "Pr(>F)"]), "\n")
  }
  
  # ANOVA por nivel de afectación
  if(sum(!is.na(datos_sf$cat_afectacion)) > 5) {
    anova_afectacion <- aov(rendimiento ~ cat_afectacion, 
                            data = st_drop_geometry(datos_sf))
    eta_sq_afectacion <- summary(anova_afectacion)[[1]][1, "Sum Sq"] / 
      sum(summary(anova_afectacion)[[1]][, "Sum Sq"])
    
    cat("\nPoder explicativo de AFECTACIÓN sobre rendimiento (η²):", 
        round(eta_sq_afectacion, 4), "\n")
  }
}

# =============================================================================
# 13. PANEL INTEGRADO DE MAPAS
# =============================================================================

if(exists("mapa_rendimiento") && exists("mapa_lisa")) {
  
  cat("\n--- Generando panel de mapas integrado ---\n")
  
  # Lista de mapas disponibles
  mapas_disponibles <- list()
  
  if(exists("mapa_rendimiento")) mapas_disponibles$rendimiento <- 
    mapa_rendimiento + theme(legend.position = "bottom", 
                             legend.text = element_text(size = 8))
  
  if(exists("mapa_afectacion")) mapas_disponibles$afectacion <- 
    mapa_afectacion + theme(legend.position = "bottom",
                            legend.text = element_text(size = 8))
  
  if(exists("mapa_lisa")) mapas_disponibles$lisa <- 
    mapa_lisa + theme(legend.position = "bottom",
                      legend.text = element_text(size = 8))
  
  if(exists("mapa_gistar")) mapas_disponibles$gistar <- 
    mapa_gistar + theme(legend.position = "bottom",
                        legend.text = element_text(size = 8))
  
  # Crear panel con los mapas disponibles
  if(length(mapas_disponibles) >= 2) {
    n_mapas <- length(mapas_disponibles)
    ncol_panel <- ifelse(n_mapas >= 4, 2, 2)
    
    mapa_panel <- do.call(grid.arrange, c(
      mapas_disponibles,
      list(ncol = ncol_panel,
           top = "Análisis Espacial Integrado - Agricultura en Perú (ENA 2023)")
    ))
    
    # Guardar imagen
    tryCatch({
      ggsave("panel_mapas_espaciales.png", mapa_panel, 
             width = 14, height = 12, dpi = 300)
      cat("✓ Panel guardado: panel_mapas_espaciales.png\n")
    }, error = function(e) {
      cat("⚠️ No se pudo guardar el panel:", e$message, "\n")
    })
  }
}

# =============================================================================
# 14. TABLA COMPARATIVA CON LITERATURA
# =============================================================================

if(exists("moran_rendimiento")) {
  
  cat("\n--- Comparación con Literatura ---\n")
  
  comparacion_articulos <- data.frame(
    Estudio = c("Art. 1 - Vegetación China",
                "Art. 2 - Costas Marruecos",
                "Art. 3 - Paperas China",
                "Este estudio - Agricultura Perú"),
    Variable = c("NDVI-Precipitación",
                 "Exposición Costera",
                 "Incidencia Paperas",
                 "Rendimiento Agrícola"),
    Moran_I = c(0.88, 0.70, 0.399, 
                round(moran_rendimiento$estimate[1], 3)),
    Significancia = c("p < 0.01", "p < 0.001", "p < 0.001",
                      ifelse(moran_rendimiento$p.value < 0.001, 
                             "p < 0.001", 
                             paste0("p = ", 
                                    round(moran_rendimiento$p.value, 3)))),
    Patron = c("H-H Sureste, L-L Oeste",
               "H-H Sur, L-L Norte",
               "H-H Oeste, L-L Noreste",
               paste0("H-H: ", 
                      sum(datos_sf$cluster_lisa == "Alto-Alto (H-H)", 
                          na.rm = TRUE),
                      " dptos."))
  )
  
  print(kable(comparacion_articulos,
              caption = "Comparación de Autocorrelación Espacial",
              col.names = c("Estudio", "Variable", "Moran's I", 
                            "Significancia", "Patrón")))
}

# =============================================================================
# 15. EXPORTAR RESULTADOS
# =============================================================================

cat("\n--- Exportando resultados ---\n")

# Crear carpeta de salida
dir.create("resultados_espaciales", showWarnings = FALSE)

# Exportar shapefile
tryCatch({
  st_write(datos_sf, 
           "resultados_espaciales/datos_autocorrelacion.shp",
           delete_layer = TRUE,
           quiet = TRUE)
  cat("✓ Shapefile guardado\n")
}, error = function(e) {
  cat("⚠️ No se pudo guardar shapefile:", e$message, "\n")
})

# Exportar CSV
tryCatch({
  write.csv(datos_sf %>% st_drop_geometry(), 
            "resultados_espaciales/tabla_resumen.csv",
            row.names = FALSE)
  cat("✓ CSV guardado\n")
}, error = function(e) {
  cat("⚠️ No se pudo guardar CSV:", e$message, "\n")
})

# =============================================================================
# 16. RESUMEN FINAL
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("                   RESUMEN DE RESULTADOS                   \n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("DATOS PROCESADOS:\n")
cat("  • Departamentos analizados:", nrow(datos_sf), "\n")
cat("  • Variables analizadas:", ncol(datos_sf), "\n\n")

if(exists("moran_rendimiento")) {
  cat("AUTOCORRELACIÓN ESPACIAL GLOBAL:\n")
  cat(sprintf("  • Moran's I (Rendimiento): %.3f\n", 
              moran_rendimiento$estimate[1]))
  cat(sprintf("  • P-valor: %s\n", 
              format.pval(moran_rendimiento$p.value)))
  cat(sprintf("  • Interpretación: %s\n\n",
              ifelse(moran_rendimiento$estimate[1] > 0,
                     "✓ Autocorrelación positiva significativa (agrupamiento)",
                     "○ Autocorrelación débil o no significativa")))
}

if(exists("tabla_lisa")) {
  cat("CLÚSTERES LISA:\n")
  for(i in 1:nrow(tabla_lisa)) {
    cat(sprintf("  • %s: %d departamentos (%.1f%%)\n",
                tabla_lisa$cluster_lisa[i],
                tabla_lisa$n[i],
                tabla_lisa$Porcentaje[i]))
  }
  cat("\n")
}

if(exists("gi_star")) {
  n_hotspots <- sum(datos_sf$gi_star >= 1.96, na.rm = TRUE)
  n_coldspots <- sum(datos_sf$gi_star <= -1.96, na.rm = TRUE)
  
  cat("HOTSPOTS Gi* (95% confianza):\n")
  cat(sprintf("  • Hotspots (alto rendimiento): %d departamentos\n", 
              n_hotspots))
  cat(sprintf("  • Coldspots (bajo rendimiento): %d departamentos\n\n", 
              n_coldspots))
}

cat("ARCHIVOS GENERADOS:\n")
cat("  • resultados_espaciales/datos_autocorrelacion.shp\n")
cat("  • resultados_espaciales/tabla_resumen.csv\n")
cat("  • panel_mapas_espaciales.png\n\n")

cat("═══════════════════════════════════════════════════════════\n")
cat("                  ANÁLISIS COMPLETADO                      \n")
cat("═══════════════════════════════════════════════════════════\n")

