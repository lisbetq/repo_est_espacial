
#-------------------------------------------------------------

library(data.table)
library(sf)
library(spdep)
library(tmap)
library(haven)
library(dplyr)

#-------------------------------------------------------------
# 1️⃣ Cargar archivos SPSS (solo columnas necesarias)
#-------------------------------------------------------------
caratula <- as.data.table(read_sav("D:/ESt_espacial/final/2023/2023 - DESCOMPRIMIDO/922-Modulo1828/01_CARATULA.sav"))
cap200ab <- as.data.table(read_sav("D:/ESt_espacial/final/2023/2023 - DESCOMPRIMIDO/922-Modulo1830/03_CAP200AB.sav"))


#-------------------------------------------------------------
# 2️⃣ Filtrar datos antes de unir (ahorra RAM)
#-------------------------------------------------------------
caratula_fil <- caratula[
  !is.na(LATITUD) & !is.na(LONGITUD) & RESFIN == 1,
  .(ID_PROD, LATITUD, LONGITUD, REGION, NOMBREDD)
]

cap200_fil <- cap200ab[
  !is.na(P217_SUP_ha) & P217_SUP_ha > 0 & !is.na(P219_EQUIV_KG),
  .(ID_PROD, P204_NOM, P217_SUP_ha, P219_EQUIV_KG)
]

#-------------------------------------------------------------
# 3️⃣ Calcular rendimiento promedio por productor
#-------------------------------------------------------------
cap200_rend <- cap200_fil[
  , .(rendimiento = mean(P219_EQUIV_KG / P217_SUP_ha, na.rm = TRUE)),
  by = ID_PROD
]

#-------------------------------------------------------------
# 4️⃣ Unir con la carátula (ahora es unión 1 a 1)
#-------------------------------------------------------------
ena <- merge(cap200_rend, caratula_fil, by = "ID_PROD", all.x = TRUE)

#-------------------------------------------------------------
# 5️⃣ Convertir a objeto espacial (sf)
#-------------------------------------------------------------
ena_sf <- st_as_sf(ena, coords = c("LONGITUD", "LATITUD"), crs = 4326)
print(paste("Filas totales:", nrow(ena_sf)))
saveRDS(ena_sf, "D:/ESt_espacial/final/ena_sf_filtrado.rds")

#-------------------------------------------------------------
# 🌍 PARTE 2: Análisis espacial (Moran, Geary, LISA, Hotspots)
#-------------------------------------------------------------
# Cargar shapefile distrital del Perú
peru_distritos <- st_read("D:/ESt_espacial/final/gadm41_PER_3.shp")
peru_distritos <- st_transform(peru_distritos, crs = 4326)

# Cargar el dataset limpio
ena_sf <- readRDS("D:/ESt_espacial/final/ena_sf_filtrado.rds")

#-------------------------------------------------------------
# 6️⃣ Unión espacial: puntos ENA dentro de distritos
#-------------------------------------------------------------
ena_join <- st_join(ena_sf, peru_distritos, join = st_within)

ena_distrito <- ena_join %>%
  st_drop_geometry() %>%
  group_by(NAME_1, NAME_2, NAME_3) %>%
  summarise(rend_medio = mean(rendimiento, na.rm = TRUE))

peru_distritos <- left_join(peru_distritos, ena_distrito,
                            by = c("NAME_1", "NAME_2", "NAME_3"))

#-------------------------------------------------------------
# 7️⃣ Matriz de pesos espaciales (revisada)
#-------------------------------------------------------------
# Crear matriz de vecinos (tipo reina, tolerante a bordes)
nb_distritos <- poly2nb(peru_distritos, queen = TRUE, snap = 1e-6)

# Verificar si hay distritos sin vecinos
no_vecinos <- which(card(nb_distritos) == 0)
length(no_vecinos)

# Crear lista de pesos (con política de cero vecinos)
lw_distritos <- nb2listw(nb_distritos, style = "W", zero.policy = TRUE)

#-------------------------------------------------------------
# 8️⃣ Índices espaciales (manejo de NA)
#-------------------------------------------------------------
# Reemplazar valores NA por el promedio general (solo para el cálculo)
rend_vals <- peru_distritos$rend_medio
rend_vals[is.na(rend_vals)] <- mean(rend_vals, na.rm = TRUE)

# Moran Global
moran_d <- moran.test(rend_vals, lw_distritos, zero.policy = TRUE)
print(moran_d)

# Geary Global
geary_d <- geary.test(rend_vals, lw_distritos, zero.policy = TRUE)
print(geary_d)


#-------------------------------------------------------------
# 🔹 Moran Local (LISA) — con manejo de valores NA
#-------------------------------------------------------------

# Crear copia del vector de rendimientos
rend_vals <- peru_distritos$rend_medio

# Reemplazar NA con el promedio general
rend_vals[is.na(rend_vals)] <- mean(rend_vals, na.rm = TRUE)

# Calcular Moran Local
local_moran <- localmoran(rend_vals, lw_distritos, zero.policy = TRUE)
#############################################
# Revisar las columnas para confirmar estructura
colnames(local_moran)

# Asignar columnas al shapefile
peru_distritos$Ii <- local_moran[, 1]       # Índice local
peru_distritos$Z_Ii <- local_moran[, 4]     # Estadístico Z
peru_distritos$p_value <- local_moran[, 5]  # p-valor


# Clasificar clústeres espaciales
peru_distritos$cluster <- case_when(
  peru_distritos$Z_Ii > 1.96 & rend_vals > mean(rend_vals, na.rm = TRUE) ~ "Alto-Alto",
  peru_distritos$Z_Ii > 1.96 & rend_vals < mean(rend_vals, na.rm = TRUE) ~ "Bajo-Bajo",
  peru_distritos$Z_Ii < -1.96 & rend_vals > mean(rend_vals, na.rm = TRUE) ~ "Alto-Bajo",
  peru_distritos$Z_Ii < -1.96 & rend_vals < mean(rend_vals, na.rm = TRUE) ~ "Bajo-Alto",
  TRUE ~ "No significativo"
)


#-------------------------------------------------------------
# 🔥 9️⃣ Hotspots (Getis-Ord Gi*) — con manejo de NA
#-------------------------------------------------------------

# Crear copia del vector de rendimientos
rend_vals <- peru_distritos$rend_medio

# Reemplazar NA con el promedio general
rend_vals[is.na(rend_vals)] <- mean(rend_vals, na.rm = TRUE)

# Calcular Gi* (Getis-Ord)
gi_star <- localG(rend_vals, lw_distritos, zero.policy = TRUE)

# Agregar resultados al shapefile
peru_distritos$GiZ <- as.numeric(gi_star)

# Clasificar zonas calientes y frías
peru_distritos$Hotspot <- case_when(
  peru_distritos$GiZ >= 1.96 ~ "Hotspot",
  peru_distritos$GiZ <= -1.96 ~ "Coldspot",
  TRUE ~ "No significativo"
)


#-------------------------------------------------------------
# 🔟 Mapas finales
#-------------------------------------------------------------
tmap_mode("plot")

# Mapa de rendimiento promedio
tm_shape(peru_distritos) +
  tm_polygons("rend_medio", palette = "YlGn", title = "Rendimiento (kg/ha)") +
  tm_layout(main.title = "Productividad Agropecuaria - Distritos del Perú (ENA 2023)")

# Mapa LISA
tm_shape(peru_distritos) +
  tm_polygons("cluster", palette = c("red", "blue", "orange", "green", "grey"),
              title = "Clústeres LISA") +
  tm_layout(main.title = "Análisis LISA (Moran Local) - Distritos del Perú")

# Mapa Hotspots
tm_shape(peru_distritos) +
  tm_polygons("Hotspot", palette = c("red", "blue", "grey"),
              title = "Hotspots (Getis-Ord Gi*)") +
  tm_layout(main.title = "Zonas Calientes y Frías de Productividad - ENA 2023")

# Guardar resultados
saveRDS(peru_distritos, "D:/ESt_espacial/final/peru_distritos_resultados.rds")
#-------------------------------------------------------------
# 📊 PARTE FINAL: Tablas y Gráficos Estadísticos + Exportación
#-------------------------------------------------------------
#-------------------------------------------------------------
# 📊 VISUALIZACIONES EXPLICATIVAS — SIN MAPAS
#-------------------------------------------------------------
library(ggplot2)

#-------------------------------------------------------------
# 1️⃣ Distribución del rendimiento (general)
#-------------------------------------------------------------
ggplot(peru_distritos %>% st_drop_geometry(),
       aes(x = rend_medio)) +
  geom_histogram(bins = 40, fill = "darkseagreen3", color = "white") +
  labs(
    title = "Distribución del Rendimiento Promedio por Distrito",
    x = "Rendimiento (kg/ha)",
    y = "Frecuencia"
  ) +
  theme_minimal()

#-------------------------------------------------------------
# 2️⃣ Moran Scatterplot (autocorrelación global)
#-------------------------------------------------------------
moran_df <- data.frame(
  Rendimiento = scale(rend_vals),
  Vecinos = scale(lag.listw(lw_distritos, rend_vals))
)

ggplot(moran_df, aes(x = Rendimiento, y = Vecinos)) +
  geom_point(alpha = 0.6, color = "#1f78b4") +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Diagrama de Dispersión de Moran",
    subtitle = paste0("I de Moran = ", round(moran_d$estimate[1], 3),
                      " (p = ", round(moran_d$p.value, 4), ")"),
    x = "Rendimiento (centrado)",
    y = "Promedio ponderado de vecinos"
  ) +
  theme_minimal()

#-------------------------------------------------------------
# 3️⃣ Boxplot por Región Natural
#-------------------------------------------------------------
ggplot(peru_distritos %>% st_drop_geometry(),
       aes(x = NAME_1, y = rend_medio, fill = NAME_1)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  labs(
    title = "Distribución del Rendimiento por Región Natural",
    x = "Región",
    y = "Rendimiento promedio (kg/ha)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

#-------------------------------------------------------------
# 4️⃣ Ranking de Zonas Calientes y Frías (Gi*)
#-------------------------------------------------------------
ranking_gi <- peru_distritos %>%
  st_drop_geometry() %>%
  select(NAME_1, NAME_2, NAME_3, GiZ, Hotspot) %>%
  arrange(desc(GiZ)) %>%
  slice_head(n = 20)

ggplot(ranking_gi, aes(x = reorder(NAME_3, GiZ), y = GiZ, fill = Hotspot)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 distritos según estadístico Gi* (Getis-Ord)",
    subtitle = "Distritos con mayor y menor concentración espacial de rendimiento",
    x = "Distrito",
    y = "Z-score (Gi*)",
    fill = "Tipo"
  ) +
  scale_fill_manual(values = c("Hotspot" = "red", "Coldspot" = "blue", "No significativo" = "grey")) +
  theme_minimal()

#-------------------------------------------------------------
# 5️⃣ Relación Moran vs. Gi* (comparativo)
#-------------------------------------------------------------
comparativo <- peru_distritos %>%
  st_drop_geometry() %>%
  filter(!is.na(Z_Ii), !is.na(GiZ)) %>%
  mutate(Significativo = ifelse(p_value < 0.05, "Sí", "No"))


ggplot(comparativo, aes(x = Z_Ii, y = GiZ, color = Significativo)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Comparación entre estadísticos LISA (Z) y Hotspots (Gi*)",
    x = "Z-score (Moran Local)",
    y = "Z-score (Getis-Ord Gi*)",
    color = "Significativo (p<0.05)"
  ) +
  theme_minimal()
coinciden <- sum(peru_distritos$cluster %in% c("Alto-Alto", "Bajo-Bajo") &
                   peru_distritos$Hotspot %in% c("Hotspot", "Coldspot"), na.rm = TRUE)

total_validos <- sum(!is.na(peru_distritos$cluster) & !is.na(peru_distritos$Hotspot))
accuracy_coincidencia <- coinciden / total_validos

cat("🔁 Coincidencia LISA vs Gi*:",
    round(accuracy_coincidencia * 100, 2), "%\n")


#-------------------------------------------------------------
# ✅ Mensaje final
#-------------------------------------------------------------
cat("\n✅ Gráficos explicativos creados:\n",
    "✔ Histograma del rendimiento promedio\n",
    "✔ Diagrama de Moran (autocorrelación)\n",
    "✔ Boxplot por región natural\n",
    "✔ Ranking de zonas calientes/frías\n",
    "✔ Comparativo entre Moran y Gi*\n")
