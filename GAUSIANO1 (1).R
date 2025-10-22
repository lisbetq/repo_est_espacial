# =============================================================================
# ANÁLISIS GAUSSIANO FUNCIONAL - CAP200AB ENA 2023
# Basado en: Diccionario_Datos_03_Cap200AB.pdf
# =============================================================================

# Librerías necesarias
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(MASS)
library(corrplot)
library(nortest)
library(mixtools)
library(viridis)
library(haven)
# Cargar datos CAP200AB
# CAMBIAR ESTA RUTA A TU ARCHIVO
base <- read_sav("D:/ESt_espacial/articulo/03_CAP200AB.sav") 

# =============================================================================
# 1. PREPARAR DATOS SEGÚN DICCIONARIO CAP200AB
# =============================================================================

# Variables del diccionario:
# P217_SUP_ha: Superficie cosechada en hectáreas (variable 49)
# P219_CANT_1: Producción total - cantidad entero (variable 53)
# P220_1_PRE_KG: Precio de venta por kilogramo (variable 63)
# P220_1_VAL: Valor de venta en S/ (variable 62)
# P204_NOM: Nombre del cultivo (variable 21)
# P204_TIPO: Tipo de cultivo (1=Transitorio, 2=Permanente estacional, 3=Permanente continuo)

datos <- base %>%
  filter(!is.na(P217_SUP_ha) & !is.na(P219_CANT_1)) %>%
  dplyr::select(
    # Variables geográficas
    ANIO,
    NOMBREDD,
    NOMBREPV,
    NOMBREDI,
    # Variables del cultivo
    P204_NOM,           # Nombre del cultivo
    P204_COD,           # Código de cultivo
    P204_TIPO,          # Tipo de cultivo
    # Variables productivas (CAP200AB)
    P217_SUP_ha,        # Superficie cosechada (ha)
    P219_CANT_1,        # Producción total (cantidad)
    P220_1_PRE_KG,      # Precio por kg
    P220_1_VAL,         # Valor total de venta
    # Variables complementarias si existen
    P210_SUP_1,         # Superficie sembrada (si está disponible)
    FACTOR_PRODUCTOR    # Factor de expansión
  ) %>%
  # Renombrar para análisis
  rename(
    superficie_ha = P217_SUP_ha,
    produccion_kg = P219_CANT_1,
    precio_kg = P220_1_PRE_KG,
    valor_total = P220_1_VAL,
    cultivo = P204_NOM,
    tipo_cultivo = P204_TIPO
  ) %>%
  # Filtros de calidad
  filter(
    superficie_ha > 0 & superficie_ha < 1000,
    produccion_kg > 0 & produccion_kg < 1000000
  ) %>%
  # Calcular variables derivadas
  mutate(
    rendimiento = produccion_kg / superficie_ha,
    log_superficie = log(superficie_ha),
    log_produccion = log(produccion_kg),
    log_rendimiento = log(rendimiento),
    tipo_cultivo_desc = case_when(
      tipo_cultivo == 1 ~ "Transitorio",
      tipo_cultivo == 2 ~ "Permanente estacional",
      tipo_cultivo == 3 ~ "Permanente continuo",
      TRUE ~ "No especificado"
    )
  ) %>%
  filter(complete.cases(log_superficie, log_produccion, log_rendimiento))

cat("✓ Datos CAP200AB preparados:", nrow(datos), "observaciones\n")
cat("✓ Departamentos:", n_distinct(datos$NOMBREDD), "\n")
cat("✓ Cultivos únicos:", n_distinct(datos$cultivo), "\n")

# =============================================================================
# 2. ANÁLISIS DE NORMALIDAD
# =============================================================================

# Variables a probar según CAP200AB
vars_test <- c("log_superficie", "log_produccion", "log_rendimiento")

# Tests de normalidad
resultados_normalidad <- data.frame()
for(var in vars_test) {
  x <- datos[[var]]
  ad_p <- ad.test(x)$p.value
  lillie_p <- lillie.test(x)$p.value
  skew <- e1071::skewness(x)
  kurt <- e1071::kurtosis(x)
  
  resultados_normalidad <- rbind(resultados_normalidad, data.frame(
    variable = var,
    n_obs = length(x),
    media = round(mean(x), 4),
    desv_std = round(sd(x), 4),
    anderson_p = round(ad_p, 6),
    lilliefors_p = round(lillie_p, 6),
    skewness = round(skew, 4),
    kurtosis = round(kurt, 4)
  ))
}

cat("\n=== TESTS DE NORMALIDAD - VARIABLES CAP200AB ===\n")
print(resultados_normalidad)

# Seleccionar mejores variables para campo gaussiano
var1 <- "log_superficie"
var2 <- "log_rendimiento"
cat("\n✓ Variables seleccionadas para campo gaussiano:", var1, "y", var2, "\n")

# =============================================================================
# 3. PARÁMETROS DEL CAMPO GAUSSIANO (CAP200AB)
# =============================================================================

# Matriz de datos bivariados
X <- as.matrix(datos[, c(var1, var2)])
n_obs <- nrow(X)

# Parámetros empíricos del campo
mu_emp <- colMeans(X)
sigma_emp <- cov(X)
correlacion <- cor(datos[[var1]], datos[[var2]])

cat("\n=== PARÁMETROS DEL CAMPO GAUSSIANO - CAP200AB ===\n")
cat("N observaciones:", n_obs, "\n")
cat("Media log_superficie (P217_SUP_ha):", round(mu_emp[1], 4), "\n")
cat("Media log_rendimiento:", round(mu_emp[2], 4), "\n")
cat("Desviación log_superficie:", round(sqrt(sigma_emp[1,1]), 4), "\n")
cat("Desviación log_rendimiento:", round(sqrt(sigma_emp[2,2]), 4), "\n")
cat("Correlación:", round(correlacion, 4), "\n")
cat("Covarianza:", round(sigma_emp[1,2], 4), "\n")

# =============================================================================
# 4. SIMULACIONES DEL CAMPO GAUSSIANO
# =============================================================================

set.seed(123)

# Simulación principal del campo gaussiano
n_sim <- 2000
simulaciones <- MASS::mvrnorm(n = n_sim, mu = mu_emp, Sigma = sigma_emp)
sim_df <- data.frame(
  log_superficie_sim = simulaciones[,1],
  log_rendimiento_sim = simulaciones[,2]
)

# Escenarios adicionales según CAP200AB
# Conservador: reducción por factores climáticos (P223B_*)
sim_conservador <- MASS::mvrnorm(500, mu = mu_emp * 0.85, Sigma = sigma_emp * 0.9)

# Optimista: mejora por buenas prácticas agrícolas (CAP300AB)
sim_optimista <- MASS::mvrnorm(500, mu = mu_emp * 1.15, Sigma = sigma_emp * 1.1)

# Realista: variación moderada
sim_realista <- MASS::mvrnorm(500, mu = mu_emp, Sigma = sigma_emp * 1.05)

cat("\n✓ Simulaciones generadas:", n_sim, "principales\n")
cat("✓ Escenarios adicionales: Conservador, Optimista, Realista\n")

# =============================================================================
# 5. VISUALIZACIONES
# =============================================================================

cat("\n=== GENERANDO VISUALIZACIONES ===\n")

# 5.1 Distribuciones marginales - P217_SUP_ha (log)
p1 <- ggplot(datos, aes(x = log_superficie)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.7, fill = "#3498db") +
  geom_density(color = "#e74c3c", linewidth = 1.2) +
  stat_function(fun = dnorm, 
                args = list(mean = mu_emp[1], sd = sqrt(sigma_emp[1,1])),
                color = "#2ecc71", linewidth = 1.2, linetype = "dashed") +
  labs(title = "Distribución Log(Superficie Cosechada) - P217_SUP_ha", 
       subtitle = "Observada (rojo) vs Normal teórica (verde)",
       x = "Log(Superficie en ha)", y = "Densidad",
       caption = "Fuente: CAP200AB - ENA 2023") +
  theme_minimal(base_size = 11)

p2 <- ggplot(datos, aes(x = log_rendimiento)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.7, fill = "#2ecc71") +
  geom_density(color = "#e74c3c", linewidth = 1.2) +
  stat_function(fun = dnorm, 
                args = list(mean = mu_emp[2], sd = sqrt(sigma_emp[2,2])),
                color = "#3498db", linewidth = 1.2, linetype = "dashed") +
  labs(title = "Distribución Log(Rendimiento) - Calculado", 
       subtitle = "Observada (rojo) vs Normal teórica (azul)",
       x = "Log(Rendimiento kg/ha)", y = "Densidad",
       caption = "Fuente: CAP200AB - ENA 2023") +
  theme_minimal(base_size = 11)

grid.arrange(p1, p2, ncol = 2)

# 5.2 Q-Q Plots
p3 <- ggplot(datos, aes(sample = log_superficie)) +
  stat_qq(alpha = 0.5, color = "#3498db") + 
  stat_qq_line(color = "#e74c3c", linewidth = 1.2) +
  labs(title = "Q-Q Plot: Log(Superficie) - P217_SUP_ha",
       subtitle = "CAP200AB",
       x = "Teórico", y = "Observado") +
  theme_minimal(base_size = 11)

p4 <- ggplot(datos, aes(sample = log_rendimiento)) +
  stat_qq(alpha = 0.5, color = "#2ecc71") + 
  stat_qq_line(color = "#e74c3c", linewidth = 1.2) +
  labs(title = "Q-Q Plot: Log(Rendimiento)",
       subtitle = "CAP200AB",
       x = "Teórico", y = "Observado") +
  theme_minimal(base_size = 11)

grid.arrange(p3, p4, ncol = 2)

# 5.3 Campo gaussiano bivariado
p_campo <- ggplot() +
  geom_point(data = datos, aes(x = log_superficie, y = log_rendimiento), 
             color = "#e74c3c", alpha = 0.4, size = 1) +
  geom_point(data = sim_df, aes(x = log_superficie_sim, y = log_rendimiento_sim), 
             color = "#3498db", alpha = 0.2, size = 0.8) +
  labs(title = "Campo Gaussiano Aleatorio Bivariado - CAP200AB",
       subtitle = "Datos reales (rojo) vs Simulaciones del campo (azul)",
       x = "Log(Superficie cosechada - P217_SUP_ha)", 
       y = "Log(Rendimiento kg/ha)",
       caption = "Fuente: ENA 2023") +
  theme_minimal(base_size = 11)

print(p_campo)

# 5.4 Contornos de densidad del campo
p_contornos <- ggplot() +
  stat_density_2d(data = datos, aes(x = log_superficie, y = log_rendimiento), 
                  color = "#e74c3c", linewidth = 1, alpha = 0.8) +
  stat_density_2d(data = sim_df, aes(x = log_superficie_sim, y = log_rendimiento_sim), 
                  color = "#3498db", linewidth = 1, alpha = 0.8) +
  geom_point(data = datos, aes(x = log_superficie, y = log_rendimiento),
             alpha = 0.3, size = 0.6, color = "#34495e") +
  labs(title = "Contornos de Densidad del Campo Gaussiano",
       subtitle = "Datos CAP200AB (rojo) vs Simulaciones (azul)",
       x = "Log(Superficie P217_SUP_ha)", 
       y = "Log(Rendimiento)",
       caption = "Fuente: ENA 2023") +
  theme_minimal(base_size = 11)

print(p_contornos)

# 5.5 Mapa de calor de densidad gaussiana
x_range <- range(X[,1])
y_range <- range(X[,2])
x_grid <- seq(x_range[1], x_range[2], length.out = 60)
y_grid <- seq(y_range[1], y_range[2], length.out = 60)
grid_points <- expand.grid(x = x_grid, y = y_grid)

# Calcular densidad gaussiana bivariada
densidad <- apply(grid_points, 1, function(punto) {
  dmvnorm(punto, mu = mu_emp, sigma = sigma_emp)
})

grid_df <- data.frame(
  x = grid_points$x,
  y = grid_points$y,
  densidad = densidad
)

p_heatmap <- ggplot(grid_df, aes(x = x, y = y, fill = densidad)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Densidad\nGaussiana", option = "plasma") +
  geom_point(data = datos, aes(x = log_superficie, y = log_rendimiento), 
             inherit.aes = FALSE, alpha = 0.3, size = 0.5, color = "white") +
  labs(title = "Campo Gaussiano - Mapa de Densidad Teórica",
       subtitle = "Puntos blancos: observaciones CAP200AB",
       x = "Log(Superficie P217_SUP_ha)", 
       y = "Log(Rendimiento)",
       caption = "Fuente: ENA 2023") +
  theme_minimal(base_size = 11)

print(p_heatmap)

# =============================================================================
# 6. ESCENARIOS COMPARATIVOS (CAP200AB)
# =============================================================================

# Crear dataframe combinado de escenarios
escenarios_df <- bind_rows(
  data.frame(sim_df, escenario = "Base"),
  data.frame(log_superficie_sim = sim_conservador[,1], 
             log_rendimiento_sim = sim_conservador[,2], 
             escenario = "Conservador\n(Sequía/Heladas)"),
  data.frame(log_superficie_sim = sim_optimista[,1], 
             log_rendimiento_sim = sim_optimista[,2], 
             escenario = "Optimista\n(Buenas prácticas)"),
  data.frame(log_superficie_sim = sim_realista[,1], 
             log_rendimiento_sim = sim_realista[,2], 
             escenario = "Realista")
)

names(escenarios_df)[1:2] <- c("log_superficie", "log_rendimiento")

p_escenarios <- ggplot(escenarios_df, aes(x = log_superficie, y = log_rendimiento, 
                                          color = escenario)) +
  geom_point(alpha = 0.5, size = 0.9) +
  stat_ellipse(level = 0.95, linewidth = 1.3) +
  scale_color_manual(values = c("Base" = "#3498db", 
                                "Conservador\n(Sequía/Heladas)" = "#e74c3c", 
                                "Optimista\n(Buenas prácticas)" = "#2ecc71",
                                "Realista" = "#f39c12")) +
  labs(title = "Simulación de Escenarios del Campo Gaussiano - CAP200AB",
       subtitle = "Diferentes parametrizaciones basadas en factores productivos",
       x = "Log(Superficie P217_SUP_ha)", 
       y = "Log(Rendimiento)",
       color = "Escenario",
       caption = "Factores: P223B (climáticos), CAP300AB (buenas prácticas)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

print(p_escenarios)


#