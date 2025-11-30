

# ============================================================
# 1. INSTALACIÓN Y CARGA DE PAQUETES
# ============================================================

# Lista de paquetes necesarios
paquetes_necesarios <- c(
  "readr",      
  "readxl",     
  "sf",         
  "dplyr",      
  "tidyr",      
  "janitor",    
  "stringi",    
  "lubridate",  
  "ggplot2",    
  "broom",    
  "estimatr",   
  "lme4",       
  "sandwich",   
  "lmtest",     
  "scales",      
  "viridis", 
  "ggrepel", 
  "patchwork", 
  "corrplot", 
  "RColorBrewer"
)

# Instalar paquetes faltantes
paquetes_faltantes <- paquetes_necesarios[!paquetes_necesarios %in% installed.packages()[, "Package"]]
if(length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

# Cargar todos los paquetes
invisible(lapply(paquetes_necesarios, library, character.only = TRUE))

# Crear carpeta de salida si no existe
if(!dir.exists("out")) dir.create("out")

cat("✓ Paquetes cargados exitosamente\n\n")

# ============================================================
# 2. CARGA DE DATOS
# ============================================================

cat("Cargando datos...\n")

ruta_conexiones <- "D:/INGENIERIA DE SISTEMAS Y COMPUTACION/Semestre VII/R FOR SOCIAL SCIENCE/PROJECT/DATA/ACCESOS_INTERNET_FIJO.csv"
ruta_ipm <- "D:/INGENIERIA DE SISTEMAS Y COMPUTACION/Semestre VII/R FOR SOCIAL SCIENCE/PROJECT/DATA/VULNRB_IPMxMZ/VULNRB_IPMxMZ.shp"
ruta_poblacion <- "D:/INGENIERIA DE SISTEMAS Y COMPUTACION/Semestre VII/R FOR SOCIAL SCIENCE/PROJECT/DATA/PPED-AreaDep-2018-2050_VP.xlsx"
ruta_poblacion_municipio<- "D:/INGENIERIA DE SISTEMAS Y COMPUTACION/Semestre VII/R FOR SOCIAL SCIENCE/PROJECT/DATA/PPED-AreaMun-2018-2042_VP.xlsx"

# Cargar datos de conexiones fijas
conexiones_raw <- read.csv(
  ruta_conexiones,
  sep = ";",
  na.strings = c("", " ", "NA", "N/A"),
  stringsAsFactors = FALSE
) %>% 
  clean_names()

# Cargar datos de IPM (shapefile)
ipm_raw <- st_read(ruta_ipm, quiet = TRUE)

#Cargar datos poblacion departamental
poblacion_raw <- read_excel(
  path = ruta_poblacion,
  sheet = 3,
  skip = 7,  # Salta hasta la fila con nombres
  col_names = TRUE  # La siguiente fila (3) son los nombres
) %>% 
  clean_names()

#Cargar datos poblacion municipal
poblacion__municipal_raw <- read_excel(
  path = ruta_poblacion_municipio,
  sheet = 3,
  skip = 7,  # Salta hasta la fila con nombres
  col_names = TRUE  # La siguiente fila (3) son los nombres
) %>% 
  clean_names()


# ============================================================
# 3. LIMPIEZA Y PREPARACIÓN DE DATOS DE CONEXIONES
# ============================================================

cat("Limpiando datos de conexiones...\n")

# Accesos fijos
conexiones <- conexiones_raw %>%
  mutate(
    # Limpiar nombre del departamento y municipio
    departamento = toupper(departamento) %>% 
      stri_trans_general("Latin-ASCII"),
    municipio = toupper(municipio) %>% 
      stri_trans_general("Latin-ASCII"),
    
    # Códigos formateados
    cod_dpto = sprintf("%02d", as.integer(id_departamento)),
    cod_mpio = sprintf("%05d", as.integer(id_municipio)),
    
    # Valores numéricos
    ano = as.integer(anno),
    trimestre = as.integer(trimestre),
    accesos = as.numeric(accesos)
    
  ) %>%
  select(cod_dpto, departamento, cod_mpio, municipio, ano, trimestre, accesos) %>%
  filter(!is.na(cod_dpto), !is.na(cod_mpio), !is.na(ano), !is.na(accesos)) %>%
  filter(ano >= 2022 & ano <= 2024)

# Validaciones de calidad de datos
stopifnot(
  "Años fuera del rango 2022-2024" = all(conexiones$ano >= 2022 & conexiones$ano <= 2024),
  "Accesos negativos detectados" = all(conexiones$accesos >= 0),
  "Códigos de departamento mal formateados" = all(nchar(conexiones$cod_dpto) == 2),
  "Códigos de municipio mal formateados" = all(nchar(conexiones$cod_mpio) == 5)
)

cat("✓ Conexiones limpias:", nrow(conexiones), "registros\n")
cat("  - Período analizado: 2022-2024\n")
cat("  - Años únicos:", paste(sort(unique(conexiones$ano)), collapse = ", "), "\n")
cat("  - Departamentos únicos:", n_distinct(conexiones$cod_dpto), "\n")
cat("  - Municipios únicos:", n_distinct(conexiones$cod_mpio), "\n\n")

length(print(unique(conexiones$departamento)))

# ============================================================
# 4. LIMPIEZA Y PREPARACIÓN DE DATOS DE IPM
# ============================================================

cat("Limpiando datos de IPM...\n")

ipm_limpio <- ipm_raw %>%
  st_drop_geometry() %>%
  clean_names() %>%
  mutate(
    # Formatear códigos
    cod_dpto = sprintf("%02d", as.numeric(cod_dpto)),
    cod_mpio = sprintf("%05d", as.numeric(cod_mpio)),
    
    # Convertir IPM a numérico
    ipm = as.numeric(ipm)
  ) %>%
  # Eliminar valores faltantes
  filter(!is.na(cod_dpto), !is.na(ipm))

# Validación: IPM debe estar entre 0 y 100
stopifnot(
  "IPM fuera del rango válido (0-100)" = all(ipm_limpio$ipm >= 0 & ipm_limpio$ipm <= 100)
)

print(length(unique(ipm_limpio$cod_dpto)))

# ============================================================
#  LIMPIEZA Y PREPARACIÓN DE DATOS POBLACION DEPARTAMENTAL
# ============================================================
poblacion <- poblacion_raw %>%
  clean_names() %>%
  
  # Solo fila de área TOTAL (cabecera + rural)
  filter(area_geografica == "Total") %>%
  
  # Solo año 2023
  filter(ano == 2023) %>%
  
  mutate(
    departamento = toupper(dpnom) %>% 
      stri_trans_general("Latin-ASCII"),
    cod_dpto = sprintf("%02d", as.numeric(dp)),
    
    total_poblacion = as.numeric(gsub(",", "", total))
  ) %>%
  
  filter(!is.na(cod_dpto), !is.na(total_poblacion)) %>%
  
  select(cod_dpto, departamento, ano, total_poblacion)

# ============================================================
#  LIMPIEZA Y PREPARACIÓN DE DATOS POBLACION MUNICIPAL
# ============================================================
poblacion_municipal <- poblacion__municipal_raw %>%
  clean_names() %>%          # Estandariza nombres
  # Solo filas del área TOTAL
  filter(area_geografica == "Total") %>%
  # Solo año 2023
  filter(ano == 2023) %>%
  
  mutate(
    
    cod_dpto = sprintf("%02d", as.numeric(dp)),
    cod_mpio = sprintf("%05d", as.numeric(mpio)),
    
    municipio = toupper(dpmp) %>% 
      stri_trans_general("Latin-ASCII"),
    total_poblacion = as.numeric(gsub(",", "", total))
  ) %>%
  
  filter(!is.na(cod_mpio), !is.na(total_poblacion)) %>%
  select(cod_dpto,cod_mpio,municipio,ano,total_poblacion)

length(unique(poblacion_municipal$municipio))

# ============================================================
# 5. DETECCIÓN Y ELIMINACIÓN DE VALORES ATÍPICOS EN IPM
#    (A NIVEL MUNICIPAL)
# ============================================================


ipm_con_outliers <- ipm_limpio %>%
  group_by(cod_mpio) %>%
  mutate(
    
    Q1 = quantile(ipm, 0.25, na.rm = TRUE),
    Q3 = quantile(ipm, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    
 
    limite_inferior = Q1 - 1.5 * IQR,
    limite_superior = Q3 + 1.5 * IQR,
    
    es_outlier = ipm < limite_inferior | ipm > limite_superior,
    
    
    total_manzanas_mpio = n()
  ) %>%
  ungroup()

# ----- Estadísticas de outliers -----
resumen_outliers <- ipm_con_outliers %>%
  group_by(cod_dpto, cod_mpio) %>%
  summarise(
    total_manzanas = n(),
    num_outliers = sum(es_outlier),
    porcentaje_outliers = round(mean(es_outlier) * 100, 1),
    ipm_min = min(ipm, na.rm = TRUE),
    ipm_max = max(ipm, na.rm = TRUE),
    ipm_promedio = mean(ipm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(num_outliers))

# Estadísticas globales
num_outliers_total <- sum(ipm_con_outliers$es_outlier)
porcentaje_outliers_total <- round(mean(ipm_con_outliers$es_outlier) * 100, 1)
municipios_con_outliers <- sum(resumen_outliers$num_outliers > 0)

cat("--- RESUMEN DE DETECCIÓN DE OUTLIERS ---\n")
cat("Total de manzanas analizadas:", nrow(ipm_con_outliers), "\n")
cat("Total de outliers detectados:", num_outliers_total, "(", porcentaje_outliers_total, "%)\n")
cat("Municipios con outliers:", municipios_con_outliers, "de", nrow(resumen_outliers), "\n\n")

# Mostrar los 10 municipios con más outliers
cat("Top 10 municipios con mayor cantidad de outliers:\n")
print(head(resumen_outliers, 10))
cat("\n")

# ----- Guardar lista detallada de outliers -----
if(num_outliers_total > 0) {
  outliers_detallado <- ipm_con_outliers %>%
    filter(es_outlier) %>%
    select(cod_dpto, cod_mpio, cod_dane, ipm, 
           limite_inferior, limite_superior, categoria, label) %>%
    arrange(cod_dpto, cod_mpio, desc(ipm))
  
  write.csv(outliers_detallado, "out/manzanas_outliers_eliminadas.csv", row.names = FALSE)
  cat("✓ Lista detallada de manzanas outliers guardada en: out/manzanas_outliers_eliminadas.csv\n")
}

# Guardar resumen por municipio
write.csv(resumen_outliers, "out/outliers_resumen_municipal.csv", row.names = FALSE)
cat("✓ Resumen de outliers por municipio guardado en: out/outliers_resumen_municipal.csv\n\n")

# ----- Visualización de outliers por municipio -----
# Gráfico: Distribución de porcentaje de outliers por municipio
p_dist_outliers <- ggplot(resumen_outliers, aes(x = porcentaje_outliers)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribución del Porcentaje de Outliers por Municipio",
    subtitle = paste("Total de municipios:", nrow(resumen_outliers)),
    x = "Porcentaje de Outliers (%)",
    y = "Número de Municipios"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("out/00_distribucion_outliers_municipios.png", p_dist_outliers, width = 10, height = 6, dpi = 300)

# ----- Eliminar outliers para el análisis -----
ipm_sin_outliers <- ipm_con_outliers %>%
  filter(!es_outlier) %>%
  select(cod_dpto, cod_mpio, cod_dane, categoria, label, ipm, 
         embarazo_a, reactivaci)  # Mantener solo columnas originales

cat("✓ Datos limpios:", nrow(ipm_sin_outliers), "manzanas\n")
cat("  Manzanas eliminadas:", num_outliers_total, "(", porcentaje_outliers_total, "%)\n\n")

# ============================================================
# 6. AGREGACIÓN DE IPM A NIVEL DEPARTAMENTO (SIN OUTLIERS)
# ============================================================

cat("Calculando IPM promedio por departamento (sin outliers)...\n")

# Primero agregar a nivel municipal
ipm_municipal <- ipm_sin_outliers %>%
  group_by(cod_dpto, cod_mpio) %>%
  summarise(
    ipm_promedio_municipal = mean(ipm, na.rm = TRUE),
    ipm_mediana_municipal = median(ipm, na.rm = TRUE),
    num_manzanas = n(),
    .groups = "drop"
  )

# Luego agregar a nivel departamental
ipm_departamento <- ipm_municipal %>%
  group_by(cod_dpto) %>%
  summarise(
    # Promedio de los promedios municipales
    ipm_promedio = mean(ipm_promedio_municipal, na.rm = TRUE),
    
    # Mediana de los promedios municipales
    ipm_mediana = median(ipm_promedio_municipal, na.rm = TRUE),
    
    # Rango
    ipm_min = min(ipm_promedio_municipal, na.rm = TRUE),
    ipm_max = max(ipm_promedio_municipal, na.rm = TRUE),
    
    # Desviación estándar entre municipios
    desv_std = sd(ipm_promedio_municipal, na.rm = TRUE),
    
    # Información adicional
    num_municipios = n(),
    total_manzanas = sum(num_manzanas),
    
    .groups = "drop"
  )

# Guardar resultados
write.csv(ipm_municipal, "out/ipm_por_municipio.csv", row.names = FALSE)
write.csv(ipm_departamento, "out/ipm_por_departamento.csv", row.names = FALSE)

cat("✓ IPM agregado:\n")
cat("  - Nivel municipal:", nrow(ipm_municipal), "municipios\n")
cat("  - Nivel departamental:", nrow(ipm_departamento), "departamentos\n")
cat("  → Guardado en: out/ipm_por_municipio.csv\n")
cat("  → Guardado en: out/ipm_por_departamento.csv\n\n")

# ----- Visualización: IPM por departamento -----
# Unir con nombres de departamento desde conexiones
ipm_depto_viz <- ipm_departamento %>%
  left_join(
    conexiones %>% select(cod_dpto, departamento) %>% distinct(),
    by = "cod_dpto"
  ) %>%
  arrange(desc(ipm_promedio))

# Top 10 departamentos con mayor IPM
p_top_ipm <- ggplot(head(ipm_depto_viz, 10), 
                    aes(x = reorder(departamento, ipm_promedio), y = ipm_promedio)) +
  geom_col(fill = "coral", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Top 10 Departamentos con Mayor IPM",
    subtitle = "Promedio sin valores atípicos (2022-2024)",
    x = "",
    y = "IPM Promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("out/00_top10_departamentos_mayor_ipm.png", p_top_ipm, width = 10, height = 6, dpi = 300)

cat("✓ Visualización de IPM departamental guardada\n\n")

# ============================================================
# 7. AGREGACIÓN DE CONEXIONES POR DEPARTAMENTO (CON TASAS PER CÁPITA)
# ============================================================


conexiones_departamento <- conexiones %>%
  group_by(cod_dpto, departamento) %>%
  summarise(
    accesos_total = sum(accesos, na.rm = TRUE),
    accesos_promedio_trimestre = mean(accesos, na.rm = TRUE),
    num_trimestres = n(),
    num_municipios = n_distinct(cod_mpio),
    .groups = "drop"
  ) %>%
  # Agregar población departamental
  left_join(
    poblacion %>% select(cod_dpto, total_poblacion),
    by = "cod_dpto"
  ) %>%
  # Calcular tasas per cápita
  mutate(
    accesos_por_10mil_hab = (accesos_total / total_poblacion) * 10000,
    accesos_por_mil_hab = (accesos_total / total_poblacion) * 1000,
    cobertura_porcentaje = (accesos_total / total_poblacion) * 100
  ) %>%
  
  filter(!is.na(total_poblacion), total_poblacion > 0, !is.na(accesos_por_10mil_hab))

write.csv(
  conexiones_departamento, 
  "out/conexiones_por_departamento_percapita.csv", 
  row.names = FALSE
)

cat("✓ Tasas per cápita calculadas (nivel departamental):", nrow(conexiones_departamento), "registros\n")
cat("  → Guardado en: out/conexiones_por_departamento_percapita.csv\n\n")

# También guardar a nivel municipal con tasas per cápita
conexiones_municipal <- conexiones %>%
  group_by(cod_dpto, departamento, cod_mpio, municipio) %>%
  summarise(
    accesos_total = sum(accesos, na.rm = TRUE),
    accesos_promedio_trimestre = mean(accesos, na.rm = TRUE),
    num_trimestres = n(),
    .groups = "drop"
  ) %>%
  # Agregar población municipal
  left_join(
    poblacion_municipal %>% select(cod_mpio, total_poblacion),
    by = "cod_mpio"
  ) %>%
  # Calcular tasas per cápita
  mutate(
    accesos_por_10mil_hab = (accesos_total / total_poblacion) * 10000,
    accesos_por_mil_hab = (accesos_total / total_poblacion) * 1000,
    cobertura_porcentaje = (accesos_total / total_poblacion) * 100
  ) %>%
  # Filtrar municipios sin población válida
  filter(!is.na(total_poblacion), total_poblacion > 0, !is.na(accesos_por_10mil_hab))

write.csv(
  conexiones_municipal, 
  "out/conexiones_por_municipio_percapita.csv", 
  row.names = FALSE
)

cat("✓ Tasas per cápita calculadas (nivel municipal):", nrow(conexiones_municipal), "registros\n")
cat("  → Guardado en: out/conexiones_por_municipio_percapita.csv\n\n")

# ============================================================
# 7B. DETECCIÓN Y ELIMINACIÓN DE OUTLIERS EN TASAS DE ACCESO
# ============================================================

cat("Detectando outliers en tasas per cápita...\n")

conexiones_departamento_out <- conexiones_departamento %>%
  mutate(
    Q1 = quantile(accesos_por_10mil_hab, 0.25, na.rm = TRUE),
    Q3 = quantile(accesos_por_10mil_hab, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    limite_inf = Q1 - 1.5 * IQR,
    limite_sup = Q3 + 1.5 * IQR,
    es_outlier_tasa = accesos_por_10mil_hab < limite_inf | accesos_por_10mil_hab > limite_sup
  )

# Contar outliers
num_outliers_tasa <- sum(conexiones_departamento_out$es_outlier_tasa)
porc_outliers_tasa <- round(mean(conexiones_departamento_out$es_outlier_tasa)*100, 2)

cat("Outliers encontrados en tasa de acceso:", num_outliers_tasa, 
    " (", porc_outliers_tasa, "% )\n")

# Guardar lista de outliers
write.csv(
  conexiones_departamento_out %>% filter(es_outlier_tasa),
  "out/outliers_tasa_acceso_departamental.csv",
  row.names = FALSE
)

# Eliminar outliers
conexiones_departamento_limpio <- conexiones_departamento_out %>%
  filter(!es_outlier_tasa) %>%
  select(-Q1, -Q3, -IQR, -limite_inf, -limite_sup, -es_outlier_tasa)

cat("Registros departamentales después de eliminar outliers:", 
    nrow(conexiones_departamento_limpio), "\n\n")

# ============================================================
# DETECCIÓN Y ELIMINACIÓN DE OUTLIERS EN TASAS MUNICIPALES
# ============================================================

cat("Detectando outliers en tasas per cápita municipales...\n")

conexiones_municipal_out <- conexiones_municipal %>%
  mutate(
    Q1 = quantile(accesos_por_10mil_hab, 0.25, na.rm = TRUE),
    Q3 = quantile(accesos_por_10mil_hab, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    limite_inf = Q1 - 1.5 * IQR,
    limite_sup = Q3 + 1.5 * IQR,
    es_outlier_tasa_mpio = accesos_por_10mil_hab < limite_inf |
      accesos_por_10mil_hab > limite_sup
  )

# Contar outliers municipales
num_outliers_mpio <- sum(conexiones_municipal_out$es_outlier_tasa_mpio)
porc_outliers_mpio <- round(mean(conexiones_municipal_out$es_outlier_tasa_mpio) * 100, 2)

cat("Outliers municipales detectados:", num_outliers_mpio,
    " (", porc_outliers_mpio, "% )\n")

# Exportar lista de outliers municipales
write.csv(
  conexiones_municipal_out %>% filter(es_outlier_tasa_mpio),
  "out/outliers_tasa_acceso_municipal.csv",
  row.names = FALSE
)

# Crear dataset MUNICIPAL limpio
conexiones_municipal_limpio <- conexiones_municipal_out %>%
  filter(!es_outlier_tasa_mpio) %>%
  select(-Q1, -Q3, -IQR, -limite_inf, -limite_sup, -es_outlier_tasa_mpio)

cat("Registros municipales después de eliminar outliers:",
    nrow(conexiones_municipal_limpio), "\n\n")

# ============================================================
# 8. UNIÓN DE DATOS: CONEXIONES + IPM (NIVEL DEPARTAMENTAL CON TASAS)
# ============================================================


datos_completos <- conexiones_departamento_limpio %>%
  rename(num_municipios_conexion = num_municipios) %>%
  
  left_join(
    ipm_departamento %>% 
      rename(num_municipios_ipm = num_municipios),
    by = "cod_dpto"
  ) %>%
  
  # Filtrar para dejar solo los registros útiles
  filter(!is.na(ipm_promedio), !is.na(accesos_por_10mil_hab))

# Verificación de calidad del join
porcentaje_perdido <- (1 - nrow(datos_completos) / nrow(conexiones_departamento)) * 100

cat("✓ Datos unidos (departamental):", nrow(datos_completos), "registros\n")
cat("  - Registros perdidos:", round(porcentaje_perdido, 1), "%\n\n")

if(porcentaje_perdido > 10) {
  warning("⚠ Más del 10% de registros se perdieron en el join. Revisar códigos de departamento.")
}

# Guardar dataset completo
write.csv(datos_completos, "out/dataset_completo_departamental_percapita.csv", row.names = FALSE)
cat("  → Guardado en: out/dataset_completo_departamental_percapita.csv\n\n")

# ============================================================
# 9. UNIÓN DE DATOS: CONEXIONES + IPM (NIVEL MUNICIPAL CON TASAS)
# ============================================================

cat("Uniendo datos de tasas de acceso e IPM a nivel municipal...\n")

datos_completos_municipal <- conexiones_municipal_limpio %>%
  left_join(
    ipm_municipal %>% select(cod_mpio, ipm_promedio_municipal, num_manzanas), 
    by = "cod_mpio"
  ) %>%
  filter(!is.na(ipm_promedio_municipal), !is.na(accesos_por_10mil_hab))

cat("✓ Datos unidos (municipal):", nrow(datos_completos_municipal), "registros\n")
cat("  → Guardado en: out/dataset_completo_municipal_percapita.csv\n\n")

write.csv(datos_completos_municipal, "out/dataset_completo_municipal_percapita.csv", row.names = FALSE)

# ============================================================
# 10. CREAR TABLAS RESUMEN (CON TASAS PER CÁPITA)
# ============================================================

cat("Creando tablas resumen con tasas per cápita...\n")

# Nivel departamental - tabla final
resumen_departamental <- datos_completos %>%
  select(cod_dpto, departamento, 
         accesos_total, total_poblacion,
         accesos_por_10mil_hab, accesos_por_mil_hab, cobertura_porcentaje,
         ipm_promedio, 
         num_municipios_conexion, num_municipios_ipm, total_manzanas) %>%
  arrange(desc(accesos_por_10mil_hab))

write.csv(resumen_departamental, "out/resumen_final_departamental_percapita.csv", row.names = FALSE)

# Nivel municipal - tabla final
resumen_municipal <- datos_completos_municipal %>%
  select(cod_mpio, municipio, departamento, 
         accesos_total, total_poblacion,
         accesos_por_10mil_hab, accesos_por_mil_hab, cobertura_porcentaje,
         ipm_promedio_municipal, num_manzanas) %>%
  arrange(desc(accesos_por_10mil_hab))

write.csv(resumen_municipal, "out/resumen_final_municipal_percapita.csv", row.names = FALSE)

cat("✓ Tablas de resumen guardadas\n")
cat("  → out/resumen_final_departamental_percapita.csv\n")
cat("  → out/resumen_final_municipal_percapita.csv\n\n")

# ============================================================
# 11. ANÁLISIS EXPLORATORIO Y VISUALIZACIONES (CON TASAS PER CÁPITA)
# ============================================================

cat("Generando visualizaciones con tasas per cápita...\n")

# --------- Gráfico 1: Relación Tasa per Cápita vs IPM (Departamental) ---------
grafico_scatter <- ggplot(datos_completos, aes(x = accesos_por_10mil_hab, y = ipm_promedio)) +
  geom_point(aes(size = total_poblacion), alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_text_repel(aes(label = departamento), size = 2.5, max.overlaps = 20) +
  scale_size_continuous(name = "Población", labels = scales::comma) +
  labs(
    title = "Relación entre Tasa de Acceso a Internet Fijo e IPM",
    subtitle = "Accesos por cada 10,000 habitantes (2022-2024) - Nivel Departamental",
    x = "Accesos por 10,000 Habitantes",
    y = "Índice de Pobreza Multidimensional (IPM) Promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("out/01_scatter_tasa_percapita_vs_ipm_departamental.png", grafico_scatter, width = 12, height = 8, dpi = 300)

# --------- Gráfico 2: Relación Tasa per Cápita vs IPM (Municipal) ---------
# Tomar muestra para visualización si hay muchos municipios
set.seed(123)
muestra_municipal <- if(nrow(datos_completos_municipal) > 500) {
  datos_completos_municipal %>% sample_n(500)
} else {
  datos_completos_municipal
}

grafico_scatter_mpio <- ggplot(muestra_municipal, 
                               aes(x = accesos_por_10mil_hab, y = ipm_promedio_municipal)) +
  geom_point(alpha = 0.4, color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Relación entre Tasa de Acceso a Internet Fijo e IPM (Municipal)",
    subtitle = "Accesos por cada 10,000 habitantes (2022-2024)",
    x = "Accesos por 10,000 Habitantes",
    y = "IPM Municipal Promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("out/01_scatter_tasa_percapita_vs_ipm_municipal.png", grafico_scatter_mpio, width = 10, height = 6, dpi = 300)

# --------- Gráfico 3: Distribución del IPM ---------
grafico_boxplot <- ggplot(datos_completos, aes(x = "", y = ipm_promedio)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7, width = 0.5) +
  geom_jitter(alpha = 0.3, width = 0.1, color = "steelblue") +
  labs(
    title = "Distribución del IPM por Departamento",
    subtitle = "Período 2022-2024 (sin valores atípicos)",
    x = "",
    y = "IPM Promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave("out/02_boxplot_ipm_departamental.png", grafico_boxplot, width = 8, height = 6, dpi = 300)

# --------- Gráfico 4: Top 10 departamentos con mayor tasa de acceso ---------
top_departamentos <- datos_completos %>%
  arrange(desc(accesos_por_10mil_hab)) %>%
  head(10)

grafico_top10 <- ggplot(top_departamentos, aes(x = reorder(departamento, accesos_por_10mil_hab), 
                                               y = accesos_por_10mil_hab)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Top 10 Departamentos con Mayor Tasa de Acceso a Internet Fijo",
    subtitle = "Accesos por cada 10,000 habitantes (2022-2024)",
    x = "",
    y = "Accesos por 10,000 Habitantes"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("out/04_top10_departamentos_tasa_acceso.png", grafico_top10, width = 10, height = 6, dpi = 300)

# --------- Gráfico 5: Top 10 departamentos con mayor IPM ---------
top_ipm <- datos_completos %>%
  arrange(desc(ipm_promedio)) %>%
  head(10)

grafico_top10_ipm <- ggplot(top_ipm, aes(x = reorder(departamento, ipm_promedio), 
                                         y = ipm_promedio)) +
  geom_col(fill = "coral", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Top 10 Departamentos con Mayor IPM",
    subtitle = "Período 2022-2024",
    x = "",
    y = "IPM Promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("out/04b_top10_departamentos_ipm.png", grafico_top10_ipm, width = 10, height = 6, dpi = 300)

# --------- Gráfico 6: Comparación IPM vs Tasa de Acceso por departamento -----
# Seleccionar 15 departamentos con menor IPM
departamentos_analisis <- datos_completos %>%
  arrange(ipm_promedio) %>%
  head(15)

grafico_comparacion <- ggplot(departamentos_analisis) +
  geom_col(aes(x = reorder(departamento, -ipm_promedio), y = ipm_promedio), 
           fill = "coral", alpha = 0.7) +
  geom_line(aes(x = reorder(departamento, -ipm_promedio), 
                y = accesos_por_10mil_hab / max(accesos_por_10mil_hab) * max(ipm_promedio), 
                group = 1), 
            color = "darkblue", size = 1.2) +
  geom_point(aes(x = reorder(departamento, -ipm_promedio), 
                 y = accesos_por_10mil_hab / max(accesos_por_10mil_hab) * max(ipm_promedio)), 
             color = "darkblue", size = 3) +
  scale_y_continuous(
    name = "IPM Promedio",
    sec.axis = sec_axis(~ . / max(departamentos_analisis$ipm_promedio) * max(departamentos_analisis$accesos_por_10mil_hab), 
                        name = "Accesos por 10k hab (escalado)",
                        labels = scales::comma)
  ) +
  coord_flip() +
  labs(
    title = "IPM vs Tasa de Acceso a Internet - Top 15 Departamentos con Menor IPM",
    subtitle = "Período 2022-2024",
    x = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y.right = element_text(color = "darkblue"),
    axis.text.y.right = element_text(color = "darkblue")
  )

ggsave("out/05_comparacion_ipm_tasa_acceso.png", grafico_comparacion, width = 12, height = 8, dpi = 300)

# --------- Gráfico 7: Dispersión con tamaño por población ---------
grafico_burbujas <- ggplot(datos_completos, 
                           aes(x = accesos_por_10mil_hab, y = ipm_promedio, size = total_poblacion)) +
  geom_point(alpha = 0.5, color = "darkviolet") +
  scale_size_continuous(name = "Población", labels = scales::comma, range = c(3, 15)) +
  labs(
    title = "Relación Tasa de Acceso, IPM y Población Departamental",
    subtitle = "Tamaño de burbuja representa población (2022-2024)",
    x = "Accesos por 10,000 Habitantes",
    y = "IPM Promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("out/06_burbujas_tasa_ipm_poblacion.png", grafico_burbujas, width = 10, height = 7, dpi = 300)

cat("✓ Gráficos guardados en carpeta 'out/'\n\n")

# ============================================================
# 12. MODELOS ESTADÍSTICOS (CON TASAS PER CÁPITA)
# ============================================================

cat("============================================================\n")
cat("ANÁLISIS ESTADÍSTICO: EFECTO DE LA TASA DE ACCESO A INTERNET EN EL IPM\n")
cat("Usando TASAS DE ACCESO PER CÁPITA (por 10,000 habitantes)\n")
cat("Período: 2022-2024\n")
cat("============================================================\n\n")

# Preparar datos para modelado
datos_modelo <- datos_completos %>%
  filter(accesos_por_10mil_hab > 0) %>%
  mutate(
    log_tasa_acceso = log(accesos_por_10mil_hab),
    id_departamento = factor(cod_dpto)
  )

cat("Registros para modelado:", nrow(datos_modelo), "\n")
cat("Departamentos incluidos:", n_distinct(datos_modelo$cod_dpto), "\n\n")

# --------- Modelo 1: Regresión Lineal Simple con Tasa per Cápita ---------
cat("--- MODELO 1: Regresión Lineal Simple con Tasa per Cápita ---\n")

modelo_ols <- lm(
  ipm_promedio ~ log_tasa_acceso,
  data = datos_modelo
)

# Mostrar resultados
print(summary(modelo_ols))

# Errores estándar robustos
coef_robustos_ols <- coeftest(modelo_ols, vcov = vcovHC(modelo_ols, type = "HC1"))
cat("\nCoeficientes con errores estándar robustos:\n")
print(coef_robustos_ols)

# Guardar resultados
resultados_ols <- broom::tidy(modelo_ols) %>%
  mutate(
    significativo = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

write.csv(resultados_ols, "out/modelo_01_ols_tasa_percapita.csv", row.names = FALSE)
cat("\n✓ Resultados guardados en: out/modelo_01_ols_tasa_percapita.csv\n\n")

# --------- Modelo 2: Con variable de control (total de manzanas) ---------
cat("--- MODELO 2: Con Variable de Control (Manzanas) ---\n")

modelo_control <- lm(
  ipm_promedio ~ log_tasa_acceso + log(total_manzanas),
  data = datos_modelo
)

print(summary(modelo_control))

resultados_control <- broom::tidy(modelo_control)
write.csv(resultados_control, "out/modelo_02_tasa_con_control.csv", row.names = FALSE)
cat("\n✓ Resultados guardados en: out/modelo_02_tasa_con_control.csv\n\n")

# --------- Modelo 3: Con control de población ---------
cat("--- MODELO 3: Con Control de Población ---\n")

modelo_poblacion <- lm(
  ipm_promedio ~ log_tasa_acceso + log(total_poblacion),
  data = datos_modelo
)

print(summary(modelo_poblacion))

resultados_poblacion <- broom::tidy(modelo_poblacion)
write.csv(resultados_poblacion, "out/modelo_03_tasa_con_poblacion.csv", row.names = FALSE)
cat("\n✓ Resultados guardados en: out/modelo_03_tasa_con_poblacion.csv\n\n")

# --------- Análisis de correlación ---------
cat("--- MATRIZ DE CORRELACIÓN ---\n")

matriz_cor <- datos_modelo %>%
  select(ipm_promedio, accesos_por_10mil_hab, total_poblacion, total_manzanas) %>%
  cor(use = "complete.obs")

print(round(matriz_cor, 3))

write.csv(matriz_cor, "out/matriz_correlacion_percapita.csv", row.names = TRUE)
cat("\n✓ Matriz de correlación guardada en: out/matriz_correlacion_percapita.csv\n\n")

# --------- Visualización de residuos del modelo principal ---------
datos_modelo$residuos <- residuals(modelo_ols)
datos_modelo$valores_ajustados <- fitted(modelo_ols)

p_residuos <- ggplot(datos_modelo, aes(x = valores_ajustados, y = residuos)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "darkblue") +
  labs(
    title = "Gráfico de Residuos del Modelo Principal",
    subtitle = "IPM ~ log(Tasa de Acceso per Cápita)",
    x = "Valores Ajustados",
    y = "Residuos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("out/07_residuos_modelo_principal.png", p_residuos, width = 10, height = 6, dpi = 300)

# ============================================================
# 13. INTERPRETACIÓN DE RESULTADOS (CON TASAS PER CÁPITA)
# ============================================================


# Extraer coeficiente de log_tasa_acceso del modelo OLS
coef_log_tasa <- coef(modelo_ols)["log_tasa_acceso"]
pvalor <- summary(modelo_ols)$coefficients["log_tasa_acceso", "Pr(>|t|)"]
r2 <- summary(modelo_ols)$r.squared

cat("--- Resultados del Modelo Principal (Tasa per Cápita) ---\n")
cat("Coeficiente de log(tasa_acceso):", round(coef_log_tasa, 4), "\n")
cat("Valor p:", format.pval(pvalor, digits = 3), "\n")
cat("R²:", round(r2, 4), "(El modelo explica el", round(r2 * 100, 1), "% de la variación)\n\n")

# Interpretación
if (coef_log_tasa < 0 & pvalor < 0.05) {
  cat("✓ CONCLUSIÓN PRINCIPAL:\n")
  cat("  Mayor tasa de acceso a internet fijo se asocia SIGNIFICATIVAMENTE con\n")
  cat("  menor Índice de Pobreza Multidimensional (IPM).\n\n")
  cat("  Interpretación práctica (controlando por población):\n")
  cat("  - Un aumento del 10% en la tasa de acceso por 10,000 hab se asocia con una\n")
  cat("    disminución de", round(abs(coef_log_tasa) * log(1.10), 3), "puntos en el IPM.\n")
  cat("  - Un aumento del 100% (duplicar) la tasa de acceso se asocia con una\n")
  cat("    disminución de", round(abs(coef_log_tasa) * log(2), 3), "puntos en el IPM.\n\n")
} else if (coef_log_tasa < 0 & pvalor >= 0.05) {
  cat("⚠ CONCLUSIÓN PRINCIPAL:\n")
  cat("  Se observa una asociación negativa entre tasa de acceso e IPM,\n")
  cat("  pero NO es estadísticamente significativa (p > 0.05).\n")
  cat("  Se requiere más evidencia para confirmar esta relación.\n\n")
} else {
  cat("✗ CONCLUSIÓN PRINCIPAL:\n")
  cat("  No se encuentra evidencia de que mayor tasa de acceso a internet\n")
  cat("  se asocie con reducción del IPM en este análisis.\n\n")
}

# Correlación simple
cor_pearson <- cor(datos_modelo$accesos_por_10mil_hab, datos_modelo$ipm_promedio)
cat("--- Estadísticas Descriptivas ---\n")
cat("Correlación de Pearson (tasa de acceso vs IPM):", round(cor_pearson, 3), "\n")
cat("Tasa promedio de acceso:", round(mean(datos_modelo$accesos_por_10mil_hab), 2), "por 10k hab\n")
cat("IPM promedio:", round(mean(datos_modelo$ipm_promedio), 2), "\n\n")

cat("--- Resumen de Datos Procesados ---\n")
cat("Outliers eliminados:", num_outliers_total, "manzanas (", porcentaje_outliers_total, "%)\n")
cat("Municipios analizados:", nrow(ipm_municipal), "\n")
cat("Departamentos analizados:", nrow(datos_modelo), "\n")
cat("Población total analizada:", scales::comma(sum(datos_modelo$total_poblacion)), "habitantes\n\n")

# Extraer coeficientes + IC
coef_df <- tidy(modelo_control, conf.int = TRUE)

# Calcular R²
r2_modelo <- summary(modelo_control)$r.squared

# Renombrar variables para que el gráfico quede más bonito
coef_df$term <- recode(coef_df$term,
                       "(Intercept)" = "Intercepto",
                       "log_tasa_acceso" = "Log(Tasa de Acceso)",
                       "log(total_manzanas)" = "Log(Total de Manzanas)")

# Gráfico coefplot
g_coef <- ggplot(coef_df %>% filter(term != "Intercepto"),
                 aes(x = term, y = estimate)) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    color = "steelblue", size = 1
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Modelo 2: Efecto de la Tasa de Acceso e\nIncorporación del Control de Manzanas",
    subtitle = paste0("Regresión Lineal: IPM ~ log(tasa_acceso) + log(total_manzanas)\n",
                      "R² del modelo: ", round(r2_modelo, 4)),
    x = "",
    y = "Coeficiente Estimado (con IC 95%)",
    caption = "*Puntos indican el efecto estimado; líneas el intervalo de confianza del 95%."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11)
  )

# Guardar
ggsave("out/modelo2_coefplot.png", g_coef, width = 10, height = 6, dpi = 300)

g_coef




