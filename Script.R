# ================= INSTALAR Y CARGAR LIBRERÍAS =====================
install.packages("readr")
install.packages("readxl")
install.packages("sf")
library(readr)
library(readxl)
library(sf)

# ================= DATOS CONEXIONES FIJAS (CSV) =====================
# Leer CSV correctamente, reconociendo valores faltantes reales
datos_conexiones_fijas <- read.csv(
  "C:/Users/msola/Downloads/ACCESOS_INTERNET_FIJO_3_18.csv",
  sep = ";",
  na.strings = c("", " ", "NA", "N/A")
)

# ---- Valores únicos ----
unicos_conexiones <- data.frame(
  variable = names(datos_conexiones_fijas),
  valores_unicos = sapply(datos_conexiones_fijas, function(x) length(unique(x)))
)

# ---- Faltantes ----
faltantes_conexiones <- data.frame(
  variable = names(datos_conexiones_fijas),
  faltantes = sapply(datos_conexiones_fijas, function(x) sum(is.na(x)))
)

# ================= DATOS IPM (SHAPEFILE) =====================
datos_ipm <- st_read("C:/Users/msola/Downloads/VULNRB_IPMxMZ (1)/VULNRB_IPMxMZ.shp")

# --------- LIMPIEZA: convertir "NA", "" y " " en NA real ------------
# Separar la geometría primero
geom <- st_geometry(datos_ipm)
datos_ipm_sin_geom <- st_drop_geometry(datos_ipm)

# Reemplazar valores vacíos y "NA" columna por columna
datos_ipm_sin_geom <- data.frame(
  lapply(datos_ipm_sin_geom, function(col) {
    # Solo aplicar a columnas de caracteres
    if (is.character(col) || is.factor(col)) {
      col <- as.character(col)
      col[col == "" | col == " " | col == "NA"] <- NA
    }
    return(col)
  }),
  stringsAsFactors = FALSE
)

# Volver a añadir la geometría
datos_ipm <- st_set_geometry(datos_ipm_sin_geom, geom)

# ---- Valores únicos ----
unicos_ipm <- data.frame(
  variable = names(datos_ipm_sin_geom),
  valores_unicos = sapply(datos_ipm_sin_geom, function(x) length(unique(x)))
)

# ---- Faltantes ----
faltantes_ipm <- data.frame(
  variable = names(datos_ipm_sin_geom),
  faltantes = sapply(datos_ipm_sin_geom, function(x) sum(is.na(x)))
)

# Ver resultados
print("=== CONEXIONES FIJAS ===")
print(unicos_conexiones)
print(faltantes_conexiones)

print("=== IPM ===")
print(unicos_ipm)
print(faltantes_ipm)
