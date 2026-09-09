# ==============================================================================
# Taller 1 - Big Data & Machine Learning
# SCRIPT MAESTRO (MAIN / MASTER)
#
# Estudiantes:
# Mauricio Aragón
# Jonathan Cadena
# Esteban Flórez
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Carga de Paquetes Básicos
# ------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(here, tidyverse, tictoc)

# Confirmación de directorio raíz detectado
cat("\n==================================================\n")
cat("Directorio Raíz detectado por 'here':\n", here(), "\n")
cat("==================================================\n\n")

# ------------------------------------------------------------------------------
# 2. Creación Automática de Carpetas de Salida
# ------------------------------------------------------------------------------
dirs <- c(
  here("02_Data", "Processed"),
  here("03_Output", "Tables"),
  here("03_Output", "Figures")
)

walk(dirs, ~dir.create(.x, recursive = TRUE, showWarnings = FALSE))

# ------------------------------------------------------------------------------
# 3. Ejecución Secuencial del Pipeline
# ------------------------------------------------------------------------------
tic("Ejecución Completa del Taller 1")

# Módulo 01: Web Scraping
cat("\n==================================================\n")
cat(" Ejecutando 01_scrape_data.R ...\n")
cat("==================================================\n")
tic("01_scrape_data")
source(here("01_Scripts", "01_scrape_data.R"), local = FALSE)
toc()

# Módulo 02: Limpieza y Procesamiento de Datos
cat("\n==================================================\n")
cat(" Ejecutando 02_clean_data.R ...\n")
cat("==================================================\n")
tic("02_clean_data")
source(here("01_Scripts", "02_clean_data.R"), local = FALSE)
toc()

# Módulo 03: Perfil Edad - Ingreso (Sección 1)
cat("\n==================================================\n")
cat(" Ejecutando 03_age_income_profile.R ...\n")
cat("==================================================\n")
tic("03_age_income_profile")
source(here("01_Scripts", "03_age_income_profile.R"), local = FALSE)
toc()

# Módulo 04: Brecha de Género (Sección 2)
cat("\n==================================================\n")
cat(" Ejecutando 04_gender_gap.R ...\n")
cat("==================================================\n")
tic("04_gender_gap")
source(here("01_Scripts", "04_gender_gap.R"), local = FALSE)
toc()

# Módulo 05: Predicción Fuera de Muestra (Sección 3)
cat("\n==================================================\n")
cat(" Ejecutando 05_prediction.R ...\n")
cat("==================================================\n")
tic("05_prediction")
source(here("01_Scripts", "05_prediction.R"), local = FALSE)
toc()

cat("\n==================================================\n")
cat(" ¡PROCESO FINALIZADO CON ÉXITO! \n")
cat("==================================================\n")
toc()