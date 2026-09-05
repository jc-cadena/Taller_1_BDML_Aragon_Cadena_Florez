# 02_clean_data.R
# Objetivo: Limpiar la base cruda del GEIH 2018 y definir la muestra de análisis
#           (empleados, 18+ años), según lo indicado en la Sección 3 del taller.
# Input: Data/Raw/geih_raw.csv
# Output: Data/Processed/geih_clean.csv
#         Data/Processed/na_income_profile.csv

library(pacman)
library(dplyr)

p_load(tidyverse, here, skimr)

geih_raw <- readRDS(here("02_Data", "Raw", "geih_raw.rds"))

# Recuperamos los missing de las variables que estén como caracter
geih_raw <- geih_raw %>%
  mutate(across(where(is.character),~ na_if(.x, "NA")))


# A continuación, decidimos volver numéricas las variables clave

# Primero, creamos vector con los nombres de las variables

nombres_vars <- c("directorio", "secuencia_p", "orden", "ocu", "y_total_m",
                  "sex", "maxEducLevel","p6050", "estrato1", "totalHoursWorked",
                  "relab", "formal", "sizeFirm", "oficio", "p6426","age","chunk")

# Luego, procedemos con el cambio de tipo de variable 

geih_raw <- geih_raw |>
  mutate(across(all_of(nombres_vars), as.numeric))

# Finalmente, hacemos un primer diagnóstico
# Vemos cuántos faltantes hay y cómo se distribuye cada variable

# skim(geih_raw) - Estadísticas descriptivas


# Incluimos el conteo de los menores del hogar (5 y 10 años)

geih_raw <- geih_raw %>%
  group_by(directorio, secuencia_p) %>%
  mutate(
    n_ninos_under5  = sum(age < 5, na.rm = TRUE),
    n_ninos_under10 = sum(age < 10, na.rm = TRUE),
    total_personas  = n()
  ) %>%
  ungroup()

# --Restringimos la muestra a personas que reportan estar empleadas y tiene más de 18 años--
geih_sample <- geih_raw |>
  filter(age >= 18, ocu == 1)

# dim(geih_sample) - dimensiones de la base filtrada inicialmente

# =============================================================
# Estandarización de variables categóricas (factor)
# =============================================================
# Se hace aquí (y no en cada script de sección) para que todos los scripts
# downstream hereden el tipo correcto sin necesidad de reconvertir.

# --- relab: tipo de ocupación ---
# Categorías 6,7,8,9 tienen muy pocas observaciones (n=207,41,1,9 respectivamente)
# y generan coeficientes no identificables de forma confiable (ver relab8 con n=1).
# Se agrupan en una categoría "Otro/marginal". Se conserva relab_detailed
# por si se necesita el detalle original en algún análisis puntual.
geih_sample <- geih_sample |>
  mutate(
    relab_detailed = as.factor(relab),
    relab = case_when(
      relab %in% c(6, 7, 8, 9) ~ "otros",
      TRUE ~ as.character(relab)
    ),
    relab = as.factor(relab)
  )

# --- sex -> Female: dummy más intuitivo para las regresiones ---
# sex: 1 = male, 0 = female (según diccionario GEIH)
geih_sample <- geih_sample |>
  mutate(Female = 1 - sex)

# --- maxEducLevel: recodificamos 9 (N/A, "no sabe/no informa") como NA real ---
# antes de convertir a factor, para no tratar "no informa" como un nivel
# educativo válido.
geih_sample <- geih_sample |>
  mutate(
    maxEducLevel = ifelse(maxEducLevel == 9, NA, maxEducLevel),
    maxEducLevel = as.factor(maxEducLevel)
  )

# Incluimos variable de jefe de hogar
geih_sample <- geih_sample |>
  mutate(
    jefe_hogar    = ifelse(p6050 == 1, 1, 0)
  )


# Verificación rápida de tipos y niveles
#sapply(geih_sample[c("relab", "Female", "maxEducLevel","jefe_hogar")], class)
#table(geih_sample$relab, useNA = "ifany")
#table(geih_sample$maxEducLevel, useNA = "ifany")

# Nuevas estadísticas descriptivas dentro de esta población objetivo
#geih_sample |>
#  summarise(
#    n = n(),
#    n_na_income = sum(is.na(y_total_m)),
#    n_zero_income = sum(y_total_m == 0, na.rm = TRUE),
#    pct_na = round(100 * n_na_income / n, 2),
#    pct_zero = round(100 * n_zero_income / n, 2)
#  )

# =============================================================
# Documentamos el grupo de NAs en ingreso
# =============================================================
# Según la decisión de limpieza: estos NAs se excluyen de los modelos de
# regresión, pero se documentan como un grupo que posiblemente está
# subreportando ingreso, relevante para la pregunta del taller sobre
# detección de subreporte para la autoridad tributaria.

#na_income_profile <- geih_sample |>
#  filter(is.na(y_total_m)) |>
#  summarise(
#    n = n(),
#    pct_female = round(100 * mean(Female, na.rm = TRUE), 1),
#    mean_age = round(mean(age, na.rm = TRUE), 1),
#    pct_informal = round(100 * mean(formal == 0, na.rm = TRUE), 1)
#  )

# na_income_profile

# Distribución de quienes sí reportan ingreso
#geih_sample |>
#  filter(!is.na(y_total_m)) |>
#  summarise(
#    min = min(y_total_m),
#    p1 = quantile(y_total_m, 0.01),
#    p5 = quantile(y_total_m, 0.05),
#    median = median(y_total_m),
#    p95 = quantile(y_total_m, 0.95),
#    p99 = quantile(y_total_m, 0.99),
#    max = max(y_total_m)
#  )

# Revisión de las colas extremas
#geih_sample |>
#  filter(!is.na(y_total_m)) |>
#  summarise(
#    n_below_p1 = sum(y_total_m < quantile(y_total_m, 0.01)),
#    n_above_p99 = sum(y_total_m > quantile(y_total_m, 0.99)),
#    n_extreme_low = sum(y_total_m < 50000),   # menos de 50 mil al mes
#    n_extreme_high = sum(y_total_m > 20000000) # más de 20 millones al mes
#  )

# Veamos la forma de la distribución del ingreso
#geih_sample |>
#  filter(!is.na(y_total_m), y_total_m > 0) |>
#  ggplot(aes(x = log(y_total_m))) +
#  geom_histogram(bins = 60) +
#  labs(
#    title = "Distribución del log(ingreso laboral total)",
#    x = "log(y_total_m)",
#    y = "Frecuencia"
#  )

# Vemos con más detalle las observaciones altas extremas
#geih_sample |>
#  filter(!is.na(y_total_m)) |>
#  arrange(desc(y_total_m)) |>
#  select(y_total_m, age, sex, relab, totalHoursWorked, maxEducLevel, oficio, formal, sizeFirm) |>
#  head(20)

# Hacemos zoom en la cola derecha
#geih_sample |>
#  filter(!is.na(y_total_m)) |>
#  ggplot(aes(x = log(y_total_m))) +
#  geom_histogram(bins = 100) +
#  coord_cartesian(xlim = c(14.5, 18.5)) +  # zoom en la cola derecha (~2M a ~70M)
#  labs(
#    title = "Zoom: cola derecha de log(ingreso laboral total)",
#    x = "log(y_total_m)",
#    y = "Frecuencia"
#  )

# --Calculamos los percentiles de corte--
p_low <- quantile(geih_sample$y_total_m, 0.005, na.rm = TRUE)
p_high <- quantile(geih_sample$y_total_m, 0.995, na.rm = TRUE)

p_low
p_high

# --Aplicamos el filtro usando esos percentiles--
geih_clean <- geih_sample |>
  filter(
    !is.na(y_total_m),
    y_total_m >= p_low,
    y_total_m <= p_high
  )

# dim(geih_clean) Dimensiones finales

# --- Guardamos la base limpia ---
dir.create(here("02_Data", "Processed"), recursive = TRUE, showWarnings = FALSE)
saveRDS(geih_clean, here("02_Data", "Processed", "geih_clean.rds"))
#write_csv(na_income_profile, here("02_Data", "Processed", "na_income_profile.rds"))


