# 04_gender_gap.R
# Objetivo: Estimar la brecha de ingreso laboral por género (Sección 2 del taller).
#           Se estima primero la brecha NO condicional, y luego (pendiente de
#           definir controles) la brecha condicional vía descomposición FWL,
#           con errores estándar analíticos y bootstrap.
# Input: Data/Processed/geih_clean.rds
# Output: (a definir - tabla de regresión, gráficos)

library(pacman)
p_load(
  tidyverse,   # Manipulación de datos y gráficos
  here,        # Rutas reproducibles
  stargazer,   # Tablas de regresión con formato publicable
  boot         # Bootstrap para errores estándar
)

geih_clean <- readRDS(here("Data", "Processed", "geih_clean.rds"))

# NOTA: Female, relab y maxEducLevel ya vienen correctamente construidos/tipados
# desde 02_clean_data.R. No es necesario recrearlos aquí.

# =============================================================
# 1. Brecha de género NO CONDICIONAL
#    log(w) = b1 + b2*Female + u
# =============================================================
model_gap_unconditional <- lm(log(y_total_m) ~ Female, data = geih_clean)
stargazer(model_gap_unconditional, type = "text")

# =============================================================
# Bootstrap SE para el coeficiente de género (no condicional)
# =============================================================
female_coef_fn <- function(data, index) {
  model <- lm(log(y_total_m) ~ Female, data = data, subset = index)
  coef(model)["Female"]
}

# Verificamos que la función reproduce el resultado original
female_coef_fn(geih_clean, 1:nrow(geih_clean))

# Fijamos semilla para reproducibilidad
set.seed(123)

# Corremos el bootstrap con R = 1000 réplicas
boot_gap_unconditional <- boot(
  data = geih_clean,
  statistic = female_coef_fn,
  R = 1000
)
boot_gap_unconditional

# --- IC bootstrap al 95% ---
ci_gap_uncond <- boot.ci(boot_gap_unconditional, type = "perc")
ci_gap_uncond

# =============================================================
# 2. Brecha de género CONDICIONAL "segura"
#    log(w) = b1 + b2*Female + b3*age + b4*age^2 + maxEducLevel + u
# =============================================================
model_gap_conditional_safe <- lm(
  log(y_total_m) ~ Female + age + I(age^2) + maxEducLevel,
  data = geih_clean
)
stargazer(model_gap_unconditional, model_gap_conditional_safe, type = "text")

female_coef_fn_safe <- function(data, index) {
  model <- lm(
    log(y_total_m) ~ Female + age + I(age^2) + maxEducLevel,
    data = data, subset = index
  )
  coef(model)["Female"]
}

# Verificamos que reproduce el resultado original
female_coef_fn_safe(geih_clean, 1:nrow(geih_clean))

set.seed(123)
boot_gap_safe <- boot(
  data = geih_clean,
  statistic = female_coef_fn_safe,
  R = 1000
)
boot_gap_safe

ci_gap_safe <- boot.ci(boot_gap_safe, type = "perc")
ci_gap_safe

# =============================================================
# 3. Brecha de género CONDICIONAL "completa"
#    + totalHoursWorked + relab (posibles bad controls, ver discusión)
# =============================================================
model_gap_conditional_full <- lm(
  log(y_total_m) ~ Female + age + I(age^2) + maxEducLevel + totalHoursWorked + relab,
  data = geih_clean
)
stargazer(model_gap_unconditional, model_gap_conditional_safe, model_gap_conditional_full, type = "text")

female_coef_fn_full <- function(data, index) {
  model <- lm(
    log(y_total_m) ~ Female + age + I(age^2) + maxEducLevel + totalHoursWorked + relab,
    data = data, subset = index
  )
  coef(model)["Female"]
}

# Verificamos que reproduce el resultado original
female_coef_fn_full(geih_clean, 1:nrow(geih_clean))

set.seed(123)
boot_gap_full <- boot(
  data = geih_clean,
  statistic = female_coef_fn_full,
  R = 1000
)
boot_gap_full

ci_gap_full <- boot.ci(boot_gap_full, type = "perc")
ci_gap_full

# =============================================================
# 4. DESCOMPOSICIÓN FRISCH-WAUGH-LOVELL (FWL)
#    Aplicada sobre la especificación "segura" (nuestra apuesta principal)
#    X1 = age, I(age^2), maxEducLevel (los controles "seguros")
#    D  = Female
# =============================================================

# Paso 1: residualizamos log(y_total_m) contra X1 (sin Female)
model_y_on_X1 <- lm(log(y_total_m) ~ age + I(age^2) + maxEducLevel, data = geih_clean)
resid_y <- residuals(model_y_on_X1)

# Paso 2: residualizamos Female contra los mismos X1
model_D_on_X1 <- lm(Female ~ age + I(age^2) + maxEducLevel, data = geih_clean)
resid_D <- residuals(model_D_on_X1)

# Paso 3: la regresión CORTA sobre los residuos recupera el mismo coeficiente
# que la regresión LARGA (model_gap_conditional_safe)
fwl_model <- lm(resid_y ~ resid_D)
summary(fwl_model)

# Comparación numérica: deben ser prácticamente idénticos
comparison_fwl <- c(
  "FWL (corta)"  = coef(fwl_model)["resid_D"],
  "Regresión completa" = coef(model_gap_conditional_safe)["Female"]
)
comparison_fwl
max(abs(diff(comparison_fwl)))

# --- SE analítico exacto de la FWL ---
# El lm() sobre residuos ajusta mal los grados de libertad (usa n-2 en vez de
# n-k del modelo completo), así que su SE por defecto no coincide exactamente
# con el de la regresión larga. El SE analítico correcto usa el sigma de la
# regresión completa y la varianza de D residualizado:
sigma_full <- summary(model_gap_conditional_safe)$sigma
se_fwl_analytic <- sigma_full / sqrt(sum(resid_D^2))
se_fwl_analytic

# Comparamos con el SE reportado directamente por la regresión completa
se_full_direct <- summary(model_gap_conditional_safe)$coefficients["Female", "Std. Error"]
c("SE FWL (fórmula exacta)" = se_fwl_analytic, "SE regresión completa" = se_full_direct)

# --- Bootstrap del procedimiento FWL completo ---
# Repetimos los 3 pasos (residualizar y, residualizar D, regresión corta)
# sobre cada muestra bootstrap, para obtener el SE bootstrap del coeficiente FWL
fwl_coef_fn <- function(data, index) {
  d <- data[index, ]
  resid_y_b <- residuals(lm(log(y_total_m) ~ age + I(age^2) + maxEducLevel, data = d))
  resid_D_b <- residuals(lm(Female ~ age + I(age^2) + maxEducLevel, data = d))
  coef(lm(resid_y_b ~ resid_D_b))["resid_D_b"]
}

# Verificamos que reproduce el resultado original
fwl_coef_fn(geih_clean, 1:nrow(geih_clean))

set.seed(123)
boot_fwl <- boot(
  data = geih_clean,
  statistic = fwl_coef_fn,
  R = 1000
)
boot_fwl

ci_fwl <- boot.ci(boot_fwl, type = "perc")
ci_fwl

