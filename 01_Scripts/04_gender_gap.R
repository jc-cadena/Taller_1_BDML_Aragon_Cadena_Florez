# 04_gender_gap.R
# Objetivo: Estimar la brecha de ingreso laboral por género (Sección 2 del taller).
#           Se estiman 4 especificaciones (no condicional -> condicional
#           creciente en controles), con SE analítico y bootstrap para cada
#           una, descomposición FWL sobre la especificación "segura", y
#           perfiles edad-ingreso predichos por género (con picos e IC).
# Input: 02_Data/Processed/geih_clean.rds
# Output: 03_Output/Tables/*.html, 03_Output/Figures/*.png

library(pacman)
p_load(
  tidyverse,   # Manipulación de datos y gráficos
  here,        # Rutas reproducibles
  stargazer,   # Tablas de regresión con formato publicable
  boot         # Bootstrap para errores estándar
)

# --- Rutas de salida (replicables con here(), sin rutas absolutas) ---
dir.create(here("03_Output", "Tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("03_Output", "Figures"), recursive = TRUE, showWarnings = FALSE)

geih_clean <- readRDS(here("02_Data", "Processed", "geih_clean.rds"))

vars3 <- c("chunk", "age", "totalHoursWorked", "relab", "Female", "p6050", "jefe_hogar",
           "n_ninos_under5", "n_ninos_under10", "total_personas", "maxEducLevel", "college", "ocu",
           "formal", "sizeFirm", "y_total_m", "estrato1", "fweight", "p6426")

geih_clean <- geih_clean %>%
  mutate(
    log_w = log(y_total_m),
    relab = as.factor(relab)
  ) %>%
  select(all_of(vars3), log_w)

geih_clean <- geih_clean |>
  mutate(
    Female        = as.numeric(Female),
    age           = as.numeric(age),
    pesos         = as.numeric(fweight),
    tiempotrabajo = as.numeric(p6426),
    RelacionJefe  = factor(p6050, levels = as.character(1:9),
                           labels = c("Jefe", "Pareja", "Hijo", "Nieto",
                                      "Otro pariente", "Empleado", "Pensionista",
                                      "Trabajador", "Otro no pariente")),
    educ_fac      = factor(maxEducLevel, levels = as.character(1:7),
                           labels = c("Ninguno", "Preescolar", "Primaria incompleta",
                                      "Primaria completa", "Secundaria incompleta",
                                      "Secundaria completa", "Superior")),
    relab_fac     = factor(relab, levels = c("1", "2", "3", "4", "5", "otros"),
                           labels = c("Empresa particular", "Gobierno",
                                      "Empleado doméstico", "Cuenta propia",
                                      "Patrón o empleador", "Otro")),
    formal_fac    = factor(formal, levels = c(0, 1), labels = c("Informal", "Formal")),
    sizeFirm_fac  = factor(sizeFirm, levels = 1:5,
                           labels = c("Cuenta propia", "2-5 trabajadores",
                                      "6-10 trabajadores", "11-50 trabajadores",
                                      "Más de 50 trabajadores")),
    estrato_fac   = factor(estrato1)
  )

geih_clean <- geih_clean |>
  drop_na(all_of(vars3)) |>
  droplevels()

# =============================================================
# 1. Brecha de género NO CONDICIONAL
# =============================================================
model_gap_unconditional <- lm(log(y_total_m) ~ Female, data = geih_clean)

female_coef_fn <- function(data, index) {
  model <- lm(log(y_total_m) ~ Female, data = data, subset = index)
  coef(model)["Female"]
}
set.seed(123)
boot_gap_unconditional <- boot(data = geih_clean, statistic = female_coef_fn, R = 1000)
ci_gap_uncond <- boot.ci(boot_gap_unconditional, type = "perc")

# =============================================================
# 2. Brecha de género CONDICIONAL 1 (edad + educación)
# =============================================================
model_gap_conditional_1 <- lm(
  log(y_total_m) ~ Female + age + I(age^2) + maxEducLevel,
  data = geih_clean
)

female_coef_fn_1 <- function(data, index) {
  model <- lm(log(y_total_m) ~ Female + age + I(age^2) + maxEducLevel,
              data = data, subset = index)
  coef(model)["Female"]
}
set.seed(123)
boot_gap_1 <- boot(data = geih_clean, statistic = female_coef_fn_1, R = 1000)
ci_gap_1 <- boot.ci(boot_gap_1, type = "perc")

# =============================================================
# 3. Brecha de género CONDICIONAL "segura"
#    (edad + educación + horas + tipo de vinculación + formalidad + tamaño firma)
# =============================================================
model_gap_conditional_safe <- lm(
  log(y_total_m) ~ Female + age + I(age^2) + educ_fac + totalHoursWorked +
    relab_fac + formal_fac + sizeFirm_fac,
  data = geih_clean
)

female_coef_fn_safe <- function(data, index) {
  model <- lm(
    log(y_total_m) ~ Female + age + I(age^2) + educ_fac + totalHoursWorked +
      relab_fac + formal_fac + sizeFirm_fac,
    data = data, subset = index
  )
  coef(model)["Female"]
}
set.seed(123)
boot_gap_safe <- boot(data = geih_clean, statistic = female_coef_fn_safe, R = 1000)
ci_gap_safe <- boot.ci(boot_gap_safe, type = "perc")

# =============================================================
# 4. Brecha de género CONDICIONAL "extra"
#    (+ estrato + jefe de hogar + niños<5 x Female -- posibles bad controls)
# =============================================================
model_gap_conditional_full <- lm(
  log(y_total_m) ~ Female + age + I(age^2) + educ_fac + totalHoursWorked +
    relab_fac + formal_fac + sizeFirm_fac + estrato_fac + n_ninos_under5:Female +
    jefe_hogar,
  data = geih_clean
)

female_coef_fn_full <- function(data, index) {
  model <- lm(
    log(y_total_m) ~ Female + age + I(age^2) + educ_fac + totalHoursWorked +
      relab_fac + formal_fac + sizeFirm_fac + estrato_fac + n_ninos_under5:Female +
      jefe_hogar,
    data = data, subset = index
  )
  coef(model)["Female"]
}
set.seed(123)
boot_gap_full <- boot(data = geih_clean, statistic = female_coef_fn_full, R = 1000)
ci_gap_full <- boot.ci(boot_gap_full, type = "perc")

# =============================================================
# 5. DESCOMPOSICIÓN FRISCH-WAUGH-LOVELL (FWL)
#    Aplicada sobre la especificación "segura"
# =============================================================
model_y_on_X1 <- lm(log(y_total_m) ~ age + I(age^2) + educ_fac + totalHoursWorked +
                      relab_fac + formal_fac + sizeFirm_fac, data = geih_clean)
resid_y <- residuals(model_y_on_X1)

model_D_on_X1 <- lm(Female ~ age + I(age^2) + educ_fac + totalHoursWorked +
                      relab_fac + formal_fac + sizeFirm_fac, data = geih_clean)
resid_D <- residuals(model_D_on_X1)

fwl_model <- lm(resid_y ~ resid_D)

comparison_fwl <- c(
  "FWL (corta)" = coef(fwl_model)["resid_D"],
  "Regresión completa" = coef(model_gap_conditional_safe)["Female"]
)
max(abs(diff(comparison_fwl)))

sigma_full <- summary(model_gap_conditional_safe)$sigma
se_fwl_analytic <- sigma_full / sqrt(sum(resid_D^2))

fwl_coef_fn <- function(data, index) {
  d <- data[index, ]
  resid_y_b <- residuals(lm(log(y_total_m) ~ age + I(age^2) + educ_fac + totalHoursWorked +
                              relab_fac + formal_fac + sizeFirm_fac, data = d))
  resid_D_b <- residuals(lm(Female ~ age + I(age^2) + educ_fac + totalHoursWorked +
                              relab_fac + formal_fac + sizeFirm_fac, data = d))
  coef(lm(resid_y_b ~ resid_D_b))["resid_D_b"]
}
set.seed(123)
boot_fwl <- boot(data = geih_clean, statistic = fwl_coef_fn, R = 1000)
ci_fwl <- boot.ci(boot_fwl, type = "perc")

# =============================================================
# 6. (NUEVO) BOOTSTRAP SE DE LAS 4 ESPECIFICACIONES -- TABLA COMPARATIVA
#    El "std. error" bootstrap es la desviación estándar de las R=1000
#    réplicas del coeficiente de Female en cada boot object.
# =============================================================
se_boot_uncond <- sd(boot_gap_unconditional$t)
se_boot_1      <- sd(boot_gap_1$t)
se_boot_safe   <- sd(boot_gap_safe$t)
se_boot_full   <- sd(boot_gap_full$t)

se_analytic_uncond <- summary(model_gap_unconditional)$coefficients["Female", "Std. Error"]
se_analytic_1      <- summary(model_gap_conditional_1)$coefficients["Female", "Std. Error"]
se_analytic_safe   <- summary(model_gap_conditional_safe)$coefficients["Female", "Std. Error"]
se_analytic_full   <- summary(model_gap_conditional_full)$coefficients["Female", "Std. Error"]

tabla_se_comparativa <- tibble(
  especificacion = c("No condicional", "Condicional 1 (edad+educ)",
                     "Condicional segura", "Condicional extra"),
  coef_female    = c(coef(model_gap_unconditional)["Female"],
                     coef(model_gap_conditional_1)["Female"],
                     coef(model_gap_conditional_safe)["Female"],
                     coef(model_gap_conditional_full)["Female"]),
  se_analitico   = c(se_analytic_uncond, se_analytic_1, se_analytic_safe, se_analytic_full),
  se_bootstrap   = c(se_boot_uncond, se_boot_1, se_boot_safe, se_boot_full),
  r2             = c(summary(model_gap_unconditional)$adj.r.squared,
                     summary(model_gap_conditional_1)$adj.r.squared,
                     summary(model_gap_conditional_safe)$adj.r.squared,
                     summary(model_gap_conditional_full)$adj.r.squared)
)
tabla_se_comparativa

# =============================================================
# 7. (NUEVO) PERFIL EDAD-INGRESO POR GÉNERO -- picos e IC
#    Se agrega la interacción Female:age y Female:age^2 sobre la
#    especificación "segura" (controles fijos, forma del perfil de edad
#    permitida a variar por género).
# =============================================================
model_gap_profile <- lm(
  log(y_total_m) ~ Female * age + Female * I(age^2) + educ_fac + totalHoursWorked +
    relab_fac + formal_fac + sizeFirm_fac,
  data = geih_clean
)
summary(model_gap_profile)

# --- Picos de edad implícitos, por género ---
b_age    <- coef(model_gap_profile)["age"]
b_age2   <- coef(model_gap_profile)["I(age^2)"]
b_fem_age  <- coef(model_gap_profile)["Female:age"]
b_fem_age2 <- coef(model_gap_profile)["Female:I(age^2)"]

peak_age_hombres <- -b_age / (2 * b_age2)
peak_age_mujeres <- -(b_age + b_fem_age) / (2 * (b_age2 + b_fem_age2))

peak_age_hombres
peak_age_mujeres

# --- Bootstrap de ambos picos simultáneamente ---
peak_age_gender_fn <- function(data, index) {
  d <- data[index, ]
  m <- lm(
    log(y_total_m) ~ Female * age + Female * I(age^2) + educ_fac + totalHoursWorked +
      relab_fac + formal_fac + sizeFirm_fac,
    data = d
  )
  b_age_b    <- coef(m)["age"]
  b_age2_b   <- coef(m)["I(age^2)"]
  b_fem_age_b  <- coef(m)["Female:age"]
  b_fem_age2_b <- coef(m)["Female:I(age^2)"]
  
  peak_h <- -b_age_b / (2 * b_age2_b)
  peak_m <- -(b_age_b + b_fem_age_b) / (2 * (b_age2_b + b_fem_age2_b))
  c(hombres = peak_h, mujeres = peak_m)
}

# Verificamos que reproduce los resultados originales
peak_age_gender_fn(geih_clean, 1:nrow(geih_clean))

set.seed(123)
boot_peak_gender <- boot(data = geih_clean, statistic = peak_age_gender_fn, R = 1000)
boot_peak_gender

ci_peak_hombres <- boot.ci(boot_peak_gender, type = "perc", index = 1)
ci_peak_mujeres <- boot.ci(boot_peak_gender, type = "perc", index = 2)
ci_peak_hombres
ci_peak_mujeres

tabla_picos_genero <- tibble(
  genero = c("Hombres", "Mujeres"),
  edad_pico = c(peak_age_hombres, peak_age_mujeres),
  ic_95_inf = c(ci_peak_hombres$percent[4], ci_peak_mujeres$percent[4]),
  ic_95_sup = c(ci_peak_hombres$percent[5], ci_peak_mujeres$percent[5])
)
tabla_picos_genero

# --- Visualización: perfiles predichos por género ---
age_range <- data.frame(age = seq(min(geih_clean$age), max(geih_clean$age), by = 1))

moda_factor <- function(x) names(sort(table(x), decreasing = TRUE))[1]

profile_grid <- expand_grid(age_range, Female = c(0, 1)) %>%
  mutate(
    educ_fac     = factor(moda_factor(geih_clean$educ_fac), levels = levels(geih_clean$educ_fac)),
    totalHoursWorked = mean(geih_clean$totalHoursWorked),
    relab_fac    = factor(moda_factor(geih_clean$relab_fac), levels = levels(geih_clean$relab_fac)),
    formal_fac   = factor(moda_factor(geih_clean$formal_fac), levels = levels(geih_clean$formal_fac)),
    sizeFirm_fac = factor(moda_factor(geih_clean$sizeFirm_fac), levels = levels(geih_clean$sizeFirm_fac))
  )

profile_grid$log_income_hat <- predict(model_gap_profile, newdata = profile_grid)
profile_grid <- profile_grid %>%
  mutate(genero = ifelse(Female == 1, "Mujeres", "Hombres"))

profile_plot <- ggplot(profile_grid, aes(x = age, y = log_income_hat, color = genero)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = peak_age_hombres, linetype = "dashed", color = "#F8766D", alpha = 0.6) +
  geom_vline(xintercept = peak_age_mujeres, linetype = "dashed", color = "#00BFC4", alpha = 0.6) +
  labs(
    title = "Perfil edad-ingreso laboral predicho, por género",
    subtitle = "Líneas punteadas indican la edad pico implícita de cada género",
    x = "Edad", y = "Log(ingreso laboral mensual) predicho", color = "Género"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

profile_plot

ggsave(here("03_Output", "Figures", "profile_by_gender.png"),
       plot = profile_plot, width = 8, height = 5, dpi = 300)

# =============================================================
# 8. RESUMEN FINAL PARA SLIDES
# =============================================================

# --- Tabla 1: las 4 especificaciones, con SE analítico, bootstrap e IC ---
# NOTA: se construye manualmente (en vez de con stargazer) porque stargazer
# tiene un bug conocido con R >= 4.2 que rompe la combinación
# type="html" + add.lines + múltiples modelos ("if (is.na(s)): the
# condition has length > 1"), sin relación con nuestro código.
ci_uncond_vals <- ci_gap_uncond$percent[4:5]
ci_1_vals      <- ci_gap_1$percent[4:5]
ci_safe_vals   <- ci_gap_safe$percent[4:5]
ci_full_vals   <- ci_gap_full$percent[4:5]

tabla_gender_gap_final <- tabla_se_comparativa %>%
  mutate(
    ic_95_bootstrap = c(
      paste0("(", round(ci_uncond_vals[1], 3), ", ", round(ci_uncond_vals[2], 3), ")"),
      paste0("(", round(ci_1_vals[1], 3), ", ", round(ci_1_vals[2], 3), ")"),
      paste0("(", round(ci_safe_vals[1], 3), ", ", round(ci_safe_vals[2], 3), ")"),
      paste0("(", round(ci_full_vals[1], 3), ", ", round(ci_full_vals[2], 3), ")")
    )
  ) %>%
  rename(
    Especificación = especificacion,
    `Coef. Female` = coef_female,
    `SE analítico` = se_analitico,
    `SE bootstrap` = se_bootstrap,
    `R²` = r2,
    `IC 95% bootstrap` = ic_95_bootstrap
  )

tabla_gender_gap_final

write_csv(tabla_gender_gap_final, here("03_Output", "Tables", "gender_gap_comparison.csv"))

# --- Versión HTML simple para pegar en las slides ---
html_rows <- tabla_gender_gap_final %>%
  mutate(across(c(`Coef. Female`, `SE analítico`, `SE bootstrap`, `R²`), ~ round(., 4))) %>%
  pmap_chr(function(...) {
    row <- list(...)
    paste0("<tr>", paste0("<td>", row, "</td>", collapse = ""), "</tr>")
  })

html_table <- paste0(
  "<table border='1' style='border-collapse:collapse; text-align:center;'>",
  "<tr>", paste0("<th>", names(tabla_gender_gap_final), "</th>", collapse = ""), "</tr>",
  paste0(html_rows, collapse = ""),
  "</table>"
)

writeLines(html_table, here("03_Output", "Tables", "gender_gap_comparison.html"))

# --- Tabla 2: picos de edad por género ---
write_csv(tabla_picos_genero, here("03_Output", "Tables", "peak_age_by_gender.csv"))

# --- Tabla 3: FWL ---
tabla_fwl <- tibble(
  metodo = c("Regresión completa", "FWL (residualizado)"),
  coeficiente = c(coef(model_gap_conditional_safe)["Female"], coef(fwl_model)["resid_D"]),
  se_analitico = c(se_analytic_safe, se_fwl_analytic),
  se_bootstrap = c(se_boot_safe, sd(boot_fwl$t)),
  ic_95_inf = c(ci_gap_safe$percent[4], ci_fwl$percent[4]),
  ic_95_sup = c(ci_gap_safe$percent[5], ci_fwl$percent[5])
)
write_csv(tabla_fwl, here("03_Output", "Tables", "fwl_comparison.csv"))
tabla_fwl