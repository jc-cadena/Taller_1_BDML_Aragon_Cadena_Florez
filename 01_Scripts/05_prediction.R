# ==============================================================================
# Problem Set 1 - Predicting Income
# BLOQUE 03: Sección 3 - Predicción del ingreso laboral (OLS puro)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Configuración Inicial
# ------------------------------------------------------------------------------
library(pacman)
p_load(tidyverse, caret, here, conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
options(scipen = 999)
username <- Sys.getenv("USERNAME")
ruta_datos <- file.path("C:/Users", username, 
                        "/OneDrive/MEcA/2026-20/Big Data/Talleres/1")
setwd(ruta_datos)

# Rutas
ruta_datos   <- here("02_Data", "Processed", "geih_clean.rds")
ruta_figuras <- here("03_Output", "Figures")
ruta_tablas  <- here("03_Output", "Tables")

dir.create(ruta_figuras, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_tablas,  recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 2. Bloque Común de Preparación de Datos
# ------------------------------------------------------------------------------
geih <- readRDS(here("02_Data", "Processed", "geih_clean.rds"))

vars3 <- c("chunk", "age", "totalHoursWorked", "relab", "log_w", "Female", "p6050", "jefe_hogar",
            "n_ninos_under5", "n_ninos_under10", "total_personas","maxEducLevel", "college", "ocu",
            "relab", "formal", "sizeFirm", "y_total_m", "estrato1","fweight","p6426","oficio")

geih <- geih %>% 
  mutate(log_w = log(y_total_m),
         relab = as.factor(relab)) %>% 
  select(vars3)

table(geih$oficio)

geih <- geih |>
  mutate(
    
    # Factores
    Female       = as.numeric(Female),
    age          = as.numeric(age),
    pesos        = as.numeric(fweight),
    tiempotrabajo = as.numeric(p6426),
    RelacionJefe = factor(p6050, levels = as.character(1:9),
                          labels = c("Jefe","Pareja","Hijo","Nieto",
                                     "Otro pariente", "Empleado","Pensionista",
                                     "Trabajador","Otro no pariente")),
    educ_fac     = factor(maxEducLevel, levels = as.character(1:7),
                          labels = c("Ninguno", "Preescolar", "Primaria incompleta",
                                     "Primaria completa", "Secundaria incompleta",
                                     "Secundaria completa", "Superior")),
    relab_fac    = factor(relab, levels = c("1", "2", "3", "4", "5", "otros"),
                          labels = c("Empresa particular", "Gobierno",
                                     "Empleado doméstico", "Cuenta propia",
                                     "Patrón o empleador", "Otro")),
    formal_fac   = factor(formal, levels = c(0, 1), labels = c("Informal", "Formal")),
    sizeFirm_fac = factor(sizeFirm, levels = 1:5,
                          labels = c("Cuenta propia", "2-5 trabajadores",
                                     "6-10 trabajadores", "11-50 trabajadores",
                                     "Más de 50 trabajadores")),
    estrato_fac  = factor(estrato1),
    oficio_fac = fct_lump_min(factor(oficio), min = 100, other_level = "Otro")
  )

geih <- geih |>
  drop_na(all_of(vars3)) |>
  droplevels()

# ------------------------------------------------------------------------------
# 3. Partición Entrenamiento (Chunks 1-7) / Validación (Chunks 8-10)
# ------------------------------------------------------------------------------
entrenamiento <- geih |> filter(chunk <= 7)
validacion    <- geih |> filter(chunk > 7)

# ------------------------------------------------------------------------------
# 4. Especificaciones de los Modelos
# ------------------------------------------------------------------------------
formulas <- list(

  "B1. Sec1 Incondicional" = 
    log_w ~ age + I(age^2),
  
  "B2. Sec1 Condicional" = 
    log_w ~ age + I(age^2) + totalHoursWorked + relab_fac,
  
  "B3. Sec2 Incondicional" = 
    log_w ~ Female,
  
  "B4. Sec2 Preferida" = 
    log_w ~ Female + age + I(age^2) + educ_fac + totalHoursWorked +
    relab_fac,

  "M1. Jonathan" = 
    log_w ~ Female + oficio_fac + age*educ_fac + I(age^2) + estrato_fac +
    formal_fac + sizeFirm_fac + poly(totalHoursWorked, 3) + tiempotrabajo,
  
  "M2. Esteban 1" = 
    log_w ~ Female * (age + I(age^2)) + educ_fac + totalHoursWorked +
    I(totalHoursWorked^2) + relab_fac + formal_fac + sizeFirm_fac +
    oficio_fac + tiempotrabajo + estrato_fac +
    jefe_hogar + n_ninos_under10 + total_personas +
    educ_fac:age + Female:educ_fac + Female:formal_fac,
  
  "M3. Esteban 2" = 
    log_w ~ poly(age, 5) * Female + poly(age, 5) * educ_fac +
    poly(totalHoursWorked, 3) + relab_fac + formal_fac + sizeFirm_fac +
    oficio_fac + tiempotrabajo + estrato_fac +
    jefe_hogar + n_ninos_under10 + total_personas,
  
  "M4. Mauricio 1" = 
    log_w ~ poly(age, 5) * Female + poly(age, 5) * educ_fac + 
    poly(totalHoursWorked, 3) + relab_fac + formal_fac*Female + sizeFirm_fac + 
    estrato_fac + n_ninos_under10*Female + total_personas +
    RelacionJefe + tiempotrabajo + I(tiempotrabajo^2),
  
  "M5. Mauricio 2" = 
    log_w ~ poly(age, 8) * Female + poly(age, 8) * educ_fac + 
    poly(totalHoursWorked, 4) + relab_fac + formal_fac*Female + sizeFirm_fac*Female + 
    estrato_fac*Female + n_ninos_under10*Female + total_personas +
    RelacionJefe + poly(tiempotrabajo,2)

)



# ------------------------------------------------------------------------------
# 5. Estimación MCO sin Pesos (OLS) y Evaluación RMSE
# ------------------------------------------------------------------------------
calcular_rmse <- function(pred, obs) sqrt(mean((obs - pred)^2))

resultados <- tibble()
modelos    <- list()

for (nombre in names(formulas)) {
  # Estimación OLS pura sobre entrenamiento
  fit <- lm(formulas[[nombre]], data = entrenamiento)
  modelos[[nombre]] <- fit
  
  pred_train <- predict(fit, newdata = entrenamiento)
  pred_val   <- predict(fit, newdata = validacion)
  
  resultados <- bind_rows(
    resultados,
    tibble(
      modelo             = nombre,
      n_parametros       = sum(!is.na(coef(fit))),
      rmse_entrenamiento = calcular_rmse(pred_train, entrenamiento$log_w),
      rmse_validacion    = calcular_rmse(pred_val,   validacion$log_w),
      error_pct_aprox    = 100 * (exp(calcular_rmse(pred_val, validacion$log_w)) - 1),
      AIC = AIC(fit),
      BIC = BIC(fit)
    )
  )
}

print(resultados |> arrange(BIC))


#---------------------
analyzed_var <- "totalHoursWorked"

perfil_observado <- geih |>
  group_by(.data[[analyzed_var]]) |>
  summarise(
    log_w_promedio = mean(log_w, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

fig_perfil_observado <- perfil_observado |>
  filter(n >= 20) |>
  ggplot(
    aes(
      x = .data[[analyzed_var]],
      y = log_w_promedio
    )
  ) +
  geom_point(color = "#3a5e8c", size = 2) +
  labs(
    title = paste("Ingreso laboral promedio por", analyzed_var),
    subtitle = "Promedio de log(ingreso)",
    x = analyzed_var,
    y = "log(ingreso laboral mensual)"
  ) +
  theme_bw()

fig_perfil_observado

#-----------------


library(tidyverse)

# 1. Construir el dataframe ordenado para los 8 modelos
graficos <- map_dfr(
  names(modelos),
  \(m) {
    pred <- predict(modelos[[m]], newdata = validacion)
    
    tibble(
      modelo    = m,
      observado = validacion$log_w, # Cambia a validacion$log_w si tu variable se llama así
      predicho  = pred
    )
  }
) |> 
  # Mantiene el orden exacto en el que definiste los 8 modelos en la lista
  mutate(modelo = factor(modelo, levels = names(modelos)))

# 2. Visualización adaptada para 8 paneles (cuadrícula 2x4)
fig_8_modelos <- ggplot(graficos, aes(x = observado, y = predicho)) +
  geom_point(alpha = 0.08, size = 0.5, color = "#2c3e50") +
  geom_abline(
    slope = 1, 
    intercept = 0, 
    color = "#e74c3c", 
    linetype = "dashed", 
    size = 0.7
  ) +
  facet_wrap(~modelo, ncol = 4) + # Distribuye 8 modelos en 2 filas x 4 columnas
  labs(
    title    = "Valores Predichos vs. Observados (Muestra de Validación)",
    subtitle = "Línea roja discontinua = Predicción perfecta (45°)",
    x        = "Log(Ingreso) Observado",
    y        = "Log(Ingreso) Predicho"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "#f8f9fa", color = NA),
    strip.text       = element_text(face = "bold", size = 8.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 13)
  )

print(fig_8_modelos)





# ------------------------------------------------------------------------------
# 6. Selección del Ganador y LOOCV Computacionalmente Eficiente
# ------------------------------------------------------------------------------
nombre_ganador <- resultados |> slice_min(rmse_validacion, n = 1) |> pull(modelo)
modelo_ganador <- modelos[[nombre_ganador]]

# ==============================================================================
# EXPLICACIÓN TÉCNICA Y ATAJO COMPUTACIONAL DE LOOCV:
# ------------------------------------------------------------------------------
# En lugar de re-estimar el modelo N veces (ej. N = 15,000 regresiones OLS),
# aprovechamos la propiedad analítica del sombrero (Hat Matrix, H):
#
#   e_i_LOO = e_i / (1 - h_ii)
#
# Donde:
#   - e_i    es el residuo OLS del modelo estimado con la muestra completa.
#   - h_ii   es el apalancamiento (leverage) de la obs i: diagonal de X(X'X)^(-1)X'.
#
# Este atajo es EXACTO para MCO/OLS. Reduce el costo computacional de O(N * k^2)
# a una sola regresión inicial O(k^2) + un cálculo vectorial O(N).
# ==============================================================================

residuos       <- residuals(modelo_ganador)
apalancamiento <- hatvalues(modelo_ganador)

# Error fuera de muestra predicho exactamente mediante leverage
residuos_loo   <- residuos / (1 - apalancamiento)
rmse_loocv     <- sqrt(mean(residuos_loo^2))

tab_loocv <- tibble(
  Métrica = c("RMSE Entrenamiento", "RMSE Validación (Chunks 8-10)", "RMSE LOOCV (Analítico)"),
  Valor   = c(
    calcular_rmse(predict(modelo_ganador, entrenamiento), entrenamiento$log_w),
    calcular_rmse(predict(modelo_ganador, validacion), validacion$log_w),
    rmse_loocv
  )
)

print(tab_loocv)

# ------------------------------------------------------------------------------
# 7. Importancia de Variables (Permutación fuera de Muestra)
# ------------------------------------------------------------------------------
importancia_permutacion <- function(modelo, datos, variables, n_perm = 10) {
  set.seed(123)
  obs <- datos$log_w
  base_rmse <- calcular_rmse(predict(modelo, datos), obs)
  
  res <- tibble()
  for (v in variables) {
    deltas <- numeric(n_perm)
    for (p in 1:n_perm) {
      d_perm <- datos
      d_perm[[v]] <- sample(d_perm[[v]])
      deltas[p] <- calcular_rmse(predict(modelo, d_perm), obs) - base_rmse
    }
    res <- bind_rows(res, tibble(variable = v, delta_rmse = mean(deltas)))
  }
  res |> arrange(desc(delta_rmse))
}

vars_ganador <- intersect(all.vars(formulas[[nombre_ganador]])[-1], names(validacion))
tab_importancia <- importancia_permutacion(modelo_ganador, validacion, vars_ganador)

print(tab_importancia)

# ------------------------------------------------------------------------------
# 8. Perfil de Dependencia Parcial (Variable más Importante)
# ------------------------------------------------------------------------------
top_var <- tab_importancia |> slice(1) |> pull(variable)

grilla <- if (is.numeric(validacion[[top_var]])) {
  seq(quantile(validacion[[top_var]], 0.01), quantile(validacion[[top_var]], 0.99), length.out = 30)
} else {
  levels(droplevels(validacion[[top_var]]))
}

pdp_datos <- tibble()
for (val in grilla) {
  d_contrafactual <- validacion
  d_contrafactual[[top_var]] <- val
  pdp_datos <- bind_rows(pdp_datos, tibble(
    valor = val,
    y_hat_mean = mean(predict(modelo_ganador, newdata = d_contrafactual))
  ))
}

# Gráfico PDP
fig_pdp <- pdp_datos |> 
  ggplot(aes(x = valor, y = y_hat_mean)) +
  { if(is.numeric(validacion[[top_var]])) geom_line(color = "#1f77b4", size = 1.2) else geom_col(fill = "#1f77b4") } +
  labs(title = paste("Dependencia Parcial de", top_var),
       x = top_var, y = "log(Ingreso) Predicho Promedio") +
  theme_minimal()

print(fig_pdp)

# ------------------------------------------------------------------------------
# 9. Guardar Resultados
# ------------------------------------------------------------------------------
write_csv(resultados, file.path(ruta_tablas, "tab_rmse_modelos_ols.csv"))
write_csv(tab_loocv,  file.path(ruta_tablas, "tab_loocv_ols.csv"))
write_csv(tab_importancia, file.path(ruta_tablas, "tab_importancia_ols.csv"))
ggsave(file.path(ruta_figuras, "fig_pdp.png"), fig_pdp, width = 7, height = 4.5, dpi = 300)