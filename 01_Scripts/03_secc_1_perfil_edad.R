# ==============================================================================
# Problem Set 1 - Predicting Income
#
# BLOQUE 01: Sección 1 - Perfil edad-ingreso laboral
# ==============================================================================



##### 1. Configuración #####

library(pacman)

p_load(tidyverse, stargazer, here, conflicted, boot)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
options(scipen = 999)

# Se definen rutas del proyecto
ruta_datos   <- here("02_Data", "Processed", "geih_clean.rds")
ruta_figuras <- here("03_Output", "Figures")
ruta_tablas  <- here("03_Output", "Tables")

dir.create(ruta_figuras, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_tablas,  recursive = TRUE, showWarnings = FALSE)


##### 2. BLOQUE DE PREPARACIÓN #####
#   - construimos log(ingreso), que es la variable dependiente de todo el taller;
#   - convertimos a factor las categóricas que 02_clean_data.R dejó numéricas
#     (formal, sizeFirm, oficio, estrato1).
#   - ponemos etiquetas legibles para que las tablas no salgan con números.
# ==============================================================================

geih <- readRDS(ruta_datos)

geih <- geih |>
  mutate(
    log_y  = log(y_total_m),
    y_mill = y_total_m / 1e6, # Transformación Variable dependiente
    # Agregamos la versión etiquetada de sexo para tablas y gráficos.
    sexo = factor(Female, levels = c(0, 1), labels = c("Hombre", "Mujer")),
    #Les ponemos nombre a los distintos niveles educativos.
    #"Ninguno" queda como categoría de referencia por ser el primer nivel.
    educ_fac = factor(maxEducLevel, levels = as.character(1:7),
                      labels = c("Ninguno", "Preescolar", "Primaria incompleta",
                                 "Primaria completa", "Secundaria incompleta",
                                 "Secundaria completa", "Superior")),
    # Tipo de vinculación
    relab_fac = factor(relab,
                       levels = c("1", "2", "3", "4", "5", "otros"),
                       labels = c("Empresa particular", "Gobierno",
                                  "Empleado doméstico", "Cuenta propia",
                                  "Patrón o empleador", "Otro")),
    # Formalidad y tamaño de firma
    formal_fac   = factor(formal, levels = c(0, 1),
                          labels = c("Informal", "Formal")),
    sizeFirm_fac = factor(sizeFirm, levels = 1:5,
                          labels = c("Cuenta propia", "2-5 trabajadores",
                                     "6-10 trabajadores", "11-50 trabajadores",
                                     "Más de 50 trabajadores")),
    # Estrato socioeconómico como categórica.
    estrato_fac = factor(estrato1),
    # Oficio: ~80 categorías, muchas con un puñado de casos. Colapsamos en
    # "Otro" las que tengan menos de 100 observaciones. Sin esto, en la Sección 3
    # es casi seguro que algún oficio aparezca en validación y no en
    # entrenamiento, y predict() se cae con "factor has new levels".
    oficio_fac = fct_lump_min(factor(oficio), min = 100, other_level = "Otro"),
    fweight = as.numeric(fweight)
  )

# Salvaguarda sobre el logaritmo. 02_clean_data.R recorta la muestra entre los
# percentiles 0.5 y 99.5 del ingreso, pero NO exige que el ingreso sea positivo.
# Si el percentil 0.5 resultara ser cero (posible si hay muchos ceros entre los
# ocupados), pasarían ingresos de cero y log(0) = -Inf, lo cual contaminaría
# todas las regresiones sin error visible. Verificamos y excluimos.
n_no_finito <- sum(!is.finite(geih$log_y))
if (n_no_finito > 0) {
  message("ATENCIÓN: ", n_no_finito,
          " observaciones con log(ingreso) no finito. Se excluyen.")
  geih <- geih |> filter(is.finite(log_y))
}

# Vamos a usar por sección solo las variables que se necesitan
# (reportar el N de cada tabla)
vars_analisis <- c("log_y", "age", "Female", "educ_fac", "totalHoursWorked",
                   "relab_fac", "formal_fac", "sizeFirm_fac", "oficio_fac",
                   "estrato_fac", "p6426", "jefe_hogar", "n_ninos_under5",
                   "n_ninos_under10", "total_personas", "chunk", "fweight")

n_antes <- nrow(geih)

geih <- geih |>
  drop_na(all_of(vars_analisis)) |>
  droplevels()

c(antes = n_antes, despues = nrow(geih), perdidas = n_antes - nrow(geih))

# ==============================================================================

##### 3. Descriptiva que motiva la sección #####

# Mostramos el hecho estilizado que justifica meter un término cuadrático.
# A partir de calcular E[y | X = x] agrupando por la variable explicativa.
perfil_observado <- geih |>
  group_by(age) |>
  summarise(
    log_y_promedio = weighted.mean(log_y, w=fweight),
    n = n(),
    .groups = "drop"
  )

# Filtramos edades con pocas observaciones: los promedios calculados con tres
# personas son ruido y distorsionan la lectura visual.
fig_perfil_observado <- perfil_observado |>
  filter(n >= 20) |>
  ggplot(aes(x = age, y = log_y_promedio)) +
  geom_point(color = "#3a5e8c", size = 2) +
  labs(
    title = "Ingreso laboral promedio por edad",
    subtitle = "Promedio de log(ingreso) en cada edad, ocupados de 18 años o más",
    x = "Edad (años)",
    y = "log(ingreso laboral mensual)",
    caption = paste0("Fuente: cálculos propios con la muestra GEIH 2018 - Bogotá.\n",
                     "Nota: solo edades con al menos 20 observaciones.")
  ) +
  theme_bw()

fig_perfil_observado



#### 4. Perfil incondicional ####

# log(w) = b1 + b2*Edad + b3*Edad^2 + u

forma_incondicional <- log_y ~ age + I(age^2)

mod_incondicional_ols <- lm(
  forma_incondicional,
  data = geih
)

mod_incondicional_wls <- lm(
  forma_incondicional,
  data = geih,
  weights = fweight
)

summary(mod_incondicional_ols)
summary(mod_incondicional_wls)

###### 5. La edad pico #####

#   d log(w) / d Edad = b2 + 2*b3*Edad = 0   =>   Edad* = -b2 / (2*b3)

calcular_edad_pico <- function(modelo) {
  b_edad  <- coef(modelo)["age"]
  b_edad2 <- coef(modelo)["I(age^2)"]
  as.numeric(-b_edad / (2 * b_edad2))
}

edad_pico_inc_ols <- calcular_edad_pico(mod_incondicional_ols)

edad_pico_inc_wls <- calcular_edad_pico(mod_incondicional_wls)

edad_pico_inc_ols
edad_pico_inc_wls

coef(mod_incondicional_ols)["I(age^2)"] < 0   # ¿es un máximo?
range(geih$age)                               # ¿está dentro del rango?

coef(mod_incondicional_wls)["I(age^2)"] < 0   # ¿es un máximo?
range(geih$age)                               # ¿está dentro del rango?

##### 6. Intervalo de confianza bootstrap para la edad pico #####

boot_edad_pico <- function(datos,formula,B = 1000,semilla = 123,usar_pesos = FALSE){
  
  set.seed(semilla)
  picos <- rep(NA, B)
  for(i in 1:B){
    
    muestra_b <- sample_frac(
      datos,
      size = 1,
      replace = TRUE
    )
    
    if(usar_pesos){
      
      modelo_b <- lm(
        formula,
        data = muestra_b,
        weights = fweight
      )
      
    } else {
      
      modelo_b <- lm(
        formula,
        data = muestra_b
      )
    }
    picos[i] <- calcular_edad_pico(modelo_b)
  }
  picos
}

# B = 1000 réplicas
picos_inc_ols <- boot_edad_pico(
  geih,
  forma_incondicional,
  B = 1000,
  usar_pesos = FALSE
)

picos_inc_wls <- boot_edad_pico(
  geih,
  forma_incondicional,
  B = 1000,
  usar_pesos = TRUE
)

ic_inc_ols <- quantile(
  picos_inc_ols,
  probs = c(.025,.975)
)

ic_inc_wls <- quantile(
  picos_inc_wls,
  probs = c(.025,.975)
)
se_inc_ols <- sd(picos_inc_ols)

se_inc_wls <- sd(picos_inc_wls)

edad_pico_inc_ols
ic_inc_ols
se_inc_ols

edad_pico_inc_wls
ic_inc_wls
se_inc_wls

# Histograma de la distribución bootstrap.
fig_boot_pico <- tibble(pico = picos_inc_ols) |>
  ggplot(aes(x = pico)) +
  geom_histogram(bins = 40, fill = "#3a5e8c", color = "white", alpha = 0.85) +
  geom_vline(xintercept = edad_pico_inc_ols, color = "#d55e00", linewidth = 1) +
  geom_vline(xintercept = ic_inc_ols, color = "#d55e00", linetype = "dashed") +
  labs(
    title = "Distribución bootstrap de la edad pico",
    subtitle = "Perfil incondicional, 1.000 réplicas",
    x = "Edad pico implicada (años)",
    y = "Frecuencia",
    caption = paste0("Fuente: cálculos propios con la muestra GEIH 2018 - Bogotá.\n",
                     "Nota: línea sólida, estimación puntual; punteadas, intervalo ",
                     "percentil al 95%.")
  ) +
  theme_bw()

fig_boot_pico

fig_boot_pico_wls <- tibble(pico = picos_inc_wls) |>
  ggplot(aes(x = pico)) +
  geom_histogram(bins = 40, fill = "#3a5e8c", color = "white", alpha = 0.85) +
  geom_vline(xintercept = edad_pico_inc_wls, color = "#d55e00", linewidth = 1) +
  geom_vline(xintercept = ic_inc_wls, color = "#d55e00", linetype = "dashed") +
  labs(
    title = "Distribución bootstrap de la edad pico (ponderado)",
    subtitle = "Perfil incondicional, 1.000 réplicas",
    x = "Edad pico implicada (años)",
    y = "Frecuencia",
    caption = paste0("Fuente: cálculos propios con la muestra GEIH 2018 - Bogotá.\n",
                     "Nota: línea sólida, estimación puntual; punteadas, intervalo ",
                     "percentil al 95%.")
  ) +
  theme_bw()

fig_boot_pico_wls

##### 7. Perfil condicional #####

#   lm(log_y ~ age + I(age^2) + totalHoursWorked + factor(relab_detailed), data = geih)

forma_condicional <- log_y ~ age + I(age^2) + totalHoursWorked + relab_fac

mod_condicional_ols <- lm(
  forma_condicional,
  data = geih
)

mod_condicional_wls <- lm(
  forma_condicional,
  data = geih,
  weights = fweight
)
summary(mod_condicional_ols)
summary(mod_condicional_wls)

edad_pico_cond_ols <- calcular_edad_pico(
  mod_condicional_ols
)

edad_pico_cond_wls <- calcular_edad_pico(
  mod_condicional_wls
)

picos_cond_ols <- boot_edad_pico(
  geih,
  forma_condicional,
  B = 1000,
  usar_pesos = FALSE
)

picos_cond_wls <- boot_edad_pico(
  geih,
  forma_condicional,
  B = 1000,
  usar_pesos = TRUE
)

ic_cond_ols <- quantile(
  picos_cond_ols,
  probs = c(.025,.975)
)

ic_cond_wls <- quantile(
  picos_cond_wls,
  probs = c(.025,.975)
)

se_cond_ols <- sd(picos_cond_ols)

se_cond_wls <- sd(picos_cond_wls)

edad_pico_cond_ols
ic_cond_ols
se_cond_ols

edad_pico_cond_wls
ic_cond_wls
se_cond_wls

##### 8. Tabla de resultados #####

stargazer(
  mod_incondicional_ols,mod_condicional_ols,
  type = "text",
  title = "Perfil edad-ingreso laboral",
  column.labels = c("Inc. OLS","Cond. OLS"),
  dep.var.labels = "log(ingreso laboral mensual)",
  covariate.labels = c("Edad", "Edad al cuadrado", "Horas trabajadas"),
  omit = "relab_fac",
  omit.labels = "Efectos de tipo de vinculación",
  add.lines = list(c("Controles de vinculación", "No", "Sí")),
  keep.stat = c("n", "rsq", "adj.rsq"),
  digits = 4
)

stargazer(
  mod_incondicional_wls,mod_condicional_wls,
  type = "text",
  title = "Perfil edad-ingreso laboral",
  column.labels = c("Inc. WLS","Cond."),
  dep.var.labels = "log(ingreso laboral mensual)",
  covariate.labels = c("Edad", "Edad al cuadrado", "Horas trabajadas"),
  omit = "relab_fac",
  omit.labels = "Efectos de tipo de vinculación",
  add.lines = list(c("Controles de vinculación", "No", "Sí")),
  keep.stat = c("n", "rsq", "adj.rsq"),
  digits = 4
)

# La edad pico y su intervalo no salen de stargazer: hay que agregarlos como
# filas adicionales al pie de la tabla.
tab_edad_pico <- tibble(
  especificacion = c(
    "Incondicional OLS",
    "Condicional OLS",
    "Incondicional WLS",
    "Condicional WLS"
  ),
  
  edad_pico = c(
    edad_pico_inc_ols,
    edad_pico_cond_ols,
    edad_pico_inc_wls,
    edad_pico_cond_wls
  ),
  
  se_bootstrap = c(
    se_inc_ols,
    se_cond_ols,
    se_inc_wls,
    se_cond_wls
  ),
  
  ic_inferior = c(
    ic_inc_ols[1],
    ic_cond_ols[1],
    ic_inc_wls[1],
    ic_cond_wls[1]
  ),
  
  ic_superior = c(
    ic_inc_ols[2],
    ic_cond_ols[2],
    ic_inc_wls[2],
    ic_cond_wls[2]
  ),
  
  r2 = c(
    summary(mod_incondicional_ols)$r.squared,
    summary(mod_condicional_ols)$r.squared,
    summary(mod_incondicional_wls)$r.squared,
    summary(mod_condicional_wls)$r.squared
  )
)

tab_edad_pico

##### 9. Visualización de los dos perfiles #####

# Usamos la media de las horas y la categoría de vinculación más frecuente. 

relab_modal <- geih |> count(relab_fac, sort = TRUE) |> slice(1) |> pull(relab_fac)

grilla_edad <- tibble(
  age = seq(min(geih$age),max(geih$age),by = 1),
  totalHoursWorked =weighted.mean(geih$totalHoursWorked,w = geih$fweight),
  relab_fac = relab_modal
)

grilla_edad <- grilla_edad |>
  mutate(
    pred_incondicional = predict(mod_incondicional_ols, newdata = grilla_edad),
    pred_condicional   = predict(mod_condicional_ols,   newdata = grilla_edad)
  )

grilla_edad <- grilla_edad |>
  mutate(pred_inc_ols =predict(mod_incondicional_ols,newdata = grilla_edad),
    pred_cond_ols =predict(mod_condicional_ols,newdata = grilla_edad),
    pred_inc_wls =predict(mod_incondicional_wls,newdata = grilla_edad),
    pred_cond_wls = predict(mod_condicional_wls,newdata = grilla_edad))

grilla_ols <- grilla_edad |>
  select(age,pred_inc_ols,pred_cond_ols) |>
  pivot_longer(-age,
    names_to = "modelo",
    values_to = "pred"
  )

fig_perfiles_ols <- ggplot() +
  
  geom_point(
    data = filter(perfil_observado, n >= 20),
    aes(age, log_y_promedio),
    color = "grey55"
  ) +
  
  geom_line(
    data = grilla_ols,
    aes(age, pred, color = modelo),
    linewidth = 1.1
  ) +
  
  labs(
    title = "Perfil edad-ingreso sin pesos",
    subtitle = "Modelos OLS"
  ) +
  
  theme_bw()
fig_perfiles_ols

grilla_wls <- grilla_edad |>
  select(age,pred_inc_wls,pred_cond_wls) |>
  pivot_longer(-age,
    names_to = "modelo",
    values_to = "pred"
  )

fig_perfiles_wls <- ggplot() +
  
  geom_point(
    data = filter(perfil_observado, n >= 20),
    aes(age, log_y_promedio),
    color = "grey55"
  ) +
  
  geom_line(
    data = grilla_wls,
    aes(age, pred, color = modelo),
    linewidth = 1.1
  ) +
  
  labs(
    title = "Perfil edad-ingreso con pesos de expansión",
    subtitle = "Modelos WLS"
  ) +
  
  theme_bw()

fig_perfiles_wls

##### 10. Exportar #####

ggsave(file.path(ruta_figuras,"fig_perfiles_ols.png"),
  fig_perfiles_ols,
  width = 7.5,
  height = 5,
  dpi = 300
)

ggsave(file.path(ruta_figuras,"fig_perfiles_wls.png"),
  fig_perfiles_wls,
  width = 7.5,
  height = 5,
  dpi = 300
)

ggsave(file.path(ruta_figuras, "fig_boot_pico.png"),
       fig_boot_pico, width = 7, height = 4.5, dpi = 300)

write_csv(tab_edad_pico, file.path(ruta_tablas, "tab_edad_pico.csv"))

stargazer(mod_incondicional_ols,mod_condicional_ols,
  type = "html",
  out = file.path(ruta_tablas,"tab_perfil_edad_ols.html"),
  title = "Perfil edad-ingreso laboral",
  column.labels = c("Inc. OLS","Cond. OLS"),
  dep.var.labels ="log(ingreso laboral mensual)",
  covariate.labels = c("Edad","Edad al cuadrado","Horas trabajadas"),
  omit = "relab_fac",
  omit.labels ="Efectos de tipo de vinculación",
  add.lines = list(c("Controles de vinculación", "No", "Sí")),
  keep.stat = c("n","rsq","adj.rsq"),
  digits = 4
)

stargazer(mod_incondicional_wls,mod_condicional_wls,
          type = "html",
          out = file.path(ruta_tablas,"tab_perfil_edad_ols.html"),
          title = "Perfil edad-ingreso laboral",
          column.labels = c("Inc. WLS","Cond. WLS"),
          dep.var.labels ="log(ingreso laboral mensual)",
          covariate.labels = c("Edad","Edad al cuadrado","Horas trabajadas"),
          omit = "relab_fac",
          omit.labels ="Efectos de tipo de vinculación",
          add.lines = list(c("Controles de vinculación", "No", "Sí")),
          keep.stat = c("n","rsq","adj.rsq"),
          digits = 4
)

# Guardamos las réplicas bootstrap por si hay que rehacer el intervalo sin
# volver a correr los 2.000 modelos.
saveRDS(
  list(
    picos_inc_ols  = picos_inc_ols,
    picos_cond_ols = picos_cond_ols,
    picos_inc_wls  = picos_inc_wls,
    picos_cond_wls = picos_cond_wls
  ),
  file.path(
    dirname(ruta_datos),
    "boot_edad_pico.rds"
  )
)
