# ==============================================================================
# Problem Set 1 - Predicting Income
#
# BLOQUE 01: Sección 1 - Perfil edad-ingreso laboral
# ==============================================================================



##### 1. Configuración #####

library(pacman)

p_load(tidyverse, stargazer, here, conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
options(scipen = 999)

# Se definen rutas del proyecto
.
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
    oficio_fac = fct_lump_min(factor(oficio), min = 100, other_level = "Otro")
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
                   "n_ninos_under10", "total_personas", "chunk")

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
    log_y_promedio = mean(log_y),
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

modelo_incondicional <- lm(forma_incondicional, data = geih)

summary(modelo_incondicional)

###### 5. La edad pico #####

#   d log(w) / d Edad = b2 + 2*b3*Edad = 0   =>   Edad* = -b2 / (2*b3)

calcular_edad_pico <- function(modelo) {
  b_edad  <- coef(modelo)["age"]
  b_edad2 <- coef(modelo)["I(age^2)"]
  as.numeric(-b_edad / (2 * b_edad2))
}

edad_pico_incondicional <- calcular_edad_pico(modelo_incondicional)

edad_pico_incondicional
coef(modelo_incondicional)["I(age^2)"] < 0   # ¿es un máximo?
range(geih$age)                               # ¿está dentro del rango?


##### 6. Intervalo de confianza bootstrap para la edad pico #####

boot_edad_pico <- function(datos, formula, B = 1000, semilla = 123) {

  set.seed(semilla)

  picos <- rep(NA, B)   # vector vacío donde guardamos las B edades pico

  for (i in 1:B) {

    # sample_frac(size = 1, replace = TRUE): muestra del 100% del tamaño
    # original, con reemplazo. Es la muestra bootstrap.
    muestra_b <- sample_frac(datos, size = 1, replace = TRUE)

    modelo_b <- lm(formula, data = muestra_b)

    picos[i] <- calcular_edad_pico(modelo_b)
  }

  picos
}

# B = 1000 réplicas
picos_incondicional <- boot_edad_pico(geih, forma_incondicional, B = 1000)

ic_incondicional <- quantile(picos_incondicional, probs = c(0.025, 0.975))
se_incondicional <- sd(picos_incondicional)

edad_pico_incondicional
ic_incondicional
se_incondicional

# Histograma de la distribución bootstrap.
fig_boot_pico <- tibble(pico = picos_incondicional) |>
  ggplot(aes(x = pico)) +
  geom_histogram(bins = 40, fill = "#3a5e8c", color = "white", alpha = 0.85) +
  geom_vline(xintercept = edad_pico_incondicional, color = "#d55e00", linewidth = 1) +
  geom_vline(xintercept = ic_incondicional, color = "#d55e00", linetype = "dashed") +
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

##### 7. Perfil condicional #####

#   lm(log_y ~ age + I(age^2) + totalHoursWorked + factor(relab_detailed), data = geih)

forma_condicional <- log_y ~ age + I(age^2) + totalHoursWorked + relab_fac

modelo_condicional <- lm(forma_condicional, data = geih)

summary(modelo_condicional)

edad_pico_condicional <- calcular_edad_pico(modelo_condicional)

picos_condicional <- boot_edad_pico(geih, forma_condicional, B = 1000)
ic_condicional    <- quantile(picos_condicional, probs = c(0.025, 0.975))
se_condicional    <- sd(picos_condicional)

edad_pico_condicional
ic_condicional

##### 8. Tabla de resultados #####

stargazer(
  modelo_incondicional, modelo_condicional,
  type = "text",
  title = "Perfil edad-ingreso laboral",
  column.labels = c("Incondicional", "Condicional"),
  dep.var.labels = "log(ingreso laboral mensual)",
  covariate.labels = c("Edad", "Edad al cuadrado", "Horas trabajadas"),
  omit = "relab_fac",
  omit.labels = "Efectos de tipo de vinculación",
  keep.stat = c("n", "rsq", "adj.rsq"),
  digits = 4
)

# La edad pico y su intervalo no salen de stargazer: hay que agregarlos como
# filas adicionales al pie de la tabla.
tab_edad_pico <- tibble(
  especificacion = c("Incondicional", "Condicional"),
  edad_pico      = c(edad_pico_incondicional, edad_pico_condicional),
  se_bootstrap   = c(se_incondicional, se_condicional),
  ic_inferior    = c(ic_incondicional[1], ic_condicional[1]),
  ic_superior    = c(ic_incondicional[2], ic_condicional[2]),
  r2             = c(summary(modelo_incondicional)$r.squared,
                     summary(modelo_condicional)$r.squared)
)

tab_edad_pico

##### 9. Visualización de los dos perfiles #####

# Usamos la media de las horas y la categoría de vinculación más frecuente. 

relab_modal <- geih |> count(relab_fac, sort = TRUE) |> slice(1) |> pull(relab_fac)

grilla_edad <- tibble(
  age              = seq(min(geih$age), max(geih$age), by = 1),
  totalHoursWorked = mean(geih$totalHoursWorked),
  relab_fac        = relab_modal
)

grilla_edad <- grilla_edad |>
  mutate(
    pred_incondicional = predict(modelo_incondicional, newdata = grilla_edad),
    pred_condicional   = predict(modelo_condicional,   newdata = grilla_edad)
  )

grilla_larga <- grilla_edad |>
  select(age, pred_incondicional, pred_condicional) |>
  pivot_longer(cols = c(pred_incondicional, pred_condicional),
               names_to = "especificacion", values_to = "prediccion") |>
  mutate(especificacion = recode(especificacion,
                                 "pred_incondicional" = "Incondicional",
                                 "pred_condicional"   = "Condicional"))

fig_perfiles <- ggplot() +
  # Puntos: el promedio observado por edad (el hecho a explicar).
  geom_point(
    data = filter(perfil_observado, n >= 20),
    aes(x = age, y = log_y_promedio),
    color = "grey55", size = 1.8, alpha = 0.8
  ) +
  # Líneas: los perfiles estimados.
  geom_line(
    data = grilla_larga,
    aes(x = age, y = prediccion, color = especificacion),
    linewidth = 1.1
  ) +
  # Banda vertical: el intervalo bootstrap de la edad pico incondicional.
  annotate("rect",
           xmin = ic_incondicional[1], xmax = ic_incondicional[2],
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#d55e00") +
  geom_vline(xintercept = edad_pico_incondicional,
             color = "#d55e00", linetype = "dashed") +
  scale_color_manual(values = c("Incondicional" = "#3a5e8c",
                                "Condicional"   = "#009e73")) +
  labs(
    title = "Perfil edad-ingreso laboral en Bogotá",
    subtitle = "Ingreso promedio observado por edad y perfiles estimados",
    x = "Edad (años)",
    y = "log(ingreso laboral mensual)",
    color = "Especificación",
    caption = paste0("Fuente: cálculos propios con la muestra GEIH 2018 - Bogotá.\n",
                     "Nota: el perfil condicional se evalúa en las horas promedio y ",
                     "en la categoría de vinculación más frecuente. La banda sombreada ",
                     "es el intervalo bootstrap al 95% de la edad pico incondicional.")
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

fig_perfiles

##### 10. Exportar #####

ggsave(file.path(ruta_figuras, "fig_perfiles_edad.png"),
       fig_perfiles, width = 7.5, height = 5, dpi = 300)

ggsave(file.path(ruta_figuras, "fig_boot_pico.png"),
       fig_boot_pico, width = 7, height = 4.5, dpi = 300)

write_csv(tab_edad_pico, file.path(ruta_tablas, "tab_edad_pico.csv"))

stargazer(
  modelo_incondicional, modelo_condicional,
  type = "html",
  out = file.path(ruta_tablas, "tab_perfil_edad.html"),
  title = "Perfil edad-ingreso laboral",
  column.labels = c("Incondicional", "Condicional"),
  dep.var.labels = "log(ingreso laboral mensual)",
  covariate.labels = c("Edad", "Edad al cuadrado", "Horas trabajadas"),
  omit = "relab_fac",
  omit.labels = "Efectos de tipo de vinculación",
  keep.stat = c("n", "rsq", "adj.rsq"),
  digits = 4
)

# Guardamos las réplicas bootstrap por si hay que rehacer el intervalo sin
# volver a correr los 2.000 modelos.
saveRDS(
  list(incondicional = picos_incondicional, condicional = picos_condicional),
  file.path(dirname(ruta_datos), "boot_edad_pico.rds")
)
