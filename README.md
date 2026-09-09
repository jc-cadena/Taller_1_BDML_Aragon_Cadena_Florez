# Taller 1 — Big Data y Machine Learning (MECA 4107)

**Autores:**
Mauricio José Aragón Ramírez (código: 201729052)
Jonathan Camilo Cadena Silva (código: 202315765)
Julio Esteban Flórez Pérez (código: 202615893)

> Un colaborador (o el evaluador) debe poder clonar el repositorio, correr un único script, y reproducir íntegramente el análisis y todos sus resultados exportados, sin necesidad de intervención manual adicional.

**Preguntas empíricas del taller.** Usando la muestra GEIH 2018 para Bogotá: (i) ¿cómo es el perfil edad–ingreso laboral de los ocupados y en qué edad se alcanza el ingreso máximo?; (ii) ¿cuál es la magnitud de la brecha de ingreso laboral por género, no condicional y condicional en controles observables?; (iii) ¿qué tan bien puede predecirse el ingreso laboral fuera de muestra a partir de las características observables de los ocupados?

## Estructura del repositorio

```
Taller_1_BDML_Aragon_Cadena_Florez/
├── README.md
├── .gitignore
├── Taller_1_BDML_Aragon_Cadena_Florez.Rproj
├── 01_Scripts/
│   ├── MasterScript.R              # script maestro: corre todo el pipeline en orden
│   ├── 01_scrape_data.R            # Sección 3: scraping de la muestra GEIH 2018
│   ├── 02_clean_data.R             # Sección 3: limpieza y definición de la muestra
│   ├── 03_age_income_profile.R     # Sección 1: perfil edad-ingreso
│   ├── 04_gender_gap.R             # Sección 2: brecha de género
│   └── 05_prediction.R             # Sección 3: predicción del ingreso fuera de muestra
├── 02_Data/
│   ├── Raw/
│   │   └── geih_raw.Rds            # datos crudos cacheados (10 chunks unidos)
│   └── Processed/
│       ├── geih_clean.rds          # muestra de análisis (ocupados, 18+ años)
│       └── boot_edad_pico.rds      # réplicas bootstrap cacheadas de la edad pico
├── 03_Output/
│   ├── Figures/                    # figuras .png generadas por los módulos 03-05
│   └── Tables/                     # tablas .csv/.html generadas por los módulos 03-05
└── 04_Slides/                      # presentaciones para la sustentación oral
```

## Datos

La fuente es la muestra GEIH 2018 para Bogotá alojada por el profesor en `https://ignaciomsarmiento.github.io/GEIH2018_sample/` (10 páginas HTML o "chunks"). El diccionario de variables está disponible en `https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html`.

`01_scrape_data.R` no hace scraping incondicional: primero revisa si `02_Data/Raw/geih_raw.Rds` ya existe y, de ser así, carga ese archivo desde disco sin hacer ninguna solicitud de red (ni siquiera consulta `robots.txt`). Solo si el archivo no existe, verifica con el paquete `robotstxt` que el acceso automatizado esté permitido y descarga los 10 chunks. Como este repositorio ya incluye `geih_raw.Rds` y `geih_clean.rds`, una corrida completa del pipeline **no requiere conexión a internet**, salvo que se borren intencionalmente esos archivos para forzar un scraping nuevo.

`02_clean_data.R` restringe la muestra a ocupados mayores de 18 años y recorta el ingreso laboral (`y_total_m`) a un rango de percentiles para excluir valores extremos, dejando la base analítica `geih_clean.rds` que usan los tres módulos siguientes.

## Paquetes requeridos

El proyecto usa `pacman::p_load()` en cada script, que instala automáticamente cualquier paquete faltante y luego lo carga — no es necesario instalar nada manualmente antes de correr el pipeline, solo tener `pacman` disponible (`MasterScript.R` lo instala si hace falta).

| Script | Paquetes | Para qué se usan |
|---|---|---|
| `MasterScript.R` | `here`, `tidyverse`, `tictoc` | Rutas reproducibles, orquestación y cronometraje del pipeline |
| `01_scrape_data.R` | `tidyverse`, `rvest`, `here`, `robotstxt` | Web scraping de los chunks GEIH y verificación de `robots.txt` |
| `02_clean_data.R` | `tidyverse`, `here`, `skimr` | Limpieza, recodificación y descriptivos rápidos de la muestra |
| `03_age_income_profile.R` | `tidyverse`, `stargazer`, `here`, `conflicted`, `boot` | Perfiles OLS/WLS edad-ingreso, tablas de regresión y bootstrap de la edad pico |
| `04_gender_gap.R` | `tidyverse`, `here`, `stargazer`, `boot` | Especificaciones de la brecha de género, bootstrap de errores estándar, descomposición FWL |
| `05_prediction.R` | `tidyverse`, `caret`, `here`, `conflicted` | Partición entrenamiento/validación, estimación y validación de 9 modelos OLS, LOOCV analítico, importancia por permutación |

## Recomendaciones de los autores para reproducir el análisis 

1. Clonar el repositorio y abrir el proyecto de RStudio, para que `here()` fije el directorio raíz correctamente:

```
git clone https://github.com/jc-cadena/Taller_1_BDML_Aragon_Cadena_Florez.git
```

Abrir `Taller_1_BDML_Aragon_Cadena_Florez.Rproj` en RStudio.

2. En la consola, correr un único comando — el script maestro:

```r
source("01_Scripts/MasterScript.R")
```

Esta es la forma más eficiente de correr el proyecto completo: `MasterScript.R` (a) carga los paquetes base con `pacman`, (b) crea automáticamente las carpetas de salida (`02_Data/Processed`, `03_Output/Tables`, `03_Output/Figures`) si no existen, y (c) ejecuta en orden los cinco módulos numerados con `source()`, cronometrando cada uno con `tictoc`. No es necesario correr los scripts uno por uno manualmente, ni preocuparse por el orden de las secciones: el maestro ya lo resuelve. Además, gracias al *caching* de `01_scrape_data.R` descrito arriba, correr el maestro repetidas veces no vuelve a scrapear la fuente cada vez — solo recalcula la limpieza, las regresiones y las exportaciones, que son rápidas en esta muestra.

## Qué produce cada módulo

| Módulo | Sección del taller | Resultados principales exportados |
|---|---|---|
| `03_age_income_profile.R` | 1. Perfil edad-ingreso | `fig_perfiles_ols.png`, `fig_perfiles_wls.png`, `fig_boot_pico.png`, `tab_edad_pico.csv`, `tab_perfil_edad_ols.html` |
| `04_gender_gap.R` | 2. Brecha de género | `profile_by_gender.png`, `gender_gap_comparison.csv/.html`, `peak_age_by_gender.csv`, `fwl_comparison.csv` |
| `05_prediction.R` | 3. Predicción del ingreso | `fig_9_modelos_ajuste.png`, `fig_importancia_vars.png`, `fig_pdp_top_var.png`, `tab_rmse_modelos_ols.csv`, `tab_loocv_ols.csv`, `tab_importancia_ols.csv` |

En la Sección 1 se estiman perfiles cuadráticos en edad (OLS y WLS, este último ponderado por `fweight`), se calcula la edad pico implícita por vértice de la parábola y su intervalo de confianza por bootstrap no paramétrico (1.000 réplicas). En la Sección 2 se estiman cuatro especificaciones de la brecha de género (de no condicional a condicional "extra"), cada una con error estándar analítico y bootstrap (1.000 réplicas), más una descomposición Frisch-Waugh-Lovell sobre la especificación "segura". En la Sección 3 se particiona la muestra en entrenamiento (chunks 1-7) y validación (chunks 8-10), se comparan nueve especificaciones OLS por RMSE de validación/AIC/BIC, se valida el modelo ganador con un LOOCV analítico exacto vía la matriz sombrero (`e_i/(1-h_ii)`) y se calcula importancia de variables por permutación fuera de muestra.

## Nota sobre el uso de IA

Para el desarrollo de este taller usamos inteligencia artificial como apoyo para llevar a la práctica la teoría de las lecturas, las clases magistrales y las clases complementarias del curso, principalmente para hacer un uso eficiente y comprensible de los paquetes y las funciones de R vistos en clase. Las decisiones de modelación, las especificaciones econométricas y la interpretación de los resultados son responsabilidad de los autores.
