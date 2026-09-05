# 01_scrape_data.R
# Objetivo: Scrapear los 10 chunks de datos GEIH 2018 desde
#           https://ignaciomsarmiento.github.io/GEIH2018_sample/
# Fecha de scraping: 2026-08-25
# Output: 02_Data/Raw/geih_raw.Rds

library(pacman)
p_load(tidyverse, rvest, here)

base_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

# Función para extraer la tabla de un chunk
scrape_chunk <- function(chunk_number) {
  page_url <- paste0(base_url, "pages/geih_page_", chunk_number, ".html")
  
  page_html <- read_html(page_url)
  
  chunk_data <- page_html |>
    html_element("table") |>
    html_table(convert = FALSE)  # <- esto nos evita que rvest adivine tipos distintos entre chunks
  
  names(chunk_data)[1] <- "id_fila_tableHTML"
  
  chunk_data
}

# Recorremos los 10 chunks con una pausa entre solicitudes (adoptando las
# buenas prácticas que vimos en la clase complementaria)
chunks_list <- map(1:10, function(i) {
  message("Descargando chunk ", i, "...")
  data_i <- scrape_chunk(i)
  data_i$chunk <- i          # <- guardamos de qué chunk vino cada fila
  Sys.sleep(0.5)
  data_i
})

# Unimos los 10 chunks en una sola base de datos
geih_raw <- bind_rows(chunks_list)

# Hacemos una revisión rápida
dim(geih_raw)
glimpse(geih_raw)

# Guardamos la base de datos en archivos crudos (en formato .rds)
dir.create(here("02_Data", "Raw"), recursive = TRUE, showWarnings = FALSE)
saveRDS(
  geih_raw,
  here("02_Data", "Raw", "geih_raw.Rds")
)