
# DISCLAIMER: La extensión de este trabajo excede mis capacidades de hacerlo plenamente por mi mano, por 
# lo que estará plagado de ayuda de IA. Cuando considere que la explicaicón de la IA no es los suficientemente clara, 
# hablaré explicitamente y la marcaré con "Bauti:". Allá vamos.

if (!require(data.table))    install.packages("data.table")
if (!require(ggplot2))    install.packages("ggplot2")
if (!require(dplyr))      install.packages("dplyr")
if (!require(showtext))   install.packages("showtext")
if (!require(scales)) install.packages("scales")
if (!require(ggplot2))    install.packages("ggplot2")
if (!require(dplyr))      install.packages("dplyr")
if (!require(showtext))   install.packages("showtext")
if (!require(scales))     install.packages("scales")
if (!require(tidyverse))  install.packages("tidyverse")
for (pkg in c("ggplot2","dplyr","showtext","scales","zoo")) {
  if (!require(pkg, character.only=TRUE))
    install.packages(pkg, repos="https://cloud.r-project.org")
  library(pkg, character.only=TRUE)
}

library(ggplot2)
library(dplyr)
library(showtext)
library(scales)
library(ggplot2)
library(dplyr)
library(showtext)
library(scales)
library(tidyverse)
library(data.table)

setwd("C:/Users/Bautista/Desktop/TRABAJO_FINAL/TP_FINAL")


# 2) Obtener la ruta del WD actual
proj_root <- getwd()


# 3) Definir rutas para cada carpeta
input_dir  <- file.path(proj_root, "input")
raw_dir    <- file.path(proj_root, "raw")
output_dir <- file.path(proj_root, "output")


# 1) Definir la ruta al archivo (ajusta el nombre si hace falta)
file_path_v2 <- "C:/Users/Bautista/Desktop/TRABAJO_FINAL/TP_FINAL/raw/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_5708.csv"


# 2a) Con data.table
pbi_paises <- fread(file_path_v2)


path_data   <- "C:/Users/Bautista/Desktop/TRABAJO_FINAL/TP_FINAL/raw"  
file_list   <- list.files(path = path_data, 
                          pattern = "^trade_[0-9]{4}\\.csv$", 
                          full.names = TRUE)


# ================================================
# Seccion 1: de extraccion de datos y trabajado de datos
#
# Paso 1: Listar los archivos CSV de 1995 a 2023
# ================================================


# Asumiendo que tus archivos se llaman "trade_1995.csv", …, "trade_2023.csv"

# Bauti: tuve que hacer esto de manera mas bien rudimentaria y cambiar a mano los nombres de todos los
# csv, porque si no no funcionaba y no se porque.Adjuntos en "raw" ya estan cambiados los nombres.

# Instalación y carga de paquetes


library(tidyverse)
library(data.table)


path_data   <- "C:/Users/Bautista/Desktop/TRABAJO_FINAL/TP_FINAL/raw"  
file_list   <- list.files(path = path_data, 
                          pattern = "^trade_[0-9]{4}\\.csv$", 
                          full.names = TRUE)


# ================================================
# Paso 2: Bauti: Leer cada CSV y unirlos en una sola tabla
# ================================================


# Usamos data.table::fread para velocidad y data.table::rbindlist
trade_list  <- lapply(file_list, fread)
trade_data  <- rbindlist(trade_list, use.names = TRUE)


# Ahora trade_data tiene las columnas:
#   t = año
#   i = exportador (código numérico ISO3‐digit)
#   j = importador (código numérico ISO3‐digit)
#   k = producto
#   v = valor monetario
#   q = cantidad


# ================================================
# Paso 3: Cargar el CSV con los códigos de país
# ================================================
# country_codes.csv contiene:
#   country_code  (numérico, coincide con i/j)
#   country_name
#   country_iso2
#   country_iso3

codes <- read_csv("C:/Users/Bautista/Desktop/TRABAJO_FINAL/TP_FINAL/raw/country_codes_V202501.csv")

# ================================================
# Paso 4: Asociar ISO3‐alfabético a exportador e importador
# ================================================
# Preparamos dos tablas auxiliares y hacemos left_join

# Auxiliar para exportadores
exp_iso3 <- codes %>% 
  select(country_code, country_iso3) %>% 
  rename(i = country_code,
         exporter_iso3 = country_iso3)

# Auxiliar para importadores
imp_iso3 <- codes %>% 
  select(country_code, country_iso3) %>% 
  rename(j = country_code,
         importer_iso3 = country_iso3)

# Unimos al dataset principal
trade_full <- trade_data %>%
  left_join(exp_iso3, by = "i") %>%
  left_join(imp_iso3, by = "j")


# ================================================
# Paso 5: Revisar resultado
# ================================================
# trade_full contiene ahora:
#   t, i, j, k, v, q, exporter_iso3, importer_iso3

head(trade_full)
str(trade_full)

# ================================================
# Paso 6: Bauti: Cambiar a nombres mas claros.
# ================================================

trade_full <- trade_full %>%
  rename(
    año        = t,
    exportador = i,
    importador = j,
    producto   = k,
    valor_usd  = v,
    cantidad   = q
  )

# Comprobación rápida
colnames(trade_full)
head(trade_full)

# 1. Asia: importaciones agregadas
trade_full <- trade_full %>%
  mutate(valor_millones_usd = valor_usd / 1000)


# ================================================
# Paso 7: Bauti: Separo los distintos datasets que voy a estar utilizando.
# ================================================


# Definimos vectores de códigos ISO3 de los grupos de países
sa_countries   <- c("ARG","BOL","BRA","CHL","COL","ECU","MEX","PRY","URY","PER")
asia_countries <- c("IDN","MYS","SGP","PHL","VNM","IND","BGD","LKA","KHM","MMR","LAO","NPL","PAK")


# 1. Dataset que contenga solo filas donde exportador o importador 
#    sea alguno de los países de América del Sur / México
trade_sa_bilateral <- trade_full %>%
  filter(exporter_iso3 %in% sa_countries | importer_iso3 %in% sa_countries)

# 2. Dataset con solo exportaciones de los países de América del Sur / México
trade_sa_exports <- trade_full %>%
  filter(exporter_iso3 %in% sa_countries)

# 3. Dataset con solo importaciones de los países de América del Sur / México
trade_sa_imports <- trade_full %>%
  filter(importer_iso3 %in% sa_countries)

# 4. Dataset con solo exportaciones de los países de Asia del Sudeste y Sur
trade_asia_exports <- trade_full %>%
  filter(exporter_iso3 %in% asia_countries)

# 5. Dataset con solo importaciones de los países de Asia del Sudeste y Sur
trade_asia_imports <- trade_full %>%
  filter(importer_iso3 %in% asia_countries)


# ================================================
# Paso 8: Bauti: Asigno algunas categorías generales de los productos
# Chiste: Que hace el tenedor y el cuchillo tristes afuera del hospital? pasa que cuchara se-opera AJJAJAJ
# ================================================


assign_subgroup <- function(df, prod_col = "producto") {
  df %>%
    mutate(
      # 1) Convertir a string de largo 6
      prod_str = str_pad(as.character(.data[[prod_col]]), width = 6, side = "left", pad = "0"),
      
      # 2) Obtener HS-2
      hs2      = as.integer(substr(prod_str, 1, 2)),
      
      # 3) Clasificar en subgrupos
      subgrupo = case_when(
        hs2 >= 1   & hs2 <= 24  ~ "Agroalimentario",
        hs2 >= 25  & hs2 <= 27  ~ "Minerales & combustibles",
        hs2 >= 28  & hs2 <= 39  ~ "Químicos & plásticos",
        hs2 >= 44  & hs2 <= 49  ~ "Madera, papel & muebles",
        (hs2 >= 40 & hs2 <= 43) |
          (hs2 >= 50 & hs2 <= 67) ~ "Textiles & confección",
        hs2 >= 72  & hs2 <= 83  ~ "Metales & manufacturas",
        hs2 >= 84  & hs2 <= 85  ~ "Maquinaria & eléctrica",
        hs2 >= 86  & hs2 <= 89  ~ "Equipo de transporte",
        hs2 >= 90  & hs2 <= 97  ~ "Otras manufacturas",
        TRUE                    ~ "Otros / no clasificado"
      )
    ) %>%
    select(-prod_str, -hs2)
}

trade_sa_bilateral <- assign_subgroup(trade_sa_bilateral)
trade_sa_exports   <- assign_subgroup(trade_sa_exports)
trade_sa_imports   <- assign_subgroup(trade_sa_imports)
trade_asia_exports <- assign_subgroup(trade_asia_exports)
trade_asia_imports <- assign_subgroup(trade_asia_imports)


# ================================================
# Paso 9: Bauti: Summary de ventas por año, pais y categoria. En asia también ya lo agrupo por continente.
# ================================================


# 1. Exportaciones de Sudamérica y México
sa_exports_summary <- trade_sa_exports %>%
  group_by(
    año,
    pais     = exporter_iso3,
    categoria = subgrupo
  ) %>%
  summarise(
    total_valor_usd = sum(valor_millones_usd, na.rm = TRUE),
    total_cantidad  = sum(cantidad, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Importaciones de Sudamérica y México
sa_imports_summary <- trade_sa_imports %>%
  group_by(
    año,
    pais       = importer_iso3,
    categoria  = subgrupo
  ) %>%
  summarise(
    total_valor_usd = sum(valor_millones_usd, na.rm = TRUE),
    total_cantidad  = sum(cantidad, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Exportaciones de Asia del Sudeste y Sur
asia_exports_summary <- trade_asia_exports %>%
  group_by(
    año,
    pais     = exporter_iso3,
    categoria = subgrupo
  ) %>%
  summarise(
    total_valor_usd = sum(valor_millones_usd, na.rm = TRUE),
    total_cantidad  = sum(cantidad, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Importaciones de Asia del Sudeste y Sur
asia_imports_summary <- trade_asia_imports %>%
  group_by(
    año,
    pais       = importer_iso3,
    categoria  = subgrupo
  ) %>%
  summarise(
    total_valor_usd = sum(valor_millones_usd, na.rm = TRUE),
    total_cantidad  = sum(cantidad, na.rm = TRUE),
    .groups = "drop"
  )

# 1. Exportaciones de Asia agregadas por año y categoría
asia_exports_agg <- trade_asia_exports %>%
  group_by(
    año,
    categoria = subgrupo
  ) %>%
  summarise(
    total_valor_usd = sum(valor_millones_usd, na.rm = TRUE),
    total_cantidad  = sum(cantidad, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Importaciones de Asia agregadas por año y categoría
asia_imports_agg <- trade_asia_imports %>%
  group_by(
    año,
    categoria = subgrupo
  ) %>%
  summarise(
    total_valor_usd = sum(valor_millones_usd, na.rm = TRUE),
    total_cantidad  = sum(cantidad, na.rm = TRUE),
    .groups = "drop"
  )


# ================================================
# Paso 10: Bauti: Ya tengo los datos que quiero de asia (creo). Voy a unificar todo latam como hice con asia y
#          ya tendre suficiente info de sudamerica como conjunto. Tambien quiero evaluar algunos paises
#          claves como Argentina, Chile, Brasil y Mexico. Esos data sets los sacaré de algunos pasos atras. 
# ================================================

# 1. Exportaciones de Sudamerica agregadas por año y categoría
sa_exports_agg <- trade_sa_exports %>%
  group_by(
    año,
    categoria = subgrupo
  ) %>%
  summarise(
    total_valor_usd = sum(valor_millones_usd, na.rm = TRUE),
    total_cantidad  = sum(cantidad, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Importaciones de Sudamerica agregadas por año y categoría
sa_imports_agg <- trade_sa_imports %>%
  group_by(
    año,
    categoria = subgrupo
  ) %>%
  summarise(
    total_valor_usd = sum(valor_millones_usd, na.rm = TRUE),
    total_cantidad  = sum(cantidad, na.rm = TRUE),
    .groups = "drop"
  )

# Vector de los 4 países
countries <- c("ARG", "BRA", "CHL", "MEX")

# 1) Tablas individuales por país (exportaciones e importaciones)
c_argentina_exports <- trade_sa_exports  %>% filter(exporter_iso3 == "ARG")
c_argentina_imports <- trade_sa_imports  %>% filter(importer_iso3 == "ARG")

c_brazil_exports    <- trade_sa_exports  %>% filter(exporter_iso3 == "BRA")
c_brazil_imports    <- trade_sa_imports  %>% filter(importer_iso3 == "BRA")

c_chile_exports     <- trade_sa_exports  %>% filter(exporter_iso3 == "CHL")
c_chile_imports     <- trade_sa_imports  %>% filter(importer_iso3 == "CHL")

c_mexico_exports    <- trade_sa_exports  %>% filter(exporter_iso3 == "MEX")
c_mexico_imports    <- trade_sa_imports  %>% filter(importer_iso3 == "MEX")

# 2) Tablas combinadas que incluyan cualquier mención de los 4 países
#    (exportaciones donde exportador o importador es uno de los 4)
all4_exports <- trade_sa_exports %>%
  filter(exporter_iso3 %in% countries | importer_iso3 %in% countries)

#    (importaciones donde exportador o importador es uno de los 4)
all4_imports <- trade_sa_imports %>%
  filter(exporter_iso3 %in% countries | importer_iso3 %in% countries)


# ================================================
# Paso 11: Bauti: Sigo obteniendo los datasets que necesito para los trescientos quintillones de graficos que quiero hacer.
#          Para empezar, necesito reducir el nivel de categorías de productos a 3. Ademas, ya que estoy,
#          obtengo la evolucion del total de expo/impo en usd current de cada region.
# ================================================


library(dplyr)

# Lista con los nombres de los data frames de cada país
datasets_countries <- c(
  "c_argentina_imports", "c_argentina_exports",
  "c_brazil_imports",    "c_brazil_exports",
  "c_chile_imports",     "c_chile_exports",
  "c_mexico_imports",    "c_mexico_exports"
)

# Renombrar 'subgrupo' a 'categoria' en cada uno
for (df_name in datasets_countries) {
  assign(
    df_name,
    get(df_name) %>%
      rename(categoria = subgrupo)
  )
}

# Verificación rápida (por ejemplo para Argentina imports)
colnames(c_argentina_imports)

# 1. Definir vectores de categorías originales
low_cats  <- c("Agroalimentario",
               "Minerales & combustibles",
               "Madera, papel & muebles")

mid_cats  <- c("Otras manufacturas",
               "Otros / no clasificado",
               "Textiles & confección")

high_cats <- c("Equipo de transporte",
               "Maquinaria & eléctrica",
               "Metales & manufacturas",
               "Químicos & plásticos")

# Función para reclasificar y agrupar
resumir_valor_agregado <- function(df) {
  df %>%
    # 1. Reclasificar a 3 niveles
    mutate(
      valor_agregado = case_when(
        categoria %in% low_cats  ~ "Valor Agregado Bajo",
        categoria %in% mid_cats  ~ "Valor Agregado Medio",
        categoria %in% high_cats ~ "Valor Agregado Alto",
        TRUE                     ~ "Sin Clasificar"
      ),
      
    ) %>%
    
    # 3. Agrupar y resumir
    group_by(año, valor_agregado) %>%
    summarise(
      total_valor_usd = sum(total_valor_usd, na.rm = TRUE),
      total_cantidad          = sum(total_cantidad %||% cantidad, na.rm = TRUE),
      .groups = "drop"
    )
}

# Aplicar la función a cada dataset
asia_exports_agg_evol <- resumir_valor_agregado(asia_exports_agg)
asia_imports_agg_evol <- resumir_valor_agregado(asia_imports_agg)
sa_exports_agg_evol   <- resumir_valor_agregado(sa_exports_agg)
sa_imports_agg_evol   <- resumir_valor_agregado(sa_imports_agg)


# 2) Renombrar la columna 'cantidad' a 'total_cantidad' en cada uno
renamed_list <- lapply(
  mget(datasets_countries),
  function(df) {
    df %>% rename(total_cantidad = cantidad)
  }
)

# 3) Sobreescribir los objetos originales en el entorno global
list2env(
  setNames(renamed_list, datasets_countries),
  envir = .GlobalEnv
)

# 4) Verificar que el cambio se aplicó correctamente
lapply(datasets_countries, function(name) {
  cat("Columnas de", name, ":\n")
  print(names(get(name)))
  cat("\n")
})


# 2) Renombrar la columna en cada data frame
renamed_list <- lapply(
  mget(datasets_countries),
  function(df) {
    df %>% rename(total_valor_usd = valor_millones_usd)
  }
)

# 3) Reemplazar los objetos originales en el entorno global
list2env(
  setNames(renamed_list, datasets_countries),
  envir = .GlobalEnv
)

# 4) Comprobación rápida: mostrar nombres de columnas en cada data frame
lapply(datasets_countries, function(name) {
  cat(name, ":\n")
  print(names(get(name)))
  cat("\n")
})


argentina_exports_va <- resumir_valor_agregado(c_argentina_exports)
argentina_imports_va <- resumir_valor_agregado(c_argentina_imports)
brazil_exports_va <- resumir_valor_agregado(c_brazil_exports)
brazil_imports_va <- resumir_valor_agregado(c_brazil_imports)
chile_exports_va <- resumir_valor_agregado(c_chile_exports)
chile_imports_va <- resumir_valor_agregado(c_chile_imports)
mexico_exports_va <- resumir_valor_agregado(c_mexico_exports)
mexico_imports_va <- resumir_valor_agregado(c_mexico_imports)


# ================================================
# Paso 12: Bauti: Obtengo la evolucion del total de expo/impo en usd current de cada region.
# ================================================


# 1. Asia: exportaciones anuales
asia_exports_yearly <- asia_exports_agg %>%
  group_by(año) %>%
  summarise(
    monto_usd_total = sum(total_valor_usd, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Asia: importaciones anuales
asia_imports_yearly <- asia_imports_agg %>%
  group_by(año) %>%
  summarise(
    monto_usd_total = sum(total_valor_usd, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Sudamérica/México: exportaciones anuales
sa_exports_yearly <- sa_exports_agg %>%
  group_by(año) %>%
  summarise(
    monto_usd_total = sum(total_valor_usd, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Sudamérica/México: importaciones anuales
sa_imports_yearly <- sa_imports_agg %>%
  group_by(año) %>%
  summarise(
    monto_usd_total = sum(total_valor_usd, na.rm = TRUE),
    .groups = "drop"
  )


# ================================================
# Paso 13: Bauti: Esto es interesante, a ver como sale. Voy a obtener el mismo dataset que el paso 10
#          pero en porcentaje de las expo e impo respectivamente.
# Chiste: Como hace el celular del carpíntero? ase-rrin, hace ring, aserin jajajaj chiste comedia gracia
# hago cumples y bautismos.
# ================================================


# Función que calcula el porcentaje de cada categoría sobre el total anual
calc_share <- function(df) {
  df %>%
    group_by(año) %>%
    mutate(
      pct_valor = total_valor_usd / sum(total_valor_usd, na.rm = TRUE) * 100
    ) %>%
    ungroup()
}

# Aplicar a cada dataset
asia_exports_pct  <- calc_share(asia_exports_agg_evol)
asia_imports_pct  <- calc_share(asia_imports_agg_evol)
sa_exports_pct    <- calc_share(sa_exports_agg_evol)
sa_imports_pct    <- calc_share(sa_imports_agg_evol)

# Comprobación rápida
asia_exports_pct %>% 
  filter(año == min(año))  # ver porcentajes del primer año


# ================================================
# Paso 14: Bauti: Fue mas facil de lo esperado, quizas, y solo quizas, soy un genio.
#          Ahora voy a calcular la balanza comercial de estas regiones.
# ================================================


# 1. Balance para Sudamérica/México
sa_trade_balance <- sa_exports_yearly %>%
  rename(exports_usd = monto_usd_total) %>%
  full_join(
    sa_imports_yearly %>% rename(imports_usd = monto_usd_total),
    by = "año"
  ) %>%
  replace_na(list(exports_usd = 0, imports_usd = 0)) %>%
  mutate(
    balance_usd = exports_usd - imports_usd
  )

# 2. Balance para Asia
asia_trade_balance <- asia_exports_yearly %>%
  rename(exports_usd = monto_usd_total) %>%
  full_join(
    asia_imports_yearly %>% rename(imports_usd = monto_usd_total),
    by = "año"
  ) %>%
  replace_na(list(exports_usd = 0, imports_usd = 0)) %>%
  mutate(
    balance_usd = exports_usd - imports_usd
  )

# Resultado: 
# sa_trade_balance  tiene columnas: año, exports_usd, imports_usd, balance_usd
# asia_trade_balance  tiene columnas: año, exports_usd, imports_usd, balance_usd

# Chequeo rápido
head(sa_trade_balance)
head(asia_trade_balance)


# ================================================
# Paso 15: Bauti: Momento importante, terminado el análisis comparativo entre regiones, procedo a 
#          utilizar los datos de los 4 paises clave seleccionados: AR, BR, CH y MX.
# ================================================


# Bauti: por aca tuve que hacer mi primer gran parche!!!! yay! 
# Chiste: porque las monjas no usan zapatillas? porque son de-botas JAJAJ


# 1. Vector con los nombres de los 8 datasets
va_datasets <- c(
  "argentina_exports_va", "argentina_imports_va",
  "brazil_exports_va",    "brazil_imports_va",
  "chile_exports_va",     "chile_imports_va",
  "mexico_exports_va",    "mexico_imports_va"
)

# 2. Función de resumen por año y valor agregado
summary_por_ano_va <- function(df) {
  df %>%
    group_by(año, valor_agregado) %>%
    summarise(
      total_valor_millones_usd = sum(total_valor_usd, na.rm = TRUE),
      .groups = "drop"
    )
}

# 3. Leer los data.frames, aplicar la función y nombrar los resultados
va_list    <- mget(va_datasets) 
va_summaries <- lapply(va_list, summary_por_ano_va)

# 4. Asignar de vuelta al entorno con sufijo "_summary"
names(va_summaries) <- paste0(va_datasets, "_summary")
list2env(va_summaries, envir = .GlobalEnv)


# ================================================
# Paso 16: Bauti: ya consegui los valores en sus 3 categorias por año para cada pais, ahora resta
#                 verlo como porcentaje de las expo/impo totales (respectivamente)
# ================================================


# 1. Vector con los nombres de los 8 data frames
va_summaries <- c(
  "argentina_exports_va_summary", "argentina_imports_va_summary",
  "brazil_exports_va_summary",    "brazil_imports_va_summary",
  "chile_exports_va_summary",     "chile_imports_va_summary",
  "mexico_exports_va_summary",    "mexico_imports_va_summary"
)

# 2. Leer, renombrar y volver a volcar al entorno
renamed_list <- lapply(
  mget(va_summaries),
  function(df) {
    df %>% rename(total_valor_usd = total_valor_millones_usd)
  }
)

# 3. Asignar de nuevo al GlobalEnv con los mismos nombres
list2env(setNames(renamed_list, va_summaries), envir = .GlobalEnv)

# Verificación rápida
lapply(va_summaries, function(name) names(get(name)))

# Aplicar a cada dataset
argentina_exports_va_pct  <- calc_share(argentina_exports_va_summary)
argentina_imports_va_pct <- calc_share(argentina_imports_va_summary)
brazil_exports_va_pct    <- calc_share(brazil_exports_va_summary)
brazil_imports_va_pct    <- calc_share(brazil_imports_va_summary)
chile_exports_va_pct  <- calc_share(chile_exports_va_summary)
chile_imports_va_pct <- calc_share(chile_imports_va_summary)
mexico_exports_va_pct    <- calc_share(mexico_exports_va_summary)
mexico_imports_va_pct    <- calc_share(mexico_imports_va_summary)


# ================================================
#          Bauti: Ya obtenido esto, me di cuenta que hay una cosa que no está unificada del todo y es
#          la manera de expresar el monto en usd (siempre en millones, pero el nombre como tal). Voy
#          a seguir con el codigo e intentar arreglarlo bien al final, si no lo logro, van a haber parches
#          correctivos mas adelante.
#
# Paso 17: Bauti: Ahora calculo la balanza comercial de los 4 juntos paises clave juntos.
#
# ================================================


# 1) Exportaciones globales (sumar los 4 países)
all4_exports <- bind_rows(
  argentina_exports_va_summary,
  brazil_exports_va_summary,
  chile_exports_va_summary,
  mexico_exports_va_summary
) %>%
  group_by(año) %>%
  summarise(
    exports_usd = sum(total_valor_usd, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Importaciones globales (sumar los 4 países)
all4_imports <- bind_rows(
  argentina_imports_va_summary,
  brazil_imports_va_summary,
  chile_imports_va_summary,
  mexico_imports_va_summary
) %>%
  group_by(año) %>%
  summarise(
    imports_usd = sum(total_valor_usd, na.rm = TRUE),
    .groups = "drop"
  )

# 3) Unir, llenar NAs y calcular balanza
all4_trade_balance <- full_join(all4_exports, all4_imports, by = "año") %>%
  replace_na(list(exports_usd = 0, imports_usd = 0)) %>%
  mutate(
    balance_usd = exports_usd - imports_usd
  )


# ================================================
#
# Paso 18: Bauti: ahora ya tengo para los graficos mas comunes. Voy a cargar una base que corresponde a los
#          pbi de los paises claves seleccionados. Para asi poder evaluar las impo/expo como porcentaje
#          del pbi.
#
# ================================================


# 2a) Con data.table
pbi_paises <- fread(file_path_v2)

# 3) Verificar estructura
str(pbi_paises)
glimpse(pbi_paises)

pbi_filtrado <- pbi_paises 

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Partimos de tu data.frame prefiltrado, llamado pbi_filtrado

# 1) Renombrar la primera columna a 'country_name'
names(pbi_filtrado)[1] <- "country_name"

# 2) Limpiar nombres: quitar prefijos como 'X', espacios, todo lo que NO sean 4 dígitos
names(pbi_filtrado) <- names(pbi_filtrado) %>%
  # primero quitamos 'X' solitario al inicio
  str_remove("^X") %>%
  # luego recortamos espacios en blanco
  str_trim()

# 3) Volcar a formato largo: todas las columnas excepto country_name
pbi_long <- pbi_filtrado %>%
  pivot_longer(
    cols      = -country_name,
    names_to  = "year_raw",
    values_to = "pbi_raw"
  )

# 4) Normalizar año y convertir PBI
pbi_tidy <- pbi_long %>%
  mutate(
    # de 'year_raw' extraigo el número (p.ej. "1960" → 1960, " 1980 " → 1980)
    año = parse_number(year_raw) %>% as.integer(),
    # convierto pbi a numérico y paso a millones
    pbi = parse_number(pbi_raw) / 1e6
  ) %>%
  # 5) Mantengo sólo lo que interesa y renombro
  select(
    pais = country_name,
    año,
    pbi
  ) %>%
  arrange(pais, año)

# 6) Verificación
glimpse(pbi_tidy)


# Eliminar filas que tengan NA en cualquiera de las tres columnas
pbi_clean <- pbi_tidy %>%
  drop_na(pais, año, pbi)

# Verificación
glimpse(pbi_clean)


# Filtrar solo los años 1996–2022 (elimina 1960–1995 y 2023–2024)
pbi_filtered_years <- pbi_clean %>%
  filter(año > 1994, año < 2024)

# Verificación
glimpse(pbi_filtered_years)


# ================================================
#
# Paso 19: Bauti: ahora voy a obtener los valores de expo e impo de all4 sobre el pbi de all4.
#
# ================================================


pbi_por_año <- pbi_filtered_years %>%
  group_by(año) %>%
  summarise(pbi_total = sum(pbi, na.rm = TRUE), .groups = "drop")


all4_expo_pct <- all4_exports %>%
  left_join(pbi_por_año, by = "año") %>%
  mutate(expo_pct = exports_usd / pbi_total * 100) %>%
  select(año, expo_pct)

all4_impo_pct <- all4_imports %>%
  left_join(pbi_por_año, by = "año") %>%
  mutate(impo_pct = imports_usd / pbi_total * 100) %>%
  select(año, impo_pct)


# ================================================
#
# Paso 20: Bauti: Nueva sección, ahora encontramos los mayores 2 socios comerciales.
#
# ================================================


# 1) Vector con tus 4 países en ISO3
paises <- c("ARG","BRA","CHL","MEX")

# 2) Ajusta estos nombres a los de tu data.frame:
exporter_col <- "exporter_iso3"   # columna con código ISO3 del exportador
importer_col <- "importer_iso3"   # columna con código ISO3 del importador
trade_col    <- "valor_millones_usd"       # columna con el valor del comercio en USD

# 3) Flujo de exportaciones de tus países
exp <- trade_sa_bilateral %>%
  filter(.data[[exporter_col]] %in% paises) %>%
  group_by(
    reporter = .data[[exporter_col]],
    partner  = .data[[importer_col]]
  ) %>%
  summarise(trade = sum(.data[[trade_col]], na.rm = TRUE), .groups = "drop")

# 4) Flujo de importaciones de tus países
imp <- trade_sa_bilateral %>%
  filter(.data[[importer_col]] %in% paises) %>%
  group_by(
    reporter = .data[[importer_col]],
    partner  = .data[[exporter_col]]
  ) %>%
  summarise(trade = sum(.data[[trade_col]], na.rm = TRUE), .groups = "drop")

# 5) Unión y suma total (export + import) por pareja
total_trade <- bind_rows(exp, imp) %>%
  group_by(reporter, partner) %>%
  summarise(total_trade_usd = sum(trade), .groups = "drop")

# 6) Para cada país reporter, seleccionar sus 2 principales socios
top2_partners <- total_trade %>%
  filter(reporter %in% paises) %>%
  group_by(reporter) %>%
  slice_max(total_trade_usd, n = 2, with_ties = FALSE) %>%
  ungroup() %>%
  rename(
    country = reporter,
    partner = partner,
    trade   = total_trade_usd
  )

# 7) Resultado
print(top2_partners)


# ================================================
#
# Paso 20: Bauti: Ya tengo quienes son los socios, ahora voy a calcular sus datos.
#
# ================================================


# Ajusta estos nombres si en tu data frame son otros
importer_col <- "importer_iso3"
exporter_col <- "exporter_iso3"

# 1) Imports de Estados Unidos
us_imports <- trade_full %>%
  filter(.data[[importer_col]] == "USA") %>%
  select(año, producto, valor_millones_usd)

# 2) Exports de Estados Unidos
us_exports <- trade_full %>%
  filter(.data[[exporter_col]] == "USA") %>%
  select(año, producto, valor_millones_usd)

# 3) Imports de China
china_imports <- trade_full %>%
  filter(.data[[importer_col]] == "CHN") %>%
  select(año, producto, valor_millones_usd)

# 4) Exports de China
china_exports <- trade_full %>%
  filter(.data[[exporter_col]] == "CHN") %>%
  select(año, producto, valor_millones_usd)


# 1) Definir función que añade la columna "categoria"
assign_categoria <- function(df, prod_col = "producto") {
  df %>%
    # convertir código a string de 6 dígitos
    mutate(
      prod_str = str_pad(as.character(.data[[prod_col]]), width = 6, side = "left", pad = "0"),
      hs2      = as.integer(substr(prod_str, 1, 2)),
      # clasificar en subgrupo y nombrarlo "categoria"
      categoria = case_when(
        hs2 >= 1   & hs2 <= 24  ~ "Agroalimentario",
        hs2 >= 25  & hs2 <= 27  ~ "Minerales & combustibles",
        hs2 >= 28  & hs2 <= 39  ~ "Químicos & plásticos",
        hs2 >= 44  & hs2 <= 49  ~ "Madera, papel & muebles",
        (hs2 >= 40 & hs2 <= 43) |
          (hs2 >= 50 & hs2 <= 67) ~ "Textiles & confección",
        hs2 >= 72  & hs2 <= 83  ~ "Metales & manufacturas",
        hs2 >= 84  & hs2 <= 85  ~ "Maquinaria & eléctrica",
        hs2 >= 86  & hs2 <= 89  ~ "Equipo de transporte",
        hs2 >= 90  & hs2 <= 97  ~ "Otras manufacturas",
        TRUE                    ~ "Otros / no clasificado"
      )
    ) %>%
    # quitar columnas auxiliares
    select(-prod_str, -hs2)
}

# 2) Aplicar a los 4 datasets de US y China
us_imports_cat   <- assign_categoria(us_imports)
us_exports_cat   <- assign_categoria(us_exports)
china_imports_cat<- assign_categoria(china_imports)
china_exports_cat<- assign_categoria(china_exports)

# 3) Verificación rápida
list(
  us_imports_cat    = names(us_imports_cat),
  us_exports_cat    = names(us_exports_cat),
  china_imports_cat = names(china_imports_cat),
  china_exports_cat = names(china_exports_cat)
)


# Vector con los nombres de tus 4 data.frames ya categorizados
datasets <- c("us_imports_cat", "us_exports_cat",
              "china_imports_cat", "china_exports_cat")

# Función para agrupar y resumir valor_millones_usd por año y categoría
group_and_summarise <- function(df) {
  df %>%
    group_by(año, categoria) %>%
    summarise(
      valor_millones_usd = sum(valor_millones_usd, na.rm = TRUE),
      .groups = "drop"
    )
}

# Aplicar la función a cada data frame y recuperar resultados en el entorno global
grouped_list <- lapply(mget(datasets), group_and_summarise)
names(grouped_list) <- paste0(datasets, "_grp")

list2env(grouped_list, envir = .GlobalEnv)

# Comprobación rápida
lapply(names(grouped_list), function(x) {
  cat(x, "— columnas:\n")
  print(names(get(x)))
  cat("Primeros registros:\n")
  print(head(get(x)))
  cat("\n")
})


# 1) Vector con los nombres de los data frames a procesar
datasets1 <- c(
  "us_exports_cat_grp",
  "us_imports_cat_grp",
  "china_exports_cat_grp",
  "china_imports_cat_grp"
)

# 2) Renombrar la columna 'valor_millones_usd' a 'total_valor_usd' en cada uno
renamed_list <- lapply(
  mget(datasets1),
  function(df) {
    df %>% rename(total_valor_usd = valor_millones_usd)
  }
)

# 3) Sobreescribir los objetos originales en el entorno global
list2env(
  setNames(renamed_list, datasets1),
  envir = .GlobalEnv
)

# 4) Verificación rápida
lapply(datasets, function(name) {
  cat(name, ":\n")
  print(names(get(name)))
  cat("\n")
})


# 1) Vuelvo a definir la función con el parámetro valor_col
resumir_valor_agregado1 <- function(df, valor_col = "total_valor_usd") {
  low_cats  <- c("Agroalimentario",
                 "Minerales & combustibles",
                 "Madera, papel & muebles")
  mid_cats  <- c("Otras manufacturas",
                 "Otros / no clasificado",
                 "Textiles & confección")
  high_cats <- c("Equipo de transporte",
                 "Maquinaria & eléctrica",
                 "Metales & manufacturas",
                 "Químicos & plásticos")
  
  df %>%
    mutate(
      valor_agregado = case_when(
        categoria %in% low_cats  ~ "Valor Agregado Bajo",
        categoria %in% mid_cats  ~ "Valor Agregado Medio",
        categoria %in% high_cats ~ "Valor Agregado Alto",
        TRUE                     ~ "Sin Clasificar"
      )
    ) %>%
    group_by(año, valor_agregado) %>%
    summarise(
      total_valor_usd = sum(.data[[valor_col]], na.rm = TRUE),
      .groups = "drop"
    )
}

# 2) Ahora sí: aplico a tus cuatro bases
us_exports_va    <- resumir_valor_agregado1(us_exports_cat_grp)
us_imports_va    <- resumir_valor_agregado1(us_imports_cat_grp)
china_exports_va <- resumir_valor_agregado1(china_exports_cat_grp)
china_imports_va <- resumir_valor_agregado1(china_imports_cat_grp)


# 1) Función para calcular porcentaje por año
calc_pct <- function(df, pct_name) {
  df %>%
    group_by(año) %>%
    mutate(
      !!pct_name := total_valor_usd / sum(total_valor_usd, na.rm = TRUE) * 100
    ) %>%
    ungroup() %>%
    select(año, valor_agregado, all_of(pct_name))
}

# 2) Aplicar a cada flujo

# 2.1 Exports de USA
us_exports_pct <- calc_pct(
  us_exports_va,
  "expo_pct_agregado"
)

# 2.2 Imports de USA
us_imports_pct <- calc_pct(
  us_imports_va,
  "impo_pct_agregado"
)

# 2.3 Exports de China
china_exports_pct <- calc_pct(
  china_exports_va,
  "expo_pct_agregado"
)

# 2.4 Imports de China
china_imports_pct <- calc_pct(
  china_imports_va,
  "impo_pct_agregado"
)

# 3) Verificación
glimpse(us_exports_pct)
glimpse(us_imports_pct)
glimpse(china_exports_pct)
glimpse(china_imports_pct)


# ================================================
#
# Paso 21: Bauti: Calculo de balanza comercial de all4.
#
# ================================================


# 2) Argentina: sumar por año y calcular saldo
argentina_trade_balance <- full_join(
  argentina_exports_va %>%
    group_by(año) %>%
    summarise(expo = sum(total_valor_usd, na.rm = TRUE)),
  
  argentina_imports_va %>%
    group_by(año) %>%
    summarise(impo = sum(total_valor_usd, na.rm = TRUE)),
  
  by = "año"
) %>%
  mutate(balance_usd = expo - impo)

# 3) Brasil
brazil_trade_balance <- full_join(
  brazil_exports_va %>%
    group_by(año) %>%
    summarise(expo = sum(total_valor_usd, na.rm = TRUE)),
  
  brazil_imports_va %>%
    group_by(año) %>%
    summarise(impo = sum(total_valor_usd, na.rm = TRUE)),
  
  by = "año"
) %>%
  mutate(balance_usd = expo - impo)

# 4) Chile
chile_trade_balance <- full_join(
  chile_exports_va %>%
    group_by(año) %>%
    summarise(expo = sum(total_valor_usd, na.rm = TRUE)),
  
  chile_imports_va %>%
    group_by(año) %>%
    summarise(impo = sum(total_valor_usd, na.rm = TRUE)),
  
  by = "año"
) %>%
  mutate(balance_usd = expo - impo)

# 5) México
mexico_trade_balance <- full_join(
  mexico_exports_va %>%
    group_by(año) %>%
    summarise(expo = sum(total_valor_usd, na.rm = TRUE)),
  
  mexico_imports_va %>%
    group_by(año) %>%
    summarise(impo = sum(total_valor_usd, na.rm = TRUE)),
  
  by = "año"
) %>%
  mutate(balance_usd = expo - impo)


# ================================================
#
# Paso 22: Bauti: Guardado de todos los CSV.
#
# ================================================

write.csv(sa_exports_yearly,
          file = file.path(input_dir, "sa_exports_yearly.csv"),
          row.names = FALSE)

write.csv(asia_exports_yearly,
          file = file.path(input_dir, "asia_exports_yearly.csv"),
          row.names = FALSE)

write.csv(sa_imports_yearly,
          file = file.path(input_dir, "sa_imports_yearly.csv"),
          row.names = FALSE)

write.csv(asia_imports_yearly,
          file = file.path(input_dir, "asia_imports_yearly.csv"),
          row.names = FALSE)

write.csv(sa_exports_pct,
          file = file.path(input_dir, "sa_exports_pct.csv"),
          row.names = FALSE)

write.csv(sa_imports_pct,
          file = file.path(input_dir, "sa_imports_pct.csv"),
          row.names = FALSE)

write.csv(asia_exports_pct,
          file = file.path(input_dir, "asia_exports_pct.csv"),
          row.names = FALSE)

write.csv(asia_imports_pct,
          file = file.path(input_dir, "asia_imports_pct.csv"),
          row.names = FALSE)

write.csv(sa_trade_balance,
          file = file.path(input_dir, "sa_trade_balance.csv"),
          row.names = FALSE)

write.csv(asia_trade_balance,
          file = file.path(input_dir, "asia_trade_balance.csv"),
          row.names = FALSE)

write.csv(all4_expo_pct,
          file = file.path(input_dir, "all4_expo_pct.csv"),
          row.names = FALSE)

write.csv(all4_impo_pct,
          file = file.path(input_dir, "all4_impo_pct.csv"),
          row.names = FALSE)

write.csv(argentina_exports_va_pct,
          file = file.path(input_dir, "argentina_exports_va_pct.csv"),
          row.names = FALSE)

write.csv(argentina_imports_va_pct,
          file = file.path(input_dir, "argentina_imports_va_pct.csv"),
          row.names = FALSE)

write.csv(brazil_exports_va_pct,
          file = file.path(input_dir, "brazil_exports_va_pct.csv"),
          row.names = FALSE)

write.csv(brazil_imports_va_pct,
          file = file.path(input_dir, "brazil_imports_va_pct.csv"),
          row.names = FALSE)

write.csv(chile_exports_va_pct,
          file = file.path(input_dir, "chile_exports_va_pct.csv"),
          row.names = FALSE)

write.csv(chile_imports_va_pct,
          file = file.path(input_dir, "chile_imports_va_pct.csv"),
          row.names = FALSE)

write.csv(mexico_exports_va_pct,
          file = file.path(input_dir, "mexico_exports_va_pct.csv"),
          row.names = FALSE)

write.csv(mexico_imports_va_pct,
          file = file.path(input_dir, "mexico_imports_va_pct.csv"),
          row.names = FALSE)

write.csv(chile_trade_balance,
          file = file.path(input_dir, "chile_trade_balance.csv"),
          row.names = FALSE)

write.csv(argentina_trade_balance,
          file = file.path(input_dir, "argentina_trade_balance.csv"),
          row.names = FALSE)

write.csv(brazil_trade_balance,
          file = file.path(input_dir, "brazil_trade_balance.csv"),
          row.names = FALSE)

write.csv(mexico_trade_balance,
          file = file.path(input_dir, "mexico_trade_balance.csv"),
          row.names = FALSE)

write.csv(china_exports_cat,
          file = file.path(input_dir, "china_exports_cat.csv"),
          row.names = FALSE)

write.csv(china_imports_cat,
          file = file.path(input_dir, "china_imports_cat.csv"),
          row.names = FALSE)

write.csv(us_exports_cat,
          file = file.path(input_dir, "us_exports_cat.csv"),
          row.names = FALSE)

write.csv(us_imports_cat,
          file = file.path(input_dir, "us_imports_cat.csv"),
          row.names = FALSE)
