
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
#
# GRAFICOS SECCION: Bauti: aca comienza el armado de los graficos.
#
# ================================================



# Carga de los CSV

# 2) Listar todos los archivos .csv dentro de “input”
csv_files <- list.files(
  path       = input_dir,
  pattern    = "\\.csv$",    # solo archivos que terminen en .csv
  full.names = TRUE          # retorna ruta completa
)

# 3) Leer cada CSV en una lista de data.frames
library(readr)               # o usa utils::read.csv
data_list <- lapply(csv_files, read_csv)

# 4) Poner nombres a cada elemento de la lista según el archivo
names(data_list) <- tools::file_path_sans_ext(basename(csv_files))

# Ahora `data_list` es algo así:
# $mi_archivo1    # data.frame leída de "mi_archivo1.csv"
# $otro_dataset   # data.frame leída de "otro_dataset.csv"
# ...

# 5) (Opcional) Asignar cada data.frame al entorno global con su nombre
for (nm in names(data_list)) {
  assign(nm, data_list[[nm]], envir = .GlobalEnv)
}

#..........................................................................................



# 1) Instalar y cargar paquetes
if (!require(ggplot2))    install.packages("ggplot2")
if (!require(dplyr))      install.packages("dplyr")
if (!require(showtext))   install.packages("showtext")
if (!require(scales)) install.packages("scales")
library(ggplot2)
library(dplyr)
library(showtext)
library(scales)


# 2) Registrar y activar Georgia
font_add("Georgia", "C:/Windows/Fonts/Georgia.ttf")
showtext_auto()


# Asia
asia_combined <- bind_rows(
  asia_imports_yearly  %>% mutate(tipo = "Importaciones"),
  asia_exports_yearly  %>% mutate(tipo = "Exportaciones")
)

# Sudamérica
sa_combined <- bind_rows(
  sa_imports_yearly    %>% mutate(tipo = "Importaciones"),
  sa_exports_yearly    %>% mutate(tipo = "Exportaciones")
)


# 3) Renombrar columna en tus data frames combinados
asia_combined <- asia_combined  %>% rename(millones_usd = monto_usd_total)
sa_combined   <- sa_combined    %>% rename(millones_usd = monto_usd_total)

# 4) Colores
asia_cols <- c("Exportaciones" = "#1b9e77", "Importaciones" = "#d95f02")
sa_cols   <- c("Exportaciones" = "#1b9e77", "Importaciones" = "#d95f02")

# 5) Función para ubicar etiqueta en ‘frac’ (0–1) del tramo
label_point <- function(df, frac = 0.5) {
  xran <- range(df$año)
  x0   <- xran[1] + frac * diff(xran)
  y0   <- approx(df$año, df$millones_usd, xout = x0)$y
  data.frame(año = x0, millones_usd = y0, tipo = unique(df$tipo))
}

# 6) Crear puntos de label para cada serie
asia_imp_lab <- label_point(filter(asia_combined, tipo == "Importaciones"), frac = 0.7)
asia_exp_lab <- label_point(filter(asia_combined, tipo == "Exportaciones"), frac = 0.3)

sa_imp_lab   <- label_point(filter(sa_combined, tipo == "Importaciones"), frac = 0.7)
sa_exp_lab   <- label_point(filter(sa_combined, tipo == "Exportaciones"), frac = 0.3)

# 7) Gráfico Asia con labels horizontales
asia_plot <- ggplot(asia_combined, 
                    aes(x = año, y = millones_usd, colour = tipo)) +
  geom_line(linewidth = 1.2) +
  geom_text(data    = asia_imp_lab,
            aes(x = año, y = millones_usd, label = tipo),
            angle   = 0,      # horizontal
            hjust   = 0.60,    # centrar
            vjust   = -3,   # ajustar vertical
            size    = 5,
            family  = "Georgia") +
  geom_text(data    = asia_exp_lab,
            aes(x = año, y = millones_usd, label = tipo),
            angle   = 0,
            hjust   = 1.0,
            vjust   =  -1.2,
            size    = 5,
            family  = "Georgia") +
  scale_colour_manual(values = asia_cols) +
  
  # -> aquí modificas la forma del eje Y
  scale_y_continuous(
    labels = comma     # 1,000,000 ; 500,000 ; etc.
    # labels = function(x) format(x, scientific = FALSE, big.mark = "") 
    # si prefieres sin separadores: 1000000, 500000
  ) +
  
  theme_light(base_family = "Georgia") +
  theme(
    legend.position = "none",
    plot.title      = element_text(size = 16, face = "bold"),
    axis.title      = element_text(size = 14),
    axis.text       = element_text(size = 12)
  ) +
  labs(
    title = "Asia: Importaciones vs Exportaciones",
    x     = "Año",
    y     = "Millones de USD"
  )


# 8) Gráfico Sudamérica con labels horizontales
sa_plot <- ggplot(sa_combined, 
                  aes(x = año, y = millones_usd, colour = tipo)) +
  geom_line(linewidth = 1.2) +
  geom_text(data    = sa_imp_lab,
            aes(x = año, y = millones_usd, label = tipo),
            angle   = 0,
            hjust   = -0.5,
            vjust   = 4.0,
            size    = 5,
            family  = "Georgia") +
  geom_text(data    = sa_exp_lab,
            aes(x = año, y = millones_usd, label = tipo),
            angle   = 0,
            hjust   = -0.2,
            vjust   =  -11.0,
            size    = 5,
            family  = "Georgia") +
  
  scale_colour_manual(values = sa_cols) +
  
  # -> aquí modificas la forma del eje Y
  scale_y_continuous(
    labels = comma     # 1,000,000 ; 500,000 ; etc.
    # labels = function(x) format(x, scientific = FALSE, big.mark = "") 
    # si prefieres sin separadores: 1000000, 500000
  ) +
  
  scale_colour_manual(values = sa_cols) +
  theme_light(base_family = "Georgia") +
  theme(
    legend.position = "none",
    plot.title      = element_text(size = 16, face = "bold"),
    axis.title      = element_text(size = 14),
    axis.text       = element_text(size = 12)
  ) +
  labs(
    title = "Sudamérica: Importaciones vs Exportaciones",
    x     = "Año",
    y     = "Millones de USD"
  )

# 9) Mostrar ambos gráficos
print(asia_plot)
print(sa_plot)


#---------------------------------------------------------
# Bauti: otros graficos, balanza comercial
#---------------------------------------------------------


# 1) Instalar y cargar paquetes necesarios
if (!require(ggplot2))    install.packages("ggplot2")
if (!require(dplyr))      install.packages("dplyr")
if (!require(showtext))   install.packages("showtext")
if (!require(scales))     install.packages("scales")

library(ggplot2)
library(dplyr)
library(showtext)
library(scales)

# 2) Registrar y activar la fuente Georgia
font_add("Georgia", "C:/Windows/Fonts/Georgia.ttf")
showtext_auto()

# 3) Preparar datos: crear columna 'sign' para condicionar color
asia_trade_balance <- asia_trade_balance %>%
  mutate(sign = ifelse(balance_usd >= 0, "Positivo", "Negativo"))

sa_trade_balance <- sa_trade_balance %>%
  mutate(sign = ifelse(balance_usd >= 0, "Positivo", "Negativo"))

# 4) Definir paleta de colores
cols_balance <- c("Positivo" = "#32cd32", "Negativo" = "#e03c31")

# 5) Función para crear bar chart con dos dígitos en X y puntos en Y
make_bar_balance <- function(df, region_title) {
  ggplot(df, aes(x = factor(año), y = balance_usd, fill = sign)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    scale_fill_manual(values = cols_balance) +
    
    # X-axis: mostrar solo últimos dos dígitos
    scale_x_discrete(
      labels = function(x) sprintf("%02d", as.integer(substr(x, 3, 4)))
    ) +
    
    # Y-axis: números con separador de miles como punto
    scale_y_continuous(
      labels = label_number(
        big.mark    = ",",
        decimal.mark= ".",
        scale       = 1
      )
    ) +
    #  ⇒ Escala Y fija de -250,000 a 250,000
    coord_cartesian(ylim = c(-250000, 250000)) +
    
    labs(
      title = paste(region_title, "– Balanza Comercial"),
      x     = "Año (’YY)",
      y     = "Saldo Comercial (USD)"
    ) +
    theme_light(base_family = "Georgia") +
    theme(
      plot.title   = element_text(size = 16, face = "bold"),
      axis.title   = element_text(size = 14),
      axis.text    = element_text(size = 12),
      panel.border = element_blank(),
      axis.line.x  = element_line(),
      axis.line.y  = element_line()
    )
}

# 6) Generar los dos gráficos
asia_balance_bar <- make_bar_balance(asia_trade_balance, "Asia")
sa_balance_bar   <- make_bar_balance(sa_trade_balance,   "Sudamérica")

# 7) Mostrar ambos plots
print(asia_balance_bar)
print(sa_balance_bar)


#---------------------------------------------------------
# Bauti: otros graficos, evolucion por tres categorias
#---------------------------------------------------------


# 1) Instalar y cargar paquetes
if (!require(ggplot2))  install.packages("ggplot2")
if (!require(dplyr))    install.packages("dplyr")
if (!require(showtext)) install.packages("showtext")
if (!require(scales))   install.packages("scales")

library(ggplot2)
library(dplyr)
library(showtext)
library(scales)


# 2) Fuente Georgia
font_add("Georgia", "C:/Windows/Fonts/Georgia.ttf")
showtext_auto()

# 3) Paletas de color
pal_fill <- c(
  "Valor Agregado Bajo"  = "#98ffdb",
  "Valor Agregado Medio" = "#ae1aff",
  "Valor Agregado Alto"  = "#ffb122"
)
pal_line <- c(
  "Valor Agregado Bajo"  = "#00e695",
  "Valor Agregado Medio" = "#9500e6",
  "Valor Agregado Alto"  = "#e69500"
)

# 4) Función genérica: bandas sin solaparse + puntos y etiquetas inicial/final
make_no_overlap_area_with_labels <- function(df, region, flow) 
  
  # 4.1) Normaliza y factoriza
  df2 <- df %>%
  filter(!is.na(valor_agregado), !is.na(pct_valor)) %>%
  mutate(
    pct = pct_valor / 1,
    valor_agregado = factor(valor_agregado,
                            levels = c("Valor Agregado Bajo",
                                       "Valor Agregado Medio",
                                       "Valor Agregado Alto"))
  )



# 5) Genera y muestra los 4 gráficos
asia_exp_plot <- make_no_overlap_area_with_labels(
  asia_exports_pct, "Asia", "Exportaciones")
asia_imp_plot <- make_no_overlap_area_with_labels(
  asia_imports_pct, "Asia", "Importaciones")
sa_exp_plot   <- make_no_overlap_area_with_labels(
  sa_exports_pct, "Sudamérica", "Exportaciones")
sa_imp_plot   <- make_no_overlap_area_with_labels(
  sa_imports_pct, "Sudamérica", "Importaciones")

print(asia_exp_plot)
print(asia_imp_plot)
print(sa_exp_plot)
print(sa_imp_plot)


library(ggplot2)
library(dplyr)
library(showtext)
library(scales)
library(zoo)

font_add("Georgia", "C:/Windows/Fonts/Georgia.ttf")
showtext_auto()


make_smooth_area <- function(df, region, flow,
                             method = c("rollmean","loess"),
                             window = 5, span = 0.5) {
  method <- match.arg(method)}


# 4) Función con suavizado y puntos/etiquetas en extremos
make_smooth_area_with_extremes <- function(df, region, flow,
                                           method = c("rollmean","loess"),
                                           window = 5, span = 0.5) {
  method <- match.arg(method)
  
  # 4.1) normalizo y factorizo
  df2 <- df %>%
    filter(!is.na(valor_agregado), !is.na(pct_valor)) %>%
    transmute(
      año,
      valor_agregado = factor(valor_agregado,
                              levels = c("Valor Agregado Bajo",
                                         "Valor Agregado Medio",
                                         "Valor Agregado Alto")
      ),
      pct = pct_valor / 100
    )
  
  # 4.2) suavizo dentro de cada categoría
  df_smooth <- df2 %>%
    group_by(valor_agregado) %>%
    group_modify(~ {
      if (method == "rollmean") {
        tibble::tibble(.x,
                       pct_smooth = zoo::rollmean(.x$pct, k = window,
                                                  fill = NA, align = "center"))
      } else {
        fit <- loess(pct ~ año, data = .x, span = span)
        tibble::tibble(.x,
                       pct_smooth = predict(fit, newdata = .x))
      }
    }) %>%
    ungroup() %>%
    mutate(pct_smooth = if_else(is.na(pct_smooth), pct, pct_smooth))
  
  # 4.3) construyo bandas sin solaparse
  band <- df_smooth %>%
    group_by(año) %>%
    arrange(pct_smooth) %>%
    mutate(
      ymin = lag(pct_smooth, default = 0),
      ymax = pct_smooth
    ) %>%
    ungroup()
  
  # 4.4) preparo datos de extremos para puntos y etiquetas
  years     <- range(df_smooth$año)
  labels_df <- df_smooth %>%
    filter(año %in% years) %>%
    mutate(
      pct_lbl = scales::label_percent(accuracy = 1)(pct_smooth),
      hjust   = ifelse(año == years[1], 1.1, -0.1),
      vjust   = -0.5
    )
  
  # 4.5) dibujo todo
  ggplot(band, aes(x = año)) +
    # bandas suavizadas
    geom_ribbon(aes(ymin = ymin, ymax = ymax,
                    fill   = valor_agregado,
                    colour = valor_agregado),
                size  = 0.3, alpha = 0.8) +
    scale_fill_manual(values   = pal_fill, drop = FALSE) +
    scale_colour_manual(values = pal_line, drop = FALSE) +
    
    # puntos en inicial y final
    geom_point(data = labels_df,
               aes(x = año, y = pct_smooth, colour = valor_agregado),
               size = 3) +
    
    # etiquetas de porcentaje
    geom_text(data = labels_df,
              aes(x = año, y = pct_smooth, label = pct_lbl,
                  colour = valor_agregado, hjust = hjust, vjust = vjust),
              family = "Georgia", size = 4, show.legend = FALSE) +
    
    # ejes
    scale_x_continuous(
      breaks = unique(band$año),
      labels = function(x) sprintf("%02d", x %% 100)
    ) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      expand = c(0, 0)
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    
    labs(
      title = paste(region, "-", flow),
      x     = "Año (’YY)",
      y     = "Porcentaje del Total"
    ) +
    theme_light(base_family = "Georgia") +
    theme(
      legend.position = "bottom",
      legend.title    = element_blank(),
      plot.title      = element_text(size = 16, face = "bold"),
      axis.title      = element_text(size = 14),
      axis.text       = element_text(size = 12)
    )
}

# 5) Ejemplo de uso
p1 <- make_smooth_area_with_extremes(
  asia_exports_pct, "Asia", "Exportaciones",
  method = "rollmean", window = 3
)
p2 <- make_smooth_area_with_extremes(
  asia_imports_pct, "Asia", "Importaciones",
  method = "loess", span = 0.4
)


# 6) Repetir para Sudamérica

# Exportaciones (suavizado rollmean)
p3 <- make_smooth_area_with_extremes(
  sa_exports_pct, "Sudamérica", "Exportaciones",
  method = "rollmean", window = 3
)

# Importaciones (suavizado LOESS)
p4 <- make_smooth_area_with_extremes(
  sa_imports_pct, "Sudamérica", "Importaciones",
  method = "loess", span = 0.4
)

# 7) Mostrar todos los plots
print(p1)  # Asia – Exportaciones
print(p2)  # Asia – Importaciones
print(p3)  # Sudamérica – Exportaciones
print(p4)  # Sudamérica – Importaciones


#---------------------------------------------------------
# Bauti: otros graficos, all4 sobre pbi
#---------------------------------------------------------


# 1) Instala y carga paquetes
for (pkg in c("ggplot2","dplyr","showtext","scales","zoo")) {
  if (!require(pkg, character.only=TRUE))
    install.packages(pkg, repos="https://cloud.r-project.org")
  library(pkg, character.only=TRUE)
}

# 2) Registra y activa la fuente Georgia
font_add("Georgia", "C:/Windows/Fonts/Georgia.ttf")
showtext_auto()

# 3) Extractor de punto en frac (0–1) de la curva
label_point_pct <- function(df, frac=0.5) {
  xran <- range(df$año)
  x0   <- xran[1] + frac*diff(xran)
  y0   <- approx(df$año, df$pct, xout=x0)$y
  data.frame(año=x0, pct=y0, tipo=unique(df$tipo))
}

# 4) Función final: líneas, labels de curva, y marcadores + % en inicio/fin
make_exim_plot_v3 <- function(expo_df, impo_df,
                              smooth       = FALSE,
                              method       = c("rollmean","loess"),
                              window       = 5,
                              span         = 0.5,
                              label_export = "Exportaciones",
                              label_import = "Importaciones",
                              frac_export  = 0.3,
                              frac_import  = 0.7,
                              hjust_exp    = 0,
                              vjust_exp    = 1.2,
                              hjust_imp    = 1,
                              vjust_imp    = -0.5) {
  method <- match.arg(method)
  
  #–– 4.1) Combinar y normalizar
  expo <- expo_df %>% transmute(año, tipo="Exportaciones", pct0=expo_pct/100)
  impo <- impo_df %>% transmute(año, tipo="Importaciones", pct0=impo_pct/100)
  df   <- bind_rows(expo, impo) %>% arrange(tipo,año) %>%
    mutate(tipo=factor(tipo, c("Exportaciones","Importaciones")))
  
  #–– 4.2) Suavizado opcional
  if (smooth) {
    df <- df %>% group_by(tipo) %>%
      group_modify(~ {
        if (method=="rollmean") {
          tibble::tibble(.x,
                         pct=zoo::rollmean(.x$pct0, k=window, fill=NA, align="center"))
        } else {
          fit <- loess(pct0 ~ año, data=.x, span=span)
          tibble::tibble(.x, pct=predict(fit, newdata=.x))
        }
      }) %>% ungroup() %>%
      mutate(pct=if_else(is.na(pct), pct0, pct))
  } else {
    df <- df %>% rename(pct=pct0)
  }
  
  #–– 4.3) Labels de línea en frac_export/frac_import
  lbl_exp <- label_point_pct(filter(df, tipo=="Exportaciones"), frac=frac_export)
  lbl_imp <- label_point_pct(filter(df, tipo=="Importaciones"), frac=frac_import)
  
  #–– 4.4) Puntos + % labels de inicio y fin
  init_df <- df %>% group_by(tipo) %>% filter(año==min(año)) %>% ungroup() %>%
    transmute(año,pct,tipo,
              pct_lbl=scales::label_percent(1)(pct),
              hjust=0, vjust=-0.5)
  fin_df  <- df %>% group_by(tipo) %>% filter(año==max(año)) %>% ungroup() %>%
    transmute(año,pct,tipo,
              pct_lbl=scales::label_percent(1)(pct),
              hjust=-0.1, vjust=-0.5)
  
  #–– 4.5) Construir el ggplot
  ggplot(df, aes(x=año,y=pct,colour=tipo)) +
    geom_line(size=1.2) +
    
    # marcadores y %labels de inicio/fin
    geom_point(data=bind_rows(init_df,fin_df), aes(y=pct), size=3) +
    geom_text(data=bind_rows(init_df,fin_df),
              aes(label=pct_lbl,hjust=hjust,vjust=vjust),
              family="Georgia",size=3.5,show.legend=FALSE) +
    
    # etiquetas de línea en la curva
    geom_text(data=lbl_exp, aes(label=label_export),
              hjust=hjust_exp, vjust=vjust_exp,
              colour="#00008b", family="Georgia", size=5,
              show.legend=FALSE) +
    geom_text(data=lbl_imp, aes(label=label_import),
              hjust=hjust_imp, vjust=vjust_imp,
              colour="#8b0000", family="Georgia", size=5,
              show.legend=FALSE) +
    
    # escalas y tema
    scale_colour_manual(values=c("Exportaciones"="#00008b","Importaciones"="#8b0000")) +
    scale_x_continuous(
      breaks=unique(df$año),
      labels=function(x) sprintf("%02d", x%%100),
      expand=expansion(add=c(0,0.1))
    ) +
    scale_y_continuous(
      labels=scales::label_percent(1),
      expand=c(0,0)
    ) +
    coord_cartesian(ylim=c(0,0.30), clip="off") +
    labs(
      title="Exportaciones vs Importaciones",
      x="Año (’YY)",
      y="Porcentaje",
      colour=NULL
    ) +
    theme_light(base_family="Georgia") +
    theme(
      legend.position="none",
      plot.title=element_text(size=16,face="bold"),
      axis.title=element_text(size=14),
      axis.text=element_text(size=12),
      plot.margin=margin(5,25,5,5,"pt")
    )
}

# 5) Ejemplo de uso
p <- make_exim_plot_v3(
  all4_expo_pct, all4_impo_pct,
  smooth        = TRUE,
  method        = "loess",
  span          = 0.4,
  label_export  = "Exportaciones",
  label_import  = "Importaciones",
  frac_export   = 0.25,
  frac_import   = 0.75,
  hjust_exp     = 0,
  vjust_exp     = -3.5,
  hjust_imp     = 1,
  vjust_imp     = 3
)
print(p)


#---------------------------------------------------------
# Bauti: otros graficos, categoria de valor agregado para cada uno del ALL4.
#---------------------------------------------------------


# 1) Instalar y cargar paquetes
for (pkg in c("ggplot2","dplyr","showtext","scales","zoo")) {
  if (!require(pkg, character.only=TRUE))
    install.packages(pkg, repos="https://cloud.r-project.org")
  library(pkg, character.only=TRUE)
}

# 2) Registrar y activar la fuente Georgia
font_add("Georgia", "C:/Windows/Fonts/Georgia.ttf")
showtext_auto()

# 3) Paletas de color
pal_fill <- c(
  "Valor Agregado Bajo"  = "#98ffdb",
  "Valor Agregado Medio" = "#ae1aff",
  "Valor Agregado Alto"  = "#ffb122"
)
pal_line <- c(
  "Valor Agregado Bajo"  = "#00e695",
  "Valor Agregado Medio" = "#9500e6",
  "Valor Agregado Alto"  = "#e69500"
)

# 4) Función con padding en X y nudge_x en labels
make_smooth_area_with_labels <- function(df, region, flow,
                                         method = c("rollmean","loess"),
                                         window = 5, span = 0.5) {
  method <- match.arg(method)
  
  # 4.1) Preparo datos
  df2 <- df %>%
    filter(!is.na(valor_agregado), !is.na(pct_valor)) %>%
    transmute(
      año,
      valor_agregado = factor(valor_agregado,
                              levels = c("Valor Agregado Bajo","Valor Agregado Medio","Valor Agregado Alto")),
      pct0 = pct_valor / 100
    )
  
  # 4.2) Suavizado opcional
  df_smooth <- df2 %>%
    group_by(valor_agregado) %>%
    group_modify(~ {
      if (method=="rollmean") {
        tibble::tibble(.x,
                       pct = zoo::rollmean(.x$pct0, k=window, fill=NA, align="center"))
      } else {
        fit <- loess(pct0 ~ año, data=.x, span=span)
        tibble::tibble(.x, pct = predict(fit, newdata=.x))
      }
    }) %>% ungroup() %>%
    mutate(pct = if_else(is.na(pct), pct0, pct))
  
  # 4.3) Bandas sin solaparse
  band <- df_smooth %>%
    group_by(año) %>%
    arrange(pct) %>%
    mutate(ymin = lag(pct, default=0), ymax = pct) %>%
    ungroup()
  
  # 4.4) Puntos + etiquetas en inicio y fin
  pts <- df_smooth %>%
    group_by(valor_agregado) %>%
    filter(año %in% c(min(año), max(año))) %>%
    ungroup() %>%
    mutate(
      pct_lbl = scales::label_percent(1)(pct),
      hjust   = ifelse(año==min(año), 1.1, -0.1),
      vjust   = -1,
      nudge_x = ifelse(año==min(año),  0.5, -0.5)
    )
  
  # 4.5) Dibujar con padding y nudge_x
  ggplot(band, aes(x=año)) +
    geom_ribbon(aes(ymin=ymin, ymax=ymax, fill=valor_agregado, colour=valor_agregado),
                size=0.3, alpha=0.8) +
    scale_fill_manual(values=pal_fill,   drop=FALSE) +
    scale_colour_manual(values=pal_line, drop=FALSE) +
    geom_point(data=pts, aes(x=año, y=pct, colour=valor_agregado), size=3) +
    geom_text(data=pts,
              aes(x=año, y=pct, label=pct_lbl, colour=valor_agregado,
                  hjust=hjust, vjust=vjust),
              nudge_x     = pts$nudge_x,
              family      = "Georgia",
              size        = 4,
              show.legend = FALSE) +
    scale_x_continuous(
      breaks = unique(band$año),
      labels = function(x) sprintf("%02d", x%%100),
      expand = expansion(add = c(1, 1))   # ← padding al principio y al final
    ) +
    scale_y_continuous(
      labels = scales::label_percent(1),
      expand = c(0, 0)
    ) +
    coord_cartesian(ylim=c(0,1), clip="off") +
    labs(
      title = paste(region, "-", flow),
      x     = "Año (’YY)",
      y     = "Porcentaje del Total"
    ) +
    theme_light(base_family="Georgia") +
    theme(
      legend.position = "bottom",
      legend.title    = element_blank(),
      plot.title      = element_text(size=16, face="bold"),
      axis.title      = element_text(size=14),
      axis.text       = element_text(size=12),
      plot.margin     = margin(5,5,5,5,"pt")
    )
}

# 5) Primer test: Argentina – Exportaciones
arg_exp_plot <- make_smooth_area_with_labels(
  argentina_exports_va_pct, "Argentina", "Exportaciones",
  method="loess", span=0.3
)
print(arg_exp_plot)


# 6) Argentina – Importaciones
arg_imp_plot <- make_smooth_area_with_labels(
  argentina_imports_va_pct, "Argentina", "Importaciones",
  method = "loess", span = 0.3
)

# 7) Brasil – Exportaciones
bra_exp_plot <- make_smooth_area_with_labels(
  brazil_exports_va_pct, "Brasil", "Exportaciones",
  method = "loess", span = 0.3
)

# 8) Brasil – Importaciones
bra_imp_plot <- make_smooth_area_with_labels(
  brazil_imports_va_pct, "Brasil", "Importaciones",
  method = "loess", span = 0.3
)

# 9) Chile – Exportaciones
chl_exp_plot <- make_smooth_area_with_labels(
  chile_exports_va_pct, "Chile", "Exportaciones",
  method = "loess", span = 0.3
)

# 10) Chile – Importaciones
chl_imp_plot <- make_smooth_area_with_labels(
  chile_imports_va_pct, "Chile", "Importaciones",
  method = "loess", span = 0.3
)

# 11) México – Exportaciones
mex_exp_plot <- make_smooth_area_with_labels(
  mexico_exports_va_pct, "México", "Exportaciones",
  method = "loess", span = 0.3
)

# 12) México – Importaciones
mex_imp_plot <- make_smooth_area_with_labels(
  mexico_imports_va_pct, "México", "Importaciones",
  method = "loess", span = 0.3
)

# 13) Mostrar todos (ejemplo uno por uno)
print(arg_imp_plot)
print(bra_exp_plot)
print(bra_imp_plot)
print(chl_exp_plot)
print(chl_imp_plot)
print(mex_exp_plot)
print(mex_imp_plot)


#---------------------------------------------------------
# Bauti: otros graficos, balanza comercial de cada uno de all4
#---------------------------------------------------------


# 1) Cargar dplyr si no está activo
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

# # 2) Argentina: sumar por año y calcular saldo
#   argentina_trade_balance <- full_join(
#   argentina_exports_va %>%
#     group_by(año) %>%
#     summarise(expo = sum(total_valor_usd, na.rm = TRUE)),
# 
#   argentina_imports_va %>%
#     group_by(año) %>%
#     summarise(impo = sum(total_valor_usd, na.rm = TRUE)),
# 
#   by = "año"
# ) %>%
#   mutate(balance_usd = expo - impo)
# 
# # 3) Brasil
# brazil_trade_balance <- full_join(
#   brazil_exports_va %>%
#     group_by(año) %>%
#     summarise(expo = sum(total_valor_usd, na.rm = TRUE)),
# 
#   brazil_imports_va %>%
#     group_by(año) %>%
#     summarise(impo = sum(total_valor_usd, na.rm = TRUE)),
# 
#   by = "año"
# ) %>%
#   mutate(balance_usd = expo - impo)
# 
# # 4) Chile
# chile_trade_balance <- full_join(
#   chile_exports_va %>%
#     group_by(año) %>%
#     summarise(expo = sum(total_valor_usd, na.rm = TRUE)),
# 
#   chile_imports_va %>%
#     group_by(año) %>%
#     summarise(impo = sum(total_valor_usd, na.rm = TRUE)),
# 
#   by = "año"
# ) %>%
#   mutate(balance_usd = expo - impo)
# 
# # 5) México
# mexico_trade_balance <- full_join(
#   mexico_exports_va %>%
#     group_by(año) %>%
#     summarise(expo = sum(total_valor_usd, na.rm = TRUE)),
# 
#   mexico_imports_va %>%
#     group_by(año) %>%
#     summarise(impo = sum(total_valor_usd, na.rm = TRUE)),
# 
#   by = "año"
# ) %>%
#   mutate(balance_usd = expo - impo)


# 1) Paleta para signo de saldo
cols_balance <- c("Positivo" = "#32cd32", "Negativo" = "#e03c31")

# 2) Función mejorada con escala automática por país
make_bar_balance <- function(df, region_title, margin_pct = 0.1) {
  df <- df %>%
    mutate(sign = ifelse(balance_usd >= 0, "Positivo", "Negativo"))
  
  # 2.1) Calcular límites del eje Y con margen adicional
  max_val <- max(abs(df$balance_usd), na.rm = TRUE)
  ylim_max <- ceiling(max_val * (1 + margin_pct))
  ylim_vals <- c(-ylim_max, ylim_max)
  
  # 2.2) Graficar
  ggplot(df, aes(x = factor(año), y = balance_usd, fill = sign)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    scale_fill_manual(values = cols_balance) +
    scale_x_discrete(
      labels = function(x) sprintf("%02d", as.integer(substr(x, 3, 4)))
    ) +
    scale_y_continuous(
      labels = label_number(big.mark = ".", decimal.mark = ",")
    ) +
    coord_cartesian(ylim = ylim_vals) +
    labs(
      title = paste(region_title, "– Balanza Comercial"),
      x     = "Año (’YY)",
      y     = "Saldo Comercial (USD)"
    ) +
    theme_light(base_family = "Georgia") +
    theme(
      plot.title   = element_text(size = 16, face = "bold"),
      axis.title   = element_text(size = 14),
      axis.text    = element_text(size = 12),
      panel.border = element_blank(),
      axis.line.x  = element_line(),
      axis.line.y  = element_line()
    )
}

argentina_bar <- make_bar_balance(argentina_trade_balance, "Argentina")
brazil_bar    <- make_bar_balance(brazil_trade_balance,    "Brasil")
chile_bar     <- make_bar_balance(chile_trade_balance,     "Chile")
mexico_bar    <- make_bar_balance(mexico_trade_balance,    "México")

print(argentina_bar)
print(brazil_bar)
print(chile_bar)
print(mexico_bar)


#---------------------------------------------------------
# Bauti: otros graficos, socios comerciales por categoria
#---------------------------------------------------------


# 1) Cargar paquetes necesarios
for (pkg in c("dplyr", "ggplot2", "treemapify", "showtext")) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

# 2) Activar fuente Georgia
font_add("Georgia", "C:/Windows/Fonts/Georgia.ttf")
showtext_auto()

# 3) Definición de niveles de valor agregado y paleta principal
low_cats  <- c("Agroalimentario", "Minerales & combustibles", "Madera, papel & muebles")
mid_cats  <- c("Otras manufacturas", "Otros / no clasificado", "Textiles & confección")
high_cats <- c("Equipo de transporte", "Maquinaria & eléctrica",
               "Metales & manufacturas", "Químicos & plásticos")

pal_main <- c(
  "Valor Agregado Bajo"  = "#89e4cb",
  "Valor Agregado Medio" = "#c586ff",
  "Valor Agregado Alto"  = "#ffc266"
)

# 4) Prepara los datos jerárquicos y agrupa subcategorías <5%
prepare_treemap_data <- function(df, threshold = 0.05) {
  df %>%
    group_by(categoria) %>%
    summarise(promedio = mean(valor_millones_usd, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      nivel_va = case_when(
        categoria %in% low_cats  ~ "Valor Agregado Bajo",
        categoria %in% mid_cats  ~ "Valor Agregado Medio",
        categoria %in% high_cats ~ "Valor Agregado Alto",
        TRUE ~ "No clasificado"
      )
    ) %>%
    group_by(nivel_va) %>%
    mutate(total_va = sum(promedio)) %>%
    ungroup() %>%
    mutate(
      pct_total  = promedio / sum(promedio),
      pct_dentro = promedio / total_va,
      subcat     = if_else(pct_dentro < threshold, "Otros", categoria),
      label_sub  = paste0(subcat, "\n", scales::percent(pct_total, accuracy = 1))
    ) %>%
    group_by(nivel_va, subcat) %>%
    summarise(
      pct_total = sum(pct_total),
      label_sub = first(label_sub),
      .groups   = "drop"
    )
}

# 5) Función para treemap jerárquico con límite superior en size_sub
make_hierarchical_treemap <- function(df, title_text,
                                      size_sub  = 1,   # ← máximo tamaño para subetiquetas
                                      size_main = -6) {   # ← tamaño de categorías principales
  ggplot(df, aes(
    area     = pct_total,
    fill     = nivel_va,
    subgroup = nivel_va,
    label    = label_sub
  )) +
    geom_treemap(color = "white") +
    geom_treemap_subgroup_border(color = "white", size = 2) +
    
    # Títulos de nivel principal (arriba)
    geom_treemap_subgroup_text(
      place    = "top",
      grow     = TRUE,
      reflow   = TRUE,
      family   = "Georgia",
      colour   = "black",
      fontface = "bold",
      size     = size_main
    ) +
    
    # Subcategorías centradas con límite superior en size_sub
    geom_treemap_text(
      aes(label = label_sub),
      place    = "centre",
      grow     = TRUE,
      reflow   = TRUE,
      family   = "Georgia",
      colour   = "black",
      size     = size_sub
    ) +
    
    scale_fill_manual(values = pal_main) +
    labs(title = title_text) +
    theme_void(base_family = "Georgia") +
    theme(
      plot.title      = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "none"
    )
}

# 6) Preparar data para China y EE. UU.
china_exp_data <- prepare_treemap_data(china_exports_cat)
china_imp_data <- prepare_treemap_data(china_imports_cat)
us_exp_data    <- prepare_treemap_data(us_exports_cat)
us_imp_data    <- prepare_treemap_data(us_imports_cat)

# 7) Crear y mostrar gráficos
treemap_china_exp <- make_hierarchical_treemap(china_exp_data, "China – Exportaciones")
treemap_china_imp <- make_hierarchical_treemap(china_imp_data, "China – Importaciones")
treemap_us_exp    <- make_hierarchical_treemap(us_exp_data,    "EE.UU. – Exportaciones")
treemap_us_imp    <- make_hierarchical_treemap(us_imp_data,    "EE.UU. – Importaciones")

print(treemap_china_exp)
print(treemap_china_imp)
print(treemap_us_exp)
print(treemap_us_imp)


#---------------------------------------------------------
# Bauti: otros graficos, grafico de barraws socios comerciales por categoria
#---------------------------------------------------------


# 1) Paquetes y fuente
library(dplyr)
library(ggplot2)
library(showtext)

font_add("Georgia", "C:/Windows/Fonts/Georgia.ttf")
showtext_auto()

# 2) Definir niveles y paleta
low_cats  <- c("Agroalimentario", "Minerales & combustibles", "Madera, papel & muebles")
mid_cats  <- c("Otras manufacturas", "Otros / no clasificado", "Textiles & confección")
high_cats <- c("Equipo de transporte", "Maquinaria & eléctrica",
               "Metales & manufacturas", "Químicos & plásticos")

pal_main <- c(
  "Valor Agregado Bajo"  = "#98ffdb",
  "Valor Agregado Medio" = "#ae1aff",
  "Valor Agregado Alto"  = "#ffb122"
)


# 3) Preparar datos para barras (igual que antes)
prepare_bar_data <- function(df, threshold = 0.05) {
  df %>%
    group_by(categoria) %>%
    summarise(promedio = mean(valor_millones_usd, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      nivel_va = case_when(
        categoria %in% low_cats  ~ "Valor Agregado Bajo",
        categoria %in% mid_cats  ~ "Valor Agregado Medio",
        categoria %in% high_cats ~ "Valor Agregado Alto",
        TRUE                     ~ "No clasificado"
      )
    ) %>%
    group_by(nivel_va) %>%
    mutate(total_va = sum(promedio)) %>%
    ungroup() %>%
    mutate(
      pct_total  = promedio / sum(promedio),
      pct_dentro = promedio / total_va,
      subcat     = if_else(pct_dentro < threshold, "Otros", categoria)
    ) %>%
    group_by(nivel_va, subcat) %>%
    summarise(
      pct_total = sum(pct_total),
      .groups   = "drop"
    ) %>%
    arrange(nivel_va, desc(pct_total))
}

# 4) Función actualizada: barras horizontales + etiquetas porcentuales
make_horizontal_bar <- function(df, title_text) {
  ggplot(df, aes(
    x    = reorder(subcat, pct_total),
    y    = pct_total,
    fill = nivel_va
  )) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(
      aes(label = scales::percent(pct_total, accuracy = 1)),
      hjust = -0.1,
      size  = 3.5,
      family = "Georgia"
    ) +
    coord_flip() +
    scale_fill_manual(values = pal_main) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      title = title_text,
      x     = "Subcategoría",
      y     = "Participación (%)"
    ) +
    theme_light(base_family = "Georgia") +
    theme(
      plot.title       = element_text(size = 16, face = "bold"),
      axis.title       = element_text(size = 14),
      axis.text        = element_text(size = 12),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank()
    )
}

# 5) Generar los datos y los gráficos
china_exp_bar <- prepare_bar_data(china_exports_cat)
china_imp_bar <- prepare_bar_data(china_imports_cat)
us_exp_bar    <- prepare_bar_data(us_exports_cat)
us_imp_bar    <- prepare_bar_data(us_imports_cat)

bar_china_exp <- make_horizontal_bar(china_exp_bar, "China – Exportaciones")
bar_china_imp <- make_horizontal_bar(china_imp_bar, "China – Importaciones")
bar_us_exp    <- make_horizontal_bar(us_exp_bar,    "EE.UU. – Exportaciones")
bar_us_imp    <- make_horizontal_bar(us_imp_bar,    "EE.UU. – Importaciones")

print(bar_china_exp)
print(bar_china_imp)
print(bar_us_exp)
print(bar_us_imp)


#---------------------------------------------------------
# Bauti: guardado de los plots.
#---------------------------------------------------------




# 2) Guardar asia_plot
ggsave(
  filename = file.path("output", "asia_gdp.png"),
  plot     = asia_plot,
  width    = 8,      # pulgadas
  height   = 6,      # pulgadas
  units    = "in",
  dpi      = 600     # máxima definición
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "sa_gdp.png"),
  plot     = sa_plot,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

#----


# 2) Guardar asia_plot
ggsave(
  filename = file.path("output", "asia_balanza_comercial.png"),
  plot     = asia_balance_bar,
  width    = 8,      # pulgadas
  height   = 6,      # pulgadas
  units    = "in",
  dpi      = 600     # máxima definición
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "sa_balanza_comercial.png"),
  plot     = sa_balance_bar,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 2) Guardar asia_plot
ggsave(
  filename = file.path("output", "asia_exportaciones_pct.png"),
  plot     = p1,
  width    = 8,      # pulgadas
  height   = 6,      # pulgadas
  units    = "in",
  dpi      = 600     # máxima definición
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "asia_importaciones_pct.png"),
  plot     = p2,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 2) Guardar asia_plot
ggsave(
  filename = file.path("output", "suda_exportaciones_pct.png"),
  plot     = p3,
  width    = 8,      # pulgadas
  height   = 6,      # pulgadas
  units    = "in",
  dpi      = 600     # máxima definición
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "suda_importaciones_pct.png"),
  plot     = p4,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)


# 2) Guardar asia_plot
ggsave(
  filename = file.path("output", "all4_sobre_pbi.png"),
  plot     = p,
  width    = 8,      # pulgadas
  height   = 6,      # pulgadas
  units    = "in",
  dpi      = 600     # máxima definición
)

#-----


# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "argentina_exportaciones_pct.png"),
  plot     = arg_exp_plot,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "argentina_importaciones_pct.png"),
  plot     = arg_imp_plot,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "brasil_exportaciones_pct.png"),
  plot     = bra_exp_plot,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "brasil_importaciones_pct.png"),
  plot     = bra_imp_plot,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "chile_exportaciones_pct.png"),
  plot     = chl_exp_plot,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "chile_importaciones_pct.png"),
  plot     = chl_imp_plot,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "mexico_exportaciones_pct.png"),
  plot     = mex_exp_plot,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "mexico_importaciones_pct.png"),
  plot     = mex_imp_plot,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

#--


# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "argentina_balanza_comercial.png"),
  plot     = argentina_bar,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "brasil_balanza_comercial.png"),
  plot     = brazil_bar,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "chile_balanza_comercial.png"),
  plot     = chile_bar,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "mexico_balanza_comercial.png"),
  plot     = mexico_bar,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

#--

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "china_exportaciones_pct.png"),
  plot     = bar_china_exp,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "china_importaciones_pct.png"),
  plot     = bar_china_imp,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "usa_exportaciones_pct.png"),
  plot     = bar_us_exp,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)

# 3) Guardar sa_plot
ggsave(
  filename = file.path("output", "usa_importaciones_pct.png"),
  plot     = bar_us_imp,
  width    = 8,      
  height   = 6,      
  units    = "in",
  dpi      = 600     
)