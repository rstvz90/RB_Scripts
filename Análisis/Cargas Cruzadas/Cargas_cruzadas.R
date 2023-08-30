library(data.table)
library(readxl)
library(skimr)
library(tidyverse)
library(scales)
library(ggplot2)

rm(list = ls())
gc()

path <- "C:/Users/REstevez/Documents/HISTORICOS/"

files <- list.files(path, full.names = TRUE)

# CHASIS -----------

tipo_chasis <- read_xlsx("C:/Users/REstevez/Documents/HISTORICOS/Tipos de chasis.xlsx")

setDT(tipo_chasis)

tipo_chasis <- tipo_chasis[, c("Descripción del Vehículo", "Tipo Combustible")]
tipo_chasis[, `Descripción del Vehículo` := as.character(`Descripción del Vehículo`)]

setnames(tipo_chasis, colnames(tipo_chasis), c("Chasis", "Combustible_chasis"))

ficha_chasis <- fread("C:/Users/REstevez/Documents/HISTORICOS/Ficha - Chasis.csv")

ficha_chasis[, Ficha := substr(Ficha, 2, 5)]
ficha_chasis[, Ficha := as.numeric(Ficha)]
ficha_chasis[, Chasis := as.character(Chasis)]
ficha_chasis[Chasis == "BUS MB OF 1418-52303", Chasis := "BUS MB OF 1418-52"]
ficha_chasis[Chasis == "MB O500 1726-59", Chasis := "BUS MB O500 1726-59"]
ficha_chasis[Chasis == "MB OF 1721L-59", Chasis := "BUS MB OF 1721L-59"]
ficha_chasis[Chasis == "MB OH 1721-62", Chasis := "BUS MB OH 1721-62"]
ficha_chasis[Chasis == "MB OF 1722-59", Chasis := "BUS MB OF 1722-59"]
ficha_chasis <- ficha_chasis[!grepl("YT", Chasis), ]

ficha_chasis <- left_join(ficha_chasis, tipo_chasis, by = c("Chasis" = "Chasis"))

# CARGAS -----------

files_LTS <- files[grepl("LITROS_2023", files)]

LTS_cargas <- lapply(files_LTS, read_xlsx)

LTS_cargas <- lapply(LTS_cargas, setDT)

LTS_cargas <- rbindlist(LTS_cargas)

LTS_cargas <- unique(LTS_cargas)

LTS_cargas[, `Fecha consumo` := if_else(Texto == "M", `Fecha consumo` + 12 * 60 * 60, `Fecha consumo`)]
LTS_cargas[, Fecha_corregida := as.Date(`Fecha consumo`)]
LTS_cargas[, Fecha_corregida := if_else(hour(`Fecha consumo`) < 6, Fecha_corregida - 1, Fecha_corregida)]
LTS_cargas[, Mes := months(Fecha_corregida, abbreviate = FALSE)]
meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
LTS_cargas[, Mes := factor(Mes, levels = meses, ordered = TRUE)]

# se filtran previo al cambio de combustible
LTS_cargas <- LTS_cargas[Fecha_corregida < "2023/07/16", ]
LTS_cargas <- LTS_cargas[Fecha_corregida < "2023/07/01", ]
LTS_cargas <- LTS_cargas[as.numeric(Mes) < 7, ]
LTS_cargas <- LTS_cargas[startsWith(Ficha, "F"), ]
LTS_cargas <- LTS_cargas[grepl("GASOIL", `Nombre del producto`), ]

LTS_cargas <- LTS_cargas[, c(
  "Ficha", "Código de almacen", "Nombre del producto",
  "Texto", "Fecha consumo", "Consumo de combustible", "Fecha_corregida", "Mes"
)]

setnames(LTS_cargas, colnames(LTS_cargas), c(
  "Ficha", "Almacen", "Combustible",
  "Texto", "Fecha_hora", "LTS", "Fecha_corregida", "Mes"
))

LTS_cargas[, Ficha := substr(Ficha, 2, 5)]
LTS_cargas[, Ficha := as.numeric(Ficha)]

# se une la ficha con el tipo de combustible que corresponde segun su chasis
LTS_cargas <- merge.data.table(LTS_cargas, ficha_chasis[, c("Ficha", "Combustible_chasis")],
  by.x = "Ficha",
  by.y = "Ficha",
  all.x = TRUE
)

setorder(LTS_cargas, Ficha, Fecha_corregida, Fecha_hora)

# se calculan los datos las cargas anteriores y proximas
LTS_cargas[, `:=`(
  HS_carga_anterior = c(0, diff(as.numeric(Fecha_hora))),
  Horas_prox_carga = c(-rev(diff(rev(as.numeric(Fecha_hora)))), 0),
  Taller_prox = shift(Almacen, fill = "", type = "lead"),
  Taller_ant = shift(Almacen, fill = "", type = "lag"),
  Proxima_carga = shift(LTS, fill = 0, type = "lead"),
  Carga_anterior = shift(LTS, fill = 0, type = "lag"),
  Mismo_combustible = Combustible == Combustible_chasis,
  Proximo_combustible = shift(Combustible, fill = "", type = "lead") == Combustible,
  Combustible_anterior = shift(Combustible, fill = "", type = "lag") == Combustible
), by = Ficha]

LTS_cargas[, HS_carga_anterior := HS_carga_anterior / 3600]
LTS_cargas[, Horas_prox_carga := Horas_prox_carga / 3600]


# se calcula si en la proxima carga o en la anterior se hizo otra carga mayor a 40 litros en el mismo taller dentro un lapso de 1hr y media
LTS_cargas[, `:=`(
  Cargas_en_1_hr_anterior =
    HS_carga_anterior < 1.5 & HS_carga_anterior > 0 &
      LTS > 40 & Carga_anterior > 40 &
      Almacen == Taller_ant,
  Cargas_en_1_hr_prox =
    Horas_prox_carga < 1.5 & Horas_prox_carga > 0 &
      LTS > 40 & Proxima_carga > 40 &
      Almacen == Taller_prox
), by = Ficha]

# se rellenan con FALSE los que no tengan nada
LTS_cargas[, `:=`(
  Cargas_en_1_hr_anterior = coalesce(Cargas_en_1_hr_anterior, FALSE),
  Cargas_en_1_hr_prox = coalesce(Cargas_en_1_hr_prox, FALSE)
)]

# si en la anterior o en la proxima hubo una carga cruzada se marca
LTS_cargas[, `:=`(
  Cargas_en_1_hr = Cargas_en_1_hr_prox | Cargas_en_1_hr_anterior
)]

# si hay cargas dentro de la 1.5 hs y de mas de 40 litros con el mismo tipo de combustible
# se consideran las 2 cargas cruzadas
LTS_cargas[, `:=`(
  chequeo_cruzado = Cargas_en_1_hr_anterior & Combustible_anterior |
    Cargas_en_1_hr_prox & Proximo_combustible,
  Cargas_mismo_dia =
    HS_carga_anterior < 6 & HS_carga_anterior > 0 &
      Almacen != Taller_ant |
      Horas_prox_carga < 2 & Horas_prox_carga > 0 &
        Almacen != Taller_prox
)]

LTS_cargas[, `:=`(
  Cargas_mismo_dia = coalesce(Cargas_mismo_dia, FALSE),
)]

# si las 2 cargas sospechosas se realizaron con distinto combustible
# se toma como cruzada la que no corresponde al chasis en cuestion
LTS_cargas[chequeo_cruzado < 1, `:=`(
  chequeo_cruzado = Cargas_en_1_hr & !Mismo_combustible
)]

cargas_cruzadas <- LTS_cargas[chequeo_cruzado == TRUE, ]

totales_cargas <- LTS_cargas[, .(
  Cantidad_cargas = .N,
  Total_LTS = sum(LTS, na.rm = TRUE)
), keyby = .(Almacen, Mes)]

totales_cruzados <- cargas_cruzadas[, .(
  Cantidad_cruzadas = .N,
  LTS_cruzados = sum(LTS, na.rm = TRUE)
), keyby = .(Almacen, Mes)]

detalle <- merge.data.table(totales_cargas, totales_cruzados,
  by.x = c("Almacen", "Mes"),
  by.y = c("Almacen", "Mes"),
  all.x = TRUE
)

setorder(detalle, Almacen, Mes)

detalle[, cargas_cruzadas_porcentaje := Cantidad_cruzadas / Cantidad_cargas]
detalle[, LTS_cruzados_porcentaje := LTS_cruzados / Total_LTS]
detalle[, Cantidad_cruzadas := coalesce(Cantidad_cruzadas, 0)]
detalle[, LTS_cruzados := coalesce(LTS_cruzados, 0)]
detalle[, cargas_cruzadas_porcentaje := coalesce(cargas_cruzadas_porcentaje, 0)]
detalle[, LTS_cruzados_porcentaje := coalesce(LTS_cruzados_porcentaje, 0)]

total_litros <- detalle[, .(Total_litros = sum(LTS_cruzados, na.rm = TRUE)),
  by = .(Almacen)
]

total_porcentaje <- detalle[, .(Total_porcentaje = sum(Cantidad_cruzadas, na.rm = TRUE) / sum(Cantidad_cargas, na.rm = TRUE)),
  by = .(Almacen)
]


ggplot() +
  geom_tile(
    data = detalle, aes(x = Mes, y = Almacen, fill = cargas_cruzadas_porcentaje),
    color = "white",
    lwd = 1.5,
    linetype = 0.1
  ) +
  geom_text(data = detalle, aes(
    x = Mes, y = Almacen,
    label = paste(round(cargas_cruzadas_porcentaje * 100, 2), "%")
  ), color = "black", size = 5) +
  geom_point(data = total_porcentaje, aes(x = "Total", y = Almacen, color = Total_porcentaje), size = 24, shape = 15) +
  geom_text(data = total_porcentaje, aes(
    x = "Total", y = Almacen,
    label = paste(round(Total_porcentaje * 100, 2), "%")
  ), color = "black", size = 5) +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "red", labels = scales::label_percent()) +
  scale_color_gradient(low = "white", high = "red") +
  guides(
    fill = "none",
    #fill = guide_colourbar(title = "Porcentaje"),
    color = "none"
  ) +
  scale_x_discrete(expand = c(0, 0.5), limits = c(as.character(unique(detalle$Mes)), "Total")) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, margin = margin(t = 0, r = 0, l = 0, b = 0.5, unit = "cm")),
    plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(t = 0, r = 0, l = 0, b = 1, unit = "cm")),
    panel.grid = element_blank(),
    plot.margin = margin(t = 1, r = 1, l = 1, b = 1, unit = "cm")
    #legend.text = element_text(size = 14),
    #legend.title = element_text(size = 16)
  ) +
  labs(
    title = "Porcentaje de cargas cruzadas",
    subtitle = "Cantidad de cargas cruzadas / Cantidad total de cargas"
  )

ggplot() +
  geom_tile(
    data = detalle, aes(x = Mes, y = Almacen, fill = LTS_cruzados), color = "white",
    lwd = 1.5,
    linetype = 0.1
  ) +
  geom_text(data = detalle, aes(x = Mes, y = Almacen, label = round(LTS_cruzados)), color = "black", size = 5) +
  geom_point(data = total_litros, aes(x = "Total", y = Almacen, color = Total_litros), size = 24, shape = 15) +
  geom_text(data = total_litros, aes(x = "Total", y = Almacen, label = round(Total_litros)), color = "black", size = 5) +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "red") +
  scale_color_gradient(low = "white", high = "red") +
  guides(
    fill = "none",
    #fill = guide_colourbar(title = "Litros"),
    color = "none"
  ) +
  scale_x_discrete(expand = c(0, 0.5), limits = c(as.character(unique(detalle$Mes)), "Total")) +
  scale_y_discrete(
    expand = c(0, 0),
    # labels = c(seq(1,6),"Total")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, margin = margin(t = 0, r = 0, l = 0, b = 0.5, unit = "cm")),
    panel.grid = element_blank(),
    plot.margin = margin(t = 1, r = 1, l = 1, b = 1, unit = "cm")
    #legend.text = element_text(size = 14),
    #legend.title = element_text(size = 16)
  ) +
  labs(title = "Cantidad de litros en cargas cruzadas")
