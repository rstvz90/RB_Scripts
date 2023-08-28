library(data.table)
library(readxl)
library(skimr)
library(tidyverse)
library(ggplot2)

rm(list = ls())
gc()

path <- "C:/Users/REstevez/Documents/HISTORICOS/"

files <- list.files(path, full.names = TRUE)

# CHASIS -----------

tipo_chasis <- read_xlsx("C:/Users/REstevez/Documents/HISTORICOS/Tipos de chasis.xlsx")

setDT(tipo_chasis)

tipo_chasis <- tipo_chasis[,c("Descripción del Vehículo", "Tipo Combustible")]
tipo_chasis[, Descripción del Vehículo` := as.character(`Descripción del Vehículo`)]

setnames(tipo_chasis, colnames(tipo_chasis), c("Chasis", "Combustible_chasis"))

ficha_chasis <- fread("C:/Users/REstevez/Documents/HISTORICOS/Ficha - Chasis.csv")

ficha_chasis[,Ficha := substr(Ficha, 2, 5)]
ficha_chasis[,Ficha := as.numeric(Ficha)]
ficha_chasis[,Chasis := as.character(Chasis)]
ficha_chasis[Chasis == "BUS MB OF 1418-52303", Chasis := "BUS MB OF 1418-52"]
ficha_chasis[Chasis == "MB O500 1726-59", Chasis := "BUS MB O500 1726-59"]
ficha_chasis[Chasis == "MB OF 1721L-59", Chasis := "BUS MB OF 1721L-59"]
ficha_chasis[Chasis == "MB OH 1721-62", Chasis := "BUS MB OH 1721-62"]
ficha_chasis[Chasis == "MB OF 1722-59", Chasis := "BUS MB OF 1722-59"]
ficha_chasis <- ficha_chasis[!grepl("YT", Chasis),]

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
LTS_cargas[, Mes := month(Fecha_corregida)]

# se filtran previo al cambio de combustible
LTS_cargas <- LTS_cargas[Fecha_corregida < "2023/07/16",]
LTS_cargas <- LTS_cargas[Mes < 7,]
LTS_cargas <- LTS_cargas[startsWith(Ficha,"F"),]
LTS_cargas <- LTS_cargas[grepl("GASOIL", `Nombre del producto`),]

LTS_cargas <- LTS_cargas[, c(
  "Ficha", "Código de almacen", "Nombre del producto",
  "Texto", "Fecha consumo", "Consumo de combustible", "Fecha_corregida", "Mes"
)]

setnames(LTS_cargas, colnames(LTS_cargas), c(
  "Ficha", "Almacen", "Combustible",
  "Texto", "Fecha_hora", "LTS", "Fecha_corregida", "Mes"
))

LTS_cargas[,Ficha := substr(Ficha, 2, 5)]
LTS_cargas[,Ficha := as.numeric(Ficha)]

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

LTS_cargas[, HS_carga_anterior := HS_carga_anterior/3600]
LTS_cargas[, Horas_prox_carga := Horas_prox_carga/3600]


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

cargas_cruzadas <- LTS_cargas[chequeo_cruzado==TRUE,]

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
all.x = TRUE)

setorder(detalle, Almacen, Mes)

detalle[, cargas_cruzadas_porcentaje := Cantidad_cruzadas / Cantidad_cargas]
detalle[, LTS_cruzados_porcentaje := LTS_cruzados / Total_LTS]
