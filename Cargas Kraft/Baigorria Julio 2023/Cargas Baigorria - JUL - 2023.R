# DIFERENCIA DE STOCK EN D500 - SE EVALUA SI ES UN PROBLEMA DE LA TELEMEDICION
# O UN PROBLEMA DE MIGRACION DE CARGAS. VER FOTOS.


# INICIACIÓN --------------------------------------------------------------

library(data.table)
library(readxl)
library(lubridate)
library(openxlsx)
library(rstudioapi)


rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Lectura Archivos --------------------------------------------------------

# DATOS DATAKRAFT
{
  datakraft <- read_xlsx("datakraft_BAI_julio.xlsx")
  setDT(datakraft)
  col_datakraft <- copy(colnames(datakraft))
}

# Transformación y Resumen de Datos -------------------------------------------------

# DATOS DATA KRAFT 
{
  # Se corrigen los tipos de variales
  # Se cambian los nombres de los productos
  # Se crea el campo Fecha con la fecha corregida
  datakraft[,`:=`(FECHA = dmy(FECHA),
                  Hora = ymd_hms(HORA),
                  LITROS = as.numeric(LITROS)
                  )]
  datakraft[, Hora := format(Hora, format = "%H:%M:%S")]
  datakraft[, Hora := substr(Hora, 1, 5)]  
  datakraft[, Fecha_hora := paste(FECHA, Hora)]
  datakraft[, Fecha_hora := ymd_hm(Fecha_hora)]
  
  datakraft[,`:=`(rango_hora = hour(HORA))]
  datakraft[rango_hora < 6, FECHA := FECHA - days(1)]
  datakraft[PRODUCTO == "INFINIA DIESEL", PRODUCTO := "GASOIL INFINIA DIESEL"]
  datakraft[PRODUCTO == "DIESEL 500", PRODUCTO := "GASOIL D500"]
  datakraft[,Ficha := as.numeric(`NUM. VEHICULO`)]
  
  
  # Resumen de cargas del Kraft por fecha y producto
  kraft_cargas <- datakraft[, .(Litros_kraft = sum(LITROS, na.rm = TRUE)), 
                            by = .(FECHA, PRODUCTO)]
  
  kraft_cargas_dia <- datakraft[, .(Litros_kraft = sum(LITROS, na.rm = TRUE)), 
                                by = .(FECHA)]
}
