# ERRORES DE MIGRACIÓN DE CARGAS, TANTO PARA EL DATAKRAFT COMO EL DYNAMICS
# LUEGO DE CAMBIO DE PC EN EL TALLER EL FIN DE MAYO 2023


# INICIACIÓN --------------------------------------------------------------

library(data.table)
library(readxl)
library(lubridate)
library(openxlsx)
library(rstudioapi)
library(DT)
library(skimr)


rm(list = ls())
gc(full = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Lectura Archivos --------------------------------------------------------

# DATOS DATAKRAFT
{
  vco <- read_xlsx("datakraft_vco.xlsx")
  setDT(vco)
  col_datakraft <- copy(colnames(vco))
}


# Transformación y Resumen de Datos -------------------------------------------------

# DATOS DATA KRAFT 
{
  # Se corrigen los tipos de variales
  # Se cambian los nombres de los productos
  # Se crea el campo Fecha con la fecha corregida
  vco[,`:=`(FECHA = dmy(FECHA),
            Hora = ymd_hms(HORA),
            LITROS = as.numeric(LITROS)
  )]
  vco[, Hora := format(Hora, format = "%H:%M:%S")]
  vco[, Hora := substr(Hora, 1, 5)]  
  vco[, Fecha_hora := paste(FECHA, Hora)]
  vco[, Fecha_hora := ymd_hm(Fecha_hora)]

  vco[,`:=`(rango_hora = hour(HORA))]
  vco[, FECHA_corregida := FECHA]
  vco[rango_hora < 6, FECHA_corregida := FECHA_corregida - days(1)]
  vco[PRODUCTO == "INFINIA DIESEL", PRODUCTO := "GASOIL INFINIA DIESEL"]
  vco[PRODUCTO == "DIESEL 500", PRODUCTO := "GASOIL D500"]
  vco[,Ficha := as.numeric(`NUM. VEHICULO`)]
  
  
  # Resumen de cargas del Kraft por fecha y producto
  kraft_cargas <- vco[, .(Litros_kraft = sum(LITROS, na.rm = TRUE)), 
                         by = .(FECHA_corregida, PRODUCTO)]
  
  kraft_cargas_dia <- vco[, .(Litros_kraft = sum(LITROS, na.rm = TRUE)), 
                             by = .(FECHA_corregida)]
}

skim(vco)

DT::datatable(vco, filter = "top", options = list(pageLength = 200)) %>% 
  formatRound('LITROS', digits = 0)
DT::datatable(kraft_cargas_dia, filter = "top", options = list(pageLength = 200)) %>% 
  formatRound('Litros_kraft', digits = 0)
