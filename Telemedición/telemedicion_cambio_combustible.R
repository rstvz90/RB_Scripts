library(data.table)
library(ggplot2)
library(readxl)
library(lubridate)
library(DT)

rm(list = ls())
gc()

telemedicion <- read_xlsx("//sm-public/Public/Mantenimiento/REstevez/Para Borrar/tank_cambio_combustible.xlsx")


setDT(telemedicion)

telemedicion[, `:=`(FECHAINICIO = dmy_hms(FECHAINICIO))]
telemedicion[, `:=`(FECHACIERRE = dmy_hms(FECHACIERRE))]

telemedicion[, `:=`(FECHA = as.IDate(FECHAINICIO))]
telemedicion[, `:=`(MES = lubridate::month(FECHA, label = TRUE, abbr = FALSE))]
telemedicion[, `:=`(YEAR = year(FECHA))]
telemedicion[, `:=`(PERIODO = floor_date(FECHA, unit = "month"))]
telemedicion[, `:=`(IDMULTITANQUE = as.numeric(IDMULTITANQUE))]
telemedicion <- telemedicion[TALLER == "BERU", TALLER := "BER"]
telemedicion <- telemedicion[TALLER == "MUN", TALLER := "CAR"]

setorder(telemedicion, TALLER, IDMULTITANQUE, FECHAINICIO)

datatable(telemedicion, filter = "top", options = list(pageLength = 100)) %>% 
  formatDate("FECHAINICIO", method = "toUTCString", params = NULL, rows = NULL) %>% 
  formatDate("FECHACIERRE", method = "toUTCString", params = NULL, rows = NULL) %>% 
  formatDate("FECHA", method = "toDateString", params = NULL, rows = NULL)
