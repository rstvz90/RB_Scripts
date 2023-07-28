library(data.table)
library(ggplot2)
library(readxl)
library(lubridate)

telemedicion <- read_xlsx("C:/Users/REstevez/Documents/Análisis/Telemedición/TELEMEDICION_HISTORICO.xlsx")


setDT(telemedicion)

telemedicion[, `:=`(FECHAINICIO = dmy_hms(FECHAINICIO))]
telemedicion[, `:=`(FECHACIERRE = dmy_hms(FECHACIERRE))]
telemedicion[, `:=`(CANT_DIAS = round(difftime(FECHACIERRE, FECHAINICIO, units = "days")))]
telemedicion[, `:=`(CANT_DIAS = as.numeric(CANT_DIAS))]
telemedicion[, `:=`(PROMEDIO_AFORADOR = DESPACHADOAFORADOR / CANT_DIAS,
                    PROMEDIO_TLS = DESPACHADOTLS / CANT_DIAS)]
telemedicion[, `:=`(FECHA = as.IDate(FECHAINICIO))]
telemedicion[, `:=`(MES = month(FECHA, label = TRUE, abbr = FALSE))]
telemedicion[, `:=`(YEAR = year(FECHA))]
telemedicion[, `:=`(PERIODO = floor_date(FECHA, unit = "month"))]
#telemedicion[, `:=`(PERIODO = format(PERIODO, "%B-%Y"))]

telemedicion <- telemedicion[PROMEDIO_AFORADOR < 15000 & PROMEDIO_AFORADOR > -15000,]
telemedicion <- telemedicion[PROMEDIO_TLS < 20000 & PROMEDIO_TLS > -20000,]
telemedicion <- telemedicion[DESPACHADOTLS > 0,]
telemedicion <- telemedicion[VOLUMENFIN > 0,]
telemedicion <- telemedicion[VOLUMENINI > 0,]
telemedicion <- telemedicion[TALLER != "MUN",]
telemedicion <- telemedicion[PERIODO < "2023/07/01",]

telemedicion_mes <- telemedicion[,
                                 .(DIFERENCIAS = sum(DIFERENCIAVOLUMEN, na.rm = TRUE) / sum(CANT_DIAS, na.rm = TRUE)),
                                 by = .(PERIODO)]

setorder(telemedicion_mes, PERIODO)

ggplot(telemedicion_mes) +
  aes(x = PERIODO, y = DIFERENCIAS, group = 1) +
  geom_line()
