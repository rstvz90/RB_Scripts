library(data.table)
library(readxl)
library(rstudioapi)
library(clipr)

rm(list = ls())
gc()
Sys.setlocale("LC_TIME", "es_US.UTF-8")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

maestro_flota <- read_xlsx("Datos/pbi_gestion_de_flota.xlsx")
setDT(maestro_flota)

maestro_flota <- maestro_flota[grepl("F*", Ficha),]
maestro_flota[, Interno := substr(Ficha, 2, 5)]
maestro_flota[, Interno := as.numeric(Interno)]
maestro_flota <- maestro_flota[!is.na(Interno)]
maestro_flota <- maestro_flota[Ubic %in% c("BAI",
                                           "BER",
                                           "VGA",
                                           "VCO",
                                           "PTO")]
maestro_flota <- maestro_flota[,c("Ficha","Dominio","Interno")]

coches_kraft <- read_xlsx("Datos/data_kraft_vehiculos.xlsx")
setDT(coches_kraft)
coches_kraft <- coches_kraft[,c("ID","VEHICULO","PATENTE","TARJETA")]

tabla <- merge.data.table(maestro_flota, coches_kraft,
                          by.x = c("Interno"),
                          by.y = c("VEHICULO"),
                          all.x = TRUE)
setorder(tabla, Interno)
write_clip(tabla)

