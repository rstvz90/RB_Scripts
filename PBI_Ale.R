
# INICIACIÓN ----------------------------------------------------

# CARGA BIBLIOTECAS
library(data.table)
library(readxl)
library(rstudioapi)

# LIMPIEZA DEL DIRECTORIO Y MEMORIA
rm(list = ls())
gc()

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# LECTURA DE ARCHIVOS -----------------------------------------------------

# RUTA DE UBICACIÓN DE LOS ARCHIVOS
path <- "//sm-public/SGC/Objetivos/Vigentes - Objetivos operativos/Listado maestro de objetivos/vigente/Análisis por UN/Bases de Datos/Costos/Mayor Contable Detallado/Bases_MayorContDet"

# LEO LOS NOMBRES DE TODOS LOS ARCHIVOS EN LA CARPETA
files <- list.files(path, full.names = TRUE)

# ME QUEDO SOLO CON LOS XLSX
files_xlsx <- files[grepl(".xlsx", files)]
files_xlsx <- files_xlsx[!grepl("~\\$", files_xlsx)] # que no lea los archivos temporarios
# CREO EL ARCHIVO FINAL COMO UNA LISTA
tabla_completa <- list()

# LOOP POR CADA ARCHIVO XLSX
for (i in c(1:length(files_xlsx))) {
  
  # ME QUEDO SOLO CON EL NOMBRE DEL ARCHIVO
  fechas_completar <- substr(files_xlsx[[i]], start = nchar(files_xlsx[[i]])-10, stop = nchar(files_xlsx[[i]])-5)
  # LO CONVIERTO EN FORMATO "AÑO-MES-DÍA"
  fechas_completar <- paste0(substr(fechas_completar, 1, 4),"-",substr(fechas_completar, 5, 6),"-01")
  # CAMBIO DE FORMATO TEXTO A FECHA
  fechas_completar <- as.Date(fechas_completar)
  
  # LEO EL ARCHIVO XLSX.
  # SALTO LAS PRIMERAS 9 FILAS
  # LE DIGO QUE LA PRIMERA FILA ES EL ENCABEZADO
  tabla_completa[[i]] <- as.data.table(read_xlsx(files_xlsx[i], 
                                                 skip = 9, 
                                                 col_names = TRUE, 
                                                 sheet = 1))
  # SACO LAS COLUMNAS QUE NO SON RELEVANTES
  tabla_completa[[i]] <- tabla_completa[[i]][,-c("...7", "Cuenta contable", "...31", "...34", 
                    "Código de impuestos", "Número de seguimiento", "Dimension")]
  # CONVIERTO LAS COLUMNAS FECHA Y FECHA DOC EN FORMATO DE FECHAS
  tabla_completa[[i]][, Fecha := as.Date(Fecha)]
  tabla_completa[[i]][, `Fecha Doc` := as.Date(`Fecha Doc`)]
  # INGRESO LA FECHA CALCULADA EN LOS NA
  tabla_completa[[i]][is.na(Fecha), Fecha := fechas_completar]
  tabla_completa[[i]][is.na(`Fecha Doc`), Fecha := fechas_completar]
    
}

# UNO TODAS LAS TABLAS DE LA LISTA
tabla_completa <- bind_rows(tabla_completa) 

# ELIMINO LAS FILAS REPETIDAS SI LAS HAY
tabla_completa <- distinct(tabla_completa)

# ESCRIBO EL ARCHIVO.
# COMO NO TENGO ACCESO, LO GUARDÉ EN LA CARPETA DE DESCARGAS
fwrite(tabla_completa, 
       file = paste0("C:/Users/REstevez/Downloads/tabla_completa.txt"))
