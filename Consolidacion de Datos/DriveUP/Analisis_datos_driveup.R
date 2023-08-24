# Alumno: Rodrigo Estevez
# Análisis Inteligente de Datos
# Universidad Austral
# Año 2023

# IMPORTANTE: DESCARGAR TODOS LOS ARCHIVOS EN UNA MISMA CARPETA

# OBJETIVO -------------------
# A CONTINUACIÓN SE ANALIZARÁN DATOS DE POSICIONAMIENTO DE COLECTIVOS
# SE CUENTA CON LA POSICIÓN CADA 10 SEGUNDOS CUANDO EL COCHE ESTÁ EN MARCHA Y
# CADA 1 HORA CUANDO EL MOTOR ESTÁ PARADO.

# DURANTE EL ANALISIS SE CALCULAS LAS SIGUIENTES VARIABLES:

# - REGISTRAR LA HORA Y LOS LITROS CARGADOS A PARTIR DE LOS VALORES DEL NIVEL DEL TANQUE
# - REGISTRAR PUNTOS CON EXCESOS DE VELOCIDAD
# - REGISTRAR BAJAS ABRUPTAS DE NIVEL DE COMBUSTIBLE
# - CALCULAR EL TIEMPO EN EL QUE SE ENCUENTRA EN RALENTI LAS UNIDEADES
# - CALCULAR LOS KM RECORRIDOS TOTAL

# SITUACIONES A CONTROLAR:
# LA APP PERMITE CONSULTAR VALORES DE TELEMETRIA DEL COCHE DURANTE EL DIA.
# PERMITE CHEQUEAR DIFERENTES VALORES DE TELEMETRIA EN CASO DE SOSPECHAR:
# - ROBO DE COMBUTIBLE
# - IDENTIFICAR EXCESOS DE VELOCIDAD
# - CONTROLAR SI EL COCHE SE ENCUENTRA LIMITADO A LA VELOCIDAD ACORDE A LA LINEA
# - CONTROLAR LOS LITROS CARGADOS EN CADA COCHE

# HAY UN SISTEMA QUE PERMITE REGISTRAR LOS LITROS CARGADOS POR SURTIDOR DE FORMA AUTOMATICA,
# PERO EN ALGUNAS OCACIONES, COMO POR EJEMPLO, POR CORTES DE LUZ, PROBLEMAS EN LAS PC O BASES DE DATOS,
# LOS LITROS CARGADOS NO PUEDAN SER REGISTRADOS. SI ALGO DE ESTO SUCEDE, SE DEBEN REGISTRAR MANUALMENTE.
# CON ESTE ANALISIS SE PODRA CONTROLAR SI LOS LITROS REGISTRADOS SON COHERENTES O NO.

# DE ESTA FORMA SE CONTROLA FALTANTES DE COMBUSTIBLE O EXCESOS DE VELOCIDAD EN CALLE.

# DATASETS ------------------------------

# TODOS LOS DATOS TRABAJADOS FUERON SUMINISTRADOS POR LA EMPRESA ROSARIO BUS.

# SE CUENTAN CON LOS SIGUIENTES DATASETS:
# "driveup_data_2023_01_30.csv"
# ESTE DATASET SON LOS DATOS DE LA PLACA CON LA TELEMETRIA DE LA UNIDAD
# CUENTA CON DATOS DE 1 DIA, DESDE LAS 00:00 A LAS 23:59.
# UNA OBSERVACION POR CADA COCHE CADA 10 SEGUNDOS SI EL MOTOR ESTA EN MARCHA
# UNA OBSERVACION POR CADA COCHE CADA 1 HORA CUANDO EL MOTOR ESTA PARADO.
# LAS VARIABLES DEL DATASET SON:
# POSICIÓN (LATITUD Y LONGITUD) --> VARIABLE CONTINUA
# ACELERADOR (POSICION DEL ACELERADOR EN PORCENTAJE DE 0 A 100) --> VARIABLE DISCRETA
# RPM DEL MOTOR --> VARIABLE CONTINUA
# NIVEL DE COMBUSTIBLE (PORCENTAJE DE LLENADO DE 0 A 100) --> VARIABLE CONTINUA
# VELOCIDAD INSTANTANEA --> VARIABLE CONTINUA
# FICHA: IDENTIFICACION DEL COCHE EJ: F4000. - VARIABLE DE TEXTO



# "limites_velocidad.txt"
# EN ESTE DATASET CUENTA CON LOS LIMITES DE VELOCIDAD QUE DEBE TENER CADA LINEA.

# "Tipos de chasis.xlsx"
# TABLA DE EXCEL CON LOS TIPOS DE CHASIS QUE CUENTA LA EMPRESA Y EL TIPO DE COMBUSTIBLE QUE CARGA CADA COCHE.

# "COORDENADAS TALLERES.csv"
# TABLA CON LOS DISTINTOS TALLERES DE LA EMPRESA Y LAS COORDENADAS DE CADA TALLER
# LOS TALLERES SON PUNTOS DE CARGA DE COMBUSTIBLE DE LOS COCHES

# "Listado Fichas.csv"
# UNA FICHA ES LA IDENTIFICACION UNICA DEL COCHE (SIMILAR A LA PATENTE)
# EL DATASET CONTIENE LA IDENTIFICACION DEL COCHE CON EL CORRESPONDIENTE CHASIS Y LINEA.
# LINEA (O BANDERA/RECORRIDO) --> INDICA EL RECORRIDO DE LA UNIDAD. --> VARIABLE CUALITATIVA


# INICIALIZACION -----------------------
# CARGA DE BIBLIOTECAS

# LIMPIEZA DE LA MEMORIA Y SETEO DEL WORKING DIRECTORY
gc()
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen = 2)

# LISTADO DE BIBLIOTECAS
listofpackages <- c(
  "readr", "tidyverse", "lubridate",
  "rstudioapi", "stringr",
  "readxl", "data.table",
  "purrr", "naniar", "roll", "bit64",
  "geosphere", "scales", "tibble", 
  "jsonlite","foreach", "doParallel"
)

# revisar e instalar librerias que no es están instaladas
newPackages <- listofpackages[!(listofpackages %in% installed.packages()[, "Package"])]

if (length(newPackages)) install.packages(newPackages)
for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}

# ESTA FUNCIÓN CORRIGE ALGUNOS VALORES ERRÓNEOS EN EL NIVEL DE COMBUSTIBLE.

# EL NIVEL DE COMBUSTIBLE SE MIDE POR MEDIO DE UN FLOTANTE EN EL TANQUE.
# EN ALGUNOS MOMENTOS, APARENTEMENTE EL NIVEL SE TRABA O TIENE UN ERROR EN LA SEÑAL ELECTRICA Y
# GENERA QUE FICTICIAMENTE EL TANQUE SE INDIQUE COMO LLENO O VACIO.

# LA FUNCION CORRECCION BUSCA IDENTIFICAR ESTAS SUBIDAS O BAJADAS O FICTICIAS DEL NIVEL.
# ESTO SE DEFINIO EN UNA FUNCION Y SE PASO POR UN MUTATE PARA MEJORAS LA PERFORMANCE DE LAS OPERACIONES.

CORRECCION <- function(Ficha, FuelLevel, Lat, Lng, Suma_delta, Delta) {
  # SE PASAN A LA FUNCION LOS SIGUIENTES ARGUMENTOS:
  # FICHA (IDENTIFICACION DEL COCHE)
  # FUELLEVEL - SEÑAL DE NIVEL DEL TANQUE
  # Lat y Lng, LATITUD Y LONGITUD DE LA POSICION DEL COCHE
  # DELTA - DIFENCIA DEL NIVEL DE COMBUSTIBLE RESPECTO A LA OBSERACION ANTERIOR
  # SUMA_DELTA - SUMA ACUMULADA DE LA DIFERENCIA DEL NIVEL DEL COMBUSTIBLE

  # SE EXCLUYEN DE ESTA CORRECCION LA FICHAS QUE PRESENTAN POCAS OBSERVACIONES (MENOS DE 20)
  # SI LA FICHA TIENE POCAS OBSERVACIONES QUIERE DECIR QUE NO ESTUVO EN MARCHA Y NO SALIO DEL TALLER
  # PIERDE SENTIDO ESTA CORRECCION
  # TODOS LOS COCHES CARGAS COMBUSTIBLE EN UN TALLER Y SE DETIENEN PARA MANTENIMIENTO DE UN DIA AL SIGUIENTE
  # NO SIEMRE REALIZAN ESTO EN EL MISMO TALLER

  if (length(Ficha) > 20) {
    # SI HAY UNA DIFERENCIA DEL NIVEL DE COMBUSTIBLE ACUMULADA MAYOR AL 20% O MENOR AL -20%
    # SE EVALUARA SI REALIZAR ESTA CORRECCION O NO
    # LOS MOVIMIENTOS DEL COCHE GENERAN MUCHO RUIDO EN LA SEÑAL Y EL NIVEL VARIA MUCHO.
    # GERERA QUE SEA DIFICIL IDENTIFICAR LAS SUBIDAS DE NIVEL POR CARGA DE COMBUSTIBLE O MOVIMIENTO DEL COCHE.

    indices_subidas <- which(Suma_delta > 20)
    indices_bajadas <- which(Suma_delta < -20)
    dist_talleres <- c()

    # SI SE IDENTIFICAN SUBIDAS DE NIVEL MAYORES A 20% SE EVALUA SI CORRESPONDE CORREGIR EL NIVEL

    if (length(indices_subidas) > 0) {
      j <- 0

      for (i in c(1:length(indices_subidas))) {
        # PARA TODOS LO PUNTOS QUE TENGAN UN DIFENCIA DE NIVEL ACUMULADA MAYOR AL 20% SE CALCULA LA DISTANCIA A CADA TALLER
        dist_talleres[i - j] <- as.integer(DISTANCIA_TALLERES(Lat[indices_subidas[i - j]], Lng[indices_subidas[i - j]])[1])

        # LAS OBSERVACIONES QUE SE ENCUENTREN A MAS DE 300 MTS DE UN TALLER SE DESCARTAN DE ESTE ANALISIS
        if (dist_talleres[i - j] < 300) {
          dist_talleres <- dist_talleres[-(i - j)]
          indices_subidas <- indices_subidas[-(i - j)]
          j <- j + 1
        }
      }

      # SI AUN LUEGO DE DESCARTAR LAS OBSERVACIONES QUE AUMENTARON EL NIVEL DENTRO DE UN TALLER QUEDAN
      # OBSERVACIONES SE CONTINUA CON EL ANALISIS
      if (length(indices_subidas) > 0) {
        if (length(indices_subidas) > 1) {
          j <- 0
          ajuste <- 0

          for (i in c(1:(length(indices_subidas) - 1))) {
            if ((indices_subidas[i + 1 - j] - indices_subidas[i - j]) == 1 & dist_talleres[i - j] > 300) {
              # SE CORRIGE LA SUBIDA DE NIVEL AL NIVEL ANTERIOR A LA SUBIDA
              ajuste <- ajuste + Delta[indices_subidas[i - j]]
              FuelLevel[indices_subidas[i - j]] <- FuelLevel[indices_subidas[i - j]] - ajuste
              indices_subidas <- indices_subidas[-(i - j)]
              dist_talleres <- dist_talleres[-(i - j)]
              j <- j + 1
            } else {
              ajuste <- 0
            }
          }
        }

        # SE CORRIGE LA BAJA ABRUPTA SOLO SI HUBO UNA SUBIDA (SE ENCUENTRA DENTRO DEL IF)
        if (length(indices_bajadas) > 1) {
          j <- 0
          ajuste <- 0

          for (i in c(1:(length(indices_bajadas) - 1))) {
            if ((indices_bajadas[i + 1 - j] - indices_bajadas[i - j]) == 1) {
              ajuste <- ajuste + Delta[indices_bajadas[i - j]]
              FuelLevel[indices_bajadas[i - j]] <- FuelLevel[indices_bajadas[i - j]] - ajuste



              indices_bajadas <- indices_bajadas[-(i - j)]
              j <- j + 1
            } else {
              ajuste <- 0
            }
          }
        }


        # SE REVIERTEN LOS INDICES PARA ANALIZAR DESDE LAS ULTIMAS SUBIDAS A LAS PRIMERAS.
        # SUPONIENDO QUE HAYA MAS DE UNA SUBIDA FICTICIA

        # indices_bajadas <- indices_bajadas
        indices_subidas <- indices_subidas - 1
        indices_bajadas <- rev(indices_bajadas)
        indices_subidas <- rev(indices_subidas)
        cant_subidas <- length(indices_subidas)

        # SE CALCULA EL VALOR DE LA CORRECCION A REALIZAR (FACTOR DE CORRECCION) A PARTIR DEL PROMEDIO DE VALORES
        # DE NIVEL PREVIO A LA SUBIDA Y EL PROMEDIO DE ALGUNOS VALORES POSTERIORES A LA SUBIDA PARA CADA SUBIDA.
        # PRIMERO SE ANALIZA LA ULTIMA SUBIDA HACIA ADELANTE EN EL TIEMPO.
        # SI NO SE ENCUENTRA NINGUNA BAJADA POSTERIOR A LA SUBIDA SE ANALIZA SI HAY UNA BAJADA PREVIA
        # EN CASO DE TAMPOCO ENCONTRAR UNA BAJADA SE CORRIGEN LOS VALORES DE NIVEL HASTA EL FIN DEL DIA.

        for (i in c(1:cant_subidas)) {
          if (!is.na(indices_bajadas[i]) & indices_bajadas[i] > indices_subidas[i]) {
            factor_correccion_sub <- mean(FuelLevel[(indices_subidas[i] - 7):(indices_subidas[i] - 2)]) - mean(FuelLevel[(indices_subidas[i] + 2):(indices_subidas[i] + 5)])
            factor_correccion_baj <- mean(FuelLevel[(indices_bajadas[i] + 2):(indices_bajadas[i] + 7)]) - mean(FuelLevel[(indices_bajadas[i] - 5):(indices_bajadas[i] + 2)])
            factor_correccion <- min(factor_correccion_baj, factor_correccion_sub)

            FuelLevel[(indices_subidas[i] + 1):(indices_bajadas[i] - 1)] <- FuelLevel[(indices_subidas[i] + 1):(indices_bajadas[i] - 1)] + factor_correccion
          } else if (!is.na(indices_bajadas[i]) & indices_bajadas[i] < indices_subidas[i]) {
            factor_correccion_sub <- mean(FuelLevel[(indices_subidas[i] + 1):(indices_subidas[i] + 5)]) - mean(FuelLevel[(indices_subidas[i] - 4):(indices_subidas[i] - 2)])
            factor_correccion_baj <- mean(FuelLevel[(indices_bajadas[i] - 5):(indices_bajadas[i] - 1)]) - mean(FuelLevel[(indices_bajadas[i] + 2):(indices_bajadas[i] + 4)])
            factor_correccion <- max(factor_correccion_baj, factor_correccion_sub)

            FuelLevel[indices_bajadas[i]:indices_subidas[i]] <- FuelLevel[indices_bajadas[i]:indices_subidas[i]] + factor_correccion
          } else {
            factor_correccion <- mean(FuelLevel[(indices_subidas[i] + 10):(indices_subidas[i] + 16)]) - mean(FuelLevel[(indices_subidas[i] - 2):(indices_subidas[i] - 6)])
            FuelLevel[(indices_subidas[i] + 1):length(FuelLevel)] <- FuelLevel[(indices_subidas[i] + 1):length(FuelLevel)] - factor_correccion
          }
        }
      }
    }
  }
  return(FuelLevel)
  # SE DEVUELVE A LA FUNCION MUTATE EL VECTOR CON EL NIVEL DEL TANQUE CORREGIDO
}

# APLICA LA MEDIA MÓVIL A UN VECTOR DE DATOS. AGREGA DATOS AL PRINCIPIO Y AL FINAL DEL SET
# PARA NO PERDER DATOS YA QUE LA FUNCION INTRODUCE NA SI NO PUEDE CALCULA LA MEDIA MOVIL.
# SE REPITEN LOS ULTIMOS Y PREMEROS VALORES DE LOS DATOS DE NIVEL LA CANTIDAD DE VECES
# QUE SE REALIZA LA MEDIA MOVIL.
# LA CARGA ES UN SALTO DISCRETO DEL NIVEL DE COMBUSTIBLE Y LA MEDIA MOVIL SUAVIZA ESTE SALTO.
# TAMBIÉN EL COCHE ESTÁ CON EL MOTOR PARADO DURANTE LA CARGA, POR LO QUE NO HAY MEDICIONES DURANTE LA SUBIDA DEL NIVEL,
# LA CARGA SE VE REFLEJADA COMO UN SALTO REPENTINO DEL NIVEL.

# EN UN PRINCIPIO SE INTENTO REDUCIR LAS FLUCTUACIONES DE LA SEÑAL DEL NIVEL DE COMBUSTIBLE CON UNA REGRESION DE LA
# SERIE TEMPORAL, PERO ESTA REGRESION (UTILIZANDO LA REGRESION LOESS) NO PODIA REPRESENTAR CORRECTAMENTE EL
# SALTO DISCRETO DE LA VARIABLE LUEGO DE LA CARGA DE COMBUSTIBLE.
# POR ESTO SE OPTO POR UTILZIAR UNA MEDIA MOVIL.

APLICAR_FILTRO <- function(Nivel) {
  a_filtrar <- append(rep(Nivel[1], puntos_filtro), Nivel)
  a_filtrar <- append(a_filtrar, rep(tail(Nivel, 1), puntos_filtro))

  litros_filtro <- stats::filter(a_filtrar, filtro)

  return(litros_filtro[!is.na(litros_filtro)])
}


# FUNCION DISTANCIA A TALLERES. CALCULA LA DISTANCIA AL TALLER MAS CERCANO Y DEVUELVE:
# LA MINIMA DISTANCIA Y EL NOMBRE DEL TALLER MAS CERCANO.

DISTANCIA_TALLERES <- function(LAT, LONG) {
  dist <- distm(cbind(LONG, LAT), cbind(COORD_TALLERES[, "LONG"], COORD_TALLERES[, "LAT"]))
  taller <- COORD_TALLERES$TALLER[which(dist == min(dist))]

  return(c(min(dist), taller))
}


# ESTA FUNCIÓN CALCULA LA DISTANCIA ENTRE PUNTOS DE POSICION
CALCULO_DISTANCIA <- function(X) {
  return(distm(
    matrix(data = as.numeric(c(X[3], X[2])), ncol = 2),
    matrix(data = as.numeric(c(X[17], X[16])), ncol = 2)
  ))
}


#EXP_1
tic()
path <- "//VM-COCHES-01/ExperimentosR"
files <- list.files(path, full.names = TRUE)

year <- "2023"
month <- "07"
day <- "02"

dia_analisis <- paste0(year, "-", month, "-", day)
dia_analisis <- as_date(dia_analisis)
files_filtrado <- files[grepl(dia_analisis, files)]
horas_extras <- list(
  paste0(path, "/", dia_analisis - 1, " 23.txt"),
  paste0(path, "/", dia_analisis - 1, " 24.txt"),
  paste0(path, "/", dia_analisis + 1, " 00.txt"),
  paste0(path, "/", dia_analisis + 1, " 01.txt")
)
files_filtrado <- append(files_filtrado, horas_extras)


j <- 0
for (i in files_filtrado) {
  jsonlist <- list()
  j <- j + 1
  con <- file(i, open = "r")

  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    jsonlist <- append(jsonlist, list(fromJSON(line)))
  }
  close(con)
  temporary <- lapply(jsonlist, as.data.frame)
  temporary <- rbindlist(temporary)
  name_file <- paste0("C:/Users/REstevez/Downloads/temp/file_",j,".csv")
  fwrite(temporary, name_file) 
  rm(list = c("temporary", "name_file", "jsonlist"))
  gc(full = TRUE)
}


path_2 <- "C:/Users/REstevez/Downloads/temp"
files <- list.files(path_2, full.names = TRUE)

DUP_dt <- lapply(files, fread)

DUP_dt <- rbindlist(DUP_dt)

DUP_dt <- distinct(DUP_dt)
toc()


#--------------------------------------------
# EXP_2
library(data.table)
library(jsonlite)
library(foreach)
library(doParallel)


# Set the number of cores to be used for parallel processing
num_cores <- 8  # Adjust as needed

# Initialize parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

tic()
path <- "//VM-COCHES-01/ExperimentosR"
files <- list.files(path, full.names = TRUE)
a <- stream_in(file(files[1]))
a <- flatten(a)
year <- "2023"
month <- "07"
day <- "02"

dia_analisis <- paste0(year, "-", month, "-", day)
dia_analisis <- as_date(dia_analisis)
files_filtrado <- files[grepl(dia_analisis, files)]
horas_extras <- list(
  paste0(path, "/", dia_analisis - 1, " 23.txt"),
  paste0(path, "/", dia_analisis - 1, " 24.txt"),
  paste0(path, "/", dia_analisis + 1, " 00.txt"),
  paste0(path, "/", dia_analisis + 1, " 01.txt")
)
files_filtrado <- append(files_filtrado, horas_extras)


j <- 0
for (i in files_filtrado) {
  jsonlist <- list()
  j <- j + 1
  con <- file(i, open = "r")

  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    jsonlist <- append(jsonlist, list(fromJSON(line)))
  }
  close(con)
  temporary <- lapply(jsonlist, as.data.frame)
  temporary <- rbindlist(temporary)
  name_file <- paste0("C:/Users/REstevez/Downloads/temp/file_",j,".csv")
  fwrite(temporary, name_file) 
  rm(list = c("temporary", "name_file", "jsonlist"))
  gc(full = TRUE)
}


path_2 <- "C:/Users/REstevez/Downloads/temp"
files <- list.files(path_2, full.names = TRUE)

DUP_dt <- lapply(files, fread)

DUP_dt <- rbindlist(DUP_dt)

DUP_dt <- distinct(DUP_dt)
toc()




#--------------------------------------------


path <- "//VM-COCHES-01/ExperimentosR"
files <- list.files(path, full.names = TRUE)

year <- "2023"
month <- "07"
day <- "02"
dia_analisis <- paste0(year, "-", month, "-", day)
dia_analisis <- as_date(dia_analisis)
files_filtrado <- files[grepl(dia_analisis, files)]
horas_extras <- list(
  paste0(path, "/", dia_analisis - 1, " 23.txt"),
  paste0(path, "/", dia_analisis - 1, " 24.txt"),
  paste0(path, "/", dia_analisis + 1, " 00.txt"),
  paste0(path, "/", dia_analisis + 1, " 01.txt")
)
files_filtrado <- append(files_filtrado, horas_extras)
# Set the number of cores to be used for parallel processing
num_cores <- 4  # Adjust as needed

# Initialize parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to process a file and return a data frame
process_file <- function(file_path) {
  con <- file(file_path, open = "r")
  json_data <- vector("list", length = 0)
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    json_data <- append(json_data, list(fromJSON(line)))
  }
  close(con)
  data_frame <- as.data.table(json_data)
  return(data_frame)
}
temporario <- list()

for (i in files_filtrado) {
# Use foreach for parallel processing
json_data_list <- foreach(file_path = i, .combine = rbindlist, .packages = c("jsonlite", "data.table")) %dopar% {
  process_file(file_path)
}
temporary <- rbindlist(json_data_list)

name_file <- paste0("C:/Users/REstevez/Downloads/temp/file_",j,".csv")
fwrite(temporary, name_file) 
rm(list = c("temporary", "name_file", "json_data_list"))
gc(full = TRUE)

}

stopCluster(cl)

path_2 <- "C:/Users/REstevez/Downloads/temp"
files <- list.files(path_2, full.names = TRUE)

DUP_dt <- lapply(files, fread)

DUP_dt <- rbindlist(DUP_dt)

DUP_dt <- distinct(DUP_dt)
toc()


# EXP3-------------------------------
tic()
path <- "//VM-COCHES-01/ExperimentosR"
files <- list.files(path, full.names = TRUE)

year <- "2023"
month <- "07"
day <- "02"
dia_analisis <- paste0(year, "-", month, "-", day)
dia_analisis <- as_date(dia_analisis)
files_filtrado <- files[grepl(dia_analisis, files)]
horas_extras <- list(
  paste0(path, "/", dia_analisis - 1, " 23.txt"),
  paste0(path, "/", dia_analisis - 1, " 24.txt"),
  paste0(path, "/", dia_analisis + 1, " 00.txt"),
  paste0(path, "/", dia_analisis + 1, " 01.txt")
)
files_filtrado <- append(files_filtrado, horas_extras)

j <- 0
for (i in files_filtrado) {
  j <- j + 1
  jsonlist <- list()
  con <- file(i, open = "r")
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    jsonlist <- append(jsonlist, list(fromJSON(line)))
  }
  close(con)
  temporary <- lapply(jsonlist, as.data.frame)
  temporary <- rbindlist(temporary)
  name_file <- paste0("C:/Users/REstevez/Downloads/temp/file_",j,".csv")
  fwrite(temporary, name_file) 
  rm(list = c("temporary", "name_file", "jsonlist"))
  gc(full = TRUE)
}

path_2 <- "C:/Users/REstevez/Downloads/temp"
files <- list.files(path_2, full.names = TRUE)
files <- files[grepl("files", files)]

DUP_dt <- lapply(files, fread)

DUP_dt <- rbindlist(DUP_dt)

DUP_dt <- distinct(DUP_dt)
toc()

# EXP4 --------------------------------
tic()
path <- "//VM-COCHES-01/ExperimentosR"
files <- list.files(path, full.names = TRUE)

year <- "2023"
month <- "07"
day <- "02"
dia_analisis <- paste0(year, "-", month, "-", day)
dia_analisis <- as_date(dia_analisis)
files_filtrado <- files[grepl(dia_analisis, files)]
horas_extras <- list(
  paste0(path, "/", dia_analisis - 1, " 23.txt"),
  paste0(path, "/", dia_analisis - 1, " 24.txt"),
  paste0(path, "/", dia_analisis + 1, " 00.txt"),
  paste0(path, "/", dia_analisis + 1, " 01.txt")
)
files_filtrado <- append(files_filtrado, horas_extras)
jsonlist <- list()
for (i in files_filtrado) {
  con <- file(i, open = "r")
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    jsonlist <- append(jsonlist, list(fromJSON(line)))
  }
  print(i)
  print(Sys.time())
  close(con)
}
DUP_dt <- lapply(jsonlist, as.data.frame)
DUP_dt <- rbindlist(DUP_dt)
toc()


# exp5

tic()
path <- "//VM-COCHES-01/ExperimentosR"
files <- list.files(path, full.names = TRUE)

year <- "2023"
month <- "07"
day <- "02"
dia_analisis <- paste0(year, "-", month, "-", day)
dia_analisis <- as_date(dia_analisis)
files_filtrado <- files[grepl(dia_analisis, files)]
horas_extras <- list(
  paste0(path, "/", dia_analisis - 1, " 23.txt"),
  paste0(path, "/", dia_analisis - 1, " 24.txt"),
  paste0(path, "/", dia_analisis + 1, " 00.txt"),
  paste0(path, "/", dia_analisis + 1, " 01.txt")
)
files_filtrado <- append(files_filtrado, horas_extras)

jsonlist <- list()
j <- 1
for (i in files_filtrado) {
  json <- stream_in(file(i))
  json <- flatten(json)
 jsonlist[[j]] <- json
 j <- j + 1
}
DUP_dt <- lapply(jsonlist, as.data.frame)
DUP_dt <- rbindlist(DUP_dt)
toc()



head(json)















#---------------------------------------------
# LECTURA DE DATOS ----------------------------------
path <- "C:/Users/REstevez/Documents/Análisis/Tapas Combustible/Datos/driveup_data_2023_07_13.csv"
# LECTURA DATOS DE DRIVE UP. SE UTILIZA FREAD DEL PAQUETE DATA.TABLE POR SER LA MAS RAPIDA.
raw_data <- fread(path, sep = ";")


# LECTURA DATOS DE CHASIS Y LINEA DE CADA FICHA
Listado_Fichas <- read_csv("Listado Fichas.csv")
# SE EXTRAE LA PRIMERA LETRA DE LA VARIABLE FICHA
# LA FICHA EN ESTA TABLA SE REPRESENTA DE LA SIGUIENTE FORMA:
# EL COCHE NUMERO 4000 FIGURA EN LA TABLA COMO F4000
# SE DEJAN SOLO LOS NUMEROS
Listado_Fichas$Ficha <- str_sub(Listado_Fichas$Ficha, -4, -1)
install.packages("languageserver", repos = "https://github.com/REditorSupport/languageserver/")

Listado_Fichas$Ficha <- as.integer(Listado_Fichas$Ficha)
colnames(Listado_Fichas) <- c("Ficha", "Chasis", "Linea")

# LECTURA DEL ARCHIVO LÍMITES DE VELOCIDAD DE CADA LÍNEA
limites_velocidad <- fread("limites_velocidad.txt")
# SE AGREGA UN 10% DE TOLERANCIA
limites_velocidad$Vel_max <- limites_velocidad$Vel_max * 1.1
colnames(limites_velocidad) <- c("Linea", "Vel_max")

# SE UNEN A LOS DATOS DE POSICIONAMIENTO LOS LÍMITES DE VELOCIDAD Y LA LÍNEA
# DEL COCHE
raw_data <- left_join(raw_data, Listado_Fichas, by = c("Ficha" = "Ficha"))
raw_data <- left_join(raw_data, limites_velocidad, by = c("Linea" = "Linea"))
# ESTO TRAE A LOS DATOS DE LA TELEMEDICION LA LINEA A LA QUE PERTENECE LA UNIDAD Y
# EL LIMITE DE VELOCIDAD QUE TIENE ASOCIADO ESA LINEA

# LECTURA DATOS DE TIPOS DE CHASIS Y TAMAÑO DE TANQUE DE COMBUSTIBLE
Tipos_de_chasis <- read_excel("Tipos de chasis.xlsx")
Tipos_de_chasis <- Tipos_de_chasis[, c(3, 6, 18)]
colnames(Tipos_de_chasis) <- c("Chasis", "Combustible", "Tanque")


# COORDEBADAS DE LOS TALLERES
# lECTURA DE LAS COORDENADAS DE CADA TALLER DONDE SE CARGA COMBUSTIBLE
COORD_TALLERES <- fread("COORDENADAS TALLERES.csv", sep = ";")


# TRANSFORMA DATOS INCOHERENTES EN NA - LEVEL = 0% Y LEVEL = 102%
raw_data <- raw_data %>%
  mutate(FuelLevel = na_if(FuelLevel, 0))

raw_data <- raw_data %>%
  mutate(FuelLevel = na_if(FuelLevel, 102))

raw_data$FuelLevel[raw_data$FuelLevel < 0] <- NA

# TRANSFORMACION DE DATOS -----------------------------

# TRANSFORMA LOS DATOS DE CARACTERES EN FECHAS Y CALCULA HORAS Y MINUTOS
# (HUBIERA SIDO CONVENIENTE COLOCAR ESTAS OPERACIONES DENTRO DE LA FUNCION MUTATE)
raw_data$FechaLocal <- ymd_hms(raw_data$FechaLocal)
raw_data$Fecha <- as.Date(raw_data$FechaLocal)
raw_data$Hora <- hour(raw_data$FechaLocal)
raw_data$Minutos <- minute(raw_data$FechaLocal)


# LISTADO DE FICHAS QUE NO REPORTAN DATOS EN EL NIVEL DE COMBUSTIBLE
nivel_roto <- raw_data %>%
  group_by(Ficha) %>%
  summarise(cantidad_datos = prop_complete(FuelLevel)) %>%
  filter(cantidad_datos == 0) %>%
  select(c(-cantidad_datos))
# SE CREA UN DATAFRAME CON EL LISTADO COCHES QUE NO REPORTAN VALORES DEL NIVEL DE TANQUE
# ESTE LISTADO SIRVE PARA IDENTIFICAR LOS COCHES A LOS CUALES SE LES DEBE REPARAR EL NIVEL


# SE CALCULA LA DIFERENCIA DE TIEMPO ENTRE PUNTOS, LA DISTANCIA RECORRIDA ENTRE PUNTOS,
# LA DIFERENCIA EN EL NIVEL DE COMBUSTIBLE Y UNA DIFERENCIA ACUMULADA EN LOS ULTIMOS 20 PUNTOS
# SE APLICA TAMBIEN LA FUNCION "CORRECCION" QUE CORRIGE VALORES ERRONEOS DEL SENSOR DE NIVEL
# LA DISTANCIA ENTRE PUNTOS SE CALCULA A PARTIR DE LA FUNCION DISTGEO DEL PAQUETE GEOSPHERE
# PASANDO LOS VALORES DE LATITUD Y LONGITUD DE CADA PUNTO.

raw_data <- raw_data %>%
  arrange(Ficha, FechaLocal) %>%
  group_by(Ficha) %>%
  mutate(
    diff_tiempo = c(0, diff(as.numeric(FechaLocal))),
    Delta = c(0, diff(FuelLevel)),
    Suma_delta = roll_sum(Delta, 30, min_obs = 1),
    Distancia = head(c(0, distGeo(cbind(Lng, Lat))), n = -1),
    litros_corregidos = CORRECCION(Ficha, FuelLevel, Lat, Lng, Suma_delta, Delta),
    litros_corregidos = coalesce(litros_corregidos, FuelLevel)
  )

# VALORES ATÍPICOS DE POSICIONAMIENTO QUE INDICAN QUE EL COCHE RECORRIÓ UNA DISTANCIA
# A UNA VELOCIDAD MEDIA MAYOR A 120 KM/H SE CONSIDERAN 0. TENER EN CUENTA QUE LOS
# PUNTOS SON CADA 10 SEGUNDOS LO QUE GENERA QUE ELIMINAR UN DATO NO SEA DE GRAN RELEVANCIA
# A VECES LOS DATOS ERRONEOS SE PRODUCEN AL PASAR DEBAJO DE UN PUENTE POR EJEMPLO.
raw_data$Distancia <- if_else((raw_data$Distancia / raw_data$diff_tiempo * 3.6) > 120, 0, raw_data$Distancia)

# SE CALCULA LA CANTIDAD DE KM RECORRIDAS EN EL DÍA
cantidad_km <- raw_data %>%
  group_by(Ficha) %>%
  summarise(KM_recorridos = sum(Distancia, na.rm = TRUE) / 1000)

# DEBIDO A LA GRAN DISPERSIÓN QUE PRESENTAN LOS DATOS DE NIVEL DEL TANQUE
# SE DECIDEN AGRUPAR PARA CONSEGUIR UNA VARIABLE MÁS UNIFORME.
# SE AGRUPAN LOS DATOS CADA 2 MINUTOS Y SE CALCULA LA MEDIA DEL NIVEL

raw_data$Intervalo_2 <- floor(raw_data$Minutos / 2) * 2
# EN LA VARIABLE INTERVALO_2 SE REGISTRAN LOS MINUTOS DE 2 EN 2 DE CADA OBSERVACION

# SE AGRUPAN LOS DATOS CADA 2 MINUTOS
# SE TOMA EL PROMEDIO DEL NIVEL DEL TANQUE
# SE TOMA LA ULTIMA COORDENADA DE POSICIONAMIENTO
# LA SUMA DE LA DISTANCIA RECORRIDA
# LA VELOCIDAD MAXIMA


consolidado <- raw_data %>%
  arrange(Ficha, Fecha, Hora, Intervalo_2) %>%
  group_by(Ficha, Fecha, Hora, Intervalo_2) %>%
  summarise(
    Nivel = mean(litros_corregidos, na.rm = TRUE),
    Distancia = sum(Distancia, na.rm = TRUE),
    diff_tiempo = sum(diff_tiempo, na.rm = TRUE),
    LAT = last(Lat),
    LONG = last(Lng),
    Linea = first(Linea),
    Vel_max = max(Vel_max, na.rm = TRUE)
  )

# ESTO GENERA QUE LA SEÑAL DEL NIVEL SEA MUCHO MAS UNIFORME Y FLUCTUE MENOS.
# AUN ASÍ SE LE APLICARA UNA MEDIA MOVIL PARA DISMINUIR SU FLUCTUACION

# SE CALCULA LA VELOCIDAD MEDIA EN EL LAPSO DE LOS 2 MINUTOS.
# SI HAY VALORES NA SE LOS COMPLETA CON 0.
consolidado$Velocidad_media <- consolidado$Distancia / consolidado$diff_tiempo * 3.6
consolidado$Velocidad_media <- coalesce(consolidado$Velocidad_media, 0)

# SE CALCULA LA DIFERENCIA DE NIVEL ENTRE PUNTOS Y LA SUMA ACUMULADA DE
# LOS ULTIMOS 20 PUNTOS

consolidado <- consolidado %>%
  arrange(Ficha, Fecha, Hora, Intervalo_2) %>%
  group_by(Ficha, Fecha, Hora, Intervalo_2) %>%
  mutate(
    Delta = append(0, diff(Nivel)),
    Delta = coalesce(Delta, 0),
    Suma_delta = roll_sum(Delta, 20, min_obs = 1)
  )


# SE DA EL FORMATO DATETIME A UNA VARIABLE CONSTITUIDA POR LA FECHA Y LA HORA CADA 2 MINUTOS
consolidado$Fecha_Hora <- paste(consolidado$Fecha, consolidado$Hora, consolidado$Intervalo_2, sep = ":")
consolidado$Fecha_Hora <- ymd_hm(consolidado$Fecha_Hora)
# (NUEVAMENTE MEJOR USAR UN MUTATE)

# SE CALCULAN LOS PUNTOS DONDE HUBO EXCESOS DE VELOCIDAD.
# SE TOMA UN 10% DE TOLERANCIA
# QUEDA REGISTRO DE LA FICHA, LA HORA, LA LINEA, LA POSICION Y LA CANTIDAD DE TIEMPO
# QUE SE ENCONTRO EXCEDIDA DE VELOCIDAD
# DEBIDO A QUE LOS DATOS ESTÁN AGRUPADOS CADA 2 MINUTOS EL TIEMPO MINIMO QUE SE ENCONTRÓ EN EXCESO NO SERÁ MENOR A 2 MINUTOS.
# SE RECUERDA QUE LOS DATOS QUE TENÍAN VELOCIDADES ENTRE PUNTOS MAYORES A 120 KM/H SE CONSIDERABAN ERRONEOS
# Y CONSIDERABA QUE EL COCHE NO HABIA RECORRIDO DISTANCI ALGUNA.
exceso <- consolidado %>%
  filter(Velocidad_media > Vel_max) %>%
  group_by(Ficha, Fecha) %>%
  summarise(
    Linea = first(Linea),
    Velocidad_maxima = max(Velocidad_media, na.rm = TRUE),
    Fecha_Hora = Fecha_Hora[min(which(Velocidad_media == Velocidad_maxima))],
    LAT = LAT[min(which(Velocidad_media == Velocidad_maxima))],
    LONG = LONG[min(which(Velocidad_media == Velocidad_maxima))],
    Minutos_en_exceso = sum(diff_tiempo, na.rm = TRUE) / 60,
    Ficha = as.character(min(Ficha)),
    Vel_max = max(Vel_max, na.rm = TRUE) / 1.1
  )

# SE APLICA UNA MEDIA MOVIL PARA DISMINUIR EL RUIDO EN LOS DATOS.
# sE TOMAN LOS 5 PUNTOS ANTERIORES Y POSTERIORES.
# SE UTILIZA LA FUNCION APLICAR_FILTRO
puntos_filtro <- 5

filtro <- rep(1 / (2 * puntos_filtro + 1), (2 * puntos_filtro + 1))

consolidado <- consolidado %>%
  arrange(Ficha, Fecha_Hora) %>%
  filter(!is.na(Nivel)) %>%
  group_by(Ficha) %>%
  mutate(litros_filtro = APLICAR_FILTRO(Nivel))

# SE CALCULA NUEVAMENTE LA DIFERENCIA Y LA SUMA ACUMULADA, Y SE
# REALIZA LA MEDIA MOVIL A LA SUMA ACUMULADA TAMBIÉN
# LA FLUCTACION DE LAS VARIABLES DEL NIVEL ES GRANDE Y SE DEBEM SUAVIZAR PARA PODER IDENTIFICAR
# CORRECTAMENTE LAS CARGAS DE COMBUSTIBLE
consolidado <- consolidado %>%
  arrange(Ficha, Fecha, Hora, Intervalo_2) %>%
  group_by(Ficha) %>%
  mutate(
    Delta = append(0, diff(litros_filtro)),
    Sum_delta = roll_sum(Delta, 15, min_obs = 1),
    Consumo = -3 * Delta / Distancia,
    Sum_delta_filtro = APLICAR_FILTRO(Sum_delta)
  )

# A CONTINUACION SE BUSCARAN LAS BAJAS ABRUPTAS EN EL NIVEL DE COMBUSTIBLE.
# ESTAS BAJAS NO CORRESPONDERIAN A QUE EL COMBUSTIBLE FUERA CONSUMIDO EN EL MOTOR,
# SI NO A QUE SE PUEDE HABER LLEGADO A EXTRAER COMBUSTIBLE DEL COCHE
# SE INICIAN VECTORES PARA IDENTIFICACION DE PERDIDAS DE COMBUSTIBLE
# (CONSUMOS REPENTINOS) Y CARGAS DE COMBUSTIBLE
cargas <- c()
fecha_carga <- c()
fecha_hora_carga <- c()
taller <- c()
ficha_carga <- c()
recorrido <- c()
lts_consumidos <- c()

cant_perdida <- c()
fecha_perdida <- c()
fecha_hora_perdida <- c()
ficha_perdida <- c()
Lat_perdida <- c()
Long_perdida <- c()

# SE CALCULAN LOS UNICOS DE CADA COCHE Y FECHA
vector_fichas <- unique(consolidado$Ficha)
vector_fechas <- unique(consolidado$Fecha)

# SE UTILIZA UN LOOP FOR. SE PUEDE MEJORAR LA PERFORMANCE USANDO MUTATE O
# SUMMARISE EN UN GROUP_BY PERO POR FALTA DE TIEMPO NO SE DESARROLLÓ
for (coche in vector_fichas) {
  # FILTRADO DE DATOS POR COCHE
  df <- consolidado %>%
    arrange(Ficha, Fecha_Hora) %>%
    filter(Ficha == coche)

  # SE IDENTIFICAN MINIMOS DE CONSUMOS A TRAVES DE LA FUNCION FINDPEAKS
  # SE UTILIZA LA VARIABLE SUM_DELTA CON LA SUMA ACUMULADA DE LA DIFERENCIA
  # DE NIVEL ENTRE OBSERVACIONES. SE MULTIPLICAN LOS VALORES POR -1 PARA ENCONTRAR LOS MINIMOS.
  # PARA CONSIDERAR UNA BAJADA EN EL NIVEL DEL TANQUE LA BAJADA ACUMULADA DEBE SER DEL 10%
  perdidas <- findpeaks((df$Sum_delta * (-1)), minpeakdistance = 50, minpeakheight = 10)
  indice_perdidas <- perdidas[, 3]

  # SE REGISTRAN LOS VALORES ENCONTRADOS EN VECTORES.
  # LOS TANQUES DE COMBUSTIBLE TIENEN 300 LTS, POR LO QUE SE MULTIPLICA POR 3 PARA PASAR
  # DE UNA VARIABLE CUYO MAXIMO 100 (%) A 300 (LTS)

  cant_perdida <- append(cant_perdida, (perdidas[, 1] * -3))
  fecha_perdida <- append(fecha_perdida, df$Fecha[indice_perdidas])
  fecha_hora_perdida <- append(fecha_hora_perdida, df$Fecha_Hora[indice_perdidas])
  ficha_perdida <- append(ficha_perdida, df$Ficha[indice_perdidas])
  Lat_perdida <- append(Lat_perdida, df$LAT[indice_perdidas])
  Long_perdida <- append(Long_perdida, df$LONG[indice_perdidas])
  vector_suma_delta <- as.vector(df$Sum_delta_filtro)
  vector_suma_delta <- append(
    vector_suma_delta,
    (df$Sum_delta_filtro[nrow(df)] - 1)
  )

  # SE IDENTIFICAN CARGAS (SUBIDAS DE NIVEL)
  # SE CONSIDETAN LAS CARGAS MAYORES A UNA SUBIDA DEL 10%
  picos <- findpeaks(vector_suma_delta, minpeakdistance = 50, minpeakheight = 10)
  taller_carga <- c()
  cant_cargas <- nrow(picos)
  # SE TOMA COMO PUNTO DE LA CARGA UN VALOR MEDIO ENTRE LA POSICION DONDE COMIENZA
  # A AUMENTAR EL NIVEL Y EL MOMENTO EN QUE ALCANZA EL MAXIMO.
  # ESTO SE DEBE A QUE EL A VECES ENTRA AL TALLER A CARGAR Y LUEGO SALE EN UN PERIODO CORTO DE TIEMPO
  # MIENTRAS ESTA EL MOTOR DETENIDO NO HAY REGISTROS DE LA TELEMEDICION Y EL SALTO DEL NIVEL ES DISCRETO.
  # COOMO SE DEBE REGISTRAR EN QUE TALLER SE PRODUJO LA CARGA Y SE CONTROLA NUEVAMENTE QUE EL COCHE ESTABA EN EL TALLER
  # AL MOMENTO DE LA CARGA (ESTO TAMBIEN ASEGURA QUE SE HAYA REGISTRADO CORRECTAMENTE LA HORA DE LA CARGA).
  indices_cargas <- round((0.6 * picos[, 2] + 0.4 * picos[, 3]))


  # SE VERIFICA QUE LAS CARGAS HAYAN SIDO DENTRO DE TALLERES.
  # SE DESESTIMAN AUMENTOS DE NIVEL FUERA DE LOS TALLERES.

  if (!is.null(picos)) {
    z <- 0
    picos <- as.data.frame(picos)

    for (j in c(1:cant_cargas)) {
      chequeo_taller <- as.numeric(DISTANCIA_TALLERES(
        df$LAT[indices_cargas[j - z]],
        df$LONG[indices_cargas[j - z]]
      )[1])
      if (chequeo_taller > 500) {
        picos <- picos[-c(j - z), ]
        indices_cargas <- indices_cargas[-c(j - z)]
        cant_cargas <- cant_cargas - 1
        z <- z + 1
      }
    }
    # SE REGISTRAN LOS NOMBRES DE LOS TALLERES DONDE SE CARGÓ, LAS FICHAS QUE CARGARON,
    # LA HORA, LOS LITROS.
    # SE ACLARA QUE CADA COCHE PUEDE CARGAR MAS UNA VEZ AL DIA Y EN DISTINTOS TALLERES.
    if (!nrow(picos) == 0) {
      taller_carga <- c()

      for (i in c(1:cant_cargas)) {
        # SE REGISTRA EL TALLER DONDE SE REALIZO LA CARGA
        taller_carga <- append(
          taller_carga,
          DISTANCIA_TALLERES(
            df$LAT[indices_cargas[i]],
            df$LONG[indices_cargas[i]]
          )[2]
        )


        taller <- append(taller, taller_carga[i])
        # SE MULTIPLICA POR 3 PARA PASAR DE VALORES DE PORCENTAJE DE LLENADO A LITROS
        cargas <- append(cargas, picos[i, 1] * 3)
        fecha_carga <- append(fecha_carga, df$Fecha[indices_cargas[i]])
        fecha_hora_carga <- append(fecha_hora_carga, df$Fecha_Hora[indices_cargas[i]])
        ficha_carga <- append(ficha_carga, df$Ficha[1])
        # SE CALCULAN LOS KM RECORRIDOS ENTRE CADA CARGA Y TAMBIEN LOS LITROS CONSUMIDOS.
        # LOS LITROS CONSUMIDOS SE OBTIENEN DE LA DIFERENCIA ENTRE EL NIVEL DE LA CARGA ANTERIOR
        # Y EL NIVEL AL COMIENZO DE LA CARGA ACTUAL

        if (i == 1) {
          recorrido <- append(
            recorrido,
            sum(df$Distancia[df$Fecha_Hora < df$Fecha_Hora[indices_cargas[i]]],
              na.rm = TRUE
            ) / 1000
          )
          lts_consumidos <- append(
            lts_consumidos,
            sum(df$Delta[df$Fecha_Hora < df$Fecha_Hora[indices_cargas[i]]],
              na.rm = TRUE
            ) * -3 + picos[i, 1] * 3
          )
        } else {
          recorrido <- append(
            recorrido,
            sum(
              df$Distancia[df$Fecha_Hora < df$Fecha_Hora[indices_cargas[i]] &
                df$Fecha_Hora > df$Fecha_Hora[indices_cargas[i - 1]]],
              na.rm = TRUE
            ) / 1000
          )
          lts_consumidos <- append(
            lts_consumidos,
            sum(
              df$Delta[df$Fecha_Hora < df$Fecha_Hora[indices_cargas[i]] &
                df$Delta > df$Fecha_Hora[indices_cargas[i - 1]]],
              na.rm = TRUE
            ) * -3 + picos[i, 1] * 3
          )
        }
        # SI SE ENCONTRO 1 SOLA CARGA SE REGISTRAN LOS VALORES
        if (i == cant_cargas) {
          taller <- append(taller, NA)
          cargas <- append(cargas, NA)
          fecha_carga <- append(fecha_carga, min(df$Fecha))
          fecha_hora_carga <- append(fecha_hora_carga, min(df$Fecha_Hora))
          ficha_carga <- append(ficha_carga, df$Ficha[1])
          recorrido <- append(
            recorrido,
            sum(df$Distancia[df$Fecha_Hora > df$Fecha_Hora[indices_cargas[i]]],
              na.rm = TRUE
            ) / 1000
          )
          lts_consumidos <- append(
            lts_consumidos,
            sum(df$Delta[df$Fecha_Hora > df$Fecha_Hora[indices_cargas[i]]],
              na.rm = TRUE
            ) * -3 + picos[i, 1] + 3
          )
        }
      }
    }
  } else {
    # SI NO SE REGISTRAN CARGAS DE COMBUSTIBLE EN EL DIA (ALGO TAMBIEN MUY COMUN)
    # SE REGISTRAN LOS KM RECORRIDOS Y LOS LITROS CONSUMIDOS
    taller <- append(taller, NA)
    cargas <- append(cargas, NA)
    fecha_carga <- append(fecha_carga, max(df$Fecha))
    fecha_hora_carga <- append(fecha_hora_carga, max(df$Fecha_Hora))
    ficha_carga <- append(ficha_carga, df$Ficha[1])
    recorrido <- append(
      recorrido,
      sum(df$Distancia, na.rm = TRUE) / 1000
    )
    lts_consumidos <- append(
      lts_consumidos,
      sum(df$Delta, na.rm = TRUE) * -3
    )
  }
}

# PUEDE HABER CÁLCULOS REALIZADOS QUE SE NO SE UTILICEN EN ESTA APP, PERO SE
# UTILIZARÁN PARA OTROS ANÁLISIS DE LA EMPRESA.

# SE REGISTRAN LAS CARGAS EN DATAFRAMES
listado_cargas <- data.frame(
  ficha_carga, taller, fecha_carga, fecha_hora_carga, cargas,
  recorrido, lts_consumidos
)

# SE REGISTRAN LAS BAJAS ABRUPTAS DE COMBUSTIBLE EN DATAFRAMES
posibles_perdidas <- data.frame(
  ficha_perdida, fecha_perdida, fecha_hora_perdida,
  cant_perdida, Lat_perdida, Long_perdida
)

colnames(posibles_perdidas) <- c("Ficha", "Fecha", "Fecha_Hora", "Litros", "LAT", "LONG")
colnames(listado_cargas) <- c(
  "Ficha", "Taller", "Fecha", "Fecha_Hora", "Litros_cargados",
  "Recorridos", "Litros_consumidos"
)

# SE REALIZA UN CHEQUEO DE QUE LAS PERDIDAS SEAN GENIUNAS Y NO BAJAS
# REPENTINAS DEL NIVEL POR EL MOVIMIENTO DEL COCHE
# EN ESTE LOOP SE REVISA QUE ESTA BAJA EN EL NIVEL DE COMBUSTIBLE SE MANTENGA
# EN UN PERIODO DE 1HR Y MEDIA. ESTO SE HACE PORQUE EL COCHE PUEDE QUEDAR ESTACIONADO
# EN UNA RAMPA, AL ESTAR INCLINADO EL COCHE EL NIVEL DE COMBUSTIBLE REGISTRADO POR EL FLOTANTE
# DISMINUYE, PERO SE RESTITUYE SI EL COCHE VUELVE A ESTAR EN POSICION HORIZONTAL.
# ES POR ESTO QUE SE SUMAN LAS DIFERENCIAS DE NIVEL ENTRE LAS OBSEVACIONES
# 45 MINUTOS ANTES Y 45 MINUTOS DESPUES
# DE LA HORA DEL REGISTRO DE LA CAIDA DEL NIVEL DE COMBUSTIBLE

if (nrow(posibles_perdidas) > 0) {
  posibles_perdidas$Confirmado <- NA
  posibles_perdidas$Consumo_hora <- NA

  for (i in c(1:nrow(posibles_perdidas))) {
    inicio <- posibles_perdidas$Fecha_Hora[i] - minutes(45)
    fin <- posibles_perdidas$Fecha_Hora[i] + minutes(45)

    analizar <- consolidado %>%
      dplyr::filter(
        Ficha == posibles_perdidas$Ficha[i],
        Fecha_Hora > inicio,
        Fecha_Hora < fin
      ) %>%
      select(Ficha, Delta, Distancia)
    # SE CALCULA LA SUMA Y LOS KILOMETROS RECORRIDOS
    delta_combustible <- sum(analizar$Delta, na.rm = TRUE)
    km_recorridos <- sum(analizar$Distancia, na.rm = TRUE) / 1000
    # SE CALCULA EL CONSUMO MEDIO EN EL PERIODO ANALIZADO.
    # EL CONSUMO SE CALCULA COMO LTS / 100 KM. ES POR ESTO QUE SE MULTIPLICA POR 300
    # AL PORCENTAJE DE LITROS
    posibles_perdidas$Consumo_hora[i] <- abs(300 * delta_combustible / km_recorridos)
    # SI EL CONSUMO ES MAYOR A UN VALOR ABSURDO COMO CONSUMIR MAS DE 120 LTS EN 100 KM
    # SE DEJA REGISTRO DE QUE SE DEBE ANALIZAR ESTA POSIBLE PERDIDA DE COMBUSTIBLE.
    if (posibles_perdidas$Consumo_hora[i] > 120 | is.na(posibles_perdidas$Consumo_hora[i] > 120)) {
      posibles_perdidas$Confirmado[i] <- "SI"
    }
  }
}

# SI HAY ALGUN VALOR MAYOR 100% DEL NIVEL DEL TANQUE SE LES ASIGNA NIVEL 100%
# SE MULTIPLICAN LOS LITROS POR 3 YA QUE LOS TANQUES TIENEN 300 LITROS

consolidado$litros_filtro[consolidado$litros_filtro > 100] <- 100
consolidado$litros_filtro <- 3 * consolidado$litros_filtro

# SE CALCULA LA VELOCIDAD MEDIA DE LOS COCHES ENTRE LAS OBSERVACIONES
raw_data$Velocidad <- raw_data$Distancia / raw_data$diff_tiempo * 3.6
raw_data$Velocidad <- coalesce(raw_data$Velocidad, 0)

# SE CALCULAN METRICAS DIARIAS Y HORARIAS DE CADA UNIDAD
# ESTOS SON KM RECORRIDOS, DIF DE LITROS Y VELOCIDAD MAXIMA
datos_diarios_km <- consolidado %>%
  group_by(Ficha, Fecha, Linea) %>%
  summarise(
    KM = sum(Distancia, na.rm = TRUE) / 1000,
    Litros_consumidos = -3 * sum(Delta, na.rm = TRUE), # ESTO SE PUEDE CALCULAR RESTANDO INICIAL - FINAL
    Velocidad_Maxima = max(Velocidad_media, na.rm = TRUE)
  )


# DATOS DIARIOS DE LITROS
datos_diarios_lts <- listado_cargas %>%
  group_by(Ficha, Fecha) %>%
  summarise(
    Taller_primera_carga = first(Taller),
    Litros_carga = sum(Litros_cargados, na.rm = TRUE)
  )

datos_diarios <- dplyr::full_join(datos_diarios_km, datos_diarios_lts,
  by = c("Ficha" = "Ficha", "Fecha" = "Fecha")
)

# SE FUCIONAN LOS DATOS DIARIOS DE KM Y LITROS.
# SE SUMAN A LOS LITROS CALCULADOS EN datos_diarios_km LAS
# CARGAS REALIZADOS CALCULANDO EL TOTAL ESTIMADO DE LITROS CONSUMIDOS
datos_diarios <- datos_diarios %>%
  mutate(
    Litros_carga = coalesce(Litros_carga, 0),
    Litros_consumidos = Litros_consumidos + Litros_carga
  )

# SE CALCULA UN CONSUMO ESTIMADO
datos_diarios$Consumo <- datos_diarios$Litros_consumidos / datos_diarios$KM * 100

datos_diarios <- datos_diarios %>%
  filter(!is.na(Consumo)) %>%
  mutate(Fecha = as.Date(Fecha))

# SE CALCULAN LOS LITROS CONSUMIDOS POR HORA Y EL CONSUMO MEDIO PARA MOSTRAR EN LA APP
# ESTAS ESTIMACIONES SON POCO PRECISAS.
datos_horario <- consolidado %>%
  arrange(Ficha, Fecha, Hora) %>%
  group_by(Ficha, Fecha, Hora) %>%
  summarise(
    Litros_consumidos = -3 * sum(Delta, na.rm = TRUE),
    Distancia_recorrida = sum(Distancia, na.rm = TRUE) / 1000,
    LAT = last(LAT),
    LONG = last(LONG)
  )

datos_horario <- datos_horario %>%
  mutate(
    Consumo_medio = Litros_consumidos / Distancia_recorrida * 100,
    Consumo_medio = coalesce(Consumo_medio, 0)
  )

datos_horario$Consumo_medio[is.infinite(datos_horario$Consumo_medio)] <- NA

datos_horario$Consumo_medio[datos_horario$Litros_consumidos == 0 &
  datos_horario$Distancia_recorrida == 0] <- 0

listado_cargas$Consumo <- listado_cargas$Litros_consumidos / listado_cargas$Recorridos * 100

listado_cargas$Consumo <- coalesce(listado_cargas$Consumo, 0)


# CALCULO DEL TIEMPO EN RALENTI DE CADA UNIDAD
# EN RALENTI EL COCHE DEBE ESTAR DETENIDO Y CON EL MOTOR EN MARCHA.
# ES POR ESTO QUE LA DISTANCIA RECORRIDA DEBE SER 0, LA VELOCIDAD DEL MOTOR > 0
# PARA IDENTIFICAR SI EL MOTOR ESTA EN MARCHA Y SIN ESTAR ACELERANDO.

# EL RALENTI SE CALCULA EN LA CANTIDAD DE MINUTOS POR DIA EN RALENTI.
# UN COLECTIVO EN RALENTI CONSUME ENTRE 1.5 Y 2 LITROS POR HORA, ES UNA VARIABLE
# IMPORTANTE DE CONTROLAR.
ralenti <- raw_data %>%
  filter(Distancia == 0 & EngineSpeed > 0 & AcceleratorPedalPosition == 0) %>%
  group_by(Ficha, Fecha, Linea) %>%
  summarise(Minutos_ralenti = sum(diff_tiempo) / 60)

# CALCULO DE VELOCIDAD MEDIA CUANDO EL COCHE SE ENCONTRABA EN MOVIMIENTO.
Velocidad <- raw_data %>%
  filter(Distancia > 0 & EngineSpeed > 0) %>%
  group_by(Ficha, Fecha, Linea) %>%
  summarise(
    Metros = sum(Distancia, na.rm = TRUE),
    Segundos = sum(diff_tiempo, na.rm = TRUE),
    Velocidad = Metros / Segundos * 3.6
  )

fichas_seleccion <- list(
  labels = vector_fichas,
  Fichas = vector_fichas
)
