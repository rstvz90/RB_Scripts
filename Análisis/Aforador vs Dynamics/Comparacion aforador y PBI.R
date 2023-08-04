# CARGA BIBLIOTECAS
library(readxl)
library(readr)
library(hms)        
library(lubridate)
library(dplyr)
library(tidyr)
library(clipr)

options(digits = 7)
options(OutDec= ",")

gc()
rm(list = ls())

# POWER BI ----------------------------------------- --------
  
# LECTURA DE DATOS

PBI_combustible <-  read_delim("//sm-public/Public/Mantenimiento/REstevez/Para Borrar/PBI combustible.csv", delim = ",")
#PBI_combustible <- PBI_combustible[PBI_combustible$Texto == "A",]

# TRANSFORMACION DE DATOS

PBI_combustible$FechaCorregida <- ymd(PBI_combustible$FechaCorregida)
PBI_combustible$CodAlmacen[PBI_combustible$CodAlmacen == "RAC"] <- "BAI"
PBI_combustible$FechaCorregida[PBI_combustible$Texto == "M"] <- PBI_combustible$FechaCorregida[PBI_combustible$Texto == "M"] + days(1)
PBI_combustible$`Fecha Hora consumo` <- ymd_hms(PBI_combustible$`Fecha Hora consumo`)
PBI_combustible$Hora <- format(PBI_combustible$`Fecha Hora consumo`, format = "%H:%M")
# PBI_combustible <- PBI_combustible[PBI_combustible$FechaCorregida == "2023/02/23",]
PBI_combustible$ProductName[PBI_combustible$ProductName == "GASOIL INFINIA DIESEL"] <-  "INFINIA DIESEL"

# TABLAS PBI

cargas_PBI <- PBI_combustible %>% 
  group_by(CodAlmacen, Ficha, FechaCorregida) %>% 
  summarise(litros = sum(ConsumoTotal,na.rm = TRUE))

resumen_PBI <- PBI_combustible %>% 
  group_by(CodAlmacen, FechaCorregida) %>% 
  summarise(DESPACHADO_D365 = sum(ConsumoTotal,na.rm = TRUE))

manuales_PBI <- PBI_combustible %>%
  filter(Texto == "M") %>% 
  group_by(CodAlmacen, FechaCorregida) %>% 
  summarise(DESPACHADO_D365 = sum(ConsumoTotal,na.rm = TRUE))
  
combustible <- PBI_combustible %>% 
  group_by(ProductName) %>%
  summarise(DESPACHADO_D365 = sum(ConsumoTotal,na.rm = TRUE))
            
# DATATANK ------------------------------------------------ ----------------
 
# LECTURA DE DATOS

datatank <-  read_excel("//sm-public/Public/Mantenimiento/REstevez/Para Borrar/datatank.xlsx")

# TRANSFORMACION DE DATOS
datatank$FECHA <- as.Date(dmy_hms(datatank$FECHAINICIO))

datatank$TALLER[datatank$TALLER == "BERU"] <- "BER"
datatank$TALLER[datatank$TALLER == "MUN"] <- "CAR"
datatank$TALLER[datatank$TALLER == "STEF"] <- "STE"
datatank$DIA <- lubridate::wday(datatank$FECHA, label = TRUE, abbr = FALSE)
datatank[is.na(datatank)] <- 0


# TABLAS DATATANK
aforador <- datatank %>% 
  group_by(TALLER, FECHA, DIA) %>% 
  summarise(DESPACHADO_AFORADOR = sum(DESPACHADOAFORADOR,na.rm = TRUE))
  
aforador[is.na(aforador)] <- 0
#clipr::write_clip(aforador)
  
# MERGE RESUMEN SEMANAL ---------------------------------------------------------------
  
tabla_comparacion <- full_join(aforador, resumen_PBI, 
                               by = c("TALLER" = "CodAlmacen", "FECHA" = "FechaCorregida"))

#tabla_comparacion$diferencia <- tabla_comparacion$DESPACHADO_AFORADOR - tabla_comparacion$DESPACHADO_d365

tabla_comparacion <- tabla_comparacion %>% 
  arrange(FECHA, TALLER)
tabla_comparacion$DIA <- lubridate::wday(tabla_comparacion$FECHA, label= TRUE, abbr= FALSE)
tabla_comparacion[is.na(tabla_comparacion)] <- 0
# tabla_comparacion <- tabla_comparacion %>%
#   filter(
#     # FECHA < "2023/05/21",
#     # FECHA > "2023/05/01",
#     #TALLER != "STE"
#           )


tabla_comparacion$Diferencia <- tabla_comparacion$DESPACHADO_AFORADOR - tabla_comparacion$DESPACHADO_D365

clipr::write_clip(tabla_comparacion, dec = ",")

sum(tabla_comparacion$Diferencia)

# DATAKRAFT -------------------------------------------- ----
  
  
# LECTURA DATOS 
datakraft <-  read_excel("//sm-public/Public/Mantenimiento/REstevez/Para Borrar/datakraft.xlsx")

datakraft <- datakraft %>% 
  select("CONTROLADOR", "NUM. VEHICULO", "FECHA", "HORA", "LITROS","PRODUCTO")

PBI_combustible_datakraft <-  read_delim("//sm-public/Public/Mantenimiento/REstevez/Para Borrar/PBI combustible DataKraft.csv",delim = ",")
#PBI_combustible <- PBI_combustible

# TRANSFORMACION DE DATOS

PBI_combustible_datakraft$FechaCorregida <- ymd(PBI_combustible_datakraft$FechaCorregida)
PBI_combustible_datakraft$CodAlmacen[PBI_combustible_datakraft$CodAlmacen == "RAC"] <- "BAI"
PBI_combustible_datakraft$FechaCorregida[PBI_combustible_datakraft$Texto == "M"] <- PBI_combustible_datakraft$FechaCorregida[PBI_combustible_datakraft$Texto == "M"] + days(1)
PBI_combustible_datakraft$`Fecha Hora consumo` <- ymd_hms(PBI_combustible_datakraft$`Fecha Hora consumo`)
PBI_combustible_datakraft$Hora <- format(PBI_combustible_datakraft$`Fecha Hora consumo`, format = "%H:%M")
PBI_combustible_datakraft$ProductName[PBI_combustible_datakraft$ProductName == "GASOIL INFINIA DIESEL"] <-  "INFINIA DIESEL"

# PBI_combustible_datakraft <- PBI_combustible_datakraft %>%
#   filter(
#     #FechaCorregida >= "2023/03/14",
#     FechaCorregida <= "2023/04/22",
#     #Texto != "M",
#          )
# TABLAS PBI

cargas_PBI_datakraft <- PBI_combustible_datakraft %>% 
  group_by(CodAlmacen, Ficha, FechaCorregida) %>% 
  summarise(litros = sum(ConsumoTotal,na.rm = TRUE))

resumen_PBI_datakraft <- PBI_combustible_datakraft %>% 
  group_by(CodAlmacen, FechaCorregida) %>% 
  summarise(DESPACHADO_D365 = sum(ConsumoTotal,na.rm = TRUE))

manuales_PBI_datakraft <- PBI_combustible_datakraft %>%
  filter(Texto == "M") %>% 
  group_by(CodAlmacen, FechaCorregida) %>% 
  summarise(DESPACHADO_D365 = sum(ConsumoTotal,na.rm = TRUE))

combustible_datakraft <- PBI_combustible_datakraft %>% 
  group_by(ProductName) %>%
  summarise(DESPACHADO_D365 = sum(ConsumoTotal,na.rm = TRUE))


# TRANSFORMACION DE DATOS

datakraft$FICHA <- paste0("F",datakraft$`NUM. VEHICULO`)

datakraft$FICHA[datakraft$FICHA == "F1002"] <- "GENERICOPILAR"
datakraft$FICHA[datakraft$FICHA == "F1001"] <- "CABINAVGA"

datakraft$LITROS <- as.numeric(datakraft$LITROS)

datakraft$HORA <- ymd_hms(datakraft$HORA)

datakraft$FECHA <- dmy(datakraft$FECHA)
datakraft$FECHA_CORREGIDA <- datakraft$FECHA
datakraft$FECHA_CORREGIDA[hour(datakraft$HORA) < 6] <- datakraft$FECHA[hour(datakraft$HORA) < 6] - days(1)


datakraft$HORA <- format(as.POSIXct(datakraft$HORA), format = "%H:%M")

# TABLAS DATAKRAFT
  
cargas_kraft <- datakraft %>% 
  group_by(FECHA_CORREGIDA, FICHA) %>% 
  summarise(litros_Kraft = sum(LITROS,na.rm = TRUE))
  
resumen_kraft <- datakraft %>% 
  group_by(FECHA_CORREGIDA) %>% 
  summarise(SUMA_Kraft = sum(LITROS,na.rm = TRUE))
  
clipr::write_clip(resumen_kraft)
  
  
# MERGE DYNAMICS Y DATAKRAFT ---------------------------- -----
  
migracion <- full_join(cargas_kraft, cargas_PBI_datakraft, 
                       by = c("FICHA" = "Ficha", "FECHA_CORREGIDA" = "FechaCorregida"))
  
migracion_2 <- full_join(datakraft, PBI_combustible_datakraft, 
                       by = c("FICHA" = "Ficha", "FECHA_CORREGIDA" = "FechaCorregida","HORA" = "Hora"))

# migracion <- migracion %>% filter(FECHA_CORREGIDA < "2023/03/19")

migracion$Diferencia <- coalesce(migracion$litros_Kraft,0) - coalesce(migracion$litros,0)

migracion_2$Diferencia <- coalesce(migracion_2$LITROS,0) - coalesce(migracion_2$ConsumoTotal,0)

migracion_2$Texto[is.na(migracion_2$Texto)] <- "No Migra"
migracion_2$FICHA[migracion_2$FICHA == "FNA"] <- "NO MIGRAN"

#migracion <- migracion[migracion$CodAlmacen == "VCO" & 
#                         migracion$FECHA_CORREGIDA >= "2022-11-22",]

resumen_migracion <- migracion %>% 
  group_by(FICHA,FECHA_CORREGIDA) %>% 
  summarise(Taller = first(CodAlmacen),
            Datakraft = sum(litros_Kraft, na.rm = TRUE),
            PBI = sum(litros, na.rm = TRUE),
            Diferencia = sum(Diferencia, na.rm = TRUE))
  


migracion_todas <-  migracion %>% 
  group_by(CodAlmacen, FICHA, FECHA_CORREGIDA) %>% 
  summarise(Datakraft = sum(litros_Kraft, na.rm = TRUE),
            PBI = sum(litros, na.rm = TRUE),
            Diferencia = sum(Diferencia, na.rm = TRUE),
            Relacion = round(sum(litros_Kraft, na.rm = TRUE)/sum(litros, na.rm = TRUE),2))


total_migracion <- migracion %>% 
  group_by(CodAlmacen, FECHA_CORREGIDA) %>% 
  summarise(Datakraft = sum(litros_Kraft, na.rm = TRUE),
            PBI = sum(litros, na.rm = TRUE),
            Diferencia = sum(Diferencia, na.rm = TRUE))
  
  
# DATAKRAFT - CIERRE TURNO -----

# LECTURA DE DATOS

cierre_turno_datakraft <- read_excel("//sm-public/Public/Mantenimiento/REstevez/Para Borrar/datakraft_cierre_turno.xlsx")

  # TRANSFORMACION DE DATOS
cierre_turno_datakraft$FECHA <- ymd_hms(cierre_turno_datakraft$`FECHA INI TURNO`)
cierre_turno_datakraft$FECHA <- as.Date(cierre_turno_datakraft$FECHA)
# cierre_turno_datakraft$FECHA <- strptime(cierre_turno_datakraft$`FECHA INI TURNO`, format = "%m/%d/%Y")
# cierre_turno_datakraft$FECHA <- format.Date(cierre_turno_datakraft$FECHA, format = "%d/%m/%Y")

cierre_turno_datakraft$PRODUCTO[cierre_turno_datakraft$PRODUCTO == "DIESEL 500"] <-  "GASOIL D500"
cierre_turno_datakraft$Aforador <- cierre_turno_datakraft$AFORADORFIN - cierre_turno_datakraft$AFORADORINI
cierre_turno_datakraft$Diferencia <- cierre_turno_datakraft$`LITROS ASIGNADOS` - cierre_turno_datakraft$Aforador
cierre_turno_datakraft <- cierre_turno_datakraft[cierre_turno_datakraft$FECHA < "2023/02/09",]

sum(cierre_turno_datakraft$Diferencia)
# TABLAS DATAKRAFT - CIERRE TURNO
kraft_turno <- cierre_turno_datakraft %>% 
  group_by(FECHA, PRODUCTO) %>% 
  summarise(LTS_ASIGNADOS = sum(`LITROS ASIGNADOS`,na.rm = TRUE),
            LTS_NO_ASIGNADOS = sum(`LITROS NO ASIGNADOS`,na.rm = TRUE))

fecha_turno <- cierre_turno_datakraft %>% 
  group_by(FECHA) %>% 
  summarise(LTS_ASIGNADOS = sum(`LITROS ASIGNADOS`,na.rm = TRUE),
            LTS_NO_ASIGNADOS = sum(`LITROS NO ASIGNADOS`,na.rm = TRUE))

suma_turno <- cierre_turno_datakraft %>% 
  group_by(PRODUCTO) %>% 
  summarise(LTS_ASIGNADOS = sum(`LITROS ASIGNADOS`,na.rm = TRUE),
            LTS_NO_ASIGNADOS = sum(`LITROS NO ASIGNADOS`,na.rm = TRUE))


tabla <- full_join(resumen_PBI,fecha_turno, by = c("FechaCorregida"= "FECHA"))
tabla_2 <- full_join(combustible,suma_turno, by = c("ProductName" = "PRODUCTO"))
tabla_2$DIF <- tabla_2$DESPACHADO_D365 - tabla_2$LTS_ASIGNADOS

tabla <- tabla[,-1]

tabla[is.na(tabla)] <- 0

tabla$Diferencia <- tabla$DESPACHADO_D365 - tabla$LTS_ASIGNADOS
tabla <- tabla %>% arrange(FechaCorregida)

sum(tabla$Diferencia) + sum(tabla$LTS_NO_ASIGNADOS)

sum(tabla$LTS_ASIGNADOS, na.rm = TRUE) - sum(tabla$DESPACHADO_D365, na.rm = TRUE)
# CLIP -----

clipr::write_clip(tabla)




# Aforador vs Tanque ------------------------------------------------------

cargas$Ficha <- paste0("F",cargas$Ficha)
cargas$Hora_carga <- ymd_hms(cargas$Hora_carga)
cargas_2 <- cargas %>% 
  group_by(Fecha,Ficha) %>% 
  summarise(total_carga = sum(Cant_carga, na.rm = TRUE),
            total_km = sum(Cant_km, na.rm = TRUE))

tabla <- left_join(PBI_combustible,cargas,by = c("FechaCorregida" = "Fecha", 
                                                 "Ficha" = "Ficha")) %>% 
  filter(!is.na(Cant_carga)) %>% 
  mutate(Cant_carga = 300*Cant_carga/100,
         Hora.y = ymd_hms(Hora.y),
         `Fecha Hora consumo` = ymd_hms(`Fecha Hora consumo`))

tabla_2 <- left_join(PBI_combustible,cargas_2,by = c("FechaCorregida" = "Fecha", 
                                                   "Ficha" = "Ficha")) %>% 
  filter(!is.na(total_carga)) %>% 
  mutate(Cant_carga = 300*total_carga/100)

tabla$Dif_litros <- tabla$ConsumoTotal - tabla$Cant_carga
tabla$Dif_hora <- difftime(tabla$Hora.y, tabla$`Fecha Hora consumo`, units = "mins")




# Compraración por Fecha y Combustible --------------------------------------------------

cargas_fecha_comb <- PBI_combustible %>% 
  group_by(CodAlmacen, FechaCorregida, ProductName) %>%
  summarise(LTS = sum(ConsumoTotal, na.rm = TRUE))

datatank$IDMULTITANQUE[datatank$TALLER == "LAQ"] <- ifelse(datatank$IDMULTITANQUE == 1, 
                                                           "GASOIL D500",
                                                           "INFINIA DIESEL")

aforador_comb <- datatank %>% 
  group_by(TALLER, FECHA, DIA, IDMULTITANQUE) %>% 
  summarise(DESPACHADO_AFORADOR = sum(DESPACHADOAFORADOR,na.rm = TRUE))

aforador_comb[is.na(aforador_comb)] <- 0
#clipr::write_clip(aforador)

# MERGE RESUMEN SEMANAL ---------------------------------------------------------------

tabla_comparacion_comb <- full_join(aforador_comb, cargas_fecha_comb, 
                                    by = c("TALLER" = "CodAlmacen", "FECHA" = "FechaCorregida", 
                                           "IDMULTITANQUE" = "ProductName"))

tabla_comparacion_comb <- tabla_comparacion_comb %>% 
  arrange(TALLER,FECHA)
tabla_comparacion_comb$DIA <- lubridate::wday(tabla_comparacion_comb$FECHA, label= TRUE, abbr= FALSE)
tabla_comparacion_comb[is.na(tabla_comparacion_comb)] <- 0
tabla_comparacion_comb <- tabla_comparacion_comb %>%
  filter(
    # FECHA < "2023/04/15",
    # FECHA > "2023/03/25",
    # TALLER != "STE"
    )


tabla_comparacion_comb$Diferencia <- tabla_comparacion_comb$DESPACHADO_AFORADOR - tabla_comparacion_comb$LTS

resumen_comb <- tabla_comparacion_comb %>% 
  group_by(TALLER, IDMULTITANQUE) %>% 
  summarise(DIF = sum(Diferencia, na.rm = TRUE))


write_clip(tabla_comparacion_comb)
