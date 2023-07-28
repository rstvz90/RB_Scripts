# ERRORES DE MIGRACIÓN DE CARGAS, TANTO PARA EL DATAKRAFT COMO EL DYNAMICS
# LUEGO DE CAMBIO DE PC EN EL TALLER EL FIN DE MAYO 2023


# INICIACIÓN --------------------------------------------------------------

library(data.table)
library(readxl)
library(lubridate)
library(openxlsx)
library(rstudioapi)
library(clipr)


rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#setwd("C:/Users/REstevez/Documents/Análisis/Cargas/Moreno Junio 2023")


# Lectura Archivos --------------------------------------------------------

# TIPOS DE CHASIS

{
  tipos_chasis <- read_xlsx("C:/Users/REstevez/Documents/Chasis/Tipos de chasis.xlsx")
  setDT(tipos_chasis)
  tipos_chasis <- tipos_chasis[,.(`Descripción del Vehículo`, `Tipo Combustible`)]
  tipos_chasis <- tipos_chasis[`Descripción del Vehículo` == "MB O500 1726-59",
                               `Descripción del Vehículo` := "BUS MB O500 1726-59",]
  
}

# LISTADO FICHAS
{
  listado_fichas <- fread("C:/Users/REstevez/Documents/Chasis/Listado Fichas.csv")
  listado_fichas <- listado_fichas[,.(Ficha, Chasis)]
  listado_fichas[, Ficha := substr(Ficha, 2, 5)]
  listado_fichas[, Ficha := as.numeric(Ficha)]
  listado_fichas <- listado_fichas[Chasis == "MB O500 1726-59",
                                   Chasis := "BUS MB O500 1726-59",]
}


# DATOS DATAKRAFT
{
  moreno <- read_xlsx("datakraft_moreno.xlsx")
  setDT(moreno)
  col_datakraft <- copy(colnames(moreno))
}

# DATOS TELEMEDICION
{
  telemedicion <- read_xlsx("telemedicion_moreno.xlsx")
  telemedicion <- copy(telemedicion)
  
  setDT(telemedicion)
}

# DATOS REGISTRADOS EN PBI
{
  pbi_moreno <- fread("PBI combustible.csv")
}

# DATOS QUE PASÓ MATÍAS ALVARADO A PARTIR DE LOS EXCEL
{
  cargas_alvarado <- read_xlsx("Registro_Cargas_Alvarado.xlsx")
  setDT(cargas_alvarado)
  col_mati <- copy(colnames(cargas_alvarado))
}

# ID TANQUES 
{
  id_tanques <- read_xlsx("C:/Users/REstevez/Documents/INFORMES MENSUAL/STOCK COMBUSTIBLE/ID TANQUES.xlsx")
  setDT(id_tanques)
}

# Transformación y Resumen de Datos -------------------------------------------------

# DATOS DATA KRAFT 
{
  # Se corrigen los tipos de variales
  # Se cambian los nombres de los productos
  # Se crea el campo Fecha con la fecha corregida
  moreno[,`:=`(FECHA = dmy(FECHA),
               Hora = ymd_hms(HORA),
               LITROS = as.numeric(LITROS)
  )]
  moreno[, Hora := format(Hora, format = "%H:%M:%S")]
  moreno[, Hora := substr(Hora, 1, 5)]  
  moreno[, Fecha_hora := paste(FECHA, Hora)]
  moreno[, Fecha_hora := ymd_hm(Fecha_hora)]

  moreno[,`:=`(rango_hora = hour(HORA))]
  moreno[rango_hora < 6, FECHA := FECHA - days(1)]
  moreno[PRODUCTO == "INFINIA DIESEL", PRODUCTO := "GASOIL INFINIA DIESEL"]
  moreno[PRODUCTO == "DIESEL 500", PRODUCTO := "GASOIL D500"]
  moreno[,Ficha := as.numeric(`NUM. VEHICULO`)]
  
  
  # Resumen de cargas del Kraft por fecha y producto
  kraft_cargas <- moreno[, .(Litros_kraft = sum(LITROS, na.rm = TRUE)), 
                         by = .(FECHA, PRODUCTO)]
  
  kraft_cargas_dia <- moreno[, .(Litros_kraft = sum(LITROS, na.rm = TRUE)), 
                             by = .(FECHA)]
}

# DATOS PBI
{
  pbi_moreno[, Ficha := substr(Ficha,2,5)]
  pbi_moreno[, Ficha := as.numeric(Ficha)]
  # Se corrige la fecha por carga manual
  pbi_moreno[Texto == "M", FechaCorregida := FechaCorregida + days(1)]
  
  # Se filtra el taller de Moreno, se agrupan los litros por fecha y taller
  pbi_cargas <- pbi_moreno[CodAlmacen == "MOR", 
                           .(Litros_pbi = sum(ConsumoTotal, na.rm = TRUE)), 
                           by = .(FechaCorregida, ProductName)]
  
  pbi_cargas[, FechaCorregida := as.IDate(FechaCorregida)]
}

# DATOS TELEMEDICIÓN
{
  # Corrección de tipos de datos Fecha
  telemedicion[,Fecha:= dmy_hms(FECHAINICIO)]
  telemedicion[,Fecha:= as.IDate(Fecha)]
  telemedicion[,IDMULTITANQUE:= as.numeric(IDMULTITANQUE)]
  
  # Filtro taller de Moreno y agrupación de litros por aforador por fecha y tipo de combustible
  telemedicion_moreno <- telemedicion[TALLER == "MOR", 
                                      .(Litros_aforador = sum(DESPACHADOAFORADOR, na.rm = TRUE),
                                        TALLER = first(TALLER)), 
                                      by = .(Fecha, IDMULTITANQUE)]
  
  telemedicion_moreno_dia <- telemedicion[TALLER == "MOR", 
                                          .(Litros_aforador = sum(DESPACHADOAFORADOR, na.rm = TRUE),
                                            TALLER = first(TALLER)), 
                                          by = .(Fecha)]
}

# ID TANQUES
{
  id_tanques[, Idtanque := as.numeric(IdTanque)]
}

# CARGAS ALVARADO
{
  cargas_alvarado[, Fecha_corregida := as.IDate(fecha)]
  cargas_alvarado[hour(fecha) < 6 & observaciones != "M",
                  Fecha_corregida := Fecha_corregida - 1]
  
  cargas_alvarado[, Ficha:=as.numeric(ficha)]
  cargas_alvarado[, Fecha:=fecha]
  

  
  
  cargas_alvarado_resumen <- cargas_alvarado[, .(Cantidad = .N,
                                                 Litros = sum(consumo, na.rm = TRUE)),
                                             by = .(Fecha_corregida, codArticulo)]
}

alvarado_filtrado <- cargas_alvarado[, 
                                     .(Litros_mati = sum(consumo, na.rm = TRUE)),
                                     by = .(Fecha_corregida)]

pbi_cargas_dia <- pbi_cargas[, 
                             .(Litros_pbi = sum(Litros_pbi, na.rm = TRUE)),
                             by = .(FechaCorregida)]

a <- merge.data.table(alvarado_filtrado, pbi_cargas_dia,
                      by.x = c("Fecha_corregida"),
                      by.y = c("FechaCorregida"),
                      all = TRUE)

a <- merge.data.table(a, telemedicion_moreno_dia,
                      by.x = c("Fecha_corregida"),
                      by.y = c("Fecha"),
                      all = TRUE)

a <- merge.data.table(a, kraft_cargas_dia,
                      by.x = c("Fecha_corregida"),
                      by.y = c("FECHA"),
                      all = TRUE)


a[, Litros_mati := nafill(Litros_mati, fill = 0)]
a[, Litros_pbi := nafill(Litros_pbi, fill = 0)]
a[, Litros_kraft := nafill(Litros_kraft, fill = 0)]
a[, Litros_aforador := nafill(Litros_aforador, fill = 0)]

a[,Diferencia_kraft := Litros_kraft - Litros_pbi]
a[,Diferencia_mati := Litros_mati - Litros_pbi]

Litros_mati <- sum(a[Fecha_corregida > "2023-05-27" & Fecha_corregida < "2023-06-03", Litros_mati], na.rm = TRUE)
Litros_pbi  <- sum(a[Fecha_corregida > "2023-05-27" & Fecha_corregida < "2023-06-03", Litros_pbi], na.rm = TRUE)
Litros_aforador <- sum(a[Fecha_corregida > "2023-05-27" & Fecha_corregida < "2023-06-03", Litros_aforador], na.rm = TRUE)
Litros_kraft <- sum(a[Fecha_corregida > "2023-05-27" & Fecha_corregida < "2023-06-03", Litros_kraft], na.rm = TRUE)

Litros_aforador - Litros_pbi
Litros_aforador - Litros_mati
Litros_aforador - Litros_kraft

cargas_a_migrar <- merge.data.table(cargas_alvarado, pbi_moreno,
                                    by.x = c("Ficha", "Fecha"),
                                    by.y = c("Ficha", "Fecha Hora consumo"),
                                    all = TRUE)

cargas_a_migrar[, Taller := CodAlmacen]
cargas_a_migrar[is.na(Taller), Taller := codAlmacen]
cargas_a_migrar <- cargas_a_migrar[Taller == "MOR" & is.na(ProductName) &
                                     fecha > "2023/05/27" & fecha < "2023/06/03",]

cargas_a_migrar <- merge.data.table(cargas_a_migrar, moreno,
                                    by.x = c("Ficha", "Fecha"),
                                    by.y = c("Ficha", "Fecha_hora"),
                                    all = TRUE)

cargas_a_migrar <- cargas_a_migrar[Fecha > "2023/05/27" & Fecha < "2023/06/03",]
cargas_a_migrar <- cargas_a_migrar[is.na(CONTROLADOR) | is.na(id),]
cargas_a_migrar[, Litros_totales := LITROS]
cargas_a_migrar[is.na(LITROS), Litros_totales := consumo]
# cargas_a_migrar <- cargas_a_migrar[,-c("DETALLE","SECTOR VEH.","ODO. INGRESADO",
#                                        "CALESITA","NUM. CHOFER","APELLIDO Y NOMBRE",
#                                        "LEGAJO","SECTOR CHO.","TIPO DE VALIDACION",
#                                        "TIPO DE ASIGNACION","DUEÑO CC","CENTRO DE COSTO",
#                                        "ALMACEN","TIPO DE IMPUTACION","DETALLE TIPO IMP.",
#                                        "RAZON SOCIAL TERCERO","CUIT TERCERO",
#                                        "RAZON SOCIAL CLIENTE","CUIT CLIENTE","USUARIO SUPERVISOR",
#                                        "NOMBRE DEL SUPERVISOR","USUARIO DEL DESPACHANTE",
#                                        "NOMBRE DEL DESPACHANTE")]
a_migrar_mati <- copy(cargas_a_migrar)
a_migrar_mati <- a_migrar_mati[!is.na(id), -c(1:2)]
a_migrar_mati <- a_migrar_mati[!is.na(id), 1:11]
colnames(a_migrar_mati) <- c(col_mati, "Fecha_corregida")

a_migrar_mati <- merge.data.table(a_migrar_mati, listado_fichas,
                                  by.x = c("ficha"),
                                  by.y = c("Ficha"),
                                  all.x = TRUE,
                                  all.y = FALSE)

a_migrar_mati <- merge.data.table(a_migrar_mati, tipos_chasis,
                                  by.x = c("Chasis"),
                                  by.y = c("Descripción del Vehículo"),
                                  all.x = TRUE,
                                  all.y = FALSE)

a_migrar_datakraft <- copy(cargas_a_migrar)
a_migrar_datakraft <- a_migrar_datakraft[!is.na(CONTROLADOR), -c(1:19)]
a_migrar_datakraft <- a_migrar_datakraft[!is.na(CONTROLADOR), -c(39:41)]

colnames(a_migrar_datakraft) <- col_datakraft

file_path <- paste0("Cargas que no migraron", ".xlsx")

wb <- createWorkbook()

# Add a worksheet to the workbook
addWorksheet(wb, sheetName = "Excel Mati")
addWorksheet(wb, sheetName = "DataKraft")

# Write the data.table to the worksheet
writeDataTable(wb, sheet = "Excel Mati", x = a_migrar_mati, 
               rowNames = FALSE)

writeDataTable(wb, sheet = "DataKraft", x = a_migrar_datakraft, 
               rowNames = FALSE)

# Save the workbook as an Excel file
saveWorkbook(wb, file_path, overwrite = TRUE)

foma_kraft <- dcast(a_migrar_datakraft, 
              ... ~ PRODUCTO, 
              value.var = "LITROS")

foma_kraft[, Taller := "MOR"]
foma_kraft <- foma_kraft[, c("Taller", "FECHA", "NUM. VEHICULO", "GASOIL D500", "GASOIL INFINIA DIESEL")]

foma_mati <- dcast(a_migrar_mati, 
                   ... ~ `Tipo Combustible`, 
                   value.var = "consumo")

foma_mati <- foma_mati[, c("codAlmacen", "Fecha_corregida", "ficha", "GASOIL D500", "GASOIL INFINIA DIESEL")]
foma_mati[, Fecha_corregida := as.Date(Fecha_corregida)]
setnames(foma_mati, colnames(foma_mati), colnames(foma_kraft))

foma <- rbindlist(list(foma_kraft, foma_mati))

write_clip(foma, col.names = FALSE, dec = ",")

# Migración de cargas -----------------------------------------------------
# DATAKRAFT
{
  # LECTURA DATOS 
  datakraft <-  read_excel("datakraft_moreno.xlsx")
  
  datakraft <- datakraft %>% 
    select("CONTROLADOR", "NUM. VEHICULO", "FECHA", "HORA", "LITROS","PRODUCTO")
  
  PBI_combustible_datakraft <- fread("PBI combustible.csv")
  PBI_combustible_datakraft <- PBI_combustible_datakraft %>% filter(CodAlmacen == "MOR")
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
  
}
# MERGE DYNAMICS Y DATAKRAFT

{
  migracion <- full_join(cargas_kraft, cargas_PBI_datakraft, 
                         by = c("FICHA" = "Ficha", "FECHA_CORREGIDA" = "FechaCorregida"))
  
  migracion <- migracion %>% filter(CodAlmacen == "MOR")
  
  sum(migracion %>% ungroup() %>% filter(is.na(litros_Kraft)) %>% select(litros))
  
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
}




# Merge de datos resumen --------------------------------------------------

# DATATOS TELEMEDICIÓN CON ID_TANQUES. PARA TENER EL NOMBRE DE COMBUSTIBLE EN LOS DATOS DE LA TELEMEDICIÓN.
{
  telemedicion_moreno <- merge.data.table(telemedicion_moreno, id_tanques,
                                          by.x = c("TALLER", "IDMULTITANQUE"),
                                          by.y = c("Taller", "IdTanque"))
}

total_cargas <- merge.data.table(pbi_cargas, kraft_cargas,
                                 by.x = c("FechaCorregida", "ProductName"),
                                 by.y = c("FECHA", "PRODUCTO"),
                                 all = TRUE)

total_cargas[, `:=`(Litros_pbi = nafill(Litros_pbi, type = "const", 0),
                    Litros_kraft = nafill(Litros_kraft, type = "const", 0))]

total_cargas[, Diferencia := Litros_pbi - Litros_kraft]
total_cargas[, `:=`(LTS_cargados = max(Litros_pbi, Litros_kraft)),
             by = .(FechaCorregida, ProductName)]

total_cargas_dia <- total_cargas[, .(Litros_pbi = sum(Litros_pbi, na.rm = TRUE),
                                     Litros_kraft = sum(Litros_kraft, na.rm = TRUE)),
                                 by = (FechaCorregida)]




por_aforador <- sum(telemedicion_moreno[Fecha > "2023/05/26" & Fecha < "2023/06/03",
                                        DESPACHADOAFORADOR], na.rm = TRUE)


lts_registrados <- sum(total_cargas[FechaCorregida > "2023/05/26" & FechaCorregida < "2023/06/03",
                                    c("LTS_cargados")])

lts_registrados - por_aforador




# Assuming you have the 'data.table' package installed
# If not, install it using: install.packages("data.table")

# Load the 'data.table' library
library(data.table)

# Sample data (replace this with your own data)
# For example, let's say you have a 'data.table' named 'dt' with columns 'ID', 'Category', and 'Value'
# 'ID' is the identifier for each row, 'Category' contains the column names after pivoting, and 'Value' contains the values for each combination of 'ID' and 'Category'.
dt <- data.table(
  ID = c(1, 1, 2, 2, 3, 3),
  Category = c("A", "B", "A", "B", "A", "C"),
  Value = c(10, 20, 30, 40, 50, 60)
)

# Use dcast to pivot the data
# The formula inside dcast specifies the aggregation to be done in case of duplicates, 
# in this case, we use 'list' to keep all values as a list.
# Using . ~ Category specifies that we want to use 'Category' as the columns after pivoting.
# Finally, the `fill` argument is set to 'NULL' to keep all rows.
pivoted_dt <- dcast(dt, ID ~ Category, value.var = "Value", fun.aggregate = list, fill = NULL)

# Output
print(pivoted_dt)


