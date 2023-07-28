gc()
(rm(list = ls())) # limpiar las var del environment
#Lista de paquetes a utilizar
listofpackages <- c("knitr", "tidyverse", "lubridate",
                    "rstudioapi", "rstatix", "ggrepel", "data.table",
                    "tibble", "ggplot2", "zoo", "DT", "pracma",
                    "formattable", "rmarkdown", "readxl","clipr",
                    "janitor", "scales", "kableExtra", "plotly")

#revisar e instalar librerias que no es estan instaladas
newPackages <- listofpackages[ !(listofpackages %in% installed.packages()[, "Package"])]

if(length(newPackages)) install.packages(newPackages)

for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}  


agrupamiento_km <- function(Fecha_hora, Producto) {
  for (i in c(1:length(Fecha_hora))) {
    if (is.na(Producto[i])) {
      Fecha_hora[i] <- Fecha_hora[i-1]
    }
  }
  return(Fecha_hora)}

Linea_texto <- function(Linea, Pos) {
  respuesta <- vector(length = length(Linea))
  for (i in c(1:length(Linea))) {
    respuesta[i] <- strsplit(Linea[i], " ")[[1]][Pos]
  }
  return(respuesta)}

agrupamiento_linea <- function(Linea) {
  if (length(Linea) > 1) {
    for (i in c(2:(length(Linea)))) {
      if (is.na(Linea[i])) {
        Linea[i] <- Linea[i-1]
      }
    }
  }
  return(Linea)}

agrupamiento_km_2 <- function(Fecha_hora) {
  for (i in c(1:length(Fecha_hora))) {
    if (is.na(Fecha_hora[i])) {
      Fecha_hora[i] <- Fecha_hora[i-1]
    }
  }
  return(Fecha_hora)}





stefani <- read_xls("C:/Users/REstevez/Documents/STEFANI - 14-06.xls")
# laq <- read_xls("C:/Users/REstevez/Documents/LAQ - marzo - 2023.xls")
# pto <- read_xls("C:/Users/REstevez/Documents/pto-abr-19.xls")
# vga <- read_xls("C:/Users/REstevez/Documents/vga-mar3.xls") %>% 
#   mutate(
#     # ,Linea = if_else(Linea == "130" | Linea == "146", "L130/146", Linea)
#     # ,Linea = if_else(Linea == "L146" | Linea == "L130", "L130/146", Linea)
#     ,Linea = if_else(Linea == "L133/125", "L145/133", Linea)
#     ,Linea = if_else(Linea == "133/125", "L145/133", Linea)
#     ) %>% 
#   filter(salidaConfirmada == "S") %>% 
#   group_by(Ficha, Fecha) %>% 
#   summarise(
#     Linea = first(Linea)
#   )

referencia <- fread("C:/Users/REstevez/Documents/HISTORICOS/CONSUMOS_REFERENCIA.txt") %>% 
  filter(Year == 2023)


# referencia_2 <- referencia %>% 
#   filter(UN == "LPGB", Dia == "domingo") %>% 
#   mutate(
#     Dia = "s?bado"
#   )

# referencia <- bind_rows(referencia, referencia_2)

# stefani enero 2023 --------

stefani <- stefani %>% 
  filter(salidaConfirmada == "S",
         !is.na(Ficha)) %>% 
  select(Ficha, Fecha, Linea) %>% 
  mutate(
    Ficha = as.numeric(Ficha)
    ,Fecha = ymd(Fecha)
    ,Linea = if_else(Linea == "", NA, Linea)
  ) %>% 
  distinct() %>% 
  arrange(Ficha, Fecha, desc(Linea)) %>% 
  mutate(
    Mes = lubridate::month(Fecha, abbr = FALSE, label = TRUE),
    Mes = if_else(Mes == "junio", "mayo", Mes),
    Dia = lubridate::wday(Fecha, abbr = FALSE, label = TRUE),
    Dia = if_else(Dia == "s?bado" | Dia == "domingo", "fin_semana", "laboral")
  )

colnames(stefani) <- c("Ficha", "Fecha", "Linea_SIGOF", "Mes", "Dia")

path <- "C:/Users/REstevez/Documents/HISTORICOS"



# list all the files in the directory
files <- list.files(path, full.names = TRUE)

files_KM <- files[grepl("KM", files)]

files_LTS <- files[grepl("LITROS", files)]

files_SIGOF <- files[grepl("SIGOF", files)]

files_linea <- files[grepl("match", files)]

files_Chasis <- files[grepl("Chasis", files)]

files_ficha_un_linea <- files[grepl("UN_Linea", files)]
# files_fichas <- fread("C:/Users/REstevez/Documents/HISTORICOS/Fichas-UN.csv") %>% 
#   mutate(
#     ,Ficha = substr(Ficha, 2, 5)
#     ,Ficha = as.numeric(Ficha)
#   )

# FICHA-UN-LINEA  ----------------------

Ficha_UN_linea <- fread(files_ficha_un_linea) %>% 
  mutate(
    Ficha = substr(Ficha, 2, 5)
    ,Ficha = as.numeric(Ficha)
  )

Ficha_UN <- Ficha_UN_linea %>% 
  select(Ficha, UN) %>% 
  group_by(Ficha, UN) %>% 
  summarise(
    UN = first(UN)
  )

Ficha_UN <- Ficha_UN %>% 
  group_by(Ficha) %>% 
  summarise(
    ,UN = first(UN)
    ,Cant = n()
  ) %>% 
  mutate(
    UN = if_else(Cant > 1, NA, UN)
  ) %>% 
  select(-c(Cant))


Ficha_Linea <- Ficha_UN_linea %>% 
  select(Ficha, Linea) %>% 
  group_by(Ficha, Linea) %>% 
  summarise(
    Linea = first(Linea)
  )

Ficha_Linea <- Ficha_Linea %>% 
  group_by(Ficha) %>% 
  summarise(
    Linea = first(Linea)
    ,Cant = n()
  ) %>% 
  mutate(
    Linea = if_else(Cant > 1, NA, Linea)
  ) %>% 
  select(-c(Cant))


#Ficha_linea <- Ficha_UN_linea %>% select(Ficha, Linea) %>% distinct()

# CHASIS  ----------------------

Chasis <- fread(files_Chasis) %>% 
  mutate(
    Ficha = substr(Ficha, 2, 5)
    ,Ficha = as.numeric(Ficha)
    ,Chasis = if_else(Chasis == "BUS MB OF 1418-52303", "BUS MB OF 1418-52", Chasis)
    ,Chasis = if_else(Chasis == "MB O500 1726-59", "BUS MB O500 1726-59", Chasis)
    ,Chasis = if_else(Chasis == "MB OF 1721L-59", "BUS MB OF 1721L-59", Chasis)
    ,Chasis = if_else(Chasis == "MB OH 1721-62", "BUS MB OH 1721-62", Chasis)
  )

# SIGOF ----------------------
lineas_correctas <- fread(files_linea)

SIGOF_df <- lapply(files_SIGOF, fread)

SIGOF_df <- bind_rows(SIGOF_df) 

SIGOF_df <- distinct(SIGOF_df)

linea_SIGOF <- SIGOF_df %>% 
  filter(salidaConfirmada == "S",
         Unidad == "Primera",
         !is.na(Ficha)) %>% 
  select(Ficha, Fecha, Linea) %>% 
  mutate(
    Ficha = as.numeric(Ficha)
    ,Fecha = dmy(Fecha)
    ,Linea = if_else(Linea == "", NA, Linea)
  ) %>% 
  distinct() %>% 
  arrange(Ficha, Fecha, desc(Linea))

colnames(linea_SIGOF) <- c("Ficha", "Fecha", "Linea_SIGOF")

linea_SIGOF <- left_join(linea_SIGOF, lineas_correctas, by = c("Linea_SIGOF" = "Lineas")) %>% 
  select(Ficha, Fecha, Linea)

# UN ----------------------

files_UN <- fread("C:/Users/REstevez/Documents/HISTORICOS/UN.csv") %>% 
  filter(Linea != "")

# KM ----------------------
KM_df <- lapply(files_KM, read_xlsx)

KM_df <- bind_rows(KM_df) 

KM_df <- distinct(KM_df)


KM_df <- KM_df %>% 
  filter(Resultado == "Successful",
         startsWith(Ficha, "F")
  ) %>% 
  mutate(
    Fecha = Linea_texto(Texto, 1)
    ,Fecha = as.Date(Fecha, format = "%Y%m%d")
    # ,Fecha_2 = ymd_hms(`Fecha de uso`)
    # ,Fecha_2 = as.Date(Fecha_2)
    # ,Fecha = coalesce(Fecha, Fecha_2)
    ,Ficha = substr(Ficha, 2, 5)
    ,Ficha = as.numeric(Ficha)
    ,Texto = if_else(grepl("2km", Texto), "-", Texto)
    ,Linea_D365 = substring(Texto, first = regexpr(",", Texto) + 5)
    ,Linea_D365 = Linea_texto(Linea_D365, 2)
    ,Linea_D365 = coalesce(Linea_D365, "-")
  ) %>% 
  arrange(Ficha, desc(Fecha), desc(Linea_D365)) %>%
  select(c(Ficha, Fecha, Linea_D365, KM)) %>%
  group_by(Ficha, Fecha) %>%
  summarise(
    Linea_D365 = sort(unique(Linea_D365))[1]
    ,KM = sum(KM, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup()

KM_df <- left_join(KM_df, linea_SIGOF, by = c("Ficha" = "Ficha",
                                              "Fecha" = "Fecha"),
                   multiple = "first")

KM_df <- KM_df %>% 
  mutate(
    Linea_D365 = if_else(Linea_D365 == "-", NA, Linea_D365)
  ) %>% 
  arrange(Ficha, desc(Fecha), desc(Linea)) %>% 
  group_by(Ficha, Fecha) %>% 
  mutate(
    Linea_D365 = sort(unique(Linea_D365))[1]
    ,Linea = sort(unique(Linea))[1]
    ,Linea = coalesce(Linea, Linea_D365)
  ) %>% 
  select(-c(Linea_D365))


KM_df <- KM_df %>% 
  arrange(Ficha, desc(Fecha), desc(Linea)) %>% 
  group_by(Ficha) %>% 
  mutate(
    Linea = agrupamiento_linea(Linea)
  )

KM_df <- KM_df %>% 
  arrange(Ficha, desc(Fecha), desc(Linea)) %>% 
  group_by(Ficha, Fecha) %>% 
  summarise(
    Linea_KM = sort(unique(Linea))[1]
    ,KM = sum(KM, na.rm = TRUE)
  ) #%>% 
  # filter(Fecha < "2023-06-01")

KM_df <- left_join(KM_df, Ficha_UN, by = c("Ficha" = "Ficha")) %>% 
  mutate(
    Linea_KM = if_else(is.na(Linea_KM) & UN == "41", "41", Linea_KM)
    ,Linea_KM = if_else(is.na(Linea_KM) & UN == "A", "A", Linea_KM)
    ,Linea_KM = if_else(is.na(Linea_KM) & UN == "SAF", "FABRICA", Linea_KM)
  )


KM_df <- left_join(KM_df, Ficha_Linea, by = c("Ficha" = "Ficha"))

KM_df <- KM_df %>% 
  mutate(
    Linea_KM = if_else(is.na(Linea_KM) & UN == "203" & Linea == "203I", "203I", Linea_KM)
    ,Linea_KM = if_else(is.na(Linea_KM) & UN == "203" & Linea == "203M", "203M", Linea_KM)
    # ,Linea_KM = if_else(is.na(Linea_KM) & UN == "A", "A", Linea_KM)
    # ,Linea_KM = if_else(is.na(Linea_KM) & UN == "SAF", "FABRICA", Linea_KM)
  ) %>% 
  select(-c(Linea))

KM_df <- left_join(KM_df, lineas_correctas, by = c("Linea_KM" = "Lineas")) %>% 
  mutate(Linea_KM = coalesce(Linea, Linea_KM))


# LITROS -------------------------

LTS_df <- lapply(files_LTS, read_xlsx)

LTS_df <- bind_rows(LTS_df)

LTS_df <- distinct(LTS_df)

# FechaCorregida[combustible_ref$Texto == "M"] <- combustible_ref$FechaCorregida[combustible_ref$Texto == "M"] + days(1)

colnames(LTS_df) <- c("Ficha", "CodAlmacen", "Cod_articulo", "ProductName", "Texto",
                      "Fecha_Hora", "ConsumoTotal", "Codigo", "CodAlmacen_2", 
                      "Cod_articulo_2", "ConsumoTotal_2", "Fecha_2", 
                      "FechaCorregida", "Ficha_2", "Resultado", "Texto_2")

LTS_df <- LTS_df %>% 
  filter(Resultado == "Successful",
         startsWith(Ficha, "F"),
         grepl("GASOIL", ProductName)
  ) %>% 
  mutate(
    `Fecha Hora consumo` = ymd_hms(`Fecha_Hora`)
    ,FechaCorregida = as.Date(`Fecha Hora consumo`)
    ,FechaCorregida = if_else(hour(`Fecha Hora consumo`) < 6, FechaCorregida - days(1), FechaCorregida)
    ,FechaCorregida = if_else(Texto == "M", FechaCorregida + days(1), FechaCorregida)
    # ,FechaCorregida = ymd_hms(FechaCorregida)
    # ,FechaCorregida = as.Date(FechaCorregida)
    ,Ficha = substr(Ficha, 2, 5)
    ,Ficha = as.numeric(Ficha)
  ) %>% 
  arrange(Ficha, `Fecha Hora consumo`) %>% 
  select(Ficha, CodAlmacen, ProductName, 
         Texto, `Fecha Hora consumo`, ConsumoTotal, FechaCorregida)



LTS_df <- LTS_df %>%
 arrange(Ficha, `Fecha Hora consumo`) %>%
 group_by(Ficha) %>%
 mutate(
   Carga_anterior = lag(`Fecha Hora consumo`)
   ,`Hs entre cargas`= as.numeric(`Fecha Hora consumo` - Carga_anterior) / 60
 )


LTS_resumido <- LTS_df %>%
 group_by(Ficha, FechaCorregida) %>%
 summarise(
   ProductName = sort(unique(ProductName))[1]
   ,LTS = sum(ConsumoTotal, na.rm = TRUE)
 )

LTS_df <- left_join(LTS_df, linea_SIGOF, by = c("Ficha" = "Ficha",
                                               "FechaCorregida" = "Fecha"),
                   multiple = "first")

colnames(LTS_df) <- c("Ficha", "Taller", "Combustible", "Texto", "Fecha_hora",
                     "LTS", "Fecha", "Carga_anterior", "Hs_entre_cargas", "Linea_LTS")


stefani <- left_join(stefani, lineas_correctas, by = c("Linea_SIGOF" = "Lineas")) %>% 
  select(Ficha, Fecha, Linea, Mes, Dia)


stefani <- left_join(stefani, Chasis, by = c("Ficha" = "Ficha"))

stefani <- left_join(stefani, KM_df, by = c("Ficha" = "Ficha",
                                            "Fecha" = "Fecha"))

stefani <- left_join(stefani, referencia, by = c("Mes" = "Mes",
                                                 "Chasis" = "Chasis",
                                                 "Dia" = "Dia",
                                                 "Linea.x" = "Linea"))

stefani$LTS_teoricos <- stefani$KM.x * stefani$Consumo_teorico / 100

LTS_dia <- LTS_df %>% 
  group_by(Ficha, Fecha) %>% 
  summarise(
    Combustible = first(Combustible),
    LTS = sum(LTS, na.rm = TRUE)
    )

stefani <- left_join(stefani, LTS_dia, by = c("Ficha" = "Ficha",
                                              "Fecha" = "Fecha"))

stefani_dia <- stefani %>% 
  filter(Fecha >= "2023-05-01") %>% 
  group_by(Fecha) %>% 
  summarise(
    LTS_teoricos = sum(LTS_teoricos, na.rm = TRUE)
  )

stefani_real <- LTS_df %>% 
  group_by(Taller, Fecha) %>% 
  summarise(
    #Combustible = first(Combustible),    
    LTS_reales = sum(LTS, na.rm = TRUE),
    LTS_reales = coalesce(LTS_reales,0)
  ) %>% 
  filter(Taller == "STE", Fecha >= "2023-05-01")


stefani_dia <- left_join(stefani_dia, stefani_real, by = c("Fecha" = "Fecha")) %>% 
  mutate(
    LTS_reales = coalesce(LTS_reales,0)
  )

stefani_dia$Diferencias <- stefani_dia$LTS_reales - stefani_dia$LTS_teoricos

faltante <- 4221.3012
sum(stefani_dia  %>% select(Diferencias))

Tipo_combustible <- LTS_df %>% 
  group_by(Ficha) %>% 
  summarise(
    Combustible = first(Combustible)
  )

FOMA_Stefani <- stefani %>% filter(is.na(LTS.y), Fecha == "2023-06-14", !is.na(LTS_teoricos)) %>% 
  select(Ficha, Fecha, LTS_teoricos)

FOMA_Stefani <- left_join(FOMA_Stefani, Tipo_combustible, by = c("Ficha" = "Ficha")) %>% 
  filter(LTS_teoricos > 15) %>% 
  pivot_wider(names_from = Combustible, values_from = LTS_teoricos) %>% 
  mutate(
    Taller = "STE"
  ) %>% 
  select("Taller","Fecha", "Ficha", "GASOIL D500", "GASOIL INFINIA DIESEL")

sum(FOMA_Stefani$`GASOIL D500`, na.rm = TRUE) + sum(FOMA_Stefani$`GASOIL INFINIA DIESEL`, na.rm = TRUE) 

write_clip(FOMA_Stefani)

# laq -------------------

laq <- laq %>% 
  filter(salidaConfirmada == "S",
         !is.na(Ficha)) %>% 
  select(Ficha, Fecha, Linea) %>% 
  mutate(
    Ficha = as.numeric(Ficha)
    ,Fecha = ymd(Fecha)
    ,Linea = if_else(Linea == "", NA, Linea)
  ) %>% 
  distinct() %>% 
  arrange(Ficha, Fecha, desc(Linea)) %>% 
  mutate(
    Mes = lubridate::month(Fecha, abbr = FALSE, label = TRUE),
    Dia = lubridate::wday(Fecha, abbr = FALSE, label = TRUE)
  )

colnames(laq) <- c("Ficha", "Fecha", "Linea_SIGOF", "Mes", "Dia")

laq <- left_join(laq, lineas_correctas, by = c("Linea_SIGOF" = "Lineas")) %>% 
  select(Ficha, Fecha, Linea, Mes, Dia)


laq <- left_join(laq, Chasis, by = c("Ficha" = "Ficha"))

laq <- left_join(laq, KM_df, by = c("Ficha" = "Ficha",
                                            "Fecha" = "Fecha"))

laq <- left_join(laq, referencia, by = c("Mes" = "Mes",
                                                 "Chasis" = "Chasis",
                                                 "Dia" = "Dia",
                                                 "Linea.x" = "Linea"))

laq$LTS_teoricos <- laq$KM.x * laq$Consumo_teorico / 100

laq_dia <- laq %>% 
  group_by(Fecha) %>% 
  summarise(
    LTS_teoricos = sum(LTS_teoricos, na.rm = TRUE)
  )

laq_real <- df_resumido %>% 
  filter(Taller == "LAQ", Mes == "marzo", Year == 2023) %>% 
  group_by(Fecha) %>% 
  summarise(
    LTS_reales = sum(LTS, na.rm = TRUE),
    LTS_reales = coalesce(LTS_reales,0)
  )

laq_dia <- left_join(laq_dia, laq_real, by = c("Fecha" = "Fecha")) %>% 
  mutate(
    LTS_reales = coalesce(LTS_reales,0)
  )

laq_dia$Diferencias <- laq_dia$LTS_reales - laq_dia$LTS_teoricos

# PTO 19/4/23---------------------------


pto <- pto %>% 
  filter(salidaConfirmada == "S",

         !is.na(Ficha)) %>% 
  select(Ficha, Fecha, Linea) %>% 
  mutate(
    Ficha = as.numeric(Ficha)
    ,Fecha = ymd(Fecha)
    ,Linea = if_else(Linea == "", NA, Linea)
  ) %>% 
  distinct() %>% 
  arrange(Ficha, Fecha, desc(Linea)) %>% 
  mutate(
    Mes = lubridate::month(Fecha, abbr = FALSE, label = TRUE),
    Dia = lubridate::wday(Fecha, abbr = FALSE, label = TRUE)
  )

colnames(pto) <- c("Ficha", "Fecha", "Linea_SIGOF", "Mes", "Dia")

pto <- left_join(pto, lineas_correctas, by = c("Linea_SIGOF" = "Lineas")) %>% 
  select(Ficha, Fecha, Linea, Mes, Dia)


pto <- left_join(pto, Chasis, by = c("Ficha" = "Ficha"))

pto <- left_join(pto, KM_df, by = c("Ficha" = "Ficha",
                                    "Fecha" = "Fecha"))

pto <- left_join(pto, referencia, by = c("Mes" = "Mes",
                                         "Chasis" = "Chasis",
                                         "Dia" = "Dia",
                                         "Linea.x" = "Linea"))

pto$LTS_teoricos <- pto$KM.x * pto$Consumo_teorico / 100

pto_dia <- pto %>% 
  group_by(Fecha) %>% 
  summarise(
    LTS_teoricos = sum(LTS_teoricos, na.rm = TRUE)
  )

pto_real <- df_resumido %>% 
  filter(Taller == "PTO", Mes == "abril", Year == 2023) %>% 
  group_by(Fecha) %>% 
  summarise(
    LTS_reales = sum(LTS, na.rm = TRUE),
    LTS_reales = coalesce(LTS_reales,0)
  )

pto_dia <- left_join(pto_dia, pto_real, by = c("Fecha" = "Fecha")) %>% 
  mutate(
    LTS_reales = coalesce(LTS_reales,0)
  )

pto_dia$Diferencias <- pto_dia$LTS_reales - pto_dia$LTS_teoricos

# VGA 02/3/23---------------------------


vga <- vga %>% 
  filter(
         !is.na(Ficha)) %>% 
  select(Ficha, Fecha, Linea) %>% 
  mutate(
    Ficha = as.numeric(Ficha)
    ,Fecha = ymd(Fecha)
    ,Linea = if_else(Linea == "", NA, Linea)
  ) %>% 
  distinct() %>% 
  arrange(Ficha, Fecha, desc(Linea)) %>% 
  mutate(
    Mes = lubridate::month(Fecha, abbr = FALSE, label = TRUE),
    Dia = lubridate::wday(Fecha, abbr = FALSE, label = TRUE)
  )

colnames(vga) <- c("Ficha", "Fecha", "Linea_SIGOF", "Mes", "Dia")

vga <- left_join(vga, lineas_correctas, by = c("Linea_SIGOF" = "Lineas")) %>% 
  select(Ficha, Fecha, Linea, Mes, Dia)


vga <- left_join(vga, Chasis, by = c("Ficha" = "Ficha"))

vga <- left_join(vga, KM_df, by = c("Ficha" = "Ficha",
                                    "Fecha" = "Fecha")) %>% 
  filter(!is.na(KM)) %>% 
  mutate(
    Linea.x = coalesce(Linea.x, Linea.y)
  )

vga <- left_join(vga, referencia, by = c("Mes" = "Mes",
                                         "Chasis" = "Chasis",
                                         "Dia" = "Dia",
                                         "Linea.x" = "Linea"))

vga <- full_join(vga, df_consolidado[, c("Ficha", "Fecha", "LTS")], 
                 by = c("Ficha" = "Ficha",
                        "Fecha" = "Fecha"))

vga$LTS_teoricos <- vga$KM.x * vga$Consumo_teorico / 100

vga_dia <- vga %>% 
  group_by(Fecha) %>% 
  summarise(
    LTS_teoricos = sum(LTS_teoricos, na.rm = TRUE)
  )

vga_real <- df_resumido %>% 
  filter(Taller == "VGA", Mes == "marzo", Year == 2023) %>% 
  group_by(Fecha) %>% 
  summarise(
    LTS_reales = sum(LTS, na.rm = TRUE),
    LTS_reales = coalesce(LTS_reales,0)
  )

vga_dia <- left_join(vga_dia, vga_real, by = c("Fecha" = "Fecha")) %>% 
  mutate(
    LTS_reales = coalesce(LTS_reales,0)
  )

vga_dia$Diferencias <- vga_dia$LTS_reales - vga_dia$LTS_teoricos

vga_2 <- vga %>% 
  filter(is.na(LTS.y))
sum(vga_2$LTS_teoricos, na.rm = TRUE)


fichas_vga <- read_xls("C:/Users/REstevez/Documents/vga-mar3.xls") %>% 
  group_by(Ficha, Fecha) %>% 
  summarise() %>% 
  mutate(
    Ficha = as.numeric(Ficha)
  )

fichas_vga <- left_join(fichas_vga, df_resumido, by = c("Ficha" = "Ficha",
                                                        "Fecha" = "Fecha"))

write_clip(fichas_vga)