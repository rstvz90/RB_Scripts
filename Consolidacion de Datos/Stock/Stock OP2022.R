# INICIACION VARIABLES --------------------------------
gc()
(rm(list = ls())) # limpiar las var del environment

#Lista de paquetes a utilizar
listofpackages <- c("tidyverse", "lubridate", "readxl",
                    "rstudioapi", "data.table",
                    "tibble", "ggplot2", "DT",  "plotly",
                    "janitor", "clipr", "skimr", "ggrepel")

#revisar e instalar librerias que no es están instaladas
newPackages <- listofpackages[ !(listofpackages %in% installed.packages()[, "Package"])]

if(length(newPackages)) install.packages(newPackages)
for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}  

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Mes_filtro <-  "abril"
# Year_filtro <- 2023

#-----------------------------
path <- "C:/Users/REstevez/Documents/HISTORICOS"

files <- list.files(path, full.names = TRUE)


# LITROS RECIBIDOS ---------------------------
Tabla_LTS <- read_xlsx("//sm-public/SGC/Abastecimiento_(AB)/Combustible/OP2022.xlsx", sheet = "ROS",
                       skip = 1)
Tabla_LTS <- Tabla_LTS %>%
  select(c("Taller / Combustible", "Fecha", "Lts Recibidos", "OP", "PC", "Proveedor", "Importe"))

Tabla_LTS <- separate(Tabla_LTS, `Taller / Combustible`, into = c("Taller", "Combustible"), sep = " ")

Tabla_LTS <- Tabla_LTS %>%
  mutate(
    Year = lubridate::year(Fecha),
    Mes = lubridate::month(Fecha, label = TRUE, abbr = FALSE)
  ) %>% 
  filter(`Lts Recibidos` != 0)

Tabla_LTS$Combustible[Tabla_LTS$Combustible == "D500"] <- "GASOIL D500"
Tabla_LTS$Combustible[Tabla_LTS$Combustible == "EURO"] <- "INFINIA DIESEL"

LTS_recibidos <- Tabla_LTS %>%
  #filter(Mes == Mes_filtro, Year == Year_filtro) %>%
  group_by(Taller, Year, Mes) %>%
  summarise(LTS_recibidos = sum(`Lts Recibidos`, na.rm = TRUE)) %>%
  filter(!is.na(Taller))

# motivos <- c(#"Error de recepcion OT"
#              #,"Error de tipeo"
#              "Material mezclado"
#              #,"Material que no se encuentra en condiciones para ser usado"
#              #,"Material recuperado"
#              #,"Pedido especial Dirección"
#              ,"Pérdida de combustible"
#              #,"Problemas de identificación"
#              #,"falta de conocimiento sobre el material"
#              ,"Purga / Limpieza de tanque" 
#              #,"Cambio de modelo de material"
#              )
# 
# Tabla_LTS_2 <- read_excel("movimientos_stock.xlsx",
#                         col_types = c("date", "text", "text", "numeric", "text", "text"))
# 
# colnames(Tabla_LTS_2) <- c("Fecha", "Referencia", "Motivo_recuento",
#                          "Cantidad", "Combustible", "Taller")
# 
# Tabla_LTS_2$Taller[Tabla_LTS_2$Taller == "RAC"] <- "BAI"
# Tabla_LTS_2$Combustible [Tabla_LTS_2$Combustible == "GASOIL INFINIA DIESEL"] <-  "INFINIA DIESEL"
# Tabla_LTS_2$Combustible [Tabla_LTS_2$Combustible == "INFINIA DIESEL" &
#                            Tabla_LTS_2$Taller == "CAR"] <-  "GASOIL D500"
# 
# Tabla_LTS_2 <- Tabla_LTS_2 %>%
#   mutate(
#     Year = lubridate::year(Fecha),
#     Mes = lubridate::month(Fecha, label = TRUE, abbr = FALSE))
# 
# LTS_recibidos <- Tabla_LTS_2 %>%
#   filter(Referencia == "Pedido de compra") %>%
#   group_by(Taller, Combustible, Year, Mes) %>%
#   summarise(LTS_recibidos = sum(Cantidad, na.rm = TRUE)) %>%
#   filter(!is.na(Taller))
files_mov_stock <- files[grepl("mov_stock", files)]

LTS_recibidos_2 <- read_xlsx(files_mov_stock)

setDT(LTS_recibidos_2)
setnames(LTS_recibidos_2, c("Almacén", "Nombre del artículo"), c("Taller" , "Combustible"))

LTS_recibidos_2[Taller == "RAC", Taller := "BAI"]
LTS_recibidos_2 <- LTS_recibidos_2[Taller != "SAA",]
LTS_recibidos_2[Combustible == "GASOIL INFINIA DIESEL", Combustible := "INFINIA DIESEL"]

LTS_recibidos_2 <- LTS_recibidos_2[Referencia == "Pedido de compra" | 
                                     Referencia == "Pedido de transferencia" |
                                     Referencia == "Pedido de venta",]

LTS_recibidos_2[, `:=`(Year = year(Fecha),
                       Mes = lubridate::month(Fecha, label = TRUE, abbr = FALSE))]

LTS_recibidos_2 <- LTS_recibidos_2[Year == 2023 &
                                   Mes < "julio",]

LTS_recibidos_2 <- LTS_recibidos_2[, .(LTS_recibidos_2 = sum(Cantidad, na.rm = TRUE)),
                                   by = .(Taller, Year, Mes)]


setDT(LTS_recibidos)
LTS_excel <- LTS_recibidos[Year == 2023 &
                             Mes < "julio",]

LTS_excel <- LTS_excel[, .(LTS = sum(LTS_recibidos, na.rm = TRUE)),
                       by = .(Year, Mes)]
#-----------------------------


# RECUENTOS -------------------------------

# Tabla_recuentos <- read_excel("Recuentos.xlsx",
#                               col_types = c("date", "text", "text", "numeric", "text", "text")
# )
# 
# colnames(Tabla_recuentos) <- c("Fecha", "Referencia", "Motivo_recuento",
#                                "Cantidad", "Nombre_articulo", "Taller")
# 
# Tabla_recuentos$Nombre_articulo[Tabla_recuentos$Nombre_articulo == "GASOIL INFINIA DIESEL"] <-  "INFINIA DIESEL"
# 
# LTS_recuentos <- Tabla_recuentos %>% 
#   mutate(
#     Year = lubridate::year(Fecha),
#     Mes = lubridate::month(Fecha,label = TRUE, abbr = FALSE)) %>% 
#   # filter(Mes == Mes_filtro, Year == Year_filtro,
#   #        Motivo_recuento == "Falta requerido" |
#   #        Motivo_recuento == "Purga / Limpieza de tanque" |
#   #        Motivo_recuento ==  "Falta calib surtidor"
#   #        ) %>% 
#   group_by(Taller, Nombre_articulo, Year, Mes) %>% 
#   summarise(LTS_recuentos = sum(Cantidad, na.rm = TRUE))
# 
# LTS_recuentos$Taller[LTS_recuentos$Taller == "RAC"] <- "BAI"


# LTS_recuentos <- Tabla_LTS_2 %>% 
#     filter(Motivo_recuento %in% motivos) %>% 
#     group_by(Taller, Combustible, Year, Mes) %>%
#     summarise(LTS_recuentos = sum(Cantidad, na.rm = TRUE))


#-----------------------------



# STOCK INICIAL Y FINAL -----------------------

Tabla_Telemedicion <- read_xlsx("C:/Users/REstevez/Documents/HISTORICOS/TELEMEDICION_HISTORICO.xlsx")

id_tanques <- read_xlsx("ID TANQUES.xlsx", sheet = "ID")

id_tanques$IdTanque <- as.character(id_tanques$IdTanque)

Tabla_Telemedicion$TALLER[Tabla_Telemedicion$TALLER == "BERU"] <- "BER"
Tabla_Telemedicion$TALLER[Tabla_Telemedicion$TALLER == "MUN"] <- "CAR"
Tabla_Telemedicion$TALLER[Tabla_Telemedicion$TALLER == "STEF"] <- "STE"
Tabla_Telemedicion[is.na(Tabla_Telemedicion)] <- 0
# 
# Tabla_Telemedicion <- left_join(Tabla_Telemedicion, id_tanques, by = c("TALLER" = "Taller",
#                                                                        "IDMULTITANQUE" = "IdTanque"))

Stock_fin_ini <- Tabla_Telemedicion %>% 
  mutate(FECHAINICIO = dmy_hms(FECHAINICIO),
         FECHACIERRE = dmy_hms(FECHACIERRE),
         Year = lubridate::year(FECHAINICIO),
         Mes = lubridate::month(FECHAINICIO,label = TRUE, abbr = FALSE)) %>% 
  # filter(Mes == Mes_filtro, Year == Year_filtro) %>% 
  arrange(TALLER, FECHAINICIO) %>% 
  group_by(TALLER, Year, Mes, IDMULTITANQUE) %>% 
  summarise(Stock_inicial = first(VOLUMENINI),
            Stock_final = last(VOLUMENFIN),
            LTS_aforador = sum(DESPACHADOAFORADOR, na.rm = TRUE)) %>%
  group_by(TALLER, Year, Mes) %>% 
  summarise(Stock_inicial = sum(Stock_inicial, na.rm = TRUE),
            Stock_final = sum(Stock_final, na.rm = TRUE),
            LTS_aforador = sum(LTS_aforador, na.rm = TRUE)) %>%
  mutate(Dif_Stock = Stock_final - Stock_inicial) 

#-----------------------------


# LITROS DESPACHADOS ------------------------

files_LTS <- files[grepl("LITROS", files)]

LTS_df <- lapply(files_LTS, read_xlsx)

LTS_df <- bind_rows(LTS_df)

LTS_df <- distinct(LTS_df)

# FechaCorregida[combustible_ref$Texto == "M"] <- combustible_ref$FechaCorregida[combustible_ref$Texto == "M"] + days(1)

colnames(LTS_df) <- c("Ficha", "CodAlmacen", "Cod_articulo", "ProductName", "Texto",
                      "Fecha_Hora", "ConsumoTotal", "Codigo", "CodAlmacen_2", 
                      "Cod_articulo_2", "ConsumoTotal_2", "Fecha_2", 
                      "FechaCorregida", "Ficha_2", "Resultado", "Texto_2")
setDT(LTS_df)

LTS_df <- LTS_df[Cod_articulo == 101293 | Cod_articulo == 101294,]

LTS_df <- LTS_df %>% 
  filter(Resultado == "Successful",
         startsWith(Ficha, "F"),
         grepl("GASOIL", ProductName)
  ) %>% 
  mutate(
    #Fecha_Hora = ymd_hms(Fecha_Hora),
    FechaCorregida = IDateTime(Fecha_Hora)[[1]],
    Hora = IDateTime(Fecha_Hora)[[2]],
    FechaCorregida = if_else(hour(Hora) < 6, FechaCorregida - 1, FechaCorregida),
    FechaCorregida = if_else(Texto == "M", FechaCorregida + 1, FechaCorregida),
    # ,FechaCorregida = ymd_hms(FechaCorregida)
    # ,FechaCorregida = as.Date(FechaCorregida)
    Ficha = substr(Ficha, 2, 5),
    Ficha = as.numeric(Ficha),
    Mes = lubridate::month(FechaCorregida, label = TRUE, abbr = FALSE),
    Year = lubridate::year(FechaCorregida)
  ) %>% 
  arrange(Ficha, Fecha_Hora) %>% 
  select(Ficha, CodAlmacen, ProductName, 
         Texto, Fecha_Hora, ConsumoTotal, FechaCorregida, Hora, Mes, Year)

LTS_df$CodAlmacen[LTS_df$CodAlmacen == "RAC"] <- "BAI"
LTS_df$ProductName [LTS_df$ProductName == "GASOIL INFINIA DIESEL"] <-  "INFINIA DIESEL"



LTS_surtidor <- LTS_df %>% 
  group_by(CodAlmacen, Year, Mes) %>% 
  summarise(LTS_surtidor = sum(ConsumoTotal, na.rm = TRUE))

#-----------------------------


# DESVIOS JUSTIFICADOS ------------------------

Tabla_desvios <- read_xlsx("//sm-public/SGC/Planes de trabajo/Mantenimiento/Proyeccion Consumos de Combustibles x Taller/Rev.3 - Comparacion Cargas Combustible por Aforador vs Sistema.xlsx",
                           sheet = "TABLA GENERAL",
                           col_types = c("text", "date", "text", "numeric", "numeric", "numeric", 
                                         "text", "text", "numeric", "numeric", "text", "text"))

Tabla_desvios$Taller[Tabla_desvios$Taller == "STEF"] <- "STE"
Tabla_desvios$`Comb. Despachados x Aforador`[Tabla_desvios$`Comb. Despachados x Aforador`> 100000] <- 0

LTS_desvios <- Tabla_desvios %>% 
  mutate(
    Year = lubridate::year(Fecha),
    Mes = lubridate::month(Fecha,label = TRUE, abbr = FALSE)) %>% 
  # filter(Mes == Mes_filtro, 
  #        Year == Year_filtro,
  #        Taller != "STE") %>% 
  group_by(Taller, Year, Mes) %>% 
  summarise(LTS_desvios = sum(`Total Desvio`, na.rm = TRUE),
            LTS_desvios_aforador = sum(`Diferencia Total`, na.rm = TRUE))




#-----------------------------


# CALCULO -----------------------

TOTAL <- full_join(LTS_recibidos, Stock_fin_ini, by = c("Taller" = "TALLER",
                                                        #"Combustible" = "Combustible",
                                                        "Mes" = "Mes",
                                                        "Year" = "Year"
))

TOTAL <- full_join(TOTAL, LTS_surtidor, by = c("Taller" = "CodAlmacen",
                                               #"Combustible" = "ProductName",
                                               "Mes" = "Mes",
                                               "Year" = "Year"))

# TOTAL <- full_join(TOTAL, LTS_recibidos_2, by = c("Taller" = "Taller",
#                                                #"Combustible" = "ProductName",
#                                                "Mes" = "Mes",
#                                                "Year" = "Year"))

# TOTAL <- full_join(TOTAL, LTS_recuentos, by = c("Taller" = "Taller",
#                                                 "Combustible" = "Combustible",
#                                                 "Mes" = "Mes",
#                                                 "Year" = "Year"))


TOTAL <- TOTAL %>% 
  filter(!is.na(Taller)) 

TOTAL[is.na(TOTAL)] <- 0

TOTAL$Diferencia <- (TOTAL$LTS_recibidos - TOTAL$LTS_surtidor - TOTAL$Dif_Stock) * (-1) #- TOTAL$LTS_recuentos



# sum(TOTAL$Diferencia, na.rm = TRUE) / sum(TOTAL$Stock_final, na.rm = TRUE) * 100
# 
# sum(TOTAL$Diferencia, na.rm = TRUE) / sum(TOTAL$LTS_recibidos, na.rm = TRUE) * 100
# 
# TOTAL <- TOTAL %>% 
#   arrange(desc(Diferencia))

TOTAL$Porcentaje <- TOTAL$Diferencia / TOTAL$LTS_recibidos * 100

# 
# TOTAL_taller <- TOTAL %>% 
#   group_by(Taller, Year, Mes) %>% 
#   summarise(
#     Stock_final = sum(Stock_final, na.rm = TRUE),
#     Stocl_inicial = sum(Stock_inicial, na.rm = TRUE),
#     LTS_recibidos = sum(LTS_recibidos, na.rm = TRUE),
#     LTS_surtidor = sum(LTS_surtidor, na.rm = TRUE),
#     Dif_Stock = sum(Dif_Stock, na.rm = TRUE),
#     #LTS_recuentos = sum(LTS_recuentos, na.rm = TRUE),
#     LTS_aforador = sum(LTS_aforador, na.rm = TRUE)
#   ) 
# 
# 
# TOTAL_taller <- full_join(TOTAL_taller, LTS_desvios, 
#                           by = c("Taller" = "Taller")) #%>% 
# #filter(Taller != "STE")
# 
# TOTAL_taller$Diferencia <- (TOTAL_taller$LTS_recibidos - TOTAL_taller$LTS_surtidor - 
#                               TOTAL_taller$Dif_Stock - TOTAL_taller$LTS_desvios) * (-1) #- TOTAL_taller$LTS_recuentos
# 
# # TOTAL_taller$Diferencia[TOTAL_taller$Taller == "MOR"] <- TOTAL_taller$Diferencia[TOTAL_taller$Taller == "MOR"] - 12582
# # TOTAL_taller$Diferencia[TOTAL_taller$Taller == "BAI"] <- TOTAL_taller$Diferencia[TOTAL_taller$Taller == "BAI"] + 20329
# 
# TOTAL_taller$Diferencia_aforador <- (TOTAL_taller$LTS_recibidos - TOTAL_taller$LTS_aforador - 
#                                        TOTAL_taller$Dif_Stock - TOTAL_taller$LTS_desvios) * (-1) #- TOTAL_taller$LTS_recuentos
# 
# TOTAL_taller$Porcentaje <- TOTAL_taller$Diferencia / TOTAL_taller$LTS_recibidos * 100
# 
# # TOTAL_taller <- TOTAL_taller %>% 
# #   arrange(desc(Diferencia))
# 
# sum(TOTAL_taller$Diferencia, na.rm = TRUE) / sum(TOTAL_taller$Stock_final, na.rm = TRUE) * 100
# 
# sum(TOTAL_taller$Diferencia, na.rm = TRUE) / sum(TOTAL_taller$LTS_recibidos, na.rm = TRUE) * 100
# 
# sum(TOTAL_taller$Diferencia_aforador, na.rm = TRUE) / sum(TOTAL_taller$Stock_final, na.rm = TRUE) * 100
# 
# sum(TOTAL_taller$Diferencia_aforador, na.rm = TRUE) / sum(TOTAL_taller$LTS_recibidos, na.rm = TRUE) * 100

#-----------------------------


# plot_diferencia  ---------------------------------------


# 
# plot_diferencia <- ggplot(data = TOTAL_taller)

# plot_diferencia  <- plot_diferencia  + aes(x = reorder(Taller, -Diferencia), y = Diferencia,
#                    label = paste0(round(Diferencia), " lts",
#                                   "\n","(",round(Porcentaje, 2),"%)"),
#                    fill = Diferencia)
# 
# plot_diferencia  <- plot_diferencia  + geom_bar(stat = "identity", color = "black", alpha = 0.8)
# plot_diferencia  <- plot_diferencia  + geom_text(aes(x = Taller, y = Diferencia),
#                          stat = "identity", nudge_y = -sign(TOTAL_taller$Diferencia) * 250,
#                          size = 6)
# plot_diferencia  <- plot_diferencia  + geom_label(mapping = aes(x = 6, y = max(Diferencia) * 0.7,
#                                                                 label = label_total, 
#                                                                 fill = NULL,
#                                                                 hjust = 0),
#                                                  size = 8)
# 
# plot_diferencia  <- plot_diferencia  + scale_fill_gradient(
#   low = "green", high = "red"
#   )
# 
# plot_diferencia  <- plot_diferencia  + theme_minimal()
# plot_diferencia  <- plot_diferencia  + theme(
#   plot.title = element_text(hjust = 0.0, size = 26),
#   plot.subtitle = element_text(hjust = 0.0, size = 20),
#   axis.title.x = element_blank(),
#   axis.text.x = element_text(size = 15),
#   axis.title.y = element_blank(),
#   axis.text.y = element_blank(),
#   legend.key.size = unit(1, "cm"),
#   legend.text = element_text(size = 10),
#   legend.title = element_text(size = 12),
#   legend.position = "none",
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank()
# )
# 
# plot_diferencia  <- plot_diferencia  + labs(title = "Diferencia en litros",
#                                             subtitle = "Diferencia = LTS recibidos - Consumo - Dif Stock",
#                                             fill = "Litros")
# 
# plot_diferencia

##
lista_talleres <- unique(TOTAL$Taller)
lista_talleres <- lista_talleres[lista_talleres != "STO"]
lista_talleres <- lista_talleres[lista_talleres != "AYE"]

TOTAL_dif <- TOTAL %>% 
  filter(Year == 2023, LTS_surtidor > 0) %>% 
  group_by(Taller) %>% 
  summarise(
    LTS_recibidos = sum(LTS_recibidos, na.rm = TRUE),
    # LTS_recibidos_2 = sum(LTS_recibidos_2, na.rm = TRUE),
    LTS_surtidor = sum(LTS_surtidor, na.rm = TRUE),
    #LTS_recuentos = sum(LTS_recuentos, na.rm = TRUE),
    Stock_inicial = first(Stock_inicial),
    Stock_final = last(Stock_final),
    Dif_Stock = Stock_final - Stock_inicial
  ) %>% 
  mutate(
    Diferencia = (LTS_recibidos - LTS_surtidor - Dif_Stock) * (-1) #- LTS_recuentos
    ,Porcentaje = Diferencia / LTS_recibidos * 100
    # ,Diferencia_2 = (LTS_recibidos_2 - LTS_surtidor - Dif_Stock) * (-1) #- LTS_recuentos
    # ,Porcentaje_2 = Diferencia_2 / LTS_recibidos_2 * 100
  )


vector_colores <- c("-1" = "red", "1" = "green")

# for (taller in lista_talleres) {
#   
#   TOTAL_plot <- TOTAL %>% 
#     filter(Year == 2023, Taller == taller, Mes < "junio") #%>% 
#   #mutate(Combustible = as.factor(Combustible))
#   
#   Diferencia_total = TOTAL_plot$Diferencia[TOTAL_plot$Taller == taller]
#   Porcentaje_total = round(TOTAL_plot$Porcentaje[TOTAL_plot$Taller == taller] * 100, 3)
#   
#   label_total <- paste0("Porcentaje Total = ", Porcentaje_total, "%","\n",
#                         "Diferencia Total = ", Diferencia_total, " litros"
#   )
#   
#   plot_diferencia_2 <- ggplot(data = TOTAL_plot)
#   
#   plot_diferencia_2  <- plot_diferencia_2  + aes(x = Mes, y = Porcentaje,
#                                                  label = paste0(round(Porcentaje, 1), "%",
#                                                                 "\n","(",round(Diferencia), " lts)"),
#                                                  colour = as.character(sign(Diferencia)))
#   
#   plot_diferencia_2  <- plot_diferencia_2  + geom_line(aes(group = 1),
#                                                        size = 1.5, alpha = 0.8)
#   
#   plot_diferencia_2  <- plot_diferencia_2  + geom_point(size = 4)
#   
#   plot_diferencia_2  <- plot_diferencia_2  + geom_text_repel(nudge_y = sign(TOTAL_plot$Porcentaje)*1.5,
#                                                              size = 5, hjust = 0.5, show.legend = FALSE,
#                                                              color = "black", alpha = 0.8)
#   
#   # plot_diferencia_2  <- plot_diferencia_2  + geom_label(mapping = aes(x = 4.5, y = max(Porcentaje) * 0.7,
#   #                                                                     label = label_total, 
#   #                                                                     fill = NULL,
#   #                                                                     hjust = 0),
#   #                                                       size = 5, show.legend = FALSE)
#   # 
#   # plot_diferencia_2  <- plot_diferencia_2  + scale_fill_gradient(
#   #   low = "green", high = "red"
#   # )
#   plot_diferencia_2  <- plot_diferencia_2 + scale_colour_manual(values = vector_colores)
#   plot_diferencia_2  <- plot_diferencia_2  + theme_minimal()
#   plot_diferencia_2  <- plot_diferencia_2  + theme(
#     plot.title = element_text(hjust = 0.5, size = 34),
#     plot.subtitle = element_text(hjust = 0.5, size = 18, 
#                                  margin = margin(t = 0.8, r = 0, b = 1, l = 0, unit = "cm")),
#     plot.caption = element_text(hjust = 0.5, size = 15,
#                                 margin = margin(t = 0.8, r = 0, b = 0.5, l = 0, unit = "cm")),
#     axis.title.x = element_blank(),
#     axis.text.x = element_text(size = 15),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     legend.key.size = unit(1, "cm"),
#     legend.text = element_text(size = 10),
#     legend.title = element_text(size = 12),
#     #legend.position = "none",
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#   )
#   
#   plot_diferencia_2  <- plot_diferencia_2  + labs(title = paste0("Diferencia Combustible - Taller ", taller),
#                                                   caption = "Diferencia = Consumo + Dif Stock - LTS recibidos",
#                                                   #fill = "Combustible",
#                                                   subtitle = label_total
#   )
#   
#   plot_diferencia_2
#   
# }

#-------------------------------

# plot_bar

for (taller in lista_talleres) {
  
  TOTAL_plot <- TOTAL %>% 
    filter(Year == 2023, Taller == taller, Mes < "julio") #%>% 
  #mutate(Combustible = as.factor(Combustible))
  
  Diferencia_total = round(sum(TOTAL_plot$Diferencia, na.rm = TRUE))
  Porcentaje_total = round(Diferencia_total / sum(TOTAL_plot$LTS_recibidos, na.rm = TRUE) * 100, 2)
  
  label_total <- paste0("Porcentaje Total = ", Porcentaje_total, "%","\n",
                        "Diferencia Total = ", Diferencia_total, " litros"
  )
  
  plot_diferencia_3 <- ggplot(data = TOTAL_plot)
  
  plot_diferencia_3  <- plot_diferencia_3  + aes(x = Mes, y = Porcentaje,
                                                 label = paste0(round(Porcentaje, 1), "%",
                                                                "\n","(",round(Diferencia), " lts)"),
                                                 #colour = Combustible,
                                                 fill = as.character(sign(Diferencia)))
  
  plot_diferencia_3  <- plot_diferencia_3  + geom_bar(stat = "identity", show.legend = FALSE,
                                                      size = 1, alpha = 0.6, color = "black")
  
  plot_diferencia_3  <- plot_diferencia_3  + geom_text_repel(nudge_y = sign(TOTAL_plot$Porcentaje)*2.5,
                                                             size = 5, hjust = 0.5, show.legend = FALSE,
                                                             color = "black", alpha = 0.8)
  
  # plot_diferencia_3  <- plot_diferencia_3  + facet_grid(~ Combustible)
  
  # plot_diferencia_2  <- plot_diferencia_2  + geom_label(mapping = aes(x = 4.5, y = max(Porcentaje) * 0.7,
  #                                                                     label = label_total, 
  #                                                                     fill = NULL,
  #                                                                     hjust = 0),
  #                                                       size = 5, show.legend = FALSE)
  # 
  # plot_diferencia_2  <- plot_diferencia_2  + scale_fill_gradient(
  #   low = "green", high = "red"
  # )
  plot_diferencia_3  <- plot_diferencia_3 + scale_discrete_manual(values = vector_colores,
                                                                  aesthetics = c("fill"))
  
  plot_diferencia_3  <- plot_diferencia_3  + theme_minimal()
  plot_diferencia_3  <- plot_diferencia_3  + theme(
    plot.title = element_text(hjust = 0.5, size = 34),
    plot.subtitle = element_text(hjust = 0.5, size = 22, 
                                 margin = margin(t = 0.5, r = 0, b = 1, l = 0, unit = "cm")),
    plot.caption = element_text(hjust = 0.5, size = 15,
                                margin = margin(t = 0.8, r = 0, b = 0.5, l = 0, unit = "cm")),
    strip.text.x = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    #legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
  
  plot_diferencia_3  <- plot_diferencia_3  + labs(title = paste0("Diferencia Combustible - Taller ", taller),
                                                  caption = paste0("Diferencia = Consumo + Dif Stock - LTS recibidos",
                                                                   "\n", "Si Deiferencia < 0, se compró más que los litros consumido"),
                                                  #fill = "Combustible",
                                                  subtitle = label_total
  )
  
  jpeg(filename = paste0(taller,".jpg"), pointsize = 12, quality = 150, 
       width = 960, height = 480, bg = "white", res = NA, restoreConsole = TRUE)
  print(plot_diferencia_3)
  dev.off()
}

#-----------------------------


# plot_porcentaje  ---------------------------------------

# Diferencia_total = round(sum(TOTAL_taller$Diferencia, na.rm = TRUE))
# Porcentaje_total = round(sum(TOTAL_taller$Diferencia, na.rm = TRUE) / 
#                            sum(TOTAL_taller$LTS_recibidos, na.rm = TRUE), 2)
# 
# label_total <- paste0("Diferencia Total = ", Diferencia_total, " litros","\n", 
#                       "Porcentaje Total = ", Porcentaje_total, "%")
# 
# plot_porcentaje <- ggplot(data = TOTAL_taller)
# plot_porcentaje  <- plot_porcentaje  + aes(x = reorder(Taller, -Porcentaje), y = Porcentaje,
#                                            label = paste0(round(Porcentaje,2),"%",
#                                                           "\n","(",round(Diferencia)," lts)"),
#                                            fill = Porcentaje)
# plot_porcentaje  <- plot_porcentaje  + geom_bar(stat = "identity", color = "grey", alpha = 0.8)
# plot_porcentaje  <- plot_porcentaje  + geom_text(aes(x = Taller, y = Porcentaje),
#                                                  stat = "identity", nudge_y = -sign(TOTAL_taller$Diferencia)*0.4,
#                                                  size = 6)
# plot_porcentaje  <- plot_porcentaje  + geom_label(mapping = aes(x = 6, y = max(Porcentaje) * 0.8,
#                                                                 label = label_total, 
#                                                                 fill = NULL,
#                                                                 hjust = 0),
#                                                   size = 8)
# plot_porcentaje  <- plot_porcentaje  + scale_fill_gradient(
#   low = "green", high = "red"
# )
# 
# plot_porcentaje  <- plot_porcentaje  + theme_minimal()
# plot_porcentaje  <- plot_porcentaje  + theme(
#   plot.title = element_text(hjust = 0.5, size = 20),
#   plot.subtitle = element_text(hjust = 0.5, size = 15),
#   axis.title.x = element_blank(),
#   axis.text.x = element_text(size = 15),
#   axis.title.y = element_blank(),
#   axis.text.y = element_blank(),
#   legend.key.size = unit(1, "cm"),
#   legend.text = element_text(size = 10),
#   legend.title = element_text(size = 12),
#   legend.position = "none",
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank()
# )
# 
# plot_porcentaje  <- plot_porcentaje  + labs(title = "Porcentaje de Diferencia / LTS Recibidos",
#                                             subtitle = "Diferencia = LTS recibidos - Consumo - Dif Stock")
# 
# plot_porcentaje 

