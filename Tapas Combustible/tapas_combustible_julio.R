
# INICIACIÓN --------------------------------------------------------------

library(data.table)
library(readxl)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(ggrepel)
library(grDevices)

rm(list = ls())
gc()
Sys.setlocale("LC_TIME", "es_US.UTF-8")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# FUNCION AGRUPAMIENTO ---------
agrupamiento_km <- function(Fecha_hora) {
  for (i in c(1:length(Fecha_hora))) {
    if (is.na(Fecha_hora[i])) {
      Fecha_hora[i] <- Fecha_hora[i-1]
    }
  }
  return(Fecha_hora)}


# LECTURA DE DATOS #####

# LTS
{
  LTS_tapas <- fread("C:/Users/REstevez/Documents/Análisis/Tapas Combustible/Datos/PBI_lts_julio.csv")
}

# KM
{
  KM_tapas <- fread("C:/Users/REstevez/Documents/Análisis/Tapas Combustible/Datos/PBI_km_julio.csv")
}

# CONSUMOS REFERENCIAS
{
  CONSUMO_REF <- fread("C:/Users/REstevez/Documents/HISTORICOS/CONSUMOS_REFERENCIA.txt")
}


# TRANSFORMACION DE DATOS #####

# LTS
{
  LTS_tapas <- LTS_tapas[grepl("F", Ficha),]
  LTS_tapas[, Ficha := substr(Ficha,2,5)]
  LTS_tapas[, Ficha := as.numeric(Ficha)]
  # Se corrige la fecha por carga manual
  LTS_tapas[Texto == "M", FechaCorregida := FechaCorregida + days(1)]
  LTS_tapas[,FechaCorregida := as.IDate(FechaCorregida)]
  
  # Se filtra el taller de Moreno, se agrupan los litros por fecha y taller
  # pbi_cargas <- pbi_tapas[CodAlmacen == "MOR", 
  #                         .(Litros_pbi = sum(ConsumoTotal, na.rm = TRUE)), 
  #                         by = .(FechaCorregida, ProductName)]
  # 
  # pbi_cargas[, FechaCorregida := as.IDate(FechaCorregida)]
}

# KM
{
  KM_tapas <- KM_tapas[grepl("F", Ficha),]
  KM_tapas[, Ficha := substr(Ficha,2,5)]
  KM_tapas[, Ficha := as.numeric(Ficha)]
  KM_tapas[, Fecha := as.IDate(Fecha)]
  # Se corrige la fecha por carga manual
  
  # Se filtra el taller de Moreno, se agrupan los litros por fecha y taller
  # pbi_cargas <- pbi_tapas[CodAlmacen == "MOR", 
  #                         .(Litros_pbi = sum(ConsumoTotal, na.rm = TRUE)), 
  #                         by = .(FechaCorregida, ProductName)]
  # 
  # pbi_cargas[, FechaCorregida := as.IDate(FechaCorregida)]
}

#  FILTRADO DE FICHAS #####

# LTS
{
  fichas_tapa <- c(2520, 3552, 4198, 4200)
  LTS_tapas <- LTS_tapas[Ficha %in% fichas_tapa,]
  LTS_tapas <- LTS_tapas[FechaCorregida < "2023-07-15",]  
}

# KM
{
  fichas_tapa <- c(2520, 3552, 4198, 4200)
  KM_tapas <- KM_tapas[Ficha %in% fichas_tapa,]
  KM_tapas <- KM_tapas[Fecha < "2023-07-15",] 
}

# MERGE DE DATOS Y CALCULO DE CONSUMO #####

consumo_tapa <- merge.data.table(KM_tapas, LTS_tapas, 
                                 by.x = c("Ficha", "Fecha"),
                                 by.y = c("Ficha", "FechaCorregida"),
                                 all = TRUE)

setorder(consumo_tapa, Ficha, -Fecha, -`Fecha Hora consumo`)

consumo_tapa[, Fecha_agrupada := agrupamiento_km(`Fecha Hora consumo`), by = Ficha]

consumo_tapa <- consumo_tapa[, `:=`(KM_agrupado = sum(Km, na.rm = TRUE),
                                    LTS_agrupado = sum(ConsumoTotal, na.rm = TRUE)),
                             by = .(Ficha, Fecha_agrupada)]


consumo_tapa[, `:=`(Consumo = LTS_agrupado / KM_agrupado * 100,
                    Dia = lubridate::wday(Fecha, label = TRUE, abbr = FALSE),
                    Mes = lubridate::month(Fecha, label = TRUE, abbr = FALSE)),]

consumo_tapa <- consumo_tapa[(Dia == "sábado" | Dia == "domingo"), Dia := "fin_semana"]
consumo_tapa <- consumo_tapa[Dia != "fin_semana", Dia := "laboral"]

ficha_corregir <- unique(consumo_tapa[Linea == "L146", Ficha])
consumo_tapa <- consumo_tapa[Ficha == ficha_corregir, Linea := "L145/133"]

consumo_tapa <- consumo_tapa[!is.na(ConsumoTotal),]

consumo_tapa <- merge.data.table(consumo_tapa, CONSUMO_REF[Year == 2022], 
                                 by.x = c("Mes","Chasis", "Linea", "Dia"),
                                 by.y = c("Mes","Chasis", "Linea", "Dia"),
                                 all.x = TRUE)

consumo_tapa <- consumo_tapa[Ficha == ficha_corregir, Linea := "L146"]

consumo_tapa[, LTS_teoricos := round(KM_agrupado * Consumo_teorico / 100, 1)]
consumo_tapa[, Consumo_teorico := round(Consumo_teorico, 1)]
consumo_tapa[, Consumo := round(Consumo, 1)]

resumen_consumo <- consumo_tapa[,
                                .(KM_total = sum(KM_agrupado, na.rm = TRUE),
                                  LTS_total = sum(LTS_agrupado, na.rm = TRUE),
                                  LTS_teoricos_total = sum(LTS_teoricos, na.rm = TRUE),
                                  consumo_teo_total = round(weighted.mean(Consumo_teorico,KM_agrupado),1),
                                  consumo_total = round(weighted.mean(Consumo,KM_agrupado),1)),
                                by = Ficha]
resumen_consumo[, Diferencia_consumo := round((consumo_total / consumo_teo_total - 1)*100, 1)]
#resumen_consumo[, Diferencia_LTS := LTS_total - LTS_teoricos_total]


for (ficha_plot in fichas_tapa) {

  pdf(paste0("graficos F", ficha_plot,".pdf"), height = 6, width = 12)
  
  plot_km <- ggplot(subset(consumo_tapa, Ficha == ficha_plot)) +
    geom_line(aes(x = Fecha, y = KM_agrupado), 
              color = "blue", alpha = 0.7, linewidth = 1.5) +
    geom_point(aes(x = Fecha, y = KM_agrupado), 
               color = "blue", alpha = 0.7, size = 3) +
    geom_label_repel(aes(x = Fecha, y = KM_agrupado, label = KM_agrupado), 
                     nudge_y = max(consumo_tapa[Ficha == ficha_plot, KM_agrupado], na.rm = TRUE) * 0.05, 
                     ylim = c(0,max(consumo_tapa[Ficha == ficha_plot, KM_agrupado], na.rm = TRUE)*1.2)) +
    labs(title = paste0("Kilómetros recorridos en julio por la ficha F", ficha_plot),
         y = "KM realizados [KM]") +
    #ylim(0, max(consumo_tapa[Ficha == ficha_plot, KM_agrupado], na.rm = TRUE) * 1.1)+
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      axis.text.x = element_text(angle = 45, size = 14, hjust = 1),
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(margin = margin(0,1,0,0, "cm"), size = 16),
      axis.title.x = element_text(margin = margin(1,0,0,0, "cm"), size = 16),

    ) +
    scale_x_date(date_labels = "%d-%b-%y", 
                 limits = c(min(consumo_tapa$Fecha), 
                            max(consumo_tapa$Fecha)),
                 breaks = "day")
  
  plot_lts <- ggplot(subset(consumo_tapa, Ficha == ficha_plot)) +
    geom_line(aes(x = Fecha, y = LTS_agrupado, colour = "Litros Cargados"), 
              alpha = 0.7, linewidth = 1.5) +
    geom_point(aes(x = Fecha, y = LTS_agrupado, colour = "Litros Cargados"), 
               alpha = 0.7, size = 3) +
    geom_line(aes(x = Fecha, y = LTS_teoricos, colour = "Litros Teórico"), 
              alpha = 0.7, linewidth = 1.5, linetype = "dashed") +
    geom_label_repel(aes(x = Fecha, y = LTS_agrupado, label = LTS_agrupado), 
                     nudge_y = max(consumo_tapa[Ficha == ficha_plot, LTS_agrupado], na.rm = TRUE) * 0.05, 
                     ylim = c(0,max(consumo_tapa[Ficha == ficha_plot, LTS_agrupado], na.rm = TRUE)*1.2)) +
    theme_minimal() +
    labs(title = paste0("Litros cargados en julio por la ficha F", ficha_plot),
         y = "LTS cargados [LTS]") +
    #ylim(0, max(consumo_tapa[Ficha == ficha_plot, LTS_agrupado], na.rm = TRUE) * 1.1)+
    theme(
      legend.position = "top",
      legend.text = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, size = 18),
      legend.title = element_text(size = 16),
      axis.text.x = element_text(angle = 45, size = 14, hjust = 1),
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(margin = margin(0,1,0,0, "cm"), size = 16),
      axis.title.x = element_text(margin = margin(1,0,0,0, "cm"), size = 16),
    ) +
    scale_colour_manual(name = "Litros",
                        values = c("Litros Cargados" = "blue",
                                   "Litros Teórico" = "red")) +
    scale_x_date(date_labels = "%d-%b-%y", 
                 limits = c(min(consumo_tapa$Fecha), 
                            max(consumo_tapa$Fecha)),
                 breaks = "day")
  
  plot_consumo <- ggplot(subset(consumo_tapa, Ficha == ficha_plot)) +
    geom_line(aes(x = Fecha, y = Consumo, colour = "Consumo [lts/100km]"), 
              alpha = 0.7, linewidth = 1.5) +
    geom_point(aes(x = Fecha, y = Consumo, colour = "Consumo [lts/100km]"), 
               alpha = 0.7, size = 3) +
    geom_line(aes(x = Fecha, y = Consumo_teorico, colour = "Consumo Teórico [lts/100km]"), 
              alpha = 0.7, linewidth = 1.5, linetype = "dashed") +
    geom_label_repel(aes(x = Fecha, y = Consumo, label = Consumo), 
                     nudge_y = max(consumo_tapa[Ficha == ficha_plot, Consumo], na.rm = TRUE) * 0.05, 
                     ylim = c(0,max(consumo_tapa[Ficha == ficha_plot, Consumo], na.rm = TRUE)*1.2)) +
    theme_minimal() +
    labs(title = paste0("Consumo en julio de la ficha F", ficha_plot),
         y = "Consumo [lts/100km]") +
    #ylim(0, max(consumo_tapa[Ficha == ficha_plot, Consumo], na.rm = TRUE) * 1.5)+
    theme(
      legend.position = "top",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      plot.title = element_text(hjust = 0.5, size = 18),
      axis.text.x = element_text(angle = 45, size = 14, hjust = 1),
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(margin = margin(0,1,0,0, "cm"), size = 16),
      axis.title.x = element_text(margin = margin(1,0,0,0, "cm"), size = 16)
    ) +
    scale_colour_manual(name = "Litros",
                        values = c("Consumo [lts/100km]" = "blue",
                                   "Consumo Teórico [lts/100km]" = "red")) +
    scale_x_date(date_labels = "%d-%b-%y", 
                 limits = c(min(consumo_tapa$Fecha), 
                            max(consumo_tapa$Fecha)),
                 breaks = "day")
  print(plot_km, width = 12)
  print(plot_lts)
  print(plot_consumo)
  dev.off()
}

write_clip(consumo_tapa)
