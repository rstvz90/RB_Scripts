library(data.table)
library(skimr)
library(lubridate)
library(ggplot2)
library(plotly)

rm(list = ls())

datos <- fread("//sm-public/Public/Mantenimiento/REstevez/Operaciones/Cambios Diagramacion Sep23.txt")
datos <- datos[Linea %in% c("MORENO 203", "PILAR 203"),]

datos[, FechaServicio := as.Date(FechaServicio)]
datos[, `Sale Real` := dmy_hm(`Sale Real`)]
datos[, `Sale Teórico` := dmy_hm(`Sale Teórico`)]
datos[, llega := dmy_hm(llega)]
datos <- datos[is.na(as.numeric(Ficha)), Tiene_ficha := "NO"]
datos <- datos[is.na(Tiene_ficha), Tiene_ficha := "SI"]
datos <- datos[Ficha == "0", Tiene_ficha := "NO"]
datos[, Tiene_Galpon := "SI"]
datos <- datos[Galpon == "", Tiene_Galpon := "NO"]
datos[, Tiene_Empleado := "SI"]
datos <- datos[EmpleadoPlanilla == "", Tiene_Empleado := "NO"]
datos[, Sale_horario := Dif == 0]
datos <- datos[is.na(CambioBandera), CambioBandera := 0]

mv_con_cambio <- datos[CambioBandera == 1,]

skim(datos)

# GRAFICOS ----------------------

ggplot(datos) +
  aes(x = as.character(CambioBandera), y = after_stat(count/tapply(count, PANEL, sum)[PANEL])*100,
      label = paste0(round(after_stat(count/tapply(count, PANEL, sum)[PANEL])*100,1),"%",
                     "\n","(",..count..,")")) +
  geom_bar(stat = "count", position = "dodge",  fill = "lightblue", color = "grey") +
  geom_text(aes(x = as.character(CambioBandera),  y = (after_stat(count/tapply(count, PANEL, sum)[PANEL])*100)),
            stat = "count", nudge_y = 12)+
  labs(title = "Cantidad de Datos sin Galpón de salida")+
  facet_grid(~Tiene_Galpon) +
  ylim(0,120) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust= 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )


ggplot(datos) +
  aes(x = as.character(CambioBandera), y = after_stat(count/tapply(count, PANEL, sum)[PANEL])*100,
      label = paste0(round(after_stat(count/tapply(count, PANEL, sum)[PANEL])*100,1),"%",
                     "\n","(",..count..,")")) +
  geom_bar(stat = "count", position = "dodge",  fill = "lightblue", color = "grey") +
  geom_text(aes(x = as.character(CambioBandera),  y = (after_stat(count/tapply(count, PANEL, sum)[PANEL])*100)),
            stat = "count", nudge_y = 12)+
  facet_grid(~Tiene_Empleado) +
  labs(title = "Cantidad de Datos sin Nombre de Empleado")+
  theme_minimal() +
  ylim(0,130)+
  theme(
    plot.title = element_text(hjust= 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )


ggplot(datos) +
  aes(x = as.character(CambioBandera), y = after_stat(count/tapply(count, PANEL, sum)[PANEL])*100,
      label = paste0(round(after_stat(count/tapply(count, PANEL, sum)[PANEL])*100,1),"%",
                     "\n","(",..count..,")")) +
  geom_bar(stat = "count", position = "dodge",  fill = "lightblue", color = "grey") +
  geom_text(aes(x = as.character(CambioBandera),  y = (after_stat(count/tapply(count, PANEL, sum)[PANEL])*100)),
            stat = "count", nudge_y = 12)+
  facet_grid(~Tiene_ficha) +
  labs(title = "Cantidad de Datos sin Ficha")+
  ylim(0,130) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust= 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

ggplot(datos) +
  aes(x = as.character(CambioBandera), y = after_stat(count/tapply(count, PANEL, sum)[PANEL])*100,
      label = paste0(round(after_stat(count/tapply(count, PANEL, sum)[PANEL])*100,1),"%",
                     "\n","(",..count..,")")) +
  geom_bar(stat = "count", position = "dodge",  fill = "lightblue", color = "grey") +
  geom_text(aes(x = as.character(CambioBandera),  y = (after_stat(count/tapply(count, PANEL, sum)[PANEL])*100)),
            stat = "count", nudge_y = 12)+
  facet_grid(~Sale_horario) +
  labs(title = "Cantidad de MV a horario")+
  ylim(0,130)+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust= 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )


ggplot(subset(datos, CambioBandera == 1)) +
  aes(x = as.character(Sale_horario), y = after_stat(count/tapply(count, PANEL, sum)[PANEL])*100,
      label = paste0(round(after_stat(count/tapply(count, PANEL, sum)[PANEL])*100,1),"%",
                     "\n","(",..count..,")")) +
  geom_bar(stat = "count", position = "dodge",  fill = "lightblue", color = "grey") +
  geom_text(aes(x = as.character(Sale_horario),  y = (after_stat(count/tapply(count, PANEL, sum)[PANEL])*100)),
            stat = "count", nudge_y = 12)+
  labs(title = "Cantidad de MV a horario")+
  ylim(0,130)+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust= 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

head(mv_con_cambio[observaciones != "", "observaciones"],50)

