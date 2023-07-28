library(data.table)
library(readxl)
library(lubridate)
library(ggplot2)
library(plotly)

novedades_vco <- read_xlsx("//sm-public/Public/Mantenimiento/REstevez/Para Borrar/novedades_vco.xlsx")

setDT(novedades_vco)

novedades_vco[, `:=`(FECHA = dmy_hms(FECHA))]
novedades_vco[, `:=`(HORA = hour(FECHA))]
novedades_vco[, FECHA_CORREGIDA := as.Date(FECHA)]
novedades_vco[HORA < 6, FECHA_CORREGIDA := FECHA_CORREGIDA - days(1)]

novedades_vco_dia <- novedades_vco[, .(Cantidad_novedades = .N),
                                   by = .(FECHA_CORREGIDA)]
ggplotly(
ggplot(novedades_vco_dia) +
  aes(x = FECHA_CORREGIDA, y = Cantidad_novedades, label = Cantidad_novedades) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(nudge_y = 3) +
  labs(title = "Cantidad de Novedades Diarias - VCO") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ),
tooltip = c("x","y")
)
