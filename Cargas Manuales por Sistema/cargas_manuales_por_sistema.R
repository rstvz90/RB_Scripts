library(data.table)
library(clipr)

cargas_manuales <- fread("C:/Users/REstevez/Documents/Análisis/Cargas Manuales por Sistema/cargas_julio_laq.csv")

cargas_manuales <- cargas_manuales[Texto != "A",]
cargas_manuales <- cargas_manuales[Texto != "M",]

cantidad_dia <- cargas_manuales[, .(Cantidad = .N), by = .(FechaCorregida)]
cantidad_ficha <- cargas_manuales[, .(Cantidad = .N), by = .(Ficha)]
cargas_f31602 <- cargas_manuales[Ficha == "F3160", FechaCorregida]

write_clip(setorder(cantidad_ficha, -Cantidad))
write_clip(cantidad_dia)
write_clip(cargas_manuales)
