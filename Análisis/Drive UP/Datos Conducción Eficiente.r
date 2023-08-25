# INICIACION VARIABLES --------------------------------
gc()
(rm(list = ls())) # limpiar las var del environment

# Lista de paquetes a utilizar
listofpackages <- c(
  "tidyverse", "lubridate", "readxl",
  "rstudioapi", "data.table", "corrplot",
  "tibble", "ggplot2", "DT", "plotly", "knitr",
  "janitor", "clipr", "skimr", "ggrepel","factoextra"
)

# revisar e instalar librerias que no es están instaladas
newPackages <- listofpackages[!(listofpackages %in% installed.packages()[, "Package"])]

if (length(newPackages)) install.packages(newPackages)
for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}

# LECTURA DE ARCHIVOS ------------------------------

path <- "C:/Users/REstevez/Documents/Conducción DriveUP/Datos Línea 41/"

files <- list.files(path, full.names = TRUE)

files_acelerador <- files[grepl("acelerador", files)]
files_velocidad <- files[grepl("velocidad", files)]
files_torque <- files[grepl("torque", files)]
files_empleados <- files[grepl("empleados", files)]
files_legajo <- files[grepl("legajo", files)]
files_rpm <- files[grepl("rpm", files)]
files_rpm_optimo <- files[grepl("rev", files)]
files_tiempos <- files[grepl("tiempos", files)]
files_freno <- files[grepl("freno", files)]
files_chasis <- files[grepl("chasis", files)]
files_consumo <- files[grepl("consumo", files)]
files_ralenti <- files[grepl("ralenti", files)]
files_pbi <- files[grepl("PBI", files)]

acelerador_41 <- fread(files_acelerador)
velocidad_41 <- fread(files_velocidad)
torque_41 <- fread(files_torque)
empleados_41 <- fread(files_empleados)
legajo_41 <- fread(files_legajo)
rpm_41 <- fread(files_rpm)
rpm_optimo_41 <- fread(files_rpm_optimo)
tiempos_41 <- fread(files_tiempos)
freno_41 <- fread(files_freno)
chasis_41 <- fread(files_chasis)
consumo_41 <- fread(files_consumo)
ralenti_41 <- fread(files_ralenti)
pbi_consumo  <- fread(files_pbi)

# ACELERADOR -----------------------------------

acelerador_cor <- merge.data.table(acelerador_41, consumo_41,
  by = "cod_desig",
  all.x = TRUE
)
acelerador_cor <- merge.data.table(acelerador_cor, chasis_41,
  by = "cod_desig",
  all.x = TRUE
)
acelerador_cor <- acelerador_cor[`Suma de Distance` > 50, ]
acelerador_cor <- acelerador_cor[`Suma de consumption.avgFuel` > 20, ]

# ggplot(acelerador_cor) +
#  aes(x = `Suma de accelerator.get80`, y = `Suma de consumption.avgFuel`, color = Chasis) +
#  geom_point() +
#  geom_smooth(method = lm)

acelerador_cor <- acelerador_cor[, c(2, 3, 4, 5, 6, 10, 13)]

setnames(acelerador_cor, colnames(acelerador_cor), gsub("Suma de ", "", colnames(acelerador_cor)))

columnas <- colnames(acelerador_cor)

tabla_acelerador <- list()
for (i in c(1:5)) {
  tabla_acelerador[[i]] <- acelerador_cor[, .(correlacion = cor(
    get(columnas[i]),
    get(columnas[6])
  ), Pos = columnas[i]),
  by = .(Chasis)
  ]
}
acelerador_cor <- rbindlist(tabla_acelerador)
acelerador_cor_matrix <- dcast(acelerador_cor, ... ~ Chasis, value.var = "correlacion")

ggplot(data = acelerador_cor) +
  aes(x = Pos, y = correlacion, label = round(correlacion, 2)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
  geom_text(nudge_y = 0.03) +
  facet_grid(~Chasis) +
  theme_minimal()

# RPM -----------------------------------

RPM_cor <- merge.data.table(rpm_41, consumo_41,
  by = "cod_desig",
  all.x = TRUE
)
RPM_cor <- merge.data.table(RPM_cor, chasis_41,
  by = "cod_desig",
  all.x = TRUE
)
RPM_cor <- RPM_cor[`Suma de Distance` > 50, ]
RPM_cor <- RPM_cor[`Suma de consumption.avgFuel` > 20, ]

# ggplot(acelerador_cor) +
#  aes(x = `Suma de accelerator.get80`, y = `Suma de consumption.avgFuel`, color = Chasis) +
#  geom_point() +
#  geom_smooth(method = lm)
colnames(RPM_cor)
RPM_cor <- RPM_cor[, c(2:11, 13, 16, 19)]

setnames(RPM_cor, colnames(RPM_cor), gsub("Suma de ", "", colnames(RPM_cor)))

columnas <- colnames(RPM_cor)

tabla_rpm <- list()
for (i in c(1:11)) {
  tabla_rpm[[i]] <- RPM_cor[, .(correlacion = cor(
    get(columnas[i]),
    get(columnas[12])
  ), Pos = columnas[i]),
  by = .(Chasis)
  ]
}
RPM_cor <- rbindlist(tabla_rpm)
rpm_cor_matrix <- dcast(RPM_cor, ... ~ Chasis, value.var = "correlacion")

ggplot(data = RPM_cor) +
  aes(x = Pos, y = correlacion, label = round(correlacion, 2)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
  geom_text(nudge_y = 0.03) +
  facet_grid(~Chasis) +
  theme_minimal()

# MAYORES vs MENORES -----------------------
consumo_empleados <- merge.data.table(acelerador_41, consumo_41,
  by = "cod_desig",
  all.x = TRUE
)
consumo_empleados <- merge.data.table(consumo_empleados, chasis_41,
  by = "cod_desig",
  all.x = TRUE
)
consumo_empleados <- merge.data.table(consumo_empleados, empleados_41,
  by = "cod_desig",
  all.x = TRUE
)
consumo_empleados <- merge.data.table(consumo_empleados, tiempos_41,
  by = "cod_desig",
  all.x = TRUE
)

servicios_por_chasis <- consumo_empleados[, .(Cant_Servicios = .N) ,by = .(cod_emp, Chasis)]
choferes_mismo_chasis <- servicios_por_chasis[, .(cant = .N), by = cod_emp]
choferes_mismo_chasis <- choferes_mismo_chasis[cant == 1, cod_emp]

consumo_empleados <- consumo_empleados[cod_emp %in% choferes_mismo_chasis, ]
consumo_empleados <- consumo_empleados[`Suma de Distance` > 50, ]
consumo_empleados <- consumo_empleados[`Suma de consumption.avgFuel` > 20, ]

colnames(consumo_empleados)
consumo_empleados <- consumo_empleados[, c(6, 8:10, 13, 14, 18, 19, 20, 21)]

setnames(consumo_empleados, colnames(consumo_empleados), gsub("Suma de ", "", colnames(consumo_empleados)))

consumo_empleados_agrupados <- consumo_empleados[, .(
  Distancia = sum(Distance, na.rm = TRUE),
  Tiempo = sum(times.driving, na.rm = TRUE),
  LTS = sum(consumption.fuel, na.rm = TRUE),
  Acelerador_time = weighted.mean(accelerator.get80, times.driving, na.rm = TRUE),
  Acelerador_time_2 = weighted.mean(accelerator.get80, times.engine, na.rm = TRUE),
  cant_servicios = .N
), by = .(cod_emp, Chasis)]

consumo_empleados_agrupados[, `:=`(
  Consumo = LTS / Distancia * 100
)]

consumo_empleados_agrupados <- consumo_empleados_agrupados[cant_servicios > 10,]

etiquetas_consumo <- c("Bajo", "Medio", "Alto")

consumo_empleados_agrupados[, Nivel_consumo := cut(Consumo,
  breaks = quantile(Consumo, probs = c(0, 0.30, 0.70, 1)),
  labels = etiquetas_consumo,
  include.lowest = TRUE
), by = Chasis]

consumo_empleados_agrupados[, .(cantidad = .N), by = .(Chasis, Nivel_consumo)]

consumo_empleados_plot <- consumo_empleados_agrupados[Nivel_consumo != "Medio", ]

ggplot(consumo_empleados_plot) +
  aes(x = Nivel_consumo, y = Acelerador_time) +
  geom_boxplot()+
  facet_grid(~Chasis)+
  labs(
    title = "Acelerador en función del consumo",
    subtitle = "30% de choferes con consumo más alto vs. el 30% con consumo más bajo",
    x = "Nivel Consumo",
    y = "Porcentaje acelerador a +80%"
  ) + 
  theme(
    plot.title = element_text(hjus = 0.5, size = 36),
    plot.subtitle = element_text(hjus = 0.5, size = 18),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    strip.text = element_text(size = 18)
  )

cant_servicios <- consumo_empleados[, .(cantidad = .N), by = .(cod_emp, Chasis)]
setorder(cant_servicios, -cantidad)

# AGRUPACIÓN POR FICHA -----------------------

consumo_agrupados_ficha <- consumo_empleados[, .(
  Distancia = sum(Distance, na.rm = TRUE),
  Tiempo = sum(times.driving, na.rm = TRUE),
  LTS = sum(consumption.fuel, na.rm = TRUE),
  Acelerador_time = weighted.mean(accelerator.get80, times.driving, na.rm = TRUE),
  Acelerador_time_2 = weighted.mean(accelerator.get80, times.engine, na.rm = TRUE),
  cant_servicios = .N
), by = .(ficha, Chasis)]

consumo_agrupados_ficha[, `:=`(
  Consumo = LTS / Distancia * 100
)]

consumo_agrupados_ficha <- consumo_agrupados_ficha[cant_servicios > 12,]

etiquetas_consumo <- c("Bajo", "Medio", "Alto")

consumo_agrupados_ficha[, Nivel_consumo := cut(Consumo,
  breaks = quantile(Consumo, probs = c(0, 0.30, 0.70, 1)),
  labels = etiquetas_consumo,
  include.lowest = TRUE
), by = Chasis]

consumo_agrupados_ficha[, .(cantidad = .N), by = .(Chasis, Nivel_consumo)]

consumo_ficha_plot <- consumo_agrupados_ficha[Nivel_consumo != "Medio", ]

ggplotly(
ggplot(consumo_ficha_plot) +
  aes(x = Nivel_consumo, y = Acelerador_time) +
  geom_boxplot()+
  facet_grid(~Chasis)
)

cant_servicios <- consumo_empleados[, .(cantidad = .N), by = .(cod_emp, Chasis)]
setorder(cant_servicios, -cantidad)

# AGRUPACIÓN POR FICHA-FECHA -----------------------

consumo_ficha_fecha <- merge.data.table(chasis_41, consumo_41,
  by = "cod_desig",
  all.x = TRUE
)
consumo_ficha_fecha <- merge.data.table(consumo_ficha_fecha, empleados_41,
  by = "cod_desig",
  all.x = TRUE
)
consumo_ficha_fecha <- merge.data.table(consumo_ficha_fecha, tiempos_41,
  by = "cod_desig",
  all.x = TRUE
)
consumo_ficha_fecha <- merge.data.table(consumo_ficha_fecha, acelerador_41,
  by = "cod_desig",
  all.x = TRUE
)

#consumo_ficha_fecha <- consumo_ficha_fecha[ficha == "F3495",]
setnames(consumo_ficha_fecha, colnames(consumo_ficha_fecha), gsub("Suma de ", "", colnames(consumo_ficha_fecha)))

consumo_ficha_fecha <- consumo_ficha_fecha[!is.na(accelerator.get80),]

consumo_agrupados_ff <- consumo_ficha_fecha[, .(
  Distancia = sum(Distance, na.rm = TRUE),
  LTS = sum(consumption.fuel, na.rm = TRUE),
  Acelerador = weighted.mean(accelerator.get80, times.driving, na.rm = TRUE),
  Tiempo = sum(times.driving, na.rm = TRUE),
  cant_servicios = .N
), by = .(ficha, Fecha, Chasis)]

consumo_agrupados_ff[, `:=`(
  Consumo_DUP = LTS / Distancia * 100
)]

consumo_agrupados_ff <- consumo_agrupados_ff[Distancia > 50,]
consumo_agrupados_ff <- consumo_agrupados_ff[Consumo_DUP > 20,]

consumo_ficha_pbi <- pbi_consumo[, .(
  KM_pbi = sum(Km, na.rm = TRUE),
  LTS_pbi = sum(ConsumoTotal, na.rm = TRUE)
), by = .(Ficha, Fecha)]

consumo_ficha_pbi <- consumo_ficha_pbi[, .SD[-1], by = Ficha]

consumo_ff <- merge.data.table(consumo_ficha_pbi, consumo_agrupados_ff,
  by.y = c("ficha", "Fecha"),
  by.x = c("Ficha", "Fecha"),
  all.x = TRUE
)

consumo_ff <- consumo_ff[, .(
  Distancia_DUP = sum(Distancia, na.rm = TRUE),
  LTS_DUP = sum(LTS, na.rm = TRUE),
  Acelerador = weighted.mean(Acelerador, Tiempo, na.rm = TRUE),
  Tiempo = sum(Tiempo, na.rm = TRUE),
  KM_pbi = sum(KM_pbi, na.rm = TRUE),
  LTS_pbi = sum(LTS_pbi, na.rm = TRUE)
), by = .(Ficha)]

consumo_ff[, `:=`(
  Consumo_DUP = LTS_DUP / Distancia_DUP * 100,
  Consumo_PBI = LTS_pbi / KM_pbi * 100
)]

consumo_ff[, `:=`(
  relacion_consumo = Consumo_DUP / Consumo_PBI - 1
)]

consumo_ff <- consumo_ff[Distancia_DUP > 0,]
consumo_ff <- consumo_ff[LTS_DUP > 0,]
consumo_ff <- consumo_ff[KM_pbi > 0,]

consumo_ff <- merge.data.table(consumo_ff, chasis_41,
  by.y = c("ficha"),
  by.x = c("Ficha"),
  all.x = TRUE
)

ggplotly(
ggplot(subset(consumo_ficha_fecha, ficha == "F3495")) +
  aes(x = Acelerador, y = Consumo_DUP) +
  geom_point()+
  facet_grid(~Chasis)
)

ggplotly(
ggplot(subset(consumo_ficha_fecha, ficha == "F3495")) +
  aes(x = Acelerador, y = get("l/100km")) +
  geom_point()+
  facet_grid(~Chasis)
)

ggplotly(
ggplot(subset(consumo_ficha_fecha, ficha == "F3495")) +
  geom_point(aes(x = Acelerador, y = get("l/100km")), color = "blue") +
  geom_point(aes(x = Acelerador, y = Consumo_DUP), color = "red")
)

etiquetas_consumo <- c("Bajo", "Medio", "Alto")

consumo_ff[, Nivel_consumo_PBI := cut(Consumo_PBI,
  breaks = quantile(Consumo_PBI, probs = c(0, 0.30, 0.70, 1)),
  labels = etiquetas_consumo,
  include.lowest = TRUE
), by = Chasis]

consumo_ff[, Nivel_consumo_DUP := cut(Consumo_DUP,
  breaks = quantile(Consumo_DUP, probs = c(0, 0.30, 0.70, 1)),
  labels = etiquetas_consumo,
  include.lowest = TRUE
), by = Chasis]

#consumo_ficha_plot <- consumo_ff[Nivel_consumo != "Medio", ]

ggplotly(
ggplot(consumo_ff) +
  aes(x = Nivel_consumo_DUP, y = Acelerador) +
  geom_boxplot()+
  facet_grid(~Chasis)
)

ggplot(subset(consumo_ff, Nivel_consumo_PBI != "Medio")) +
  aes(x = Nivel_consumo_PBI, y = Acelerador) +
  geom_boxplot()+
  facet_grid(~Chasis)+
  labs(
    title = "Acelerador en función del consumo",
    subtitle = "30% de choferes con consumo más alto vs. el 30% con consumo más bajo",
    x = "Nivel Consumo",
    y = "Porcentaje acelerador a +80%"
  ) + 
  theme(
    plot.title = element_text(hjus = 0.5, size = 36),
    plot.subtitle = element_text(hjus = 0.5, size = 18),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    strip.text = element_text(size = 18)
  )

ggplot(subset(consumo_ff, Nivel_consumo_DUP != "Medio")) +
  aes(x = Nivel_consumo_DUP, y = Acelerador) +
  geom_boxplot()+
  facet_grid(~Chasis)+
  labs(
    title = "Acelerador en función del consumo",
    subtitle = "30% de choferes con consumo más alto vs. el 30% con consumo más bajo",
    x = "Nivel Consumo",
    y = "Porcentaje acelerador a +80%"
  ) + 
  theme(
    plot.title = element_text(hjus = 0.5, size = 36),
    plot.subtitle = element_text(hjus = 0.5, size = 18),
    axis.title.x = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    strip.text = element_text(size = 18)
  )

ggplotly(
ggplot(consumo_ff) +
  aes(x = Consumo_PBI, y = Consumo_DUP) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~Chasis)
)

ggplotly(
ggplot(consumo_ficha_fecha) +
  aes(x = Consumo_PBI, y = Consumo_DUP) +
  geom_point() +
  facet_grid(~Chasis)
)

consumo_ff[, .(correlacion = cor(Consumo_DUP, Consumo_PBI)), by = .(Chasis)]

cor(consumo_ff[Chasis == "BUS AGRALE MT 17.0/LE", Consumo_DUP], consumo_ff[Chasis == "BUS AGRALE MT 17.0/LE",Consumo_PBI])
cor(consumo_ff[Chasis == "MB O500U 1826-59", Consumo_DUP], consumo_ff[Chasis == "MB O500U 1826-59",Consumo_PBI])

# PCA -------------------------

datos_pca <- merge.data.table(consumo_41, acelerador_41,
  by = "cod_desig",
  all.x = TRUE
)
datos_pca <- merge.data.table(datos_pca, chasis_41,
  by = "cod_desig",
  all.x = TRUE
)
datos_pca <- merge.data.table(datos_pca, empleados_41,
  by = "cod_desig",
  all.x = TRUE
)
datos_pca <- merge.data.table(datos_pca, freno_41,
  by = "cod_desig",
  all.x = TRUE
)
datos_pca <- merge.data.table(datos_pca, ralenti_41,
  by = "cod_desig",
  all.x = TRUE
)
datos_pca <- merge.data.table(datos_pca, rpm_41,
  by = "cod_desig",
  all.x = TRUE
)
datos_pca <- merge.data.table(datos_pca, rpm_optimo_41,
  by = "cod_desig",
  all.x = TRUE
)
datos_pca <- merge.data.table(datos_pca, tiempos_41,
  by = "cod_desig",
  all.x = TRUE
)
datos_pca <- merge.data.table(datos_pca, torque_41,
  by = "cod_desig",
  all.x = TRUE
)
datos_pca <- merge.data.table(datos_pca, velocidad_41,
  by = "cod_desig",
  all.x = TRUE
)

setnames(datos_pca, colnames(datos_pca), gsub("Suma de ", "", colnames(datos_pca)))

datos_pca <- datos_pca[,-c("Aceleradorprom", "Empleado", "Tipo dia", "Fecha")]

datos_pca[, ficha := substr(ficha, 2, 5)]
datos_pca[, ficha := as.numeric(ficha)]

datos_pca_agrale <- datos_pca[Chasis == "BUS AGRALE MT 17.0/LE",]
datos_pca_agrale <- datos_pca_agrale[!is.na(cod_empleado)]
datos_pca_agrale <- datos_pca_agrale[,-c("Chasis")]

datos_pca_mb <- datos_pca[Chasis == "MB O500U 1826-59",]
datos_pca_mb <- datos_pca_mb[!is.na(cod_empleado)]
datos_pca_mb <- datos_pca_mb[,-c("Chasis")]

pca_agrale <- FactoMineR::PCA(X = datos_pca_agrale, scale.unit = T, ncp = ncol(datos_pca_agrale), graph = F)

fviz_eig(pca_agrale, addlabels = TRUE, ncp = ncol(datos_pca_agrale))

eig.val <- factoextra::get_eigenvalue(pca_agrale)
kable(eig.val)

var_agrale <- get_pca_var(pca_agrale)

corrplot(var_agrale$contrib, is.corr=FALSE) 

contribucion_consumo_agrale <-  sort(t(var_agrale$cos2)[,"consumption.avgFuel"], decreasing = TRUE)
contribucion_consumo_agrale <- as.data.frame(contribucion_consumo_agrale)
dimensiones <- rownames(contribucion_consumo_agrale)
contribucion_consumo_agrale <- data.frame(dimensiones, contribucion_consumo_agrale)
colnames(contribucion_consumo_agrale) <- c("componente", "contribucion")

ggplot(contribucion_consumo_agrale,aes(x = reorder(componente, -contribucion), y = contribucion, 
               label = paste0(round(contribucion * 100),"%"))) +
  geom_bar(stat = "identity", fill = "lightblue", color = "grey") +
  geom_text(nudge_y = 0.02) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  labs(title = "Contribución de la variable Consumo",
       x = "Componentes", y = "Porcentaje Contribución") +
  theme(plot.title = element_text(hjust = 0.5))

fviz_pca_ind(pca_agrale, axes = c(3,1), label = "none",
             habillage = "consumption.avgFuel")
