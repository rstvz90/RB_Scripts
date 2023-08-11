# INICIACION VARIABLES --------------------------------
gc()
(rm(list = ls())) # limpiar las var del environment

# Lista de paquetes a utilizar
listofpackages <- c(
  "tidyverse", "lubridate", "readxl",
  "rstudioapi", "data.table",
  "tibble", "ggplot2", "DT", "plotly",
  "janitor", "clipr", "skimr", "ggrepel"
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

consumo_empleados <- consumo_empleados[, .(
  Distancia = sum(Distance, na.rm = TRUE),
  Tiempo = sum(times.driving, na.rm = TRUE),
  LTS = sum(consumption.fuel, na.rm = TRUE),
  Acelerador_dist = weighted.mean(accelerator.get80, Distance, na.rm = TRUE),
  Acelerador_time = weighted.mean(accelerator.get80, times.driving, na.rm = TRUE),
  Acelerador_time_2 = weighted.mean(accelerator.get80, times.engine, na.rm = TRUE),
  cant_servicios = .N
), by = .(cod_emp, Chasis)]

consumo_empleados[, `:=`(
  Consumo = LTS / Distancia * 100
)]
choferes_mismo_chasis <- consumo_empleados[, .(cant = .N), by = cod_emp]
choferes_mismo_chasis <- choferes_mismo_chasis[cant == 1, cod_emp]
consumo_empleados_2 <- consumo_empleados[cant_servicios > 10 & cod_emp %in% choferes_mismo_chasis,]

etiquetas_consumo <- c("Bajo", "Medio", "Alto")

consumo_empleados[, Nivel_consumo := cut(Consumo,
  breaks = quantile(Consumo, probs = c(0, 0.20, 0.80, 1)),
  labels = etiquetas_consumo,
  include.lowest = TRUE
), by = Chasis]

consumo_empleados_plot <- consumo_empleados[Nivel_consumo != "Medio", ]

ggplot(consumo_empleados_plot) +
  aes(x = Nivel_consumo, y = Acelerador_dist) +
  geom_boxplot()


ggplot(consumo_empleados_plot) +
  aes(x = Nivel_consumo, y = Acelerador_time) +
  geom_boxplot()

