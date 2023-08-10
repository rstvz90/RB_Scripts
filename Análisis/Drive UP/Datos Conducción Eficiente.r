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

correlaciones <- merge.data.table(acelerador_41, consumo_41,
  by = "cod_desig",
  all.x = TRUE
)
correlaciones <- merge.data.table(correlaciones, chasis_41,
  by = "cod_desig",
  all.x = TRUE
)
correlaciones <- correlaciones[`Suma de Distance` > 50, ]
correlaciones <- correlaciones[`Suma de consumption.avgFuel` > 20, ]
colnames(correlaciones)
ggplot(correlaciones) +
  aes(x = `Suma de accelerator.get80`, y = `Suma de consumption.avgFuel`, color = Chasis) +
  geom_point() +
  geom_smooth(method = lm)

correlaciones <- correlaciones[, c(2, 3, 4, 5, 6, 10, 13)]

setnames(correlaciones, colnames(correlaciones), gsub("Suma de ", "", colnames(correlaciones)))

columnas <- colnames(correlaciones)

tabla <- list()
for (i in c(1:5)) {
  tabla[[i]] <- correlaciones[, .(correlacion = cor(
      get(columnas[i]),
      get(columnas[6])
    ), Pos = columnas[i]),
    by = .(Chasis)
    ]
}
correlaciones <- rbindlist(tabla)
correlaciones_matrix <- dcast(correlaciones, ...~Chasis, value.var = "correlacion")

ggplot(data = correlaciones) +
  aes(x = Pos, y = correlacion, label = round(correlacion,2)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
  geom_text(nudge_y = 0.03) +
  facet_grid(~Chasis) +
  theme_minimal()

