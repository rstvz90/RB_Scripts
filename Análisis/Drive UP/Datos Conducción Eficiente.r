# INICIACION VARIABLES --------------------------------

gc()
(rm(list = ls())) # limpiar las var del environment

# Lista de paquetes a utilizar
listofpackages <- c(
  "tidyverse", "lubridate", "readxl",
  "rstudioapi", "data.table", "corrplot",
  "tibble", "ggplot2", "DT", "plotly", "knitr",
  "janitor", "clipr", "skimr", "ggrepel", "factoextra",
  "corrplot", "ade4", "cluster", "stats",
  "cowplot", "rpart.plot",
  "rpart", "caret", "ROCR", "Metrics"
)

# revisar e instalar librerias que no es están instaladas
newPackages <- listofpackages[!(listofpackages %in% installed.packages()[, "Package"])]

if (length(newPackages)) install.packages(newPackages)
for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}


generar_boxplots_por_grupos <- function(df, var_interes, grupos = "cluster") {
  # Convertir a cuadro de datos
  df <- data.frame(df)

  # Definir etiquetas que con la cantidad de observaciones por grupos
  etiquetas <- paste(
    levels(factor(df[, grupos])), "\n(N = ", table(df[, grupos]), ")",
    sep = ""
  )

  # Generar gráfico
  boxplot <- ggplot(
    df,
    aes(
      x = factor(get(grupos)), y = get(var_interes),
      fill = factor(get(grupos))
    )
  ) +
    geom_boxplot() +
    theme(legend.position = "none") +
    scale_x_discrete(name = paste0(grupos), labels = etiquetas) +
    scale_y_continuous(name = paste0(var_interes)) +
    geom_hline(yintercept = median(df[, var_interes])) +
    theme(axis.text.x = element_text(size = rel(0.75)))

  return(boxplot)
}


generar_boxplots_por_grupos_2 <- function(df, var_interes, grupos = "Nivel_consumo") {
  # Convertir a cuadro de datos
  df <- data.frame(df)

  # Definir etiquetas que con la cantidad de observaciones por grupos
  etiquetas <- paste(
    levels(factor(df[, grupos])), "\n(N = ", table(df[, grupos]), ")",
    sep = ""
  )

  # Generar gráfico
  boxplot <- ggplot(
    df,
    aes(
      x = factor(get(grupos)), y = get(var_interes),
      fill = factor(get(grupos))
    )
  ) +
    geom_boxplot() +
    theme(legend.position = "none") +
    scale_x_discrete(name = paste0(grupos), labels = etiquetas) +
    scale_y_continuous(name = paste0(var_interes)) +
    geom_hline(yintercept = median(df[, var_interes])) +
    theme(axis.text.x = element_text(size = rel(0.75)))

  return(boxplot)
}


agrupamiento_km <- function(Fecha_hora) {
  for (i in c(1:length(Fecha_hora))) {
    if (is.na(Fecha_hora[i])) {
      Fecha_hora[i] <- Fecha_hora[i - 1]
    }
  }
  return(Fecha_hora)
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
files_chasis_41 <- files[grepl("_chasis", files)]
files_consumo <- files[grepl("consumo", files)]
files_ralenti <- files[grepl("ralenti", files)]
files_combustible <- files[grepl("combustible", files)]
files_km <- files[grepl("km", files)]
files_chasis <- files[grepl("Tipos de", files)]
files_fichas <- files[grepl("Ficha -", files)]
files_sube <- files[grepl("Sube", files)]

acelerador_41 <- fread(files_acelerador)
velocidad_41 <- fread(files_velocidad)
torque_41 <- fread(files_torque)
empleados_41 <- fread(files_empleados)
legajo_41 <- fread(files_legajo)
rpm_41 <- fread(files_rpm)
rpm_optimo_41 <- fread(files_rpm_optimo)
tiempos_41 <- fread(files_tiempos)
freno_41 <- fread(files_freno)
chasis_41 <- fread(files_chasis_41)
consumo_41 <- fread(files_consumo)
ralenti_41 <- fread(files_ralenti)
pbi_combustible <- fread(files_combustible) # fecha como POSIXct
pbi_km <- fread(files_km) # fecha como POSIXct




# CHASIS -----------

tipo_chasis <- read_xlsx(files_chasis) %>% 
  select(`Descripción del Vehículo`, `Tipo Combustible`) %>% 
  mutate(
    `Descripción del Vehículo` = as.character(`Descripción del Vehículo`)
  )

colnames(tipo_chasis) <- c("Chasis", "Combustible_chasis")

ficha_chasis <- fread(files_fichas)

ficha_chasis[,Ficha := substr(Ficha, 2, 5)]
ficha_chasis[,Ficha := as.numeric(Ficha)]
ficha_chasis[,Chasis := as.character(Chasis)]
ficha_chasis[Chasis == "BUS MB OF 1418-52303", Chasis := "BUS MB OF 1418-52"]
ficha_chasis[Chasis == "MB O500 1726-59", Chasis := "BUS MB O500 1726-59"]
ficha_chasis[Chasis == "MB OF 1721L-59", Chasis := "BUS MB OF 1721L-59"]
ficha_chasis[Chasis == "MB OH 1721-62", Chasis := "BUS MB OH 1721-62"]
ficha_chasis[Chasis == "MB OF 1722-59", Chasis := "BUS MB OF 1722-59"]
ficha_chasis <- ficha_chasis[!grepl("YT", Chasis),]

ficha_chasis <- left_join(ficha_chasis, tipo_chasis, by = c("Chasis" = "Chasis"))




pbi_combustible <- pbi_combustible[CodAlmacen == "CAR", ]
pbi_combustible[, hs_entre_cargas := gsub(",", ".", `Hs entre cargas`)]
pbi_combustible[, hs_entre_cargas := as.numeric(hs_entre_cargas)]
pbi_combustible <- pbi_combustible[, -"Hs entre cargas"]
pbi_combustible[, Ficha := substr(Ficha, 2, 5)]
pbi_combustible[, Ficha := as.numeric(Ficha)]
pbi_km[, Ficha := substr(Ficha, 2, 5)]
pbi_km[, Ficha := as.numeric(Ficha)]

pbi_combustible <- merge.data.table(pbi_combustible, ficha_chasis[,c("Ficha", "Combustible_chasis")],
                         by.x = "Ficha",
                         by.y ="Ficha",
                         all.x = TRUE)

setorder(pbi_combustible, Ficha, FechaCorregida, `Fecha Hora consumo`)

pbi_combustible[, `:=`(
  Horas_prox_carga = (shift(`Fecha Hora consumo`, type = "lead") - `Fecha Hora consumo`)/3600
),
by = "Ficha"
]

pbi_combustible[is.na(Horas_prox_carga), 
  Horas_prox_carga := 0]


pbi_combustible[, `:=`(
  Taller_prox = lead(CodAlmacen, default = ""),
  Taller_ant = lag(CodAlmacen, default = ""),
  Proxima_carga = lead(ConsumoTotal, default = 0),
  Carga_anterior = lag(ConsumoTotal, default = 0),
  #Mismo_combustible = ProductName == Combustible_chasis,
  Mismo_combustible = TRUE,
  Proximo_combustible = lead(ProductName, default = "") == ProductName,
  Combustible_anterior = lag(ProductName, default = "") == ProductName
),
by = "Ficha"
]

pbi_combustible[, `:=`(
  Cargas_en_1_hr_anterior =
    hs_entre_cargas < 1.5 & hs_entre_cargas > 0 &
      ConsumoTotal > 40 & Carga_anterior > 40 &
      CodAlmacen == Taller_ant,
  Cargas_en_1_hr_prox =
    Horas_prox_carga < 1.5 & Horas_prox_carga > 0 &
      ConsumoTotal > 40 & Proxima_carga > 40 &
      CodAlmacen == Taller_prox
),
by = "Ficha"
]

pbi_combustible[, `:=`(
  Cargas_en_1_hr_anterior = coalesce(Cargas_en_1_hr_anterior, FALSE),
  Cargas_en_1_hr_prox = coalesce(Cargas_en_1_hr_prox, FALSE)
),
by = "Ficha"
]

pbi_combustible[, `:=`(
  Cargas_en_1_hr = Cargas_en_1_hr_prox | Cargas_en_1_hr_anterior
),
by = "Ficha"
]

pbi_combustible[, `:=`(
  chequeo_cruzado = Cargas_en_1_hr_anterior & Combustible_anterior |
    Cargas_en_1_hr_prox & Proximo_combustible,
  Cargas_mismo_dia =
    hs_entre_cargas < 6 & hs_entre_cargas > 0 &
      CodAlmacen != Taller_ant |
      Horas_prox_carga < 2 & Horas_prox_carga > 0 &
        CodAlmacen != Taller_prox
),
by = "Ficha"
]

pbi_combustible[, `:=`(
  Cargas_mismo_dia = coalesce(Cargas_mismo_dia, FALSE)
),
by = "Ficha"
]

pbi_combustible[, `:=`(
 ConsumoTotal = if_else(Cargas_en_1_hr & !Mismo_combustible, 0, ConsumoTotal)
 ),
by = "Ficha"
]


cruzado <- pbi_combustible[chequeo_cruzado == TRUE, c("Ficha", "Fecha Hora consumo")]
cruzado$Filtrar <- TRUE

pbi_combustible <- pbi_combustible[chequeo_cruzado < 1, chequeo_cruzado := Cargas_en_1_hr & !Mismo_combustible] 

cruzado_2 <- pbi_combustible[chequeo_cruzado == TRUE, c("Ficha", "Fecha Hora consumo")]
cruzado_2$Filtrar <- TRUE

cruzado <- rbind(cruzado, cruzado_2)
cruzado$Filtrar <- TRUE

pbi_consumo <- merge.data.table(pbi_combustible, pbi_km,
  by.x = c("Ficha", "FechaCorregida"),
  by.y = c("Ficha", "Fecha"),
  all = TRUE
)

duplicados <- pbi_consumo[duplicated(pbi_consumo[, c("Ficha", "hs_entre_cargas", "Km", "FechaCorregida")]), ]

if (nrow(duplicados) > 0) {
  for (i in c(1:nrow(duplicados))) {
    pbi_consumo[Ficha == duplicados$Ficha[i] &
      FechaCorregida == duplicados$FechaCorregida[i], hs_entre_cargas := 0]
  }
}


pbi_consumo[duplicated(pbi_consumo[, c("Ficha", "Km", "FechaCorregida")]), Km := 0]
pbi_consumo[(hs_entre_cargas == 0 & Texto != "M" & !is.na(hs_entre_cargas)), ]

setorder(pbi_consumo, Ficha, -FechaCorregida, -`Fecha Hora consumo`)

pbi_consumo[, Fecha_hora := agrupamiento_km(`Fecha Hora consumo`), by = "Ficha"]

setorder(pbi_consumo, Ficha, FechaCorregida, Fecha_hora)

pbi_consumo <- pbi_consumo[!is.na(Fecha_hora),]

pbi_consumo[!is.na(Texto) & Texto == "M", `Fecha Hora consumo` := `Fecha Hora consumo` + 12*60*60]
pbi_consumo[,Fecha := as.Date(Fecha_hora)]
pbi_consumo[, Hora := lubridate::hour(Fecha_hora)]
pbi_consumo[Hora < 6, Fecha := Fecha - 1]

pbi_consumo <- merge.data.table(pbi_consumo,
cruzado,
                   by.x = c("Ficha", "Fecha_hora"),
                   by.y = c("Ficha", "Fecha Hora consumo"),
                   all.x = TRUE)

pbi_consumo[is.na(Filtrar), Filtrar := FALSE]

pbi_consumo <- pbi_consumo[Filtrar == FALSE,]
pbi_consumo <- pbi_consumo[,Filtrar := NULL]

pbi_consumo_hora <- pbi_consumo[,.(
  Almacen = first(sort(CodAlmacen)),
  LTS = sum(ConsumoTotal, na.rm = TRUE),
  KM = sum(Km, na.rm = TRUE),
  Combustible = first(sort(ProductName)),
  Texto = first(sort(Texto))
  ),
  by = .(Ficha, Fecha_hora)]

pbi_consumo_hora <- pbi_consumo_hora[Almacen == "CAR",]
pbi_consumo_hora <- pbi_consumo_hora[, .SD[-1], by = Ficha]

pbi_consumo_fecha <- pbi_consumo_hora[, .(Cantidad_cargas = .N,
LTS_PBI = sum(LTS, na.rm = TRUE),
KM_PBI = sum(KM, na.rm = TRUE)),
by = .(Ficha)]

pbi_consumo_fecha[, Consumo_PBI := round(LTS_PBI / KM_PBI * 100, 2)]



# PREPARACION DE DATOS ----------------------------

# LECTURA DE DATOS
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

# ETL
setnames(datos_pca, colnames(datos_pca), gsub("Suma de ", "", colnames(datos_pca)))
setnames(datos_pca, colnames(datos_pca), gsub("profile", "", colnames(datos_pca)))

datos_pca <- datos_pca[, -c("Aceleradorprom", "Empleado", "Tipo dia", "Linea 1", "% Frenadas bruscas")]

variables_rpm <- colnames(datos_pca)[grepl("rpm", colnames(datos_pca), ignore.case = TRUE)]
variables_acelerador <- colnames(datos_pca)[grepl("accel", colnames(datos_pca), ignore.case = TRUE)]
variables_torque <- colnames(datos_pca)[grepl("torque", colnames(datos_pca), ignore.case = TRUE)]
variables_freno <- colnames(datos_pca)[grepl("brake", colnames(datos_pca), ignore.case = TRUE)]
variables_consumo <- colnames(datos_pca)[grepl("consump", colnames(datos_pca), ignore.case = TRUE)]
variables_consumo <- variables_consumo[-3]
variables_ralenti <- colnames(datos_pca)[grepl("ralenti", colnames(datos_pca), ignore.case = TRUE)]
variables_velocidad <- colnames(datos_pca)[grepl("speed", colnames(datos_pca), ignore.case = TRUE)]
variables_tiempo <- colnames(datos_pca)[grepl("times", colnames(datos_pca), ignore.case = TRUE)]

lista_variables <- list()

lista_variables[["rpm"]] <- variables_rpm
lista_variables[["acelerador"]] <- variables_acelerador
lista_variables[["torque"]] <- variables_torque
lista_variables[["freno"]] <- variables_freno
lista_variables[["consumo"]] <- variables_consumo
lista_variables[["ralenti"]] <- variables_ralenti
lista_variables[["velocidad"]] <- variables_velocidad
lista_variables[["tiempos"]] <- variables_tiempo

etiquetas_consumo <- c("Bajo", "Medio", "Alto")

datos_pca[, Nivel_consumo := cut(consumption.avgFuel,
  breaks = quantile(consumption.avgFuel, probs = c(0, 0.25, 0.75, 1)),
  labels = etiquetas_consumo,
  include.lowest = TRUE
), by = Chasis]

datos_pca[, ficha := substr(ficha, 2, 5)]
datos_pca[, ficha := as.numeric(ficha)]
# datos_pca[, Nivel_consumo := as.numeric(Nivel_consumo)]



datos_pca_agrale <- datos_pca[Chasis == "BUS AGRALE MT 17.0/LE", ]
datos_pca_agrale <- datos_pca_agrale[Distance > 30, ]
datos_pca_agrale <- datos_pca_agrale[consumption.avgFuel > 15, ]
datos_pca_agrale <- datos_pca_agrale[!is.na(cod_empleado)]
datos_pca_agrale_2 <- datos_pca_agrale
Nivel_consumo_agrale <- datos_pca_agrale[, "Nivel_consumo"]
datos_pca_agrale <- datos_pca_agrale[, -c("Chasis", "Nivel_consumo", "cod_empleado", "Fecha", "cod_desig")]
var_agrale <- skim(datos_pca_agrale)[c(2, 7, 11)]
setDT(var_agrale)
var_agrale <- unlist(var_agrale[numeric.p0 == numeric.p100, "skim_variable"])
var_agrale <- as.vector(var_agrale)
datos_pca_agrale <- datos_pca_agrale[, -..var_agrale]
datos_pca_agrale <- as.data.frame(scale(datos_pca_agrale))
datos_pca_agrale$Nivel_consumo <- as.data.frame(Nivel_consumo_agrale)
datos_pca_agrale <- as.data.table(datos_pca_agrale)


datos_pca_mb <- datos_pca[Chasis == "MB O500U 1826-59", ]
datos_pca_mb <- datos_pca_mb[Distance > 30, ]
datos_pca_mb <- datos_pca_mb[consumption.avgFuel > 15, ]
datos_pca_mb <- datos_pca_mb[!is.na(cod_empleado)]
datos_pca_mb_2 <- datos_pca_mb
Nivel_consumo_mb <- datos_pca_mb[, "Nivel_consumo"]
datos_pca_mb <- datos_pca_mb[, -c("Chasis", "Nivel_consumo", "cod_empleado", "Fecha", "cod_desig")]
var_mb <- skim(datos_pca_mb)[c(2, 7, 11)]
setDT(var_mb)
var_mb <- unlist(var_mb[numeric.p0 == numeric.p100, "skim_variable"])
var_mb <- as.vector(var_mb)
datos_pca_mb <- datos_pca_mb[, -..var_mb]
datos_pca_mb <- as.data.frame(scale(datos_pca_mb))
datos_pca_mb$Nivel_consumo <- as.data.frame(Nivel_consumo_mb)
datos_pca_mb <- as.data.table(datos_pca_mb)

datos_cluster_agrale <- datos_pca_agrale[, -c("ficha", "cod_emp", "Mes")]

datos_cluster_mb <- datos_pca_mb[, -c("ficha", "cod_emp", "Mes")]

vars_to_cluster <- c(
  "consumption.avgFuel", "consumption.fuel", "accelerator.get80",
  "Speed.speedAvg", "Torque.torqueAvg", "rpm.optimalRangePercentage",
  "times.engine", "brakeApplication", "Distance"
)


dt_cluster_agrale <- scale(datos_cluster_agrale[, -c("Nivel_consumo")])
dt_cluster_agrale <- as.data.frame(dt_cluster_agrale)
setDT(dt_cluster_agrale)
dt_cluster_agrale$Nivel_consumo <- datos_cluster_agrale[, c("Nivel_consumo")]

dt_cluster_mb <- scale(datos_cluster_mb[-370, -c("Nivel_consumo")])
dt_cluster_mb <- as.data.frame(dt_cluster_mb)
setDT(dt_cluster_mb)
dt_cluster_mb$Nivel_consumo <- datos_cluster_mb[-370, c("Nivel_consumo")]

# BOXPLOT DE CONSUMO ------------------
for (j in 1:9) {
  nombregrafico <- paste0("c", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_agrale,
      var_interes = vars_to_cluster[j]
    )
  )
}

p_ag <- plot_grid(c1, c2, c3, c4, c5, c6, c7, c8, c9, ncol = 3)
title_ag <- ggdraw() + draw_label("BUS AGRALE MT 17.0/LE", fontface = "bold")
plot_grid(title_ag, p_ag, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

for (j in 1:9) {
  nombregrafico <- paste0("d", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_mb,
      var_interes = vars_to_cluster[j]
    )
  )
}

p_mb <- plot_grid(d1, d2, d3, d4, d5, d6, d7, d8, d9, ncol = 3)
title_mb <- ggdraw() + draw_label("MB O500U 1826-59", fontface = "bold")
plot_grid(title_mb, p_mb, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

# BOXPLOT ACELERADOR

for (j in 1:5) {
  nombregrafico <- paste0("a", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_agrale,
      var_interes = variables_acelerador[j]
    )
  )
}

p_ag <- plot_grid(a1, a2, a3, a4, a5, ncol = 3)
title_ag <- ggdraw() + draw_label("BUS AGRALE MT 17.0/LE", fontface = "bold")
plot_grid(title_ag, p_ag, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

for (j in 1:5) {
  nombregrafico <- paste0("b", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_mb,
      var_interes = variables_acelerador[j]
    )
  )
}

p_mb <- plot_grid(b1, b2, b3, b4, b5, ncol = 3)
title_mb <- ggdraw() + draw_label("MB O500U 1826-59", fontface = "bold")
plot_grid(title_mb, p_mb, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

# BOXPLOT CONSUMO

for (j in 1:2) {
  nombregrafico <- paste0("a", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_agrale,
      var_interes = variables_consumo[j]
    )
  )
}

p_ag <- plot_grid(a1, a2, ncol = 2)
title_ag <- ggdraw() + draw_label("BUS AGRALE MT 17.0/LE", fontface = "bold")
plot_grid(title_ag, p_ag, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

for (j in 1:2) {
  nombregrafico <- paste0("b", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_mb,
      var_interes = variables_consumo[j]
    )
  )
}

p_mb <- plot_grid(b1, b2, ncol = 2)
title_mb <- ggdraw() + draw_label("MB O500U 1826-59", fontface = "bold")
plot_grid(title_mb, p_mb, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

# BOXPLOT FRENO

for (j in 1:2) {
  nombregrafico <- paste0("a", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_agrale,
      var_interes = variables_freno[j]
    )
  )
}

p_ag <- plot_grid(a1, a2, ncol = 2)
title_ag <- ggdraw() + draw_label("BUS AGRALE MT 17.0/LE", fontface = "bold")
plot_grid(title_ag, p_ag, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

for (j in 1:2) {
  nombregrafico <- paste0("b", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_mb,
      var_interes = variables_freno[j]
    )
  )
}

p_mb <- plot_grid(b1, b2, ncol = 2)
title_mb <- ggdraw() + draw_label("MB O500U 1826-59", fontface = "bold")
plot_grid(title_mb, p_mb, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

# BOXPLOT RALENTI

for (j in 1:1) {
  nombregrafico <- paste0("a", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_agrale,
      var_interes = variables_ralenti[j]
    )
  )
}

p_ag <- plot_grid(a1, ncol = 1)
title_ag <- ggdraw() + draw_label("BUS AGRALE MT 17.0/LE", fontface = "bold")
plot_grid(title_ag, p_ag, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

for (j in 1:1) {
  nombregrafico <- paste0("b", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_mb,
      var_interes = variables_ralenti[j]
    )
  )
}

p_mb <- plot_grid(b1, ncol = 1)
title_mb <- ggdraw() + draw_label("MB O500U 1826-59", fontface = "bold")
plot_grid(title_mb, p_mb, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

# BOXPLOT RPM

variables <- variables_rpm[!variables_rpm %in% var_agrale]
for (j in 1:13) {
  nombregrafico <- paste0("a", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_agrale,
      var_interes = variables[j]
    )
  )
}

p_ag <- plot_grid(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, ncol = 4)
title_ag <- ggdraw() + draw_label("BUS AGRALE MT 17.0/LE", fontface = "bold")
plot_grid(title_ag, p_ag, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

variables <- variables_rpm[!variables_rpm %in% var_mb]
for (j in 1:13) {
  nombregrafico <- paste0("b", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_mb,
      var_interes = variables[j]
    )
  )
}

p_mb <- plot_grid(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, ncol = 3)
title_mb <- ggdraw() + draw_label("MB O500U 1826-59", fontface = "bold")
plot_grid(title_mb, p_mb, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

# BOXPLOT TIEMPO

variables <- variables_tiempo[!variables_tiempo %in% var_agrale]
for (j in 1:3) {
  nombregrafico <- paste0("a", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_agrale,
      var_interes = variables[j]
    )
  )
}

p_ag <- plot_grid(a1, a2, a3, ncol = 3)
title_ag <- ggdraw() + draw_label("BUS AGRALE MT 17.0/LE", fontface = "bold")
plot_grid(title_ag, p_ag, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

variables <- variables_tiempo[!variables_tiempo %in% var_mb]
for (j in 1:3) {
  nombregrafico <- paste0("b", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_mb,
      var_interes = variables[j]
    )
  )
}

p_mb <- plot_grid(b1, b2, b3, ncol = 3)
title_mb <- ggdraw() + draw_label("MB O500U 1826-59", fontface = "bold")
plot_grid(title_mb, p_mb, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

# BOXPLOT TORQUE

for (j in 1:12) {
  nombregrafico <- paste0("a", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_agrale,
      var_interes = variables_torque[j]
    )
  )
}

p_ag <- plot_grid(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, ncol = 4)
title_ag <- ggdraw() + draw_label("BUS AGRALE MT 17.0/LE", fontface = "bold")
plot_grid(title_ag, p_ag, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

for (j in 1:12) {
  nombregrafico <- paste0("b", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_mb,
      var_interes = variables_torque[j]
    )
  )
}

p_mb <- plot_grid(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, ncol = 4)
title_mb <- ggdraw() + draw_label("MB O500U 1826-59", fontface = "bold")
plot_grid(title_mb, p_mb, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

# BOXPLOT VELOCIDAD

variables <- variables_velocidad[!variables_velocidad %in% var_agrale]
for (j in 1:10) {
  nombregrafico <- paste0("a", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_agrale,
      var_interes = variables[j]
    )
  )
}

p_ag <- plot_grid(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, ncol = 4)
title_ag <- ggdraw() + draw_label("BUS AGRALE MT 17.0/LE", fontface = "bold")
plot_grid(title_ag, p_ag, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

variables <- variables_velocidad[!variables_velocidad %in% var_mb]
for (j in 1:14) {
  nombregrafico <- paste0("b", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = dt_cluster_mb,
      var_interes = variables[j]
    )
  )
}

p_mb <- plot_grid(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, ncol = 4)
title_mb <- ggdraw() + draw_label("MB O500U 1826-59", fontface = "bold")
plot_grid(title_mb, p_mb, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins
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

servicios_por_chasis <- consumo_empleados[, .(Cant_Servicios = .N), by = .(cod_emp, Chasis)]
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

consumo_empleados_agrupados <- consumo_empleados_agrupados[cant_servicios > 10, ]

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
  geom_boxplot() +
  facet_grid(~Chasis) +
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

consumo_agrupados_ficha <- consumo_agrupados_ficha[cant_servicios > 12, ]

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
    geom_boxplot() +
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

# consumo_ficha_fecha <- consumo_ficha_fecha[ficha == "F3495",]
setnames(consumo_ficha_fecha, colnames(consumo_ficha_fecha), gsub("Suma de ", "", colnames(consumo_ficha_fecha)))

consumo_ficha_fecha[, ficha := substr(ficha, 2, 5)]
consumo_ficha_fecha[, ficha := as.numeric(ficha)]

consumo_ficha_fecha <- consumo_ficha_fecha[!is.na(accelerator.get80), ]

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

#consumo_agrupados_ff <- consumo_agrupados_ff[Distancia > 50, ]
#consumo_agrupados_ff <- consumo_agrupados_ff[Consumo_DUP > 20, ]

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

#consumo_ff <- consumo_ff[Distancia_DUP > 0, ]
#consumo_ff <- consumo_ff[LTS_DUP > 0, ]
#consumo_ff <- consumo_ff[KM_pbi > 0, ]

chasis_41[, ficha := substr(ficha, 2, 5)]
chasis_41[, ficha := as.numeric(ficha)]

consumo_ff <- merge.data.table(consumo_ff, chasis_41,
  by.y = c("ficha"),
  by.x = c("Ficha"),
  all.x = TRUE
)

#ggplotly(
#  ggplot(subset(consumo_ficha_fecha, ficha == "F3495")) +
#    aes(x = Acelerador, y = Consumo_DUP) +
#    geom_point() +
#    facet_grid(~Chasis)
#)

#ggplotly(
#  ggplot(subset(consumo_ficha_fecha, ficha == "F3495")) +
#    aes(x = Acelerador, y = get("l/100km")) +
#    geom_point() +
#    facet_grid(~Chasis)
#)

#ggplotly(
#  ggplot(subset(consumo_ficha_fecha, ficha == "F3495")) +
#    geom_point(aes(x = Acelerador, y = get("l/100km")), color = "blue") +
#    geom_point(aes(x = Acelerador, y = Consumo_DUP), color = "red")
#)

etiquetas_consumo <- c("Bajo", "Medio", "Alto")

consumo_ff[, Nivel_consumo_PBI := cut(Consumo_PBI,
  breaks = quantile(Consumo_PBI, probs = c(0, 0.30, 0.70, 1)),
  labels = etiquetas_consumo,
  include.lowest = TRUE
), by = Chasis]

consumo_ff[, Nivel_consumo_DUP := cut(Consumo_DUP,
  breaks = quantile(Consumo_DUP, probs = c(0, 0.30, 0.70, 1), na.rm = TRUE),
  labels = etiquetas_consumo,
  include.lowest = TRUE
), by = Chasis]

# consumo_ficha_plot <- consumo_ff[Nivel_consumo != "Medio", ]

ggplotly(
  ggplot(consumo_ff) +
    aes(x = Nivel_consumo_DUP, y = Acelerador) +
    geom_boxplot() +
    facet_grid(~Chasis)
)

ggplot(subset(consumo_ff, Nivel_consumo_PBI != "Medio")) +
  aes(x = Nivel_consumo_PBI, y = Acelerador) +
  geom_boxplot() +
  facet_grid(~Chasis) +
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
  geom_boxplot() +
  facet_grid(~Chasis) +
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

cor(consumo_ff[Chasis == "BUS AGRALE MT 17.0/LE", Consumo_DUP], consumo_ff[Chasis == "BUS AGRALE MT 17.0/LE", Consumo_PBI])
cor(consumo_ff[Chasis == "MB O500U 1826-59", Consumo_DUP], consumo_ff[Chasis == "MB O500U 1826-59", Consumo_PBI])

# PCA -------------------------

pca_agrale <- FactoMineR::PCA(X = datos_pca_agrale, scale.unit = T, ncp = ncol(datos_pca_agrale), graph = F)

fviz_eig(pca_agrale, addlabels = TRUE, ncp = ncol(datos_pca_agrale))

eig.val <- factoextra::get_eigenvalue(pca_agrale)
kable(eig.val)

var_agrale <- get_pca_var(pca_agrale)

corrplot(var_agrale$contrib, is.corr = FALSE)

contribucion_consumo_agrale <- sort(t(var_agrale$cos2)[, "consumption.avgFuel"], decreasing = TRUE)
contribucion_consumo_agrale <- as.data.frame(contribucion_consumo_agrale)
dimensiones <- rownames(contribucion_consumo_agrale)
contribucion_consumo_agrale <- data.frame(dimensiones, contribucion_consumo_agrale)
colnames(contribucion_consumo_agrale) <- c("componente", "contribucion")

ggplot(contribucion_consumo_agrale, aes(
  x = reorder(componente, -contribucion), y = contribucion,
  label = paste0(round(contribucion * 100), "%")
)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "grey") +
  geom_text(nudge_y = 0.02) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  labs(
    title = "Contribución de la variable Consumo",
    x = "Componentes", y = "Porcentaje Contribución"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

fviz_pca_ind(pca_agrale, axes = c(3, 1), label = "none")

fviz_pca_var(pca_agrale,
  col.var = "cos2", axes = c(3, 1),
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

mayor_cos2 <- var_agrale$cos2[, 1:26]
mayor_cos2 <- apply(mayor_cos2, MARGIN = 1, sum)
mayor_cos2 <- sort(mayor_cos2, decreasing = TRUE)
variables_cos2 <- as.data.frame(c(mayor_cos2))
colnames(variables_cos2) <- c("cos2")
variables_cos2 <- rownames(variables_cos2)
kable(variables_cos2)
# CLUSTER --------------------------------
# AGRALE
tipos_datos <- skim(dt_cluster_agrale)[, c(2, 1)]
cant_unicos <- apply(dt_cluster_agrale, MARGIN = 2, FUN = function(x) length(unique(x)))
cant_unicos <- as.data.frame(cant_unicos)
cant_unicos$variables <- colnames(dt_cluster_agrale)
tipos_datos <- left_join(tipos_datos, cant_unicos, by = c("skim_variable" = "variables"))

var_factores <- which(tipos_datos$skim_type == "factor")
var_numeric <- which(tipos_datos$skim_type == "numeric")

distancia <- cluster::daisy(dt_cluster_agrale,
  metric = "gower", stand = TRUE,
  type = list(
    factor = var_factores,
    numeric = var_numeric
  )
)

fit <- hclust(distancia, method = "ward.D2")

# plot(fit, labels = NULL)

# datos_cluster <- apply(dt_cluster_agrale, MARGIN = 2, FUN = function(x) coalesce(x, 0))

# fviz_nbclust(dt_cluster_agrale, FUNcluster = kmeans, method = "silhouette",
#             k.max = 20,
#             diss = distancia) +
#  labs(title    = "Número óptimo de clusters a considerar",
#       subtitle = "Indice Silhouette")

# wss <- (nrow(datos_cluster) - 1) * sum(apply(datos_cluster, 2, var))
# for (i in 2:10) {
#  wss[i] <- sum(kmeans(datos_cluster, centers = i)$withinss)
# }
# plot(1:10, wss, type = "b", xlab = "Number of Clusters",
#     ylab = "Suma de cuadrados dentro de los clusters (within)")

groups <- cutree(fit, k = 4)

dt_cluster_agrale$cluster <- groups
dt_cluster_agrale$cluster <- paste0("Cluster ", dt_cluster_agrale$cluster)
cant_obs <- dt_cluster_agrale %>%
  group_by(cluster) %>%
  summarise(
    cant_observaciones = n(),
    proporción = n() / nrow(dt_cluster_agrale) * 100
  )

kable(cant_obs)

for (j in 1:9) {
  nombregrafico <- paste0("e", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos(
      df = dt_cluster_agrale,
      var_interes = vars_to_cluster[j]
    )
  )
}

p_clust_ag <- plot_grid(e1, e2, e3, e4, e5, e6, e7, e8, e9, ncol = 3)
title_ag <- ggdraw() + draw_label("BUS AGRALE MT 17.0/LE", fontface = "bold")
plot_grid(title_ag, p_clust_ag, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

# MERCEDES BENZ

tipos_datos <- skim(dt_cluster_mb)[, c(2, 1)]
cant_unicos <- apply(dt_cluster_mb, MARGIN = 2, FUN = function(x) length(unique(x)))
cant_unicos <- as.data.frame(cant_unicos)
cant_unicos$variables <- colnames(dt_cluster_mb)
tipos_datos <- left_join(tipos_datos, cant_unicos, by = c("skim_variable" = "variables"))

var_factores <- which(tipos_datos$skim_type == "factor")
var_numeric <- which(tipos_datos$skim_type == "numeric")

distancia <- cluster::daisy(dt_cluster_mb,
  metric = "gower", stand = TRUE,
  type = list(
    factor = var_factores,
    numeric = var_numeric
  )
)

fit <- hclust(distancia, method = "ward.D2")

# plot(fit, labels = NULL)

datos_cluster <- apply(dt_cluster_mb[, -"Nivel_consumo"], MARGIN = 2, FUN = function(x) coalesce(x, 0))

fviz_nbclust(datos_cluster,
  FUNcluster = kmeans, method = "silhouette",
  k.max = 20,
  diss = distancia
) +
  labs(
    title = "Número óptimo de clusters a considerar",
    subtitle = "Indice Silhouette"
  )

wss <- (nrow(datos_cluster) - 1) * sum(apply(datos_cluster, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(datos_cluster, centers = i)$withinss)
}
plot(1:10, wss,
  type = "b", xlab = "Number of Clusters",
  ylab = "Suma de cuadrados dentro de los clusters (within)"
)

groups <- cutree(fit, k = 3)

dt_cluster_mb$cluster <- groups
dt_cluster_mb$cluster <- paste0("Cluster ", dt_cluster_mb$cluster)
cant_obs <- dt_cluster_mb %>%
  group_by(cluster) %>%
  summarise(
    cant_observaciones = n(),
    proporción = n() / nrow(dt_cluster_mb) * 100
  )

kable(cant_obs)

for (j in 1:9) {
  nombregrafico <- paste0("f", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos(
      df = dt_cluster_mb,
      var_interes = vars_to_cluster[j]
    )
  )
}

p_clust_mb <- plot_grid(f1, f2, f3, f4, f5, f6, f7, f8, f9, ncol = 3)
title_mb <- ggdraw() + draw_label("MB O500U 1826-59", fontface = "bold")
plot_grid(title_mb, p_clust_mb, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

cor(dt_cluster_mb$consumption.avgFuel, dt_cluster_mb$accelerator.get80)
cor(dt_cluster_agrale$consumption.avgFuel, dt_cluster_agrale$accelerator.get80)

cor(dt_cluster_mb$consumption.avgFuel, dt_cluster_mb$rpm.optimalRangePercentage)
cor(dt_cluster_agrale$consumption.avgFuel, dt_cluster_agrale$rpm.optimalRangePercentage)

cor(dt_cluster_mb$accelerator.get80, dt_cluster_mb$rpm.optimalRangePercentage)
cor(dt_cluster_agrale$accelerator.get80, dt_cluster_agrale$rpm.optimalRangePercentage)


agrupados_agrale <- datos_pca_agrale_2[, .(
  KM_DUP = sum(Distance, na.rm = TRUE),
  Tiempo_motor = sum(times.engine, na.rm = TRUE),
  Tiempo_conduccion = sum(times.driving, na.rm = TRUE),
  LTS_DUP = sum(consumption.fuel, na.rm = TRUE),
  Acelerador_80 = weighted.mean(accelerator.get80, times.engine, na.rm = TRUE),
  Torque_promedio = weighted.mean(Torque.torqueAvg, times.engine, na.rm = TRUE),
  Freno = weighted.mean(brakeApplication, times.engine, na.rm = TRUE),
  RPM = weighted.mean(rpm.optimalRangePercentage, times.engine, na.rm = TRUE)
),
by = .(ficha)
]

agrupados_agrale[, Consumo_DUP := round(LTS_DUP / KM_DUP * 100, 2)]
agrupados_agrale[, Velocidad_promedio := round(KM_DUP / Tiempo_conduccion * 3600, 2)]


agrupados_agrale <- merge.data.table(pbi_consumo_fecha, agrupados_agrale,
  by.y = c("ficha"),
  by.x = c("Ficha")
)

agrupados_agrale[, Relacion_Consumo := round(Consumo_PBI / Consumo_DUP, 3)]

agrupados_agrale[, Nivel_consumo_DUP := cut(Consumo_DUP,
  breaks = quantile(Consumo_DUP, probs = c(0, 0.25, 0.75, 1)),
  labels = etiquetas_consumo,
  include.lowest = TRUE
)]

agrupados_agrale[, Nivel_consumo_PBI := cut(Consumo_PBI,
  breaks = quantile(Consumo_PBI, probs = c(0, 0.25, 0.75, 1)),
  labels = etiquetas_consumo,
  include.lowest = TRUE
)]

cor(agrupados_agrale$Consumo_PBI, agrupados_agrale$Consumo_DUP)
cor(agrupados_agrale$Consumo_PBI, agrupados_agrale$Acelerador_80)
cor(agrupados_agrale$Consumo_DUP, agrupados_agrale$Acelerador_80)
cor(agrupados_agrale$RPM, agrupados_agrale$Acelerador_80)
cor(agrupados_agrale$RPM, agrupados_agrale$Consumo_DUP)
cor(agrupados_agrale$RPM, agrupados_agrale$Consumo_PBI)

ggplot(agrupados_agrale)+
aes(x = Nivel_consumo_DUP, y = Relacion_Consumo) +
geom_boxplot()+
theme_minimal() +
ggtitle("Consumo Agrale")

ggplot(agrupados_agrale)+
aes(x = Consumo_DUP, y = Consumo_PBI) +
geom_point()+
geom_smooth(method = "lm") +
coord_fixed()+
theme_minimal() +
ggtitle("Consumo Agrale")


variables_2 <- c(
  "Consumo_PBI", "Consumo_DUP", "Acelerador_80",
  "KM_PBI", "KM_DUP", "Torque_promedio",
  "Freno", "RPM", "Velocidad_promedio"
)



for (j in 1:9) {
  nombregrafico <- paste0("c", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = agrupados_agrale,
      var_interes = variables_2[j],
      grupos = "Nivel_consumo_DUP"
    )
  )
}

p_ag <- plot_grid(c1, c2, c3, c4, c5, c6, c7, c8, c9, ncol = 3)
title_ag <- ggdraw() + draw_label("BUS AGRALE MT 17.0/LE", fontface = "bold")
plot_grid(title_ag, p_ag, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins




agrupados_mb <- datos_pca_mb_2[, .(
  KM_DUP = sum(Distance, na.rm = TRUE),
  Tiempo_motor = sum(times.engine, na.rm = TRUE),
  Tiempo_conduccion = sum(times.driving, na.rm = TRUE),
  LTS_DUP = sum(consumption.fuel, na.rm = TRUE),
  Acelerador_80 = weighted.mean(accelerator.get80, times.engine, na.rm = TRUE),
  Torque_promedio = weighted.mean(Torque.torqueAvg, times.engine, na.rm = TRUE),
  Freno = weighted.mean(brakeApplication, times.engine, na.rm = TRUE),
  RPM = weighted.mean(rpm.optimalRangePercentage, times.engine, na.rm = TRUE)
),
by = .(ficha)
]

agrupados_mb[, Consumo_DUP := round(LTS_DUP / KM_DUP * 100, 2)]
agrupados_mb[, Velocidad_promedio := round(KM_DUP / Tiempo_conduccion * 3600, 2)]


agrupados_mb <- merge.data.table(pbi_consumo_fecha, agrupados_mb,
  by.y = c("ficha"),
  by.x = c("Ficha")
)

agrupados_mb[, Relacion_Consumo := round(Consumo_PBI / Consumo_DUP, 3)]

agrupados_mb[, Nivel_consumo_DUP := cut(Consumo_DUP,
  breaks = quantile(Consumo_DUP, probs = c(0, 0.25, 0.75, 1)),
  labels = etiquetas_consumo,
  include.lowest = TRUE
)]

agrupados_mb[, Nivel_consumo_PBI := cut(Consumo_PBI,
  breaks = quantile(Consumo_PBI, probs = c(0, 0.25, 0.75, 1)),
  labels = etiquetas_consumo,
  include.lowest = TRUE
)]

cor(agrupados_mb$Consumo_PBI, agrupados_mb$Consumo_DUP)
cor(agrupados_mb$Consumo_PBI, agrupados_mb$Acelerador_80)
cor(agrupados_mb$Consumo_DUP, agrupados_mb$Acelerador_80)
cor(agrupados_mb$RPM, agrupados_mb$Acelerador_80)
cor(agrupados_mb$RPM, agrupados_mb$Consumo_DUP)
cor(agrupados_mb$RPM, agrupados_mb$Consumo_PBI)

ggplot(agrupados_mb)+
aes(x = (Consumo_DUP), y = (Relacion_Consumo)) +
geom_point()+
geom_smooth(method = "lm") +
theme_minimal() +
ggtitle("Consumo Agrale")

ggplot(agrupados_mb)+
aes(x = Nivel_consumo_DUP, y = Relacion_Consumo) +
geom_boxplot()+
theme_minimal() +
ggtitle("Consumo MB")

skim(agrupados_mb)


variables_2 <- c(
  "Consumo_PBI", "Consumo_DUP", "Acelerador_80",
  "KM_PBI", "KM_DUP", "Torque_promedio",
  "Freno", "RPM", "Velocidad_promedio"
)


for (j in 1:9) {
  nombregrafico <- paste0("d", j)
  assign(
    nombregrafico,
    generar_boxplots_por_grupos_2(
      df = agrupados_mb,
      var_interes = variables_2[j],
      grupos = "Nivel_consumo_PBI"
    )
  )
}

p_mb <- plot_grid(d1, d2, d3, d4, d5, d6, d7, d8, d9, ncol = 3)
title_mb <- ggdraw() + draw_label("MB O500U 1826-59", fontface = "bold")
plot_grid(title_mb, p_mb, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins


cor(agrupados_mb$Consumo_PBI, agrupados_mb$Consumo)

ggplot(agrupados_mb) +
  aes(x = Consumo, y = Consumo_PBI) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_fixed() +
  theme_minimal()

# ERRORES SUBE ---------------------------------------

errores_sube <- read_xlsx(files_sube)

setDT(errores_sube)

columnas_sube <- c("Fecha", "Nro_de_inspeccion", "Ubicacion_Historica", "Ficha", "Tipo_de_falla",
                         "Descripcion", "Reparado", "Estado_operativo", "Recurrencia", "Origen_de_Inspeccion")


setnames(errores_sube, columnas_sube)

colnames(consumo_agrupados_ff)

errores_sube[, Ficha := substr(Ficha, 2, 5)]
errores_sube[, Ficha := as.numeric(Ficha)]

sube <- grep("sube", unique(errores_sube$Descripcion), ignore.case = TRUE, value = TRUE)
sube <- sube[c(2,3,5)]

gps <- grep("gps", unique(errores_sube$Descripcion), ignore.case = TRUE, value = TRUE)

analisis_errores_sube <- merge.data.table(consumo_agrupados_ff , errores_sube,
by.x = c("ficha", "Fecha"),
by.y = c("Ficha", "Fecha"),
all.x = TRUE)

analisis_errores_sube <- merge.data.table(analisis_errores_sube, pbi_km,
by.x = c("ficha", "Fecha"),
by.y = c("Ficha", "Fecha"),
all.x = TRUE)

analisis_errores_sube <- analisis_errores_sube[Distancia < 2000,]

analisis_errores_sube[, Descripcion := if_else(Descripcion %in% gps, "Error_gps", Descripcion)]
analisis_errores_sube[, Descripcion := if_else(Descripcion %in% sube, "Error_sube", "Sin_error")]

analisis_errores_sube[,.(Cantidad = .N), by = .(Descripcion)]

analisis_errores_sube[, Relacion_KM := round(Distancia / Km, 2)]

#analisis_errores_sube <- analisis_errores_sube[Relacion_KM < 2,]

ggplot(analisis_errores_sube) +
aes(x = Descripcion, y = Relacion_KM) +
geom_boxplot()

ggplot(subset(analisis_errores_sube, Distancia < 500)) +
aes(x = Distancia, y = Km, color = Relacion_KM) +
geom_point() +
geom_smooth(method = "lm") +
coord_fixed() +
facet_grid(~Descripcion)


analisis_errores_sube[, .(Correlaciones = cor(Distancia, Km, use = "na.or.complete")), by = .(Descripcion)]

max(analisis_errores_sube$Relacion_KM, na.rm = TRUE)

analisis_errores_sube[, KM_DUP_discreto := cut_interval(Distancia, length = 5)]
analisis_errores_sube[, KM_GPS_discreto := cut_interval(Km, length = 5)]

ggplotly(
ggplot(analisis_errores_sube) +
aes(x = KM_DUP_discreto) +
geom_bar()
)

ggplotly(
ggplot(analisis_errores_sube) +
aes(x = KM_GPS_discreto) + 
geom_bar()
)
