library(data.table)
library(clipr)
library(openxlsx)


# Limpio directorio y memoria
rm(list = ls())
gc()

#options(encoding = "ISO-8859-1")
path <- "C:/Users/REstevez/Documents/Análisis/Exceso Velocidad"

# Seteo working directory
setwd(path)

# Fecha de hoy
hoy <- Sys.Date()

# LEO LOS NOMBRES DE TODOS LOS ARCHIVOS EN LA CARPETA
files <- list.files(path, full.names = TRUE)
# ME QUEDO SOLO CON LOS XLSX
files_xlsx <- files[grepl(".xlsx", files)]
files_xlsx <- files_xlsx[!grepl("~\\$", files_xlsx)] # que no lea los archivos temporarios
files_xlsx <- files_xlsx[!grepl("Excesos Velocidad ", files_xlsx)]

# Obtengo la información de los archivos
info_archivo <- file.info(files_xlsx)
# Creo el data table
setDT(info_archivo)
# Uno el vector nombre con la info
info_archivo[, nombre := files_xlsx]
# Ordeno el datatable por fecha de creación
setorder(info_archivo, -ctime)

# Me quedo con el último archivo creado
archivo <- info_archivo[1, nombre]

# Lectura del archivo
df_exceso <- readxl::read_xlsx(archivo)

# Creo el data.table
setDT(df_exceso)

# Limpio las filas que no tienen fecha
df_exceso <- df_exceso[!is.na(FechaLocal),]

# Cambio el formato de la fecha
df_exceso[, Fecha := as.IDate(FechaLocal)]

# Cambio el enconding de la columna empleado a latin-1
Encoding(df_exceso$Empleado) <- "ISO-8859-1"

# Agrupo por fecha, ficha, linea y empleado, y calculo la velocidad máxima
# Filtro la ficha 0 y los coches que no tienen nombre en la diagramación
exceso_diario <- df_exceso[!is.na(Empleado) & Ficha > 0, .(
  Vel_max = max(Velocidad, na.rm = TRUE),
  Limite = max(`Velocidad Max`, na.rm = TRUE)
  ), 
  by = .(Fecha, Ficha, Linea, Empleado)]

# Ordeno por fecha, ficha y línea
setorder(exceso_diario, Fecha, Linea, -Vel_max, Ficha)

# fwrite(exceso_diario, file = paste0("Excesos Velocidad Diario ", hoy, ".csv"))

# Calculo la cantidad de excesos de velocidad de los empleados en la semana. (No se distingue por fecha)
exceso_empleado <- df_exceso[!is.na(Empleado) & Ficha > 0,.(
  Cantidad_excesos = .N
  ),
  by = .(Empleado)]

setorder(exceso_empleado, -Cantidad_excesos)

# fwrite(exceso_empleado, file = paste0("Excesos Velocidad Empleado ", hoy, ".csv"))

# Creo el nombre del archivo con el día de hoy en el nombre
file_path <- paste0("Excesos Velocidad ", hoy, ".xlsx")

# Create a new workbook
wb <- createWorkbook()

# Add a worksheet to the workbook
addWorksheet(wb, sheetName = "Excesos Maximos")
addWorksheet(wb, sheetName = "Cantidad Excesos")

# Write the data.table to the worksheet
writeDataTable(wb, sheet = "Excesos Maximos", x = exceso_diario, rowNames = FALSE)
writeDataTable(wb, sheet = "Cantidad Excesos", x = exceso_empleado, rowNames = FALSE)

# Save the workbook as an Excel file
saveWorkbook(wb, file_path, overwrite = TRUE)








