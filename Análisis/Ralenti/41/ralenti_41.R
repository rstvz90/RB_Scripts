library(data.table)
library(ggplot2)
library(plotly)
library(readxl)
library(rstudioapi)
library(lubridate)

rm(list = ls())

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ralenti_41 <- read_xlsx("C:/Users/REstevez/Documents/Análisis/Datos/Ralenti PBI/RALENTI 41 Nov.xlsx", sheet = "Hoja1")
setDT(ralenti_41)

#ralenti_41 <- ralenti_41[!is.na(Taller), ]

ralenti_41[, Fecha := as.Date(Desde)]

dia_ralenti_41 <- ralenti_41[, .(Horas_ralenti = sum(`Estado (min.)`, na.rm = TRUE) / 60),
    by = .(Fecha)
]

dia_ralenti_41 <- dia_ralenti_41[1:(.N - 1)]

plazo <- max(dia_ralenti_41$Fecha) - as.Date("2023/04/01")


promedio_ralenti_ini <- round(mean(head(dia_ralenti_41$Horas_ralenti, 20)))
promedio_ralenti_fin <- round(mean(tail(dia_ralenti_41$Horas_ralenti, as.numeric(plazo))))
ahorro <- promedio_ralenti_ini - promedio_ralenti_fin
disminucion <- round((1 - promedio_ralenti_fin / promedio_ralenti_ini) * 100)

label_ini <- paste0(
    "Ralenti", "\n",
    promedio_ralenti_ini, " hs diarias"
)
label_fin <- paste0(
    "Ralenti", "\n",
    promedio_ralenti_fin, " hs diarias"
)
label_placa <- paste0(
    "Comienzo instalación", "\n",
    "de corte automático"
)
label_ahorro <- paste0(
    "Disminución de ", disminucion, "% de ralenti", "\n",
    "Ahorro de ", ahorro, " hs diarias", "\n",
    "Aproximadamente ", ahorro * 30 * 2, " lts mensuales"
)

pos_texto <- mean(dia_ralenti_41$Fecha) - 50


ggplot() +
    geom_line(
        data = dia_ralenti_41,
        aes(x = Fecha, y = Horas_ralenti),
        color = "blue", alpha = 0.8, linewidth = 1
    ) +

    # geom_point(data = dia_ralenti_41,
    #           aes(x = Fecha, y = Horas_ralenti),
    #           color = "blue", alpha = 0.8) +

    geom_smooth(
        data = dia_ralenti_41,
        aes(x = Fecha, y = Horas_ralenti),
        span = 1, se = FALSE, color = "purple",
        alpha = 0.8, linetype = "longdash"
    ) +
    geom_segment(
        aes(
            x = as.Date("2022/11/01"), xend = as.Date("2022/11/01"),
            y = 25, yend = max(dia_ralenti_41$Horas_ralenti) + 30
        ),
        color = "black", linetype = "longdash",
        linewidth = 1, alpha = 0.7
    ) +
    geom_vline(
        xintercept = as.numeric(min(dia_ralenti_41$Fecha)) + 22,
        linetype = "longdash", color = "red", alpha = 0.6, linewidth = 1
    ) +
    geom_vline(
        xintercept = as.numeric(max(dia_ralenti_41$Fecha)) - as.numeric(plazo),
        linetype = "longdash", color = "red", alpha = 0.6, linewidth = 1
    ) +
    geom_text(aes(x = min(dia_ralenti_41$Fecha) + 5, y = 30),
        label = label_ini,
        hjust = 0.5,
        position = position_dodge(0.5), size = 6,
        fontface = "bold"
    ) +
    geom_text(aes(x = max(dia_ralenti_41$Fecha) - 60, y = 100),
        label = label_fin,
        hjust = 0.5,
        position = position_dodge(0.5), size = 6,
        fontface = "bold"
    ) +
    geom_text(
        aes(
            x = (as.Date("2022/11/01")+10),
            y = max(dia_ralenti_41$Horas_ralenti)-40
        ),
        label = label_placa,
        nudge_y = 30, nudge_x = 1, size = 5,
        hjust = 0, color = "black"
    ) +
    geom_segment(
        aes(
            x = min(dia_ralenti_41$Fecha) + days(23),
            xend = max(dia_ralenti_41$Fecha) - plazo - 1,
            y = 200, yend = 200
        ),
        color = "blue", linewidth = 2.5,
        arrow = arrow(), alpha = 0.8
    ) +
    geom_segment(
        aes(
            x = as.Date("2022/11/01"),
            xend = as.Date("2022/11/01") + days(40),
            y = max(dia_ralenti_41$Horas_ralenti) + 30,
            yend = max(dia_ralenti_41$Horas_ralenti) + 30
        ),
        color = "black", linewidth = 1, linetype = "longdash",
        arrow = arrow(), alpha = 0.7
    ) +
    geom_segment(
        aes(
            x = as.Date("2022/11/01"),
            xend = as.Date("2022/11/01") + days(40),
            y = 25, yend = 25
        ),
        color = "black", linewidth = 1, linetype = "longdash",
        arrow = arrow(), alpha = 0.7
    ) +
    geom_label(aes(x = pos_texto, y = 245),
        size = 5, hjust = 0.5, fontface = "bold",
        label.padding = unit(0.5, "lines"),
        fill = "lightblue",
        label = label_ahorro
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%F") +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(
            hjust = 0.5, size = 14,
            margin = margin(0, 0, 20, 0)
        ),
        axis.text.x = element_text(size = 12, angle = 30),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(
            size = 14, face = "plain",
            margin = margin(10, 0, 5, 0)
        ),
        axis.title.y = element_text(
            size = 14, face = "plain",
            margin = margin(0, 10, 0, 5)
        )
    ) +
    ylim(0, max(dia_ralenti_41$Horas_ralenti) * 1.5) +
    labs(
        title = "Horas de Ralenti 203",
        subtitle = paste0(
            "Talleres de Saavedra, Moreno y Pilar (período: ",
            min(dia_ralenti_41$Fecha), " - ", max(dia_ralenti_41$Fecha), ")"
        ),
        y = "Horas de Ralenti",
        x = "Fecha"
    )

#ggsave("C:/Users/REstevez/Documents/Análisis/Resultados/ralenti_41_2.jpg", width = 1920, height = 1002, units = "px")

#min_total <- sum(ralenti_41[, `Min Ralentí`])
#min_15 <- sum(ralenti_41[`Min Ralentí` > 15, `Min Ralentí`])
#min_20 <- sum(ralenti_41[`Min Ralentí` > 20, `Min Ralentí`])

#min_15 / min_total
#min_20 / min_total

