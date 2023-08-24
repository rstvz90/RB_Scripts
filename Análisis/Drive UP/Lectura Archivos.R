library(data.table)

options(timeout = max(1000, getOption("timeout")))

url <- "http://192.168.201.150:5000/HistoriaCochesDriveUp?desde=2023-08-15"

download.file(url, destfile = "data.csv", method = "libcurl")

fread("data.csv")