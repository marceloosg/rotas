library(data.table)
rm(list=ls())
motoboy=data.table(read.csv("csv/motoboy.csv"  ,sep=";",colClasses ="character"))
cliente=data.table(read.csv("csv/entidades.csv",sep=";",colClasses ="character"))
