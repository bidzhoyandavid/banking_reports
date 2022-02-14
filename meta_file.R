library(foreign)
library(readxl)
library(dplyr)
library(reshape2)
library(stringr)
library(sqldf)
library(openxlsx)

con <- dbConnect(RPostgres::Postgres()
                 , host='localhost'
                 , port='5432'
                 , dbname='bankometr'
                 , user='postgres'
                 , password='Davit')



year <- as.numeric(readline(prompt = "Input a year:"))
month <- as.numeric(readline(prompt = "Input a month:"))

files <- list('f101.R', 'f123.R', 'f135.R')
# files <- list('f135.R') 

for (file in files){
  setwd("/Users/davitbidzoan/Desktop/Data center")
  source(file = file)
}

if (month %in% c(1, 4, 7, 10)){
  setwd("/Users/davitbidzoan/Desktop/Data center")
  source('f102.R')
} else {cat(" f102 is not run for this period", "\n")}





