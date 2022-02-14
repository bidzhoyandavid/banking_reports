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

setwd("/Users/davitbidzoan/Desktop/Data center")

# data upload -------------------------------------------------------------

f123_maket <- read_excel('f123.xlsx')

# functions ---------------------------------------------------------------

query_compose <- function(data){
  query <- "select distinct \"REGN\" "
  for (i in 1:nrow(f123_maket)){
    query <- paste0(query, ", sum(case when C1 in ('", f123_maket[i, 'Value'], "') then C3 else null end) as ", f123_maket[i, 'Acronym code'])     
  }
  query <- paste0(query, " from ", data, " group by \"REGN\"")
  return(query)
}



# calculation -------------------------------------------------------------

MainPath <- '/Users/davitbidzoan/Desktop/Stress-tests/Data/0409123'

for (i in year:year){
  for (j in month:month){
    m <- ifelse(j<10, '0', '')
    dateAn <- paste0(i, '-', m, j, '-01')
    
    setwd(paste0(MainPath, '/123-', i, m, j, '01'))
    
    listdbf <- dir(pattern = "*.dbf") # creates the list of all the dbf files in the directory
    
    for (k in 1:length(listdbf)){
      if (str_sub(listdbf[[k]], start = length(listdbf[[k]])-6) == "D.dbf"){
        df1 <-  read.dbf(listdbf[[k]])
        break
      } 
    }
    
    data_f123 <- sqldf(query_compose('df1'))
    data_f123['dt'] <- dateAn
    names(data_f123) <- tolower(names(data_f123))
    
    table_id <- Id(schema="bank_data", table='f123_capital')
    dbWriteTable(con, name=table_id, value=data_f123, append=TRUE)
    
    cat("Calculation of f123 on ", dateAn, " is ready", "\n")
  }
}

rm(list = ls()[! ls() %in% c('year', 'month')])

