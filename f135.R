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

# data preparation --------------------------------------------------------

f135_maket <- read_excel('f135.xlsx')
f135_maket$Name <- tolower(f135_maket$Name)


# functions --------------------------------------------------------------

query_compose <- function(lev, data){
  query <- 'Select distinct \"REGN\" '
  if (data == 'df2'){
    col1 <- '\"C1_2\"'
    col2 <- '\"C2_2\"'
  } else {
    col1 <- '\"C1_3\"'
    col2 <- '\"C2_3\"'
  }
  
  df <- f135_maket %>% filter(Level == lev & Dataframe == data)
  for (i in 1:nrow(df)){
    query <- paste0(query, ", sum(case when ", col1, " in ('", df[i, "Name"], "') then ", col2, " else null end) as '", df[i, 'Acronym code'], "'")   
  }
  query <- paste0(query, " from ", data, " group by \"REGN\"")
  return(query)
}

# define the variables depending of level
levelFunc <- function(urov){
  vars <- c()
  data <- f135_maket%>%
    filter(Level == urov)
  for (i in 1:nrow(data)){
    vars <- c(vars, data[i, 'Acronym code'])
  }
  return(vars)
}


# calculations ------------------------------------------------------------


MainPath <- '/Users/davitbidzoan/Desktop/Stress-tests/Data/0409135'

for (i in year:year){
  for (j in month:month){
    m <- ifelse(j<10, '0', '')
    dateAn <- paste0(i, '-', m, j, '-01')
    
    setwd(paste0(MainPath, '/135-', i, m, j, '01/xlsx'))
    
    listxlsx <- dir(pattern = "*.xlsx") # creates the list of all the dbf files in the directory
    for (k in 1:length(listxlsx)){
      if (str_sub(listxlsx[[k]], start = length(listxlsx[[k]])-7) == "4.xlsx"){
        df4 <-  read_excel(listxlsx[[k]])
        break
        
      } else if (str_sub(listxlsx[[k]], start = length(listxlsx[[k]])-7) == "3.xlsx"){
        df3 <-  read_excel(listxlsx[[k]])
        df3$C1_3 <- tolower(df3$C1_3)
      } else if (str_sub(listxlsx[[k]], start = length(listxlsx[[k]])-7) == "2.xlsx"){
        df2 <- read_excel(listxlsx[[k]])
        df2$C1_2 <- tolower(df2$C1_2)
      }
    }
    
    colnames(df4)  <- c('regn', 'number', 'norm', 'value', 'date_of_viol')
    table_id <- Id(schema="bank_data", table='f135_violation')
    dbWriteTable(con, name=table_id, value=df4, append=TRUE)
    
    level0 <- sqldf(query_compose(0, 'df2')) %>% left_join(sqldf(query_compose(0, 'df3')), by = "REGN")
    level1 <- sqldf(query_compose(1, 'df2')) %>% left_join(sqldf(query_compose(1, 'df3')), by = "REGN")
    level2 <- sqldf(query_compose(2, 'df2')) %>% left_join(sqldf(query_compose(2, 'df3')), by = "REGN")
    level9 <- sqldf(query_compose(9, 'df2')) %>% left_join(sqldf(query_compose(9, 'df3')), by = "REGN")


    level0['DT'] <- dateAn
    level1['DT'] <- dateAn
    level2['DT'] <- dateAn
    level9['DT'] <- dateAn

    # ------------------------------------------------------------------------
    f135_list <- lst(level0, level1, level2, level9)

    for (name in names(f135_list)){

      urov <- str_sub(name, start = nchar(name), end = nchar(name))

      nameOfTable <- paste0('f135_level_', urov)

      d <- f135_list[[name]]
      names(d) <- tolower(names(d))

      table_id <- Id(schema="bank_data", table=nameOfTable)
      dbWriteTable(con, name=table_id, value=d, append=TRUE)
    }
    cat("Calculation of f135 on ", dateAn, " is ready", "\n")
  }
}

rm(list = ls()[! ls() %in% c('year', 'month')])








