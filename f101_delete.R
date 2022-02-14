library(foreign)
library(DBI)
library(dplyr)
library(stringr)

con <- dbConnect(RPostgres::Postgres()
                 , host='localhost'
                 , port='5432'
                 , dbname='bankometr'
                 , user='postgres'
                 , password='Davit')


list_f101 <- list('df_iitg_ost', 'df_rub_ost', 'df_inval_ost', 'df_iitg_do', 'df_rub_do', 'df_inval_do', 'df_iitg_co', 'df_rub_co', 'df_inval_co')

for (name in (list_f101)){
  for (lev in 1:7){
    
    nameOfTable <- paste0(str_replace(name, "df", "f101"), "_level_", lev)
    # cat(nameOfTable, "\n")
    
    dbSendQuery(con, paste0("delete from bank_data.", nameOfTable, " where dt = '2014-09-01'"))
    
    
  }
}
