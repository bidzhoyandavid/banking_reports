library(tictoc)
tic('f102 execution time:')

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

# data preparation --------------------------------------------------------


setwd("/Users/davitbidzoan/Desktop/Data center")

f102_maket <- read_excel('f102.xlsx', sheet = 1)
f102_dates <- read_excel('f102.xlsx', sheet = 2)


f102_maket_long <- melt(f102_maket, id = c('Hierarchy', 'Level', 'Name', 'Acronym code', 'Order', 'Internal'))
f102_maket_long['plus'] <- ''
f102_maket_long['minus'] <- ''
f102_maket_long['date_start'] <- ''
f102_maket_long['date_end'] <- ''

f102_maket_long$value <- as.character(f102_maket_long$value)

f102_maket_long  <- 
  f102_maket_long %>%
  mutate_at('variable', as.character)

f102_dates$date_start <- as.character(f102_dates$date_start)
f102_dates$date_end <- as.character(f102_dates$date_end)

f102_maket_long_symb <- f102_maket_long[(f102_maket_long$Order) == '1', ]

for (i in 1:nrow(f102_maket_long_symb)){
  len <-  nchar(f102_maket_long_symb[i, 'value'])
  a <-  c()
  d <-  c()
  if (is.na(f102_maket_long_symb[i, 'value']) == FALSE){
    for (j in seq(1, len, 6)){
      
      if (substr(f102_maket_long_symb[i, 'value'], j, j) == '+'){
        b <-  substr(f102_maket_long_symb[i, 'value'], j+1, j+5)
        a <-  paste0(a, ", '", b, "'")
      } else {
        c <-  substr(f102_maket_long_symb[i, 'value'], j+1, j+5)
        d <-  paste0(d, ", '", c, "'")
      }
    }
    
    ifelse(is.null(a) == FALSE, f102_maket_long_symb[i, 'plus'] <-  a, f102_maket_long_symb[i, 'plus'] <- sprintf("'%s'",'aaaa'))
    ifelse(is.null(d) == FALSE, f102_maket_long_symb[i, 'minus'] <- d, f102_maket_long_symb[i, 'minus'] <-  sprintf("'%s'",'aaaa'))
    
    ifelse(f102_maket_long_symb[i, 'plus'] != sprintf("'%s'",'aaaa'), f102_maket_long_symb[i, 'plus'] <- substr(f102_maket_long_symb[i, 'plus'], 3, nchar(f102_maket_long_symb[i, 'plus'])), f102_maket_long_symb[i, 'plus'] <- sprintf("'%s'",'aaaa'))
    ifelse(f102_maket_long_symb[i, 'minus'] != sprintf("'%s'",'aaaa'), f102_maket_long_symb[i, 'minus'] <- substr(f102_maket_long_symb[i, 'minus'], 3, nchar(f102_maket_long_symb[i, 'minus'])), f102_maket_long_symb[i, 'minus'] <- sprintf("'%s'",'aaaa'))
  } else {f102_maket_long_symb[i, 'plus'] <-  ''}
  f102_maket_long_symb[i, 'variable'] <-  substr(f102_maket_long_symb[i, 'variable'], 3, nchar(f102_maket_long_symb[i, 'variable']))
  f102_maket_long_symb[i, 'variable'] <- as.character(as.Date(f102_maket_long_symb[i, 'variable'], "%d.%m.%Y"))
  f102_maket_long_symb[i, 'date_start'] <- f102_maket_long_symb[i, 'variable']
  k = 1
  while (f102_dates[k, "date_start"] != f102_maket_long_symb[i, 'variable']){
    k = k+1
  } 
  f102_maket_long_symb[i, 'date_end'] <- f102_dates[k, "date_end"]
}

remove(a, b, c, d, i, j, k, len)

# functions ------------------------------------------------------------

# composing query
query_compos <- function(x, y){
  query <- "select distinct  \"REGN\" "
  new_column <- c()
  for (i in 1:nrow(df)){
    if (length(df[i, 'value']) != 0){
      query <- paste(query, ", sum(case when \"CODE\" in (", df[i, 'plus'], ") then", x ," else 0 end) - sum(case when \"CODE\" in(", df[i, 'minus'], ") then ", x, " else 0 end) as ", df[i, 'Acronym code'] ) 
    } else {new_column <-  c(new_column, df[i, "Acronym code"]) }
  } 
  query <- paste0(query, " from ", y, " group by \"REGN\" ")
  return(query)
}


#  split string by ","
split <- function(x){
  bb <- unlist(list(strsplit(toString(x), ",")))
  compon <-  c()
  for (i in 1:length(bb)){
    compon <- c(compon, sub(' ', '', bb[i]))  
    
  }
  return(compon)
}

# complete other columns
complete <- function(data){
  for (i in 2:8){
    f102_maket_lev <- f102_maket[f102_maket$Order == i, ]
    for (j in 1:nrow(f102_maket_lev)){
      data$aa  <- apply(X = data[ split(f102_maket_lev[j, 'Internal'])], MARGIN = 1, FUN = sum)
      colnames(data)[colnames(data)  == 'aa'] <- f102_maket_lev[j, 'Acronym code']
    }
  }
  return(data)
}


# subtract for data quarters other than 1
subtract <- function(data1, data2){
  query <- 'select  a.\"REGN\" '
  for (i in 1:nrow(f102_maket)){
    query <- paste0(query, ", a.", f102_maket[i, 'Acronym code'], "- b.", f102_maket[i, 'Acronym code'], " as ", f102_maket[i, 'Acronym code'] )
  }
  query <- paste0(query, " from ", data1,  " a left  join ", data2, " b on a.\"REGN\" = b.\"REGN\" ")
  return(query)
}


# determines vars for level
levelFunc <- function(urov){
  vars <- c()
  data <- f102_maket%>%
    filter(Level == urov)
  for (i in 1:nrow(data)){
    vars <- c(vars, data[i, 'Acronym code'])
  }
  return(vars)
}

# function of preparing data of some date
prep <- function(date, env){

  listdbf <- dir(pattern = "*.DBF") # creates the list of all the dbf files in the directory
  
  for (k in 1:length(listdbf)){
    if (str_sub(listdbf[[k]], start = length(listdbf[[k]])-7) == "P1.DBF"){
      env$df1 <-  read.dbf(listdbf[[k]])
      break
    }
  }
  env$df1$CODE <- as.character(env$df1$CODE)
  
  f102_maket_long_symb$bin <- ifelse(date >= f102_maket_long_symb$date_start & date < f102_maket_long_symb$date_end, 1, 0)
  env$df <- f102_maket_long_symb[f102_maket_long_symb$bin == 1, ]
  
}



# calculation --------------------------------------------------

MainPath <- '/Users/davitbidzoan/Desktop/Stress-tests/Data/0409102'



for (i in year:year){
  for (j in month:month){
    m <- ifelse(j<10, '0', '')
    dateAn <- paste0(i, '-', m, j, '-01')
    
    setwd(paste0(MainPath, '/102-', i, m, j, '01'))
    # define data for current date
    myEnv <- new.env()
    prep(dateAn, myEnv)
    df1 <- myEnv$df1
    df <- myEnv$df
    
    # parsing
    df_itogo <- complete(sqldf(query_compos('\"SIM_ITOGO\"', 'df1')))
    df_rub <- complete(sqldf(query_compos('\"SIM_R\"', 'df1')))
    df_inval <- complete(sqldf(query_compos('\"SIM_V\"', 'df1')))

    df_itogo['DT'] <- dateAn
    df_rub['DT'] <-  dateAn
    df_inval['DT'] <- dateAn
    
    # calculate difference for the quarters except the 1 quarter
    if (j != 4){
      # i <-  i-1
      # define previous date
      current <- c(7, 10, 1)
      previous <- c(4, 7, 10)
      
      k=1
      while (current[k] != j){k = k+1} 
      m_new <- ifelse(j == 1, '', '0')
      dateAn_new = paste0(i, "-", m_new, previous[k], '-01')
      
      # set working directory of previous date
      setwd(paste0(MainPath, '/102-', i, m_new, previous[k], '01'))
      
      # define data for previous date
      myEnv_new <- new.env()
      prep(dateAn_new, myEnv_new)
      
      df1_new <- myEnv_new$df1
      df_new <- myEnv_new$df
      
      df_itogo_p <- complete(sqldf(query_compos('\"SIM_ITOGO\"', 'df1_new')))
      df_rub_p <- complete(sqldf(query_compos('\"SIM_R\"', 'df1_new')))
      df_inval_p <- complete(sqldf(query_compos('\"SIM_V\"', 'df1_new')))

      # calculate net difference
      df_rub_7 <- sqldf(subtract('df_rub', 'df_rub_p'))
      df_inval_7 <- sqldf(subtract('df_inval', 'df_inval_p'))
      df_itogo_7 <- sqldf(subtract('df_itogo', 'df_itogo_p'))
      
      df_itogo_7['DT'] <- dateAn
      df_rub_7['DT'] <-  dateAn
      df_inval_7['DT'] <- dateAn

    }
    

    
    # split data by level and store to the database---------------------------------
    if (j == 4){
      list_f102 <- lst(df_rub, df_inval, df_itogo)
    } else {
      list_f102 <- lst(df_rub_7, df_inval_7, df_itogo_7)
    }

    for (name in names(list_f102)){
      for (lev in 1:8){
        nameOfTable <- paste0(str_replace(str_replace(name, 'df', 'f102'), "_7", ""), "_level_", lev)

        df_level <- f102_maket %>% filter(Level == lev)
        vars <- c("REGN", "DT", levelFunc(lev))
        d <- list_f102[[name]]
        d2 <- d[, unlist(vars)]
        names(d2) <- tolower(names(d2))

        table_id <- Id(schema="bank_data", table=nameOfTable)
        dbWriteTable(con, name=table_id, value=d2, append=TRUE)

      }
    }
    cat("Calculation of f102 on ", dateAn, " is ready ", "\n")
    
  }
}

rm(list = ls()[! ls() %in% c('year', 'month')])
toc()