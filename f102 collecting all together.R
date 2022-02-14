library(foreign)
library(readxl)
library(dplyr)
library(reshape2)
library(stringr)
library(sqldf)
library(openxlsx)

# data preparation --------------------------------------------------------


setwd("D:/Data center")

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

# создает функцию запроса
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


# создает список переменных для суммирования
split <- function(x){
  bb <- unlist(list(strsplit(toString(x), ",")))
  compon <-  c()
  for (i in 1:length(bb)){
    compon <- c(compon, sub(' ', '', bb[i]))  
    
  }
  return(compon)
}

# Заполняет остальные поля
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


# вычитает суммы за кварталы, кроме 1 квартала
subtract <- function(data1, data2){
  query <- 'select  \"REGN\" '
  for (i in 1:nrow(f102_maket)){
    query <- paste0(query, ", a.", f102_maket[i, 'Acronym code'], "- b.", f102_maket[i, 'Acronym code'], " as ", f102_maket[i, 'Acronym code'] )
  }
  query <- paste0(query, "from ", data1,  " a full outer join ", data2, " b where a.\"REGN\" = b.\"REGN\"")
  return(sqldf(query))
}


# выделяет переменные по уровням
levelFunc <- function(urov){
  vars <- c()
  data <- f102_maket%>%
    filter(Level == urov)
  for (i in 1:nrow(data)){
    vars <- c(vars, data[i, 'Acronym code'])
  }
  return(vars)
}

# заполняет данные по уровням 
fillData <- function(data){
  for (i in 2:8){
    data[]
  }
  
}






# calculation --------------------------------------------------


MainPath <- 'D:/project/Stress-tests/Data/0409102'


# содаем финальную data куда все данные будут сохраняться
f102_final_rub <- df_rub[0, ]
f102_final_inval <- df_inval[0, ]
f102_final_itogo <- df_itogo[0, ]

for (i in 2009:2009){
  for (j in 4:4){
    m <- ifelse(j<10, '0', '')
    dateAn <- paste0(i, '-', m, j, '-01')
    
    setwd(paste0(MainPath, '/102-', i, m, j, '01'))
    
    listdbf <- dir(pattern = "*.DBF") # creates the list of all the dbf files in the directory
    
    for (k in 1:length(listdbf)){
      if (str_sub(listdbf[[k]], start = length(listdbf[[k]])-7) == "P1.DBF"){
        df1 <-  read.dbf(listdbf[[k]])
        break
      } else if (str_sub(listdbf[[k]], start = length(listdbf[[k]])-6) == "P.DBF"){
        df2 <-  read.dbf(listdbf[[k]])
      }
    }
    df1$CODE <- as.character(df1$CODE)
    
    f102_maket_long_symb$bin <- ifelse(dateAn >= f102_maket_long_symb$date_start & dateAn < f102_maket_long_symb$date_end, 1, 0)
    df <- f102_maket_long_symb[f102_maket_long_symb$bin == 1, ]
    
    df_itogo <- complete(sqldf(query_compos('\"SIM_ITOGO\"', 'df1')))
    df_rub <- complete(sqldf(query_compos('\"SIM_R\"', 'df1')))
    df_inval <- complete(sqldf(query_compos('\"SIM_V\"', 'df1')))
    
    
    # на те периоды, когда некоторые банки сдавали только рез-ты итого только
    if (exists("df2")){
      df2$CODE <- as.character(df2$CODE)
      df_itogo2 <- complete(sqldf(query_compos('\"SIM_ITOGO\"', 'df2')))

      df_itogo <- rbind(df_itogo, df_itogo2)
      remove(df2)
    }
    
    # запись данных в общую базу f102_final_ 
    if (j == 4){
      f102_final_rub <- rbind(f102_final_rub, df_rub)
      f102_final_inval <- rbind(f102_final_inval, df_inval)
      f102_final_itogo <- rbind(f102_final_itogo, df_itogo)
      
      df_rub_q <- df_rub
      df_inval_q <- df_inval
      df_itogo_q <- df_itogo
    } else {
      # рассчитываем разницу между текущим значением и предыдущего квартала
      df_rub_new <- sqldf(subtract('df_rub', 'df_rub_q'))
      df_inval_new <- sqldf(subtract('df_inval', 'df_inval_q'))
      df_itogo_new <- sqldf(subtract('df_itogo', 'df_itogo_q'))
      
      # записываем в общий data
      f102_final_rub <- rbind(f102_final_rub, df_rub_new)
      f102_final_inval <- rbind(f102_final_inval, df_inval_new)
      f102_final_itogo <- rbind(f102_final_itogo, df_itogo_new)
      
      # перезаписываем для следующего квартала
      df_rub_q <- df_rub
      df_inval_q <- df_inval
      df_itogo_q <- df_itogo
    }
    
    
  }
}


# split data by level and store to the database---------------------------------
# run this code after data of P&L collected
list_f102 <- lst(f102_final_rub, f102_final_inval, f102_final_itogo)

for (k in names(list_f102)){
  for (i in 1:8){
    nameOfTable <- paste0(k, "_level_", i)
    
    # table_id <- Id(schema="bank_schema", table=nameOfTable)
    # dbWriteTable(con, name=table_id, value=list_f101[[k]], append=TRUE)
    
  }
}

















