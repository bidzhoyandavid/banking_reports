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

# Data preparation --------------------------------------------------------

setwd("/Users/davitbidzoan/Desktop/Data center")

f101_maket <- read_excel('f101.xlsx', sheet = 1)
f101_dates <- read_excel('f101.xlsx', sheet = 2)

f101_maket_long <- melt(f101_maket, id = c('Hierarchy', 'Level', 'Name', 'Acronym code', 'Order', 'Internal'))
f101_maket_long['plus'] <- ''
f101_maket_long['minus'] <- ''
f101_maket_long['date_start'] <- ''
f101_maket_long['date_end'] <- ''

f101_maket_long$value <- as.character(f101_maket_long$value)

f101_maket_long  <- 
  f101_maket_long %>%
  mutate_at('variable', as.character)


f101_dates$date_start <- as.character(f101_dates$date_start)
f101_dates$date_end <- as.character(f101_dates$date_end)

f101_maket_long_symb <- f101_maket_long[(f101_maket_long$Order) == '1', ]



for (i in 1:nrow(f101_maket_long_symb)){
  x <-  f101_maket_long_symb[i, 'value']
  if (grepl('(', x, fixed = T)){
    len <- str_locate(x, "\\(")[, 1]- 2
  } else { len <- nchar(x)}
  
  a <-  c()
  d <-  c()
  if (is.na(x) == FALSE){
    for (j in seq(1, len, 6)){
      
      if (substr(x, j, j) == '+'){
        b <-  substr(x, j+1, j+5)
        a <-  paste0(a, ", '", b, "'")
      } else {
        c <-  substr(x, j+1, j+5)
        d <-  paste0(d, ", '", c, "'")
      }
    }
    
    ifelse(is.null(a) == FALSE, f101_maket_long_symb[i, 'plus'] <-  a, f101_maket_long_symb[i, 'plus'] <- sprintf("'%s'",'aaaa'))
    ifelse(is.null(d) == FALSE, f101_maket_long_symb[i, 'minus'] <- d, f101_maket_long_symb[i, 'minus'] <-  sprintf("'%s'",'aaaa'))
    
    ifelse(f101_maket_long_symb[i, 'plus'] != sprintf("'%s'",'aaaa'), f101_maket_long_symb[i, 'plus'] <- substr(f101_maket_long_symb[i, 'plus'], 3, nchar(f101_maket_long_symb[i, 'plus'])), f101_maket_long_symb[i, 'plus'] <- sprintf("'%s'",'aaaa'))
    ifelse(f101_maket_long_symb[i, 'minus'] != sprintf("'%s'",'aaaa'), f101_maket_long_symb[i, 'minus'] <- substr(f101_maket_long_symb[i, 'minus'], 3, nchar(f101_maket_long_symb[i, 'minus'])), f101_maket_long_symb[i, 'minus'] <- sprintf("'%s'",'aaaa'))
  } else {f101_maket_long_symb[i, 'plus'] <-  ''}
  f101_maket_long_symb[i, 'variable'] <-  substr(f101_maket_long_symb[i, 'variable'], 3, nchar(f101_maket_long_symb[i, 'variable']))
  f101_maket_long_symb[i, 'variable'] <- as.character(as.Date(f101_maket_long_symb[i, 'variable'], "%d.%m.%Y"))
  f101_maket_long_symb[i, 'date_start'] <- f101_maket_long_symb[i, 'variable']
  k = 1
  while (f101_dates[k, "date_start"] != f101_maket_long_symb[i, 'variable']){
    k = k+1
  } 
  f101_maket_long_symb[i, 'date_end'] <- f101_dates[k, "date_end"]
  
}

remove(a, b, c, d, i, j, k, len, x)

# Functions ------------------------------------------------------------

dop <- c('(30232-30233>0)', '(30222-30221>0)', '303(??)', '304(??)', '(47424-47421>0)', '(30221-30222>0)', '(30233-30232>0)', '303(??)', '304(??)','(47421-47424>0)', '(40109-40108>0)', '(40111-40110>0)', '(40108-40109>0)', '(40110-40111>0)')


# additional calculation like (30222-30221>0)
minus <- function(x, y, curr){
  query <- paste0("max((sum(case when \"NUM_SC\" in ('", x, "') then ", curr, " else 0 end) - sum(case when \"NUM_SC\" in ('", y, "') then ", curr,  " else 0 end)), 0)")
  return(query)
}


# determine the list of dop
spisok <-  function(bb){
  spis <- c()
  i = which(dop %in% unlist(strsplit(bb, '[+]')))
  spis <- c()
  if (length(i)>0){
    spis <- c(spis, dop[i])
    # paste(a[i], collapse = " ")
    return(spis)
  } else {NA}
}


# composing a query
query_compos <- function(curr, datafr){
  query <- "select distinct  \"REGN\", \"DT\" "
  new_column <- c()
  for (i in 1:nrow(df)){
    var <-  df[i, 'value']
    if (length(var) != 0){
      query <- paste0(query, ", sum(case when \"NUM_SC\" in (", df[i, 'plus'], ") then ", curr ," else 0 end) - sum(case when \"NUM_SC\" in (", df[i, 'minus'], ") then ", curr, " else 0 end) ") 
      
      if (grepl('(', var, fixed = T)){
        sps <- spisok(var)
        for (k in sps){
          if (grepl("??", k, fixed = T)){ 
            if (grepl("303", k, fixed = T)){
              query <- paste0(query, "+ max( sum(case when \"NUM_SC\" in ('30302', '30306', '30304') then ", curr, " else 0 end) -  sum(case when \"NUM_SC\" in ('30301', '30303', '30305') then ", curr, " else 0 end), 0)")
            } else {
              query <- paste0(query, "+ max( sum(case when \"NUM_SC\" in ('30402', '30404', '30406', '30409') then ", curr, " else 0 end) -  sum(case when \"NUM_SC\" in ('30401', '30403', '30405', '30408') then ", curr, " else 0 end), 0)")
            }
          } else if (grepl("??", k, fixed = T)){
            if (grepl("303", k, fixed = T)){
              query <- paste0(query, "+ max( sum(case when \"NUM_SC\" in ('30301', '30303', '30305') then ", curr, " else 0 end) -  sum(case when \"NUM_SC\" in ('30302', '30304', '30306') then ", curr, " else 0 end), 0)")
            } else{
              query <- paste0(query, "+ max( sum(case when \"NUM_SC\" in ('30401', '30403', '30405', '30408') then ", curr, " else 0 end) -  sum(case when \"NUM_SC\" in ('30402', '30404', '30406', '30409') then ", curr, " else 0 end), 0)")
            }
          } else {
            query <- paste0(query, "+", minus(substr(k, 2, 6), substr(k, 8, 12), curr))
          }
        }
      } 
      
      query <- paste0(query, " as ", df[i, 'Acronym code'])
      
    } else {new_column <-  c(new_column, df[i, "Acronym code"]) }
  } 
  query <- paste0(query, " from ", datafr, " group by \"REGN\", \"DT\" ")
  return(query)
}



# split string
split <- function(x){
  bb <- unlist(list(strsplit(toString(x), ",")))
  compon <-  c()
  for (i in 1:length(bb)){
    compon <- c(compon, sub(' ', '', bb[i]))  
  }
  return(compon)
}

# complete the data
complete <- function(data){
  for (level in 2:7){
    f101_maket_lev <- f101_maket[f101_maket$Order == level, ]
    for (row in 1:nrow(f101_maket_lev)){
      data$aa  <- apply(X = data[ split(f101_maket_lev[row, 'Internal'])], MARGIN = 1, FUN = sum)
      colnames(data)[colnames(data)  == 'aa'] <- f101_maket_lev[row, 'Acronym code']
    }
  }
  return(data)
}

# define the variables depending of level
levelFunc <- function(urov){
  vars <- c()
  data <- f101_maket%>%
    filter(Level == urov)
  for (i in 1:nrow(data)){
    vars <- c(vars, data[i, 'Acronym code'])
  }
  return(vars)
}




# Calculation --------------------------------------------------

MainPath <- '/Users/davitbidzoan/Desktop/Stress-tests/Data/0409101'


for (i in year:year){
  for (j in month:month){
    m <- ifelse(j<10, '0', '')
    dateAn <- paste0(i, '-', m, j, '-01')
    
    setwd(paste0(MainPath, '/101-', i, m, j, '01'))
    
    listdbf <- dir(pattern = "*.DBF") # creates the list of all the dbf files in the directory
    
    for (k in 1:length(listdbf)){
      if (str_sub(listdbf[[k]], start = length(listdbf[[k]])-7) == "B1.DBF"){
        df1 <-  read.dbf(listdbf[[k]])
        df1 <- df1 %>%
          filter(DT == dateAn)
        break
      } else if (str_sub(listdbf[[k]], start = length(listdbf[[k]])-6) == "B.DBF"){
        df2 <-  read.dbf(listdbf[[k]])
        df2 <- df2 %>%
          filter(DT == dateAn)
      }
    }
    
    f101_maket_long_symb$bin <- ifelse(dateAn >= f101_maket_long_symb$date_start & dateAn < f101_maket_long_symb$date_end, 1, 0)
    df <- f101_maket_long_symb[f101_maket_long_symb$bin == 1, ]
    row.names(df) <- 1:nrow(df)
    
    # outcoming remains -----------------------------------------------------------------------------
    df_iitg_ost <- complete(sqldf(query_compos('\"IITG\"', 'df1')))
    # write.xlsx(sqldf("select \"REGN\", \"DT\", A_Assets, L_Liab, C_Capital, (A_Assets - L_Liab -C_Capital) as Prov from df_iitg_ost"), 'D:/Data center/Balance.xlsx')
    # openXL('D:/Data center/Balance.xlsx')
    
    # checking out the balance
    prov <- df_iitg_ost %>%
      mutate(prov = A_Assets - C_Capital -L_Liab) %>%
      filter(prov != 0) 
    # %>%
    #   select(REGN, prov)

    if (nrow(prov) != 0) {
      cat("   f101 disbalance detected on ", dateAn, "\n")
    }

    df_rub_ost <- complete(sqldf(query_compos('\"IR\"', 'df1')))
    df_inval_ost <- complete(sqldf(query_compos('\"IV\"', 'df1')))

    # debit turnover -------------------------------------------------
    df_iitg_do <- complete(sqldf(query_compos('\"OITGA\"', 'df1')))
    df_rub_do <- complete(sqldf(query_compos('\"ORA\"', 'df1')))
    df_inval_do <- complete(sqldf(query_compos('\"OVA\"', 'df1')) )

    # credit turnover -------------------------------------------------
    df_iitg_co <- complete(sqldf(query_compos('\"OITGP\"', 'df1')))
    df_rub_co <- complete(sqldf(query_compos('\"ORP\"', 'df1')))
    df_inval_co <- complete(sqldf(query_compos('\"OVP\"', 'df1')))

    # if additional balance file exists -------------------------------
    if(exists('df2')){
      df_iitg_ost2 <- complete(sqldf(query_compos('\"ITOGO\"', 'df2')))
      df_iitg_ost <- rbind(df_iitg_ost, df_iitg_ost2) %>%distinct()
      remove(df2)
    }
    
    # split data by level ---------------------------------------------------------------------------
    list_f101 <- lst(df_iitg_ost, df_rub_ost, df_inval_ost, df_iitg_do, df_rub_do, df_inval_do, df_iitg_co, df_rub_co, df_inval_co)

    for (name in names(list_f101)){
      for (lev in 1:7){

        nameOfTable <- paste0(str_replace(name, "df", "f101"), "_level_", lev)
        # cat(nameOfTable, "\n")

        df_level <- f101_maket %>% filter(Level == lev)
        vars <- c("REGN", "DT", levelFunc(lev))
        d <- list_f101[[name]]
        d2 <- d[, unlist(vars)]
        names(d2) <- tolower(names(d2))

        table_id <- Id(schema="bank_data", table=nameOfTable)
        dbWriteTable(con, name=table_id, value=d2, append=TRUE)

      }
    }

    cat("Calculation of f101 on", dateAn, " is ready", "\n")
  }
}

rm(list = ls()[! ls() %in% c('year', 'month')])
