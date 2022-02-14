library(foreign)
library(readxl)
library(openxlsx)

setwd('D:/Data center')

f101_maket <- read_excel('f101.xlsx', sheet = 4)

f101_maket['Internal'] = ''
for (i in 1:nrow(f101_maket)){
  vars <-  c()
  if (is.na(f101_maket[i, 'Order']) == T){
    for (j in i+1 : nrow(f101_maket)){
      if (f101_maket[j, 'Level'] - f101_maket[i, 'Level'] == 1 & startsWith(toString(f101_maket[j, 'Hierarchy']), toString(f101_maket[i, 'Hierarchy']) )){
        vars <- paste0(vars,  f101_maket[j, 'Acronym code'], ", ")
      }
    }
    f101_maket[i, 'Internal'] <- vars
  }
  
}

write.xlsx(f101_maket, 'f101_maket.xlsx')
