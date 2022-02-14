library(openxlsx)



f102_maket$order <- ifelse(f102_maket$`с 01.04.2021` == 'Summa', '', 1)

while(any(is.na(f102_maket$Order) == TRUE)){
  for (i in 1:nrow(f102_maket)){
    if (is.na(f102_maket[i, 'Order']) == TRUE){
      k=1
      while (is.na(f102_maket[i+k, 'Order']) == TRUE){
        k = k+1
      }
      f102_maket[i+k-1, 'Order'] <-  f102_maket[i+k, 'Order'] +1
    }
  }
}


while(any(is.na(f102_maket$Order) == TRUE)){
  for (i in 1:nrow(f102_maket)){
    if (is.na(f102_maket[i, 'Order']) == TRUE){
      b <- f102_maket[split(f102_maket[i, 'Internal']), ]
      if (!(any(is.na(b['Order'])) == TRUE)){
        f102_maket[i, 'Order'] <- max(b['Order'])+1
      }
    }
  }
}



if (!exists("df2")){
  cat("Yes")
}









f102_maket <- f102_maket %>%
  arrange({{Hierarchy}})


f102_maket['Internal'] = ''
for (i in 1056:nrow(f102_maket)){
  vars <-  c()
  if (f102_maket[i, 'Order'] != 1){
    for (j in i+1 : nrow(f102_maket)){
      if (f102_maket[j, 'Level'] - f102_maket[i, 'Level'] == 1 & startsWith(toString(f102_maket[j, 'Hierarchy']), toString(f102_maket[i, 'Hierarchy']) )){
        vars <- paste0(vars,  f102_maket[j, 'Acronym code'], ", ")
      }
    }
    f102_maket[i, 'Internal'] <- vars
  }
  
}
        
        
write.xlsx(f102_maket, 'f102_new.xlsx')     

str(f102_maket[2, 'Hierarchy'])     

f102_maket_inter <- f102_maket[is.na(f102_maket$Internal) == F, ]
f102_maket_inter[1:20, 'Acronym code']




write.xlsx(df_itogo, "D:/Data center/df_itogo.xlsx")

