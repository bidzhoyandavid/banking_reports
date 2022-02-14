acc <- c()
for (i in 1:nrow(df)){
  x <- df[i, 'value']
  if(is.na(x)==F){
    if (grepl('(', x, fixed = T)){
      len <- str_locate(x, "\\(")[, 1]- 2
    } else{len <- nchar(x)}
    
    for (j in seq(1, len, 6)){
      acc <- c(acc, substr(x, j+1, j+5))
    }
  }
}
acc
acc <- sort(acc, decreasing  = F)

for (k in 2:length(acc)){
  if (nchar(acc[k]) != 5){
    cat(acc[k], "\n")
  }
  
    # if (acc[k] == acc[k-1]){
  #   cat(acc[k], "\n")
  # }
}


p302 <- read_excel("D:/Data center/101 form/302-p/302-п счета.xlsx", sheet = 3)




acc302 <- c()

for (i in 1:nrow(p302)){
  acc302 <- c(acc302, p302[i, 'account'])
}

length(acc302)
length(acc)

setdiff(acc302, acc)
setdiff(acc, acc302)




d1 <- sqldf("select distinct \"REGN\", \"DT\", max(   (sum(case when \"NUM_SC\" in ('40111') then\"IITG\"else 0 end) - sum(case when \"NUM_SC\" in ('40110') then\"IITG\"else 0 end)    ), 0) as A_OtherAssets_Other from df1 group by \"REGN\", \"DT\"")



library(dplyr)


df3 <- df1%>%
  filter(DT == ("2016-02-01"))
typeof("DT")
class(df1$DT)

df1[1, 'DT']
