lt <- list()
for (f in files) {
  tmp <- read.delim(f)
  lt[[paste0(f)]] <- tmp
}

tmp <- lt[4]$nationwidechildrens.org_clinical_patient_cesc.txt
setwd('C:/TCGA/Arquivos')

files <- list.files(pattern = 'patient')

library (data.table)

library (dplyr)

col_igual
#adicionar tabelas com as mesmas colunas
aux <- data.frame(final_data$bcr_patient_uuid)
t <-read.delim(f, skip=2)

cols <- adc_linhas(aux, nm_col_di, files)


newcols$final_data.bcr_patient_uuid <- NULL


dataf<-adc_linhas(aux, nm_col_dif, files)

a<- 'race'


#adicionar tabelas com as mesmas colunas
adc_linhas <- function(data_func, nms, files){
  for (n in nms){
    data <-  data.frame()
    data [n] <- factor()
    print(n)
    for (f in files){
      tmp <- read.delim(f)
      if (any(grep (n, colnames(tmp), fixed = TRUE))){
        data <- rbind(data, select(tmp, n))
      }else{
        aux <- data.frame(rep('[Not Available]', nrow (tmp)))
        names(aux) <- names(data)
        data <- rbind (data, aux)
      }
    }
    data_func <- cbind(data_func, data)
  }
  return(data_func)
}

library (dplyr)
init <- read.csv ('TCGA.csv')
data <- adc_linhas (aux, nv, files)
aux <- init$bcr_patient_uuid
data1 <- adc_linhas (aux, 'history_hormonal_contraceptives_use', files)
data <- cbind (data, col)

data <- rename(data, race = col..race)
names(data)[576] <- 'race'
datateste <- data


#adicionar manualmente colunas que foram removidas
col_rmv <- function(files, nms){
  df <- data.frame()
  df [nms] <- factor()
  for (f in files){
    tmp <- read.delim(f)
    l <-length(which(names(tmp) %like% nms))
    idx <- which(names(tmp) %like% nms)
    if(l>1){
      for (i in idx){
        if (names(tmp [i])==nms){
          df <- rbind(df, tmp[i])
        }
      }
    }else if(l==1){
      df <- rbind(df, tmp[idx])
      print(f)
    }else{
      aux <- data.frame(rep('[Not Available]', nrow (tmp)))
      names(aux) <- names(df)
      df<- rbind (df, aux)
    }
  }
  return (df)
}
library(data.table)

col<- col_rmv(files, 'extranodal_involvement')
col_igual
data <- cbind (data, r)



#funçao pegar o nome de todas as colunas de todas as tabelas
grab_cols <- function(files){
  nm_colunas <- c()
  for (f in files){
    tmp <- read.delim(f)
    nm_colunas <- c(nm_colunas, colnames(tmp))
    
    }
  return (nm_colunas)
}

nm_col_di <- unique(grab_cols(files))

idx <- grep ('history_colorectal_cancer', nm_col_di)

#função para separar as colunas que podem ser a mesma
sep_col_igual <- function (nm_col_di){
  df <- c()
  for (nms in nm_col_di){
    idx<-(length(which(like(nm_col_di, nms))))
    if (idx>1){
        df <- c(df, nm_col_di[which(like(nm_col_di, nms))])
    }
  }
  return (df)
}


col_igual <- unique(sep_col_igual(nm_col_di))

library (data.table)

#função para remover colunas parecidas
rmv_colunas_parecidas <- function (col_igual, nm_col_di){
  n<-nm_col_di
  for (c in col_igual){
    idx <- which(like(n, c))
    if (any(idx)){
      n <- n[-idx]

    }
  }
  return (n)
}

nv <- rmv_colunas_parecidas(col_igual, nm_col_di)







aux <- rmv (aux)

aux <- data$bcr_patient_barcode

colnames(aux)<- 'bcr_patient_barcode'
aux <- data.frame(aux <- aux)


nrmv <- aux[, 'bcr_patient_barcode'][1]


?replace

r<- data[, n]=='[Not Available]'


data[, n][1]
summary(r)

length (g[g==TRUE])

?mutate_all
?coalesce

data %>% 
  mutate_all(coalesce, '[Not Available]')

data %>% replace(data, c('[Not Available]', '[Not Aplicable]', '[Not Evaluate]'), c(NA, NA, NA))

g <- percdata[, 'Porcentagem'] > 90

############################################################
#Remover primeiras 2 linhas de cada tabela
r <- data[, "bcr_patient_uuid"]=='bcr_patient_uuid'
l<- c()
for (i in 1:length(r)){
  if (r[i]==TRUE){
    l <- c(l, i)
  }
}

col <- data.frame(race <- col)
l = rev(l)

col <- col[-j, ]

col <- col[-l, ]

summary (data$gender)
#############################################

sub_NA <- function(data){
  name <- colnames(data)
  
  for (n in name){
    print(n)
    ava <- which(data[, n] == '[Not Available]')
    apl <- which(data[, n] == '[Not Applicable]')
    eva <- which(data[, n] == '[Not Evaluated]')
    unk <- which(data[, n] == '[Unknown]')

    if (any(ava)){
      data <- nas(ava, data, n)
    }else if(any(apl)){
      data <- nas(apl, data, n)
    }else if (any(eva)){
      data <- nas(eva, data, n)
    }else if (any(unk)){
      data <- nas(unk, data, n)
    }
  }
  return(data)
}

nas <- function(non, data, n){
  for (i in non){
    data[i, n] <- NA
  }
  return (data)
}

datateste <- sub_NA(datateste)

datateste <- data
summary(datateste$gender)



per <- round(colSums(is.na(datateste))*100/n, 2)

n <- nrow (datateste)
occurences<-table(unlist(data$race)) 


rm_col_100 <- function(data, percdata){
  
  
  i <- which(percdata[, 'Porcentagem'] == 100)
  
  data <- data[, -i]
  
  percdata <- percdata [-i, ]
      
    
}

datateste <- rm_col_100 (datateste, percdata)


 
































































