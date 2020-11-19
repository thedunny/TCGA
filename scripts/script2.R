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
        aux <- data.frame(rep(NA, nrow (tmp)))
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
datat <- adc_linhas (aux, nv, files)
aux <- init$bcr_patient_uuid
data1 <- adc_linhas (aux, 'history_other_malignancy.1', files)
data <- cbind (data, col)

data <- rename(data, race = col..race)
names(data)[576] <- 'race'
datateste <- data
aux <- init$bcr_patient_barcode

#adicionar manualmente colunas que foram removidas
col_rmv <- function(files, nms){
  df <- data.frame()
  df [nms] <- factor()
  print(nms)
  for (f in files){
    tmp <- read.delim(f)
    flag <-0 
    idx <- which(names(tmp) %like% nms)
    print (length(idx))
    if(length(idx)>1){
      for (i in idx){
        if (colnames(tmp [i])==nms){
          print('l>1')
          df <- rbind(df, tmp[i])
          flag <- 1
        }
      }
      if (flag == 0){
        aux <- data.frame(rep(NA, nrow (tmp)))
        names(aux) <- names(df)
        df<- rbind (df, aux)
      }
      
    }else{ 
      if(length(idx)==1){
        if(colnames(tmp [idx])==nms){
          print('==')
          df <- rbind(df, tmp[idx])
          
        }else if(colnames(tmp [idx])!=nms) {
          print('=!')
          aux <- data.frame(rep(NA, nrow (tmp)))
          names(aux) <- names(df)
          df<- rbind (df, aux)
        }
      }else{
        print('else')
        aux <- data.frame(rep(NA, nrow (tmp)))
        names(aux) <- names(df)
        df<- rbind (df, aux)
      }
    }
  }
  return (df)
}

library(data.table)

ds <- data.frame(aux <- aux)
for (c in col_igual){
  col <- col_rmv(files, c)
  ds <- cbind (ds, col)
}
col<- col_rmv(files, 'race')
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
col <- data.frame(col)
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
    
    if(any(ava)){
      data <- nas(ava, data, n)
    }
    if(any(apl)){
      data <- nas(apl, data, n)
    }
    if (any(eva)){
      data <- nas(eva, data, n)
    }
    if (any(unk)){
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

col <- sub_NA(col)

#Calcular a porcentagem de NAs presentes nas colunas
per_col_igual<- round(colSums(is.na(ds))*100/n, 2)

n <- nrow (ds)
o <- o[-1, ]
#Remover colunas que tenham 100% de valores faltantes
o <- data.frame(o)

i <- which(o[, 'o' ] ==100 )

o <- cbind(o, nv)

nulos<- o[i, ]

ds <- ds[, -i]
  
per_col_igual <- per_col_igual [-i, ]
      
    

rm_col_100 (ds, per_col_igual, 'per_col_igual')


col_igual <- col_igual[-1]


tmp <- lt[3]$nationwidechildrens.org_clinical_patient_brca.txt


data <- cbind(data, ds$retrospective_collection)


names(data)[503] <- 'retrospective_collection'

summary(ds$sdha
)

data[503]<- NULL

nms_col_igual <- c (11, 12, 13, 14, 15, 21, 22, 23, 24, 1, 3,5,7,8, 9, 10, 27, 28, 29, 30, 32, 34, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,53, 54, 55, 56,57, 59, 95, 117, 118, 119, 146,147,148,149)

for (n in nms_col_igual){
  data <- cbind(data, ds[n])
}

data[502:527] <- NULL












































