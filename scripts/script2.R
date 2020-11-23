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
datat <- adc_linhas (aux, , files)
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
for (c in sel_cols){
  col <- col_rmv(files, c)
  ds <- cbind (d, col)
}
aux<- col_rmv(files, 'bcr_patient_uuid')
col_igual
data <- cbind (data, r)
tmp$nte_pr_status_by_ihc
ds$bcr_patient_uuid <- NULL

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
r <- ds[, "bcr_patient_uuid"]=='CDE_ID:'
j<- c()
for (i in 1:length(r)){
  if (r[i]==TRUE){
    j <- c(j, i)
  }
}

col <- data.frame(race <- col)
j = rev(j)

col1 <- col1[-j, ]

col1 <- col1[-l]

col1 <- data.frame(col1)

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
ds<- sub_NA(ds)

#Calcular a porcentagem de NAs presentes nas colunas
n <- nrow (df)

per_col_igual<- round(colSums(is.na(ds))*100/n, 2)

#Remover colunas que tenham 100% de valores faltantes
per_col_igual <- data.frame(per_col_igual)

i <- which(per_col_igual[, 'per_col_igual' ] ==100 )

per_col_igual$nm_col <- colnames(df)

nulos<- per_col_igual[i, ]
nulos

write.csv(ds, 'Dados Adenocarcinoma.csv', row.names = FALSE)
