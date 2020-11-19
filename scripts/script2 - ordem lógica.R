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

col_dif <- rmv_colunas_parecidas(col_igual, nm_col_di)


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
aux <- init$bcr_patient_uuid
data <- adc_linhas (aux, col_dif, files)


#adicionar manualmente colunas que foram removidas
col_rmv <- function(files, nms){
  df <- data.frame()
  df [nms] <- factor()
  print(nms)
  for (f in files){
    tmp <- read.delim(f)
    flag <-0 
    idx <- which(names(tmp) %like% nms)
    if(length(idx)>1){
      for (i in idx){
        if (colnames(tmp [i])==nms){
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
          df <- rbind(df, tmp[idx])
          
        }else if(colnames(tmp [idx])!=nms) {
          aux <- data.frame(rep(NA, nrow (tmp)))
          names(aux) <- names(df)
          df<- rbind (df, aux)
        }
      }else{
        aux <- data.frame(rep(NA, nrow (tmp)))
        names(aux) <- names(df)
        df<- rbind (df, aux)
      }
    }
  }
  return (df)
}


ds <- data.frame(aux <- aux)
for (c in col_igual){
  col <- col_rmv(files, c)
  ds <- cbind (ds, col)
}

col <- col_rmv(files, 'race')
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

ds <- ds[-j, ]

ds <- ds[-l, ]


#Substituir valores inválidos por NA
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

ds <- sub_NA(ds)


#Calcular a porcentagem de NAs presentes nas colunas
n <- nrow (ds)
per_col_igual <- round(colSums(is.na(ds))*100/n, 2)


#Remover colunas que tenham 100% de valores faltantes
per_col_igual <- data.frame(per_col_igual)

i <- which(per_col_igual[, 'per_col_igual' ] ==100 )

per_col_igual <- cbind(per_col_igual, colnames(ds))

per_col_igual [i, ]

ds <- ds[,-i]

per_col_igual <- per_col_igual [-i, ]

col_igual <- col_igual[-1]

row.names(per_col_igual) <- c(1:149)


