lt <- list()
for (f in files) {
  tmp <- read.delim(f)
  lt[[paste0(f)]] <- tmp
}

tmp <- lt[4]$nationwidechildrens.org_clinical_patient_cesc.txt
setwd('C:/TCGA/Arquivos')



library (data.table)

library (dplyr)


#adicionar tabelas com as mesmas colunas
aux <- data.frame(final_data$bcr_patient_uuid)


cols <- adc_linhas(aux, nm_col_di, files)


newcols$final_data.bcr_patient_uuid <- NULL


dataf<-adc_linhas(aux, nm_col_dif, files)


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

data <- adc_linhas (aux, nv, files)

data1 <- adc_linhas (aux, 'history_hormonal_contraceptives_use', files)


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
        if (names(tmp [i])==nms)
          df <- rbind(df, tmp[i])
      }
    }else if(l==1){
      df <- rbind(df, tmp[idx])
    }else{
      aux <- data.frame(rep('[Not Available]', nrow (tmp)))
      names(aux) <- names(df)
      df<- rbind (df, aux)
    }
  }
  return (df)
}

r <- col_rmv(files, 'race')

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


