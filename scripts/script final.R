# Load packages
library(stringr)

files <- paste('./Arquivos/', list.files('./Arquivos'), sep = '')

#Escolher colunas que só tenham o subtipo Adenocarcinoma
cancer_types <- c()
for (f in 1:length(files)){
  cancer_types[f] <- str_sub(strsplit(files[f], '_')[[1]][-1][-1][-1],1,-5)
}
cancer_types
cancer_types_filt <- cancer_types[c(2,3,4,5,6,8,12,13,17,20,21,23,24,27,29,31)]
files <- files[c(2,3,4,5,6,8,12,13,17,20,21,23,24,27,29,31)]
#seleção de colunas que existem em todas as tabelas escolhidas
cancer_types_attibutes <- c()
cancer_types_desc <- c()
idx = 1
for (f in files) {
  tmp <- read.delim(f)[-1,]
  # nB <- apply(tmp, 2, grep, pattern='[[]')
  # nb <- lapply(nB, function(x) length(x)/nrow(tmp))
  # badCols <- names(nb)[nb > 0.35]
  #tmp <- tmp[, !colnames(tmp) %in% badCols]
  cancer_types_attibutes[cancer_types_filt[idx]] <- list(colnames(tmp))
  cancer_types_desc[cancer_types_filt[idx]] <- tmp$tumor_tissue_site[1]
  idx <- idx + 1
}

ColNames_filt <- sort(unique(unlist(cancer_types_attibutes)))

#seleção de colunas de interesse
ColNames_count <- c()
for (cn in ColNames_filt) {
  n = 0
  for (i in 1:length(cancer_types_attibutes)) {
    if (cn %in% cancer_types_attibutes[[i]])
      n = n+1
  }
  ColNames_count[cn] <- n
}

cnInAll <- names(ColNames_count[ColNames_count == length(cancer_types_filt)])

df_cn <- data.frame(n = sort(ColNames_count, decreasing = T))
df_cn$Colnames <- rownames(df_cn)
df_cn <- df_cn[, c(2,1)]
rownames(df_cn) <- seq(1, nrow(df_cn))
head(df_cn)
df_cn
sel_cols <- df_cn$Colnames[c(1:43,45,46,48:50,52:57, 63, 65, 172, 177, 295, 254)]


#Adicionar colunas
adc_cols <- function(files, nms){
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

library(data.table)

ds <- data.frame(aux <- aux)
for (c in sel_cols){
  col <- adc_cols(files, c)
  ds <- cbind (d, col)
}
aux<- adc_cols(files, 'history_neoadjuvant_treatment')

#Remover as primeiras duas linhas do dataframe
r <- ds[, "bcr_patient_uuid"]=='CDE_ID:'
j<- c()
for (i in 1:length(r)){
  if (r[i]==TRUE){
    j <- c(j, i)
  }
}

j = rev(j)

aux <- aux[-j ]

aux <- aux[-l, ]


#Substituir o '[Not Available]' por NA
sub_NAS <- function(data){
  na <- c('[Not Available]', '[Not Applicable]', '[Not Evaluated]', '[Unknown]')
  for (n in colnames(data)){
    for (i in na){
      p <- which(data[, n] == i)
      if (any(p))
        data <- nas(p, data, n)
    }
  }
  return (data)
}

nas <- function(non, data, n){
  for (i in non){
    data[i, n] <- NA
  }
  return (data)
}
aux<- sub_NAS(aux)

#Calcular a porcentagem de NAs presentes nas colunas
n <- nrow (ds)

per_col_igual<- round(colSums(is.na(ds))*100/n, 2)

#Remover colunas que tenham 100% de valores faltantes
per_col_igual <- data.frame(per_col_igual)

i <- which(per_col_igual[, 'per_col_igual' ] > 90)

per_col_igual$nm_col <- colnames(ds)

nulos<- per_col_igual[i, ]
nulos

ds <- ds [, -i]


write.csv(ds, 'DadosAdenocarcinoma.csv')
