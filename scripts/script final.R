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
  col <- adc_cols(files, c)
  ds <- cbind (d, col)
}
aux<- adc_cols(files, 'bcr_patient_uuid')

#Remover as primeiras duas linhas do dataframe
r <- ds[, "bcr_patient_uuid"]=='CDE_ID:'
j<- c()
for (i in 1:length(r)){
  if (r[i]==TRUE){
    j <- c(j, i)
  }
}

j = rev(j)

ds <- ds, ]

ds <- ds[-l, ]


#Substituir o '[Not Available]' por NA
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


