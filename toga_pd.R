setwd("/home/dtiezzi/Documents/Projetos/TCGA/")

# Load packages
library(stringr)

files <- paste('./Arquivos/', list.files('./Arquivos'), sep = '')

cancer_types <- c()
for (f in 1:length(files))
  cancer_types[f] <- str_sub(strsplit(files[f], '_')[[1]][-1][-1][-1],1,-5)
cancer_types
cancer_types_filt <- cancer_types[c(2,3,4,5,6,8,12,13,17,20,21,23,24,27,29,31)]
files <- files[c(2,3,4,5,6,8,12,13,17,20,21,23,24,27,29,31)]

cancer_types_attibutes <- c()
cancer_types_desc <- c()
idx = 1
for (f in files) {
  tmp <- read.delim(f, skip = 1)[-1,]
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
sel_cols
