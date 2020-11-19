setwd("/home/dtiezzi/Documents/Projetos/TCGA/")

# Load packages
library(stringr)

files <- paste('./Arquivos/', list.files('./Arquivos'), sep = '')

cancer_types <- c()
for (f in 1:length(files))
  cancer_types[f] <- str_sub(strsplit(files[f], '_')[[1]][-1][-1][-1],1,-5)

cancer_types_attibutes <- c()
idx = 1
for (f in files) {
  tmp <- read.delim(f, skip = 1)[-1,]
  nB <- apply(tmp, 2, grep, pattern='[[]')
  nb <- lapply(nB, function(x) length(x)/nrow(tmp))
  badCols <- names(nb)[nb > 0.5]
  tmp <- tmp[, !colnames(tmp) %in% badCols]
  cancer_types_attibutes[cancer_types[idx]] <- list(colnames(tmp))
  idx <- idx + 1
}
cancer_types_attibutes
