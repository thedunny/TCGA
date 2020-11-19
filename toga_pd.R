setwd("/home/dtiezzi/Documents/Projetos/TCGA/")

# Load packages
library(stringr)

files <- paste('./Arquivos/', list.files('./Arquivos'), sep = '')
cancer_types <- c()
for (f in 1:length(files))
  cancer_types[f] <- str_sub(strsplit(files[f], '_')[[1]][-1][-1][-1],1,-5)
cancer_types

cancer_types_attibutes <- c()
idx = 1
for (f in files) {
  tmp <- read.delim(files[1], skip = 1)[-1,]
  cancer_types_attibutes[cancer_types[idx]] <- list(colnames(tmp))
  idx <- idx + 1
}
cancer_types_attibutes
