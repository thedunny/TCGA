files <- list.files(pattern="follow_up")

setwd("C:/TCGA")

lt <- list()

for (f in files) {
  tmp <- read.delim(f)
  lt[[paste0(f)]] <- tmp
}

