files <- list.files(pattern="follow_up")

setwd("C:/TCGA/Arquivos")

getwd ()
lt <- list()

rm (ndata)
ndata <- data.frame(bcr_patient_uuid = factor())


ndata ["bcr_patient_barcode"] <- factor()

s_bcr <- 0
for (f in files) {
  tmp <- read.delim(f)
  #lt[[paste0(f)]] <- tmp
  if (tmp$bcr_patient_uuid){
    s_bcr <- s_bcr+1
  }
}

if(s_bcr == 65){
  for(f in files){
  tmp <- read.delim(f)
  ndata <-  rbind (ndata, select (tmp, bcr_patient_uuid))
  }
}





tmp <- lt[1]$nationwidechildrens.org_clinical_follow_up_v1.0_coad.txt



