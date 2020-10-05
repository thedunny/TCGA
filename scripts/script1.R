try_col <- function(files){
  cont <- 0
  for (f in files) {
    tmp <- read.delim(f)
    #lt[[paste0(f)]] <- tmp
    if (tmp$bcr_patient_barcode){
      cont <- cont+1
    }
  }
  return (cont)
}

#tentar juntar new_col e add_col
new_col <- function(files, cont){
  if(cont == 65){
    ndata <- data.frame(bcr_patient_barcode = factor())
    for(f in files){
      tmp <- read.delim(f)
      ndata <-  rbind (ndata, select (tmp, bcr_patient_barcode))
    }
  }
  return (ndata)
}


add_col <- function(ndata, dataf){
    return( cbind (dataf, ndata))
}







files <- list.files(pattern="follow_up")

setwd("C:/TCGA/Arquivos")

#lt <- list()

ndata ["bcr_patient_barcode"] <- factor()
rm (bcr_patient_uuid)
tmp <- lt[1]$nationwidechildrens.org_clinical_follow_up_v1.0_coad.txt
dataff <- data.frame()
ndataff <- data.frame()

cont <- try_col(files) 
#primeira vez
dataff <- new_col(files, cont)
#outras vezes
ndataff <- new_col(files, cont)

dataff <- add_col(ndataff, dataff)









