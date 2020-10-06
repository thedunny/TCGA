try_col <- function(files){
  cont <- 0
  for (f in files) {
    tmp <- read.delim(f)
    #lt[[paste0(f)]] <- tmp
    if (is.null(tmp$new_tumor_event_site)){
      print("SEM TEMPO IRMAO")
    }else{
      cont<-cont+1
    }
  }
  return (cont)
}

#tentar juntar new_col e add_col
new_col <- function(files, cont){
  if(cont == 65){
    ndata <- data.frame(bcr_followup_barcode = factor())
    for(f in files){
      tmp <- read.delim(f)
      ndata <-  rbind (ndata, select (tmp, bcr_followup_barcode))
    }
  }
  return (ndata)
}


add_col <- function(ndata, dataf){
    return( cbind (dataf, ndata))
}






library (dplyr)
files <- list.files(pattern="follow_up")


dataff <- data.frame()
ndataff <- data.frame()

cont <- try_col(files) 
#primeira vez
dataff <- new_col(files, cont)
#outras vezes
ndataff <- new_col(files, cont)

dataff <- add_col(ndataff, dataff)




