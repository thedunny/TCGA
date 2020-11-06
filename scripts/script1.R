


try_col <- function(files){
  cont <- 0
  add<-0
  for (f in files) {
    tmp <- read.delim(f)
    lt[[?paste0(f)]] <- tmp
    if (is.null(tmp$last_contact_days_to)){
      tmp2<-lt[f]
      print(tmp2)
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

files <- list.files(pattern="patient")

setwd("C:/TCGA/Arquivos")

lt <- list()

nm_col <- "bcr_patient_barcode"

ndata ["bcr_patient_barcode"] <- factor()
rm (bcr_patient_uuid)
tmp <- print(lt[65])
dataff <- data.frame()
auxdata <- data.frame()

cont <- try_col(files) 
#primeira vez
dataff <- new_col(files, cont)
#outras vezes
auxdata <- new_col(files, cont)

dataff <- add_col(ndataff, dataff)

nome_col<- list()


procura ('./', 'follow_up', nome_col)


a <- procura(f, nome_col)

getwd()


procura <- function (f, nome_col){
  files <- list.files(pattern="patient")
  tmp <- read.csv(f)
  idx <- grep(nome_col, colnames(tmp), ignore.case = TRUE)
  return (idx)
}



#criação de uma tabela com o nome das colunas que faltam em cada tabela
c_tabela <- function (files, v_nmcol){
  a<-0
  nm_n <- data.frame(nm_col = factor(), nm_tab = factor())
  for (col in v_nmcol){
    for (f in files){
      tmp <- read.delim(f)
      x <- grep(col, colnames(tmp), ignore.case = TRUE)
      if (any(x)){
        a<-a+1
      }else{
        n <- data.frame(nm_col = col, nm_tab=f)
        nm_n <- rbind (nm_n, n)
      }
    }
  }
  print(a)
  return (nm_n)
}

col_n <- c_tabela(files, v_nmcol)


