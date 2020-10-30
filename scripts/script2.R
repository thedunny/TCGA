
procura ('./', nome_col)


a <- procura(f, nome_col)

getwd()
setwd('C:/TCGA/Arquivos')

procura <- function (f, nome_col){
  files <- list.files(pattern="patient")
  for f in files{
  tmp <- read.csv(f)
  idx <- grep(nome_col, colnames(tmp), ignore.case = TRUE)
  if (idx){
    
  }
  }
  return (idx)
}

lt <- list()
for (f in files) {
  tmp <- read.delim(f)
  
}

tmp <- lt[4]$nationwidechildrens.org_clinical_patient_cesc.txt
setwd('C:/TCGA/Arquivos')

nome_col <- 'tumor_tissue_site'
files <- list.files(pattern="patient")
tmp <- lt[3]$nationwidechildrens.org_clinical_patient_brca.txt
a<- procura (tmp, nome_col)

procura <- function (f, nome_col){
  idx <- ?grep(nome_col, colnames(f), ignore.case = TRUE)
  return (idx)
}

v_nmcol <- colnames(tmp)

rm(nm_n)
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

rm(tmp)

library (dplyr)

x <- col_n %>% group_by(nm_col)%>% summarise(r = n()) %>% arrange(r)
?count(col_n, nm_col)



#adicionar tabelas com as mesmas colunas

n <- ('bcr_patient_uuid')

#data(dataframe vazio com o nome da coluna), n (nome da coluna)  
adc_linhas <- function(n, files){
  data <-  c()
    for (f in files){
      tmp <- read.delim(f)
      idx <- grep(n, colnames(tmp))
      data <- c(data, tmp[, idx])
  }
  return(data)
}

data1 <- data.frame(bcr_patient_uuid <- factor())

data1 <- adc_linhas (n, files)

class ()






