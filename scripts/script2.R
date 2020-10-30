lt <- list()
for (f in files) {
  tmp <- read.delim(f)
  lt[[paste0(f)]] <- tmp
}

tmp <- lt[4]$nationwidechildrens.org_clinical_patient_cesc.txt
setwd('C:/TCGA/Arquivos')

v_nmcol <- colnames(tmp)


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


library (dplyr)


#adicionar tabelas com as mesmas colunas

n <- ('birth_days_to')

#data(dataframe vazio com o nome da coluna), n (nome da coluna)  
adc_linhas <- function(n, files){
  data <-  data.frame()
  for (f in files){
    tmp <- read.delim(f)
    if (any(grep(n, colnames(tmp)))){
        data <- rbind(data, select(tmp, n))
    }else{
        aux <- data.frame(rep('[Not Available]', nrow (tmp)))
        names(aux) <- names(data)
        data <- rbind (data, aux)
    }
  }
  return(data)
}
#problema com names,criar vetor com o nome das colunas e depois realizar um for, mas primeiro names
adc_coluna <- function(final_data, prosp){
  return (cbind(final_data, prosp))
}

bdt <- adc_linhas(n, files)

final_data <- adc_coluna(final_data, bdt)

tmp <- lt[33]$nationwidechildrens.org_clinical_patient_uvm.txt