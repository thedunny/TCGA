lt <- list()
for (f in files) {
  tmp <- read.delim(f)
  lt[[paste0(f)]] <- tmp
}

tmp <- lt[4]$nationwidechildrens.org_clinical_patient_cesc.txt
setwd('C:/TCGA/Arquivos')

v_nmcol <- colnames(tmp)
v_nmcol <- v_nmcol[-47] #histologic_diagnosis
#Perguntar para o prof se histologic_diagnosis e histologic_diagnosis_others é a mesma coisa
v_nmcol <- v_nmcol[-80]
#lymphovascular_invasion e lymphovascular_invasion_indicator
v_nmcol['lymphovascular_invasion']
v_nmcol <- v_nmcol[-1]

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
i<- 'bcr_patient_uuid'
#data(dataframe vazio com o nome da coluna), n (nome da coluna)  
adc_linhas <- function(data_func, nms, files){
  for (n in nms){
    data <-  data.frame()
    data [n] <- factor()
    print(n)
    for (f in files){
      tmp <- read.delim(f)
      if (any(grep (n, colnames(tmp), fixed = TRUE))){
          data <- rbind(data, select(tmp, n))
      }else{
          aux <- data.frame(rep('[Not Available]', nrow (tmp)))
          names(aux) <- names(data)
          data <- rbind (data, aux)
      }
    }
    data_func <- cbind(data_func, data)
  }
  return(data_func)
}



adc_coluna <- function(final_data, prosp){
  return (cbind(final_data, prosp))
}

data <- data.frame(br <- factor())
data <- data.frame()

cols <- adc_linhas(final_data, v_nmcol, files)

final_data <- adc_coluna(final_data, bdt)

tmp <- lt[4]$nationwidechildrens.org_clinical_patient_cesc.txt

tmp[32]

final_data <- cols

a <- grep('lymphovascular_invasion', colnames(tmp))