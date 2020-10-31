lt <- list()
for (f in files) {
  tmp <- read.delim(f)
  lt[[paste0(f)]] <- tmp
}

tmp <- lt[4]$nationwidechildrens.org_clinical_patient_cesc.txt
setwd('C:/TCGA/Arquivos')

v_nmcol <- colnames(tmp)
v_nmcol <- v_nmcol[-47] #histologic_diagnosis

#lymphovascular_invasion e lymphovascular_invasion_indicator lymphovascular_invasion_present
nm_col_dif <- nm_col_dif[-534]
nm_col_dif <- nm_col_dif[-176]
nm_col_dif <- nm_col_dif[-66]
#tumor_basal_diameter e tumor_basal_diameter_mx
nm_col_dif <- nm_col_dif[-587]
nm_col_dif <- nm_col_dif[-586]
#tumor_thickness e tumor_thickness_measurement
nm_col_dif <- nm_col_dif[-587]
nm_col_dif <- nm_col_dif[-586]
#mitotic_rate e primary_melanoma_mitotic_rate
nm_col_dif <- nm_col_dif[-477]
nm_col_dif <- nm_col_dif[-34]
#necrosis, tumor_total_necrosis e necrosis_percent
nm_col_dif <- nm_col_dif[-449]
nm_col_dif <- nm_col_dif[-448]
nm_col_dif <- nm_col_dif[-34]
#tumor_tissue_site e tumor_tissue_site_other
nm_col_dif <- nm_col_dif[-482]
nm_col_dif <- nm_col_dif[-46]
#Perguntar para o prof se histologic_diagnosis, histologic_diagnosis_others, histologic_diagnosis.1, histologic_diagnosis_percent  é a mesma coisa
v_nmcol <- v_nmcol[-80]
nm_col_dif <- nm_col_dif[-506]
nm_col_dif <- nm_col_dif[-370]
nm_col_dif <- nm_col_dif[-72]
nm_col_dif <- nm_col_dif[-10]
#anatomic_neoplasm_subdivision e anatomic_neoplasm_subdivision_other
nm_col_dif <- nm_col_dif[-374]
nm_col_dif <- nm_col_dif[-68]
#surgical_procedure_other e first_surgical_procedure_other
nm_col_dif <- nm_col_dif[-137]
nm_col_dif <- nm_col_dif[-71]
#vascular_invasion e vascular_invasion_indicator
nm_col_dif <- nm_col_dif[-166]
nm_col_dif <- nm_col_dif[-136]
#platelet_count_preresection e platelet_count
nm_col_dif <- nm_col_dif[-268]
nm_col_dif <- nm_col_dif[-144]
#egfr_mutation_identified e egfr_mutation_identified_type
nm_col_dif <- nm_col_dif[-361]
nm_col_dif <- nm_col_dif[-357]
#surgical_procedure definitive_surgical_procedure surgical_procedure_first
nm_col_dif <- nm_col_dif[-394]
nm_col_dif <- nm_col_dif[-135]
nm_col_dif <- nm_col_dif[-70]
#metastatic_site, metastatic_site_other, metastatic_site_at_diagnosis, metastatic_site_at_diagnosis_other, other_metastic_site
nm_col_dif <- nm_col_dif[-534]
nm_col_dif <- nm_col_dif[-533]
nm_col_dif <- nm_col_dif[-360]
nm_col_dif <- nm_col_dif[-359]
nm_col_dif <- nm_col_dif[-65]
#family_history_colorectal_cancer e history_colorectal_cancer
nm_col_dif <- nm_col_dif[-537]
nm_col_dif <- nm_col_dif[-174]


a <- grep('history_colorectal_cancer', nm_col_dif, fixed = TRUE, useBytes = TRUE)

nm_col_dif[174]




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

aux <- data.frame(final_data$bcr_patient_uuid)


cols <- adc_linhas(a, nm_col_dif, files)


newcols$final_data.bcr_patient_uuid <- NULL
rm (dataf)

newcols <-adc_linhas(aux, nm_col_dif, files)
final_data <- dataf
dataf <- cbind (final_data, newcols)
 #funçao para testar as colunas de outras tabelas
others_cols <- function(final_data, files){
    nm_colunas <- c()
    for (f in files){
      tmp <- read.delim(f)
      col_names <- colnames(tmp)
      for (i in col_names){
        if (any(grep(i, colnames(final_data)))){
          print("1")
        }else{
          nm_colunas <- c(nm_colunas, i)
        }
      }
    }
  return (nm_colunas)
}

nm_col_di <- unique(others_cols(final_data, files))



