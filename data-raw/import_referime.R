library(tidyverse)
library (RCurl)
library(jsonlite)

get_listes<-function(noms_listes,var = "diags"){
  df <- NULL
  for(nom in noms_listes){
    tmp<- fromJSON( getURL(paste0("http://referime.aphp.fr:8000/v0.2/listes/",nom),.encoding =  "UTF-8" ),
                    flatten=TRUE)

    df <- c(df,tmp[[var]])

  }

  return(df)
}

get_liste<-function(nom_liste){
  return(
    as.tibble(fromJSON( getURL(paste0("http://referime.aphp.fr:8000/v0.2/listes/",nom_liste) ),
            flatten=TRUE) )
    )
}
path=paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/')

library (RCurl)
download <- getURL("http://referime.aphp.fr:8000/v0.1/csv/ref?table=dictionnaire_tables")

dict_referentiels <- dplyr::as_tibble(read.csv (text = download))
dict_referentiels%>%writexl::write_xlsx(paste0(path,'referime_referentiels.xls'))

for(l in unique(dict_referentiels$nom_table)){
  download <- getURL(paste0("http://referime.aphp.fr:8000/v0.1/csv/ref?table=",l))

  as.tibble(read.csv (text = download))%>%
    writexl::write_xlsx(paste0(path,'REFERIME_REFERENTIELS/',l,'.xls'))
}

dict_listes <- get_liste("dictionnaire")
dict_listes%>%writexl::write_xlsx(paste0(path,'referime_listes.xlsx'))
json_files = c('cathe_cardiaque_interv_pediatrique_cardiopathie_congenitales',
               "chir_ambu_ghm_C_7_racines",
               "chir_oesophage_hors_cancer",
               "chir_ped_complex_cardiopath_congenit",
               "chir_sarcome",
               "m4_inca_gyneco",
               "m4_inca_digestif",
               "m4_inca_orl_mf",
               "m4_inca_sein",
               "m4_inca_thorax",
               "m4_inca_urologie",
               "nut_parenterale_adulte",
               "nut_parenterale_enfant",
               "reconstruction_uro_genetale",
               "sommeil")
for(l in unique(dict_listes$nom_abrege)){
  if(!l%in%json_files){

    df= get_liste(l)
    df%>%writexl::write_xlsx(paste0(path,'REFERIME_REFERENTIELS/',l,'.xlsx'))

  }


}


