#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
# Réfentiels
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/")
#Ensemble des listes disponibles sur referime
#listes <- dplyr::as_tibble(fromJSON( getURL("http://referime.aphp.fr:8000/v0.2/listes/dictionnaire",.encoding =  "UTF-8" ),
#                                     flatten=TRUE))

##Listes locales
#########################################################################################
# Listes Specifiques actes CCAM
#	Liste de regroupement de codes CIM
#
##########################################################################################

folder<-paste0(path,"CIM/")
files<-list.files(folder)

for (file in files){
  nom_liste<-substr(file,1,(nchar(file)-4))
  tmp<-read.table(file=paste(folder,file,sep=''),sep='\t',header=T,
                  quote="\"", dec=".", na.strings = "NULL",fill = TRUE,stringsAsFactors = F)
  names(tmp)[1]<-c('CODE')
  names(tmp)<-tolower(names(tmp))

  assign( tolower(nom_liste) , tmp )

  do.call("use_data", list( as.name( tolower(nom_liste) ), internal = FALSE, overwrite = TRUE))

}



#########################################################################################
# Listes Specifiques actes CCAM
#	Liste de regroupement de codes CCAM
#
##########################################################################################


folder<-paste0(path,"CCAM/")
files<-list.files(folder)

for (file in files){

  nom_liste<-substr(file,1,(nchar(file)-4))

  tmp<-read.table(file=paste(folder,file,sep=''),sep='\t',header=T,
                  quote="\"", dec=".", na.strings = "NULL",fill = TRUE,stringsAsFactors = F)
  names(tmp)[1]<-c('ACTE')
  names(tmp)<-tolower(names(tmp))

  assign( tolower(nom_liste) , tmp )

  do.call("use_data", list( as.name( tolower(nom_liste) ), internal = FALSE, overwrite = TRUE))

}

#########################################################################################
# Listes Specifiques actes CCAM
#  Liste de regroupement de codes CCAM
#
##########################################################################################

folder<-paste0(path,"AssociationsDiagActes/")
files<-list.files(folder)

for (file in files){

  nom_liste<-substr(file,1,(nchar(file)-4))

  tmp<-read.table(file=paste(folder,file,sep=''),sep='\t',header=T,
                                             quote="\"", dec=".", na.strings = "NULL",fill = TRUE,fileEncoding = "latin1",stringsAsFactors = F)

  assign( tolower(nom_liste) , tmp )

  do.call("use_data", list( as.name( tolower(nom_liste) ), internal = FALSE, overwrite = TRUE))

}


#########################################################################################
# Listes GHM
#
#
##########################################################################################

folder<-paste0(path,"GHM/")

files<-list.files(folder)

for (file in files){

  nom_liste<-substr(file,1,(nchar(file)-4))

  tmp<-read.table(file=paste(folder,file,sep=''),sep='\t',header=T,
                  quote="\"", dec=".", na.strings = "NULL",fill = TRUE,fileEncoding = "latin1",stringsAsFactors = F)

  names(tmp)<-tolower(names(tmp))

  assign( tolower(nom_liste) , tmp )

  do.call("use_data", list( as.name( tolower(nom_liste) ), internal = FALSE, overwrite = TRUE))

}


#########################################################################################
# Listes Ipop
#
##########################################################################################

folder<-paste0(path,"IPOP/",sep="")
files<-list.files(folder)

for (file in files){

  nom_liste<-substr(file,1,(nchar(file)-4))

  tmp<-read.table(file=paste(folder,file,sep=''),sep='\t',header=T,
                  quote="\"", dec=".", na.strings = "NULL",fill = TRUE,fileEncoding = "latin1",stringsAsFactors = F)

  names(tmp)<-tolower(names(tmp))

  assign( tolower(nom_liste) , tmp )

  do.call("use_data", list( as.name( tolower(nom_liste) ), internal = FALSE, overwrite = TRUE))

}



#########################################################################################
# Listes MO
#
##########################################################################################

folder<-paste(path,"MO/",sep="")
files<-list.files(folder)

for (file in files){

  nom_liste<-substr(file,1,(nchar(file)-4))

  tmp<-read.table(file=paste(folder,file,sep=''),sep='\t',header=T,
                  quote="\"", dec=".", na.strings = "NULL",fill = TRUE,fileEncoding = "latin1",stringsAsFactors = F)

  names(tmp)<-tolower(names(tmp))

  assign( tolower(nom_liste) , tmp )

  do.call("use_data", list( as.name( tolower(nom_liste) ), internal = FALSE, overwrite = TRUE))

}

#########################################################################################
#
#       Listes CMA: COMPLICATIONS ET MORBIDITÉS ASSOCIÉES
#
#
##########################################################################################

#
# Annexe 4
# Liste des diagnostics classés CMA avec :
#   leur niveau de sévérité et les numéros de listes correspondants aux  exclusions de la CMA avec le DP et/ou avec les racines de GHM
#
#
# Le contenu des listes d’exclusions figure dans l’annexe 5 de ce volume.
#
# Signification des colonnes :
# - niv = niveau de sévérité (2 à 4)
# - Liste diag = numéro de la liste d’exclusions entre diagnostic principal (DP) et CMA. Il s’agit d’une liste de DP
# - Liste racine = numéro de la liste d’exclusions entre racines de GHM et CMA. Il s’agit d’une liste de racines.
#


folder<-paste0(path,"CMA/",sep="")
files<-list.files(folder)

for (file in files){

  nom_liste<-substr(file,1,(nchar(file)-4))

  tmp<-read.table(file=paste(folder,file,sep=''),sep='\t',header=T,
                  quote="\"", dec=".", na.strings = "NULL",fill = TRUE,fileEncoding = "latin1",stringsAsFactors = F)

  names(tmp)<-tolower(names(tmp))

  assign( tolower(nom_liste) , tmp )

  do.call("use_data", list( as.name( tolower(nom_liste) ), internal = FALSE, overwrite = TRUE))

}

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
###Table GhmGhs
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------



##############################################
#Import des tables
##############################################
# Table de reference des versions de GHM et Tarifs
# Fichier source : /PARAMETRES/version.ghm.txt
#################################################
library(data.table)
VersionsGHM<-read.table(paste(path,'TARIFS/VersionsTarifs.txt',sep=''),header=T, quote="\"", dec=".", na.strings = "NULL",fill = TRUE)
VersionsGHM$v.deb<-as.Date(VersionsGHM$v.deb,"%d/%m/%Y")
VersionsGHM$v.fin<-as.Date(VersionsGHM$v.fin,"%d/%m/%Y")
VersionsGHM$t.deb<-as.Date(VersionsGHM$t.deb,"%d/%m/%Y")
VersionsGHM$t.fin<-as.Date(VersionsGHM$t.fin,"%d/%m/%Y")

NamesTabGhm<-c('GHS','GHM','LIBELLE','BB','BH','TARIF','FORF_EXB','TARIF_EXB','TARIF_EXH','V.Tarifs')
GhmGhs<-NULL
#if(exists('GhmGhs')) rm(GhmGhs)
for (i in VersionsGHM$V.Tarifs){
  PathGHM=paste(path,'TARIFS/',format(VersionsGHM$t.deb[VersionsGHM$V.Tarifs==i], "%Y%m%d"),'.',
                format(VersionsGHM$t.fin[VersionsGHM$V.Tarifs==i], "%Y%m%d"),sep='')
  print(PathGHM)
  if(file.exists(PathGHM)){
    GHMtmp<-read.table(PathGHM,header = TRUE, sep = "\t", quote="\"", na.strings = "NULL",fill = TRUE,stringsAsFactors=F, dec = ".")
    print(paste(i,nrow(GHMtmp)))
    LegGHMtmp<-read.table(paste(PathGHM,'.Leg',sep=""),header = TRUE, sep = "\t", quote="\"", dec=".", na.strings = "NULL",fill = TRUE,stringsAsFactors=F)
    names(GHMtmp)<-c(LegGHMtmp$Leg)
    GHMtmp[,'V.Tarifs']<-i
    GHMtmp[,NamesTabGhm[!NamesTabGhm%in%names(GHMtmp)]]<-NA


    GHMtmp<-GHMtmp[,c("GHS","GHM","LIBELLE","BB","BH","TARIF","FORF_EXB","TARIF_EXB","TARIF_EXH","V.Tarifs")]

    GHMnonValorises<-read.table(paste(path,'GHM/GhmNonValorises.txt',sep=""),
                                header = TRUE, sep = "\t", dec=".", na.strings = "NULL",fill = TRUE,stringsAsFactors=F)
    names(GHMnonValorises)[1]<-'GHS'

    GHMnonValorises<-GHMnonValorises[,c("GHS","GHM","LIBELLE","BB","BH","TARIF","FORF_EXB","TARIF_EXB","TARIF_EXH","V.Tarifs")]
    GHMnonValorises$V.Tarifs<-VersionsGHM$V.Tarifs[i]
    GHMnonValorises$TARIF<-0
    GHMtmp<-rbind(GHMtmp,GHMnonValorises)
    GHMtmp<-merge(GHMtmp,VersionsGHM[VersionsGHM$V.Tarifs==i,],by='V.Tarifs',all.x=T)
    GHMtmp$Racine<-substr(GHMtmp$GHM,1,5)

    GhmGhs<-rbind(GhmGhs,GHMtmp)

  }
}
GhmGhs$TARIF<-as.numeric(GhmGhs$TARIF)


GhmGhs$BB[ which(substr(GhmGhs$GHM,6,6)==2 & GhmGhs$BB == 0) ] = 2
GhmGhs$BB[ which(substr(GhmGhs$GHM,6,6)==3 & GhmGhs$BB == 0) ] = 3
GhmGhs$BB[ which(substr(GhmGhs$GHM,6,6)==4 & GhmGhs$BB == 0) ] = 4


#########################################################################################
# Table des supplements  : Supp
#  Valeur des supplements
#	1 seule fichier pour l'ensemble des annee
#	Format :
#			Fichier TXT seperateur TAB
#			Variable : SUP	LIB	VAL.DEB	VAL.FIN	TARIF
#
##########################################################################################
#Supp<-read.table(paste(PathParametres,'Tarifs/SupplementsTarifs.txt',sep=""), header = TRUE, sep = "\t", quote="\"", dec=".", na.strings = "NULL",fill = TRUE)
#Supp$val.deb<-as.Date(Supp$val.deb,"%d/%m/%Y")
#Supp$val.fin<-as.Date(Supp$val.fin,"%d/%m/%Y")



#########################################################################################
#
#Regroupements
#
#########################################################################################

Folder<-paste(path,"REGROUPEMENTS/",sep="")
FileFolder<-list.files(Folder)

Regroupement<-NULL
for (File in FileFolder){

  tmp<-read.table(paste(Folder,File,sep=""),
                  header = TRUE, sep = "\t", quote="\"", dec=".", na.strings = "NULL",fill = TRUE,
                  stringsAsFactors = F)
  tmp$version<-File
  Regroupement<-rbind(Regroupement,tmp)
}

GhmGhs<-merge(GhmGhs,Regroupement[c('GHM','version','DA','GP','GA')],by=c('GHM','version'),all.x=T)

##Ajout information sur les GHM des mêmes racines
o<-which(substr(GhmGhs$GHM,1,5)%in%substr(GhmGhs$GHM[which(substr(GhmGhs$GHM,6,6)=='1')],1,5))
o<-intersect(grep('11',GhmGhs$version),o)
Racines<-GhmGhs[o,]
Racines<-Racines[!duplicated(Racines[,c('V.Tarifs','Racine')]),c('V.Tarifs','Racine')]
for(i in 1:4){
  a<-GhmGhs[which(substr(GhmGhs$GHM,6,6)==i),c('V.Tarifs','Racine',"BB","BH")]
  a<-a[!duplicated(a[,c('V.Tarifs','Racine')]),]
  names(a)<-c('V.Tarifs','Racine',paste(c("BB_NS","BH_NS"),i,sep=''))
  Racines<-merge(Racines,a,
                 by=c('V.Tarifs','Racine')
  )
}
GhmGhs<-merge(GhmGhs,Racines,by=c('V.Tarifs','Racine'),all.x=T)


referentiel_ghm_tarifs<-dplyr::as_tibble(GhmGhs)
names(referentiel_ghm_tarifs)<-tolower(names(referentiel_ghm_tarifs))
referentiel_ghm_tarifs<-referentiel_ghm_tarifs%>%dplyr::distinct(ghm,ghs,anseqta,.keep_all = T)
use_data(referentiel_ghm_tarifs,internal = FALSE, overwrite = TRUE)


#Actes de radiothérapie donnant lieu à facturation d'un supplément
actesradio <- "AZNL001|QZNL001|ZZNL045|ZZNL046|ZZNL047|ZZNL048|ZZNL050|ZZNL051|ZZNL052|ZZNL053|ZZNL054|ZZNL058|ZZNL059|ZZNL060|ZZNL061|ZZNL062|ZZNL063|ZZNL064|ZZNL065|ZZNL066"
devtools::use_data(actesradio,internal = FALSE, overwrite = TRUE)
#Actes de dialyse donnant lieu à facturation d'un supplément
actesdialyse <- "JVJF003|JVJF004|JVJF008|JVRP004|JVRP007|JVRP008"
devtools::use_data(actesdialyse,internal = FALSE, overwrite = TRUE)
#Actes pose défibrilateur cardiaque
actessdc <- "DEKA002|DELA004|DELA007|DELF013|DELF014|DELF016|DELF020|DELF900"
devtools::use_data(actessdc,internal = FALSE, overwrite = TRUE)
#Actes d'aphérère donnant lieu à facturation d'un supplément
actesapherese <- "FEFF001|FEFF002|FEJF001|FEJF002|FEJF004|FEJF005|FEJF007|FEJF009|FEPF001|FEPF002|FEPF003|FEPF004|FEPF005|FERP001"
use_data(actesapherese,internal = FALSE, overwrite = TRUE)




AM<-c("FAFA001","FAFA002","FAFA008","FAFA013","NFPC002","NFFC002","NFEC001","NFEC002","NFFC003","NFFC004","NFJC001",
      "NFJC002","NFPC001","NFQC001","EGFA005","EGJA001","HJAD001","HKFA001","HKFA008","HKFA009","HKPA003","HKPA006",
      "HKFA002","HKFA004","QBFA004","AHPA009","AHPA021","AHPA022","AHPA023","AHPA028","AHPC001","BCFA003","BCFA004",
      "BCFA005","BCFA006","BCFA008","BCFA009","MJFA006","MJFA010","MJPA005","MJPB001","EGFA008","EGFA010","EGFC001",
      "EGSA001","EGSA002","EGSF001","EGSF002","EGSF003","JHBA001","JHDA001","JHEA002","JHFA001","JHFA004","JHFA014",
      "JHSA001","LMMA008","LMMA012","LMMA016","LMMA017","LMMC002","LMMC003","EJFA002","EJFA004","EJFA006","EJFA007",
      "EJFB001","EJGA001","EJGA002","EJGA003","EJSA001","BFEA001","BFGA001","BFGA002","BFGA003","BFGA004","BFGA005",
      "BFGA006","BFGA007","BFGA008","BFGA009","BFGA010","BFKA001","BFLA001","BFLA002","BFLA003","BFLA004","QEFA004",
      "MHCA002","MHCA003","MJPA013","PCPA006","BJDA001","BJDA002","BJEA001","BJMA001","BJMA002","BJMA003","BJMA004",
      "BJMA005","BJMA006","BJMA007","BJMA008","BJMA009","QZFA036","GAFA007","CAFA005","PDFA001","MJFA008","MGFA004",
      "MGFA007","MHFA002","HBED022","HBGD003","HBGD004","HBGD007","HBGD010","HBGD016","HBGD017","HBGD018","HBGD021",
      "HBGD025","HBGD026","HBGD029","HBGD038","HBGD042","HBGD045","HBPA001","HBPD002","MGCC001","MJFA004","MJPA011",
      "NDGA003","NJPA007","NDFA002","NGFA002","QAMA002","LAGA003","LAGA005","LBFA023","LBFA030","LBFA031","EBLA003",
      "JANM001","JANM002","JCNM001","JCNM002","LMMA011","LMMA014","LMMA018","LMMC004","JCLE004","JDNE001","JCAE001",
      "JCGE001","JCKE002","EPLA002","EZFA002","EZMA001","JHEA003","HMFC004","GAMA007","LAEA007","LAEP002","LMMA006",
      "LMMA009","LMMC020","MHFA001","MDFA002","MHDB001","MHPA004","MJFA012","MJFA015","MJPA002","MJPA009","MDHA001",
      "NDPA004","NDPA009","NHMA002","NGJC001","JJFJ001","JJPA004","JJPC003","JJPE001","EZAF002","EFAF002","EZAF001",
      "EZJF002","EZPF004","JEFA003","JEMA013","JEMA017","MEMA006","MEMC001","MEMC002","MEMC003","MEMC004","JKFA031",
      "JKND002","JKFE001","JKFE002","JKGD002","JKGD003","JKND001","JKNE001","JKPE001","JKQE001","JNBD001","JNJD002",
      "JNMD001","ELSA001","ELSC001","JHEA001","JHFA013","JHFA019","GBPE001","GBPE003","GBPA004","GBGD001","BGFA014",
      "BGFA005","CBMA008","CBMA009","HAFA021","HAFA034","HASA018","HASA025","HASA013","QZMA006","QZPA008","HCFA007",
      "HCPA001","HCGA001","HAMA027","HAMA028","QZMA001","QZMA004","BGPA002","BGFA001")
use_data(AM,internal = FALSE, overwrite = TRUE)

##Typologie des structures
nomenclature_uma<-readxl::read_excel(paste(path,"nomenclature_uma.xlsx",sep=''))
names(nomenclature_uma) <- c('typeaut','libelle_typeaut','mode_hospitalisation',
                               'discipline','historique')
use_data(nomenclature_uma,internal = FALSE, overwrite = TRUE)
