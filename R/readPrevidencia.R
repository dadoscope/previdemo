library(tidyverse)
library(brazilmaps)

setwd("/home/charles/GitRepos/dadoscope/previdemo/data/eleicoes")
csvfiles <- c(
"votacao_candidato_munzona_2018_AC.csv",
"votacao_candidato_munzona_2018_AL.csv",
"votacao_candidato_munzona_2018_AM.csv",
"votacao_candidato_munzona_2018_AP.csv",
"votacao_candidato_munzona_2018_BA.csv",
"votacao_candidato_munzona_2018_CE.csv",
"votacao_candidato_munzona_2018_DF.csv",
"votacao_candidato_munzona_2018_ES.csv",
"votacao_candidato_munzona_2018_GO.csv",
"votacao_candidato_munzona_2018_MA.csv",
"votacao_candidato_munzona_2018_MG.csv",
"votacao_candidato_munzona_2018_MS.csv",
"votacao_candidato_munzona_2018_MT.csv",
"votacao_candidato_munzona_2018_PA.csv",
"votacao_candidato_munzona_2018_PB.csv",
"votacao_candidato_munzona_2018_PE.csv",
"votacao_candidato_munzona_2018_PI.csv",
"votacao_candidato_munzona_2018_PR.csv",
"votacao_candidato_munzona_2018_RJ.csv",
"votacao_candidato_munzona_2018_RN.csv",
"votacao_candidato_munzona_2018_RO.csv",
"votacao_candidato_munzona_2018_RR.csv",
"votacao_candidato_munzona_2018_RS.csv",
"votacao_candidato_munzona_2018_SC.csv",
"votacao_candidato_munzona_2018_SE.csv",
"votacao_candidato_munzona_2018_SP.csv",
"votacao_candidato_munzona_2018_TO.csv")
zipfile <- "votacao_candidato_munzona_2018.zip"
todososestados <- data.frame()
for(csvfile in csvfiles){
  cat(csvfile,sep = "\n")
  data <- read.csv2(unz(zipfile, csvfile), sep=";",check.names = F, encoding = 'utf8', stringsAsFactors = TRUE)
  todososestados <- rbind(todososestados,data)
}

names(todososestados) <- c("DT_GERACAO",
                           "HH_GERACAO",
                           "ANO_ELEICAO",
                           "CD_TIPO_ELEICAO",
                           "NM_TIPO_ELEICAO",
                           "NR_TURNO",
                           "CD_ELEICAO",
                           "DS_ELEICAO",
                           "DT_ELEICAO",
                           "TP_ABRANGENCIA",
                           "SG_UF",
                           "SG_UE",
                           "NM_UE",
                           "CD_MUNICIPIO",
                           "NM_MUNICIPIO",
                           "NR_ZONA",
                           "CD_CARGO",
                           "DS_CARGO",
                           "SQ_CANDIDATO",
                           "NR_CANDIDATO",
                           "NM_CANDIDATO",
                           "NM_URNA_CANDIDATO",
                           "NM_SOCIAL_CANDIDATO",
                           "CD_SITUACAO_CANDIDATURA",
                           "DS_SITUACAO_CANDIDATURA",
                           "CD_DETALHE_SITUACAO_CAND",
                           "DS_DETALHE_SITUACAO_CAND",
                           "TP_AGREMIACAO",
                           "NR_PARTIDO",
                           "SG_PARTIDO",
                           "NM_PARTIDO",
                           "SQ_COLIGACAO",
                           "NM_COLIGACAO",
                           "DS_COMPOSICAO_COLIGACAO",
                           "CD_SIT_TOT_TURNO",
                           "DS_SIT_TOT_TURNO",
                           "ST_VOTO_EM_TRANSITO",
                           "QT_VOTOS_NOMINAIS")

todososestados <- todososestados %>% filter(DS_CARGO=="Deputado Federal")
todososestados <- todososestados %>% select(NM_URNA_CANDIDATO, SG_UE, QT_VOTOS_NOMINAIS)
todososestados <- todososestados %>% mutate(SG_UE = as.character(SG_UE))
votosprevidencia <- read.csv("../previdencia.csv")

mergeddata <- todososestados %>% left_join(votosprevidencia, by=c("SG_UE"="UF"))
