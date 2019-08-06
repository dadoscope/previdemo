fixDataFrameEleicoes <- function(eleicoes2018){
  eleicoes2018 <- eleicoes2018 %>% ungroup() %>% mutate(NOME_URNA_CANDIDATO = stringr::str_trim(NOME_URNA_CANDIDATO))
  eleicoes2018 <- eleicoes2018 %>% ungroup() %>% mutate(NOME_URNA_CANDIDATO = stringr::str_squish(NOME_URNA_CANDIDATO))
  eleicoes2018 <- eleicoes2018 %>% ungroup() %>% mutate(NOME_CANDIDATO = stringr::str_trim(NOME_CANDIDATO))
  eleicoes2018 <- eleicoes2018 %>% ungroup() %>% mutate(NOME_CANDIDATO = stringr::str_squish(NOME_CANDIDATO))
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "ALCIDES RODRIGUES" ~ "PATRI",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "DRA. MARINA" ~ "SOLIDARIEDADE",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "WLADIMIR GAROTINHO" ~ "PSD",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "ZÉ VITOR" ~ "PL",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "ULDÚRICO JÚNIOR" ~ "PROS",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "PASTOR GILDENEMYR" ~ "PL",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "RAIMUNDO COSTA" ~ "PL",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "MARCELO ARO" ~ "PP",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "BENES LEOCADIO" ~ "PRB",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "FERNANDO RODOLFO" ~ "PL",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "JÚNIOR MANO" ~ "PL",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "FRANCO CARTAFINA" ~ "PP",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "GELSON AZEVEDO" ~ "PL",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "BIA KICIS" ~ "PSL",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(NOME_URNA_CANDIDATO == "BOSCO COSTA" ~ "PL",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(SIGLA_PARTIDO == "PPS" ~ "CIDADANIA",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(SIGLA_PARTIDO = 
             case_when(SIGLA_PARTIDO == "PR" ~ "PL",
                       TRUE ~ as.character(SIGLA_PARTIDO)))	
  
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(NOME_URNA_CANDIDATO = 
             case_when(NOME_URNA_CANDIDATO == "ALEXIS" ~ "ALEXIS FONTEYNE",
                       TRUE ~ as.character(NOME_URNA_CANDIDATO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(NOME_URNA_CANDIDATO = 
             case_when(NOME_URNA_CANDIDATO == "CLARISSA GARONTINHO" ~ "CLARISSA GAROTINHO",
                       TRUE ~ as.character(NOME_URNA_CANDIDATO)))	
  eleicoes2018 <- eleicoes2018 %>% 
    mutate(NOME_URNA_CANDIDATO = 
             case_when(NOME_URNA_CANDIDATO == "DR.JAZIEL" ~ "DR. JAZIEL",
                       TRUE ~ as.character(NOME_URNA_CANDIDATO)))	
  
  return(eleicoes2018)
}

library(tidyverse)
library(brazilmaps)
library(readxl)

setwd("/home/charles/GitRepos/dadoscope/previdemo/data/eleicoes/")
muni <- readRDS("eleicoes2018_municipios.rds") 
votos_estado <- readRDS("dataframe_votos_deputados_reforma_eleicaonosestados.rds")
votosprevidencia <- read.csv("../previdencia.csv")
votosprevidencia <- votosprevidencia %>% mutate(Deputado = toupper(Deputado))

muni_deputado <- muni %>% filter(DESCRICAO_CARGO == "Deputado Federal")
muni_deputado <- fixDataFrameEleicoes(muni_deputado)
votos_deputados_estados_parte1 <- muni_deputado %>% left_join(votosprevidencia, by = c("NOME_URNA_CANDIDATO" = "Deputado","SIGLA_PARTIDO" = "Partido","SIGLA_UF" = "UF" )) %>% filter(!is.na(Voto)) %>% mutate(Deputado = NOME_URNA_CANDIDATO)
votos_deputados_estados_parte2 <- muni_deputado %>% left_join(votosprevidencia, by = c("NOME_CANDIDATO" = "Deputado","SIGLA_PARTIDO" = "Partido","SIGLA_UF" = "UF" ))  %>% filter(NOME_CANDIDATO != NOME_URNA_CANDIDATO) %>% filter(!is.na(Voto)) %>% mutate(Deputado = NOME_CANDIDATO)
votos_todos_deputados <- rbind(votos_deputados_estados_parte1, votos_deputados_estados_parte2) %>% arrange(Deputado)

ufmap <- get_brmap("State")
munimap <- get_brmap("City")

uf_code <- data.frame(UF = sort(unique(votos_todos_deputados$SIGLA_UF)), 
                      code = c(12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17))

votos_por_cidade <- votos_todos_deputados %>% 
  mutate(NOME_MUNICIPIO = toupper(NOME_MUNICIPIO))%>%
  mutate(NOME_MUNICIPIO = iconv(NOME_MUNICIPIO,from="UTF-8",to="ASCII//TRANSLIT"))%>%
  left_join(uf_code, by = c("SIGLA_UF" = "UF")) %>%
  group_by(NOME_MUNICIPIO, SIGLA_UF, code) %>% 
  dplyr::summarise(votossim = sum(TOTAL_VOTOS[Voto=="Sim"]),
            votosnao = sum(TOTAL_VOTOS[Voto=="Não"]),
            propsim = 100*(votossim/(votossim+votosnao))) %>%
  complete(NOME_MUNICIPIO,fill = list(votossim = 0, votosnao = 0, propsim = 0))


cities_with_na <- munimap %>% 
  mutate(nome = iconv(nome,from="UTF-8",to="ASCII//TRANSLIT")) %>%
  mutate(nome = str_replace_all(string = nome, pattern = "D'", replacement = "D ")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ESPIGAO D OESTE", 
                                replacement = "ESPIGAO DO OESTE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ALVORADA D OESTE", 
                                replacement = "ALVORADA DO OESTE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ELDORADO DO CARAJAS", 
                                replacement = "ELDORADO DOS CARAJAS")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SANT'ANA DO LIVRAMENTO", 
                                replacement = "SANT ANA DO LIVRAMENTO")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SANTA IZABEL DO PARA", 
                                replacement = "SANTA ISABEL DO PARA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "GRACHO CARDOSO", 
                                replacement = "GRACCHO CARDOSO")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "CAMACAN", 
                                replacement = "CAMACA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "QUIJINGUE", 
                                replacement = "QUINJINGUE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "OLHOS-D AGUA", 
                                replacement = "OLHOS D AGUA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SAO LUIZ DO PARAITINGA", 
                                replacement = "SAO LUIS DO PARAITINGA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "GRAO PARA", 
                                replacement = "GRAO-PARA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SEM-PEIXE", 
                                replacement = "SEM PEIXE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "AUGUSTO SEVERO", 
                                replacement = "CAMPO GRANDE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "JANUARIO CICCO", 
                                replacement = "BOA SAUDE")) %>%
  left_join(votos_por_cidade, by = c("nome" = "NOME_MUNICIPIO",
                                     "State" = "code")) %>% 
  filter(is.na(propsim)) %>% 
  select(nome)

names_all_cities <- data.frame(votos_por_cidade$NOME_MUNICIPIO)
#the map should come before the data you want to merge

#To load PNUD Censo 2010 data

pnud_muni <- abjData::pnud_muni
pnud_muni_2010 <- pnud_muni %>% filter(ano == 2010)

#To Plot figures

setwd("/home/charles/GitRepos/dadoscope/previdemo/figures")

muni_map_previ_pnud <- munimap %>%   
  mutate(nome = iconv(nome,from="UTF-8",to="ASCII//TRANSLIT")) %>%
  mutate(nome = str_replace_all(string = nome, pattern = "D'", replacement = "D ")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ESPIGAO D OESTE", 
                                replacement = "ESPIGAO DO OESTE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ALVORADA D OESTE", 
                                replacement = "ALVORADA DO OESTE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ELDORADO DO CARAJAS", 
                                replacement = "ELDORADO DOS CARAJAS")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SANT'ANA DO LIVRAMENTO", 
                                replacement = "SANT ANA DO LIVRAMENTO")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SANTA IZABEL DO PARA", 
                                replacement = "SANTA ISABEL DO PARA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "GRACHO CARDOSO", 
                                replacement = "GRACCHO CARDOSO")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "CAMACAN", 
                                replacement = "CAMACA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "QUIJINGUE", 
                                replacement = "QUINJINGUE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "OLHOS-D AGUA", 
                                replacement = "OLHOS D AGUA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SAO LUIZ DO PARAITINGA", 
                                replacement = "SAO LUIS DO PARAITINGA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "GRAO PARA", 
                                replacement = "GRAO-PARA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "AUGUSTO SEVERO", 
                                replacement = "CAMPO GRANDE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SEM-PEIXE", 
                                replacement = "SEM PEIXE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "JANUARIO CICCO", 
                                replacement = "BOA SAUDE")) %>%
  left_join(votos_por_cidade, 
            by = c("nome" = "NOME_MUNICIPIO",
                   "State" = "code")) %>%
  left_join(pnud_muni_2010, 
            by = c("City" = "codmun7"))


p1 <- muni_map_previ_pnud %>%   
  ggplot() + geom_sf(aes(fill = espvida, col = espvida)) +
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "gray75", size = 0.5) +
  #  geom_sf(data = semiarido, fill = "transparent", colour = "white", size = 0.5) +
  viridis::scale_fill_viridis(option = "magma", direction = 1) +
  viridis::scale_color_viridis(option = "magma", direction = 1, guide=FALSE) +
  theme_bw()    +
  labs(title = "Esperança de vida ao nascer",
       fill = "Anos")
png("proporcao_espvida.png",width=3200,height=1800,res=300)
print(p1)
dev.off()


p1 <- muni_map_previ_pnud %>%   
  ggplot() + geom_sf(aes(fill = razdep, col = razdep)) +
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "gray75", size = 0.5) +
  #  geom_sf(data = semiarido, fill = "transparent", colour = "white", size = 0.5) +
  viridis::scale_fill_viridis(option = "magma", direction = 1) +
  viridis::scale_color_viridis(option = "magma", direction = 1, guide=FALSE) +
  theme_bw()    +
  labs(title = "Razão de dependência",
       fill = "(%)")
png("proporcao_dependencia.png",width=3200,height=1800,res=300)
print(p1)
dev.off()

library(sf)
semiarido <- read_sf("../data/semiarido/LIM_Semiarido_OFICIAL_POLIGONAL.shp")
p1 <- munimap %>%   mutate(nome = iconv(nome,from="UTF-8",to="ASCII//TRANSLIT")) %>%
  mutate(nome = str_replace_all(string = nome, pattern = "D'", replacement = "D ")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ESPIGAO D OESTE", 
                                replacement = "ESPIGAO DO OESTE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ALVORADA D OESTE", 
                                replacement = "ALVORADA DO OESTE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ELDORADO DO CARAJAS", 
                                replacement = "ELDORADO DOS CARAJAS")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SANT'ANA DO LIVRAMENTO", 
                                replacement = "SANT ANA DO LIVRAMENTO")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SANTA IZABEL DO PARA", 
                                replacement = "SANTA ISABEL DO PARA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "GRACHO CARDOSO", 
                                replacement = "GRACCHO CARDOSO")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "CAMACAN", 
                                replacement = "CAMACA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "QUIJINGUE", 
                                replacement = "QUINJINGUE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "OLHOS-D AGUA", 
                                replacement = "OLHOS D AGUA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SAO LUIZ DO PARAITINGA", 
                                replacement = "SAO LUIS DO PARAITINGA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "GRAO PARA", 
                                replacement = "GRAO-PARA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "AUGUSTO SEVERO", 
                                replacement = "CAMPO GRANDE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SEM-PEIXE", 
                                replacement = "SEM PEIXE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "JANUARIO CICCO", 
                                replacement = "BOA SAUDE")) %>%
  left_join(votos_por_cidade, 
            by = c("nome" = "NOME_MUNICIPIO",
                   "State" = "code")) %>%
  ggplot() + geom_sf(aes(fill = propsim, col = propsim)) +
  geom_sf(data = semiarido,fill = "transparent",colour = "white", size = 0.8) +
  viridis::scale_fill_viridis(option = "magma", direction = -1) +
  viridis::scale_color_viridis(option = "magma", direction = -1, guide=FALSE) +
  theme_bw() +
  labs(title = "Votos em candidatos que votaram a favor da Reforma da Previdência nas eleições de 2018",
       fill = "Votos (%)") 
png("proporcao_votos_sim_semiarido.png",width=3200,height=1800,res=300)
print(p1)
dev.off()

p1 <- munimap %>%   mutate(nome = iconv(nome,from="UTF-8",to="ASCII//TRANSLIT")) %>%
  mutate(nome = str_replace_all(string = nome, pattern = "D'", replacement = "D ")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ESPIGAO D OESTE", 
                                replacement = "ESPIGAO DO OESTE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ALVORADA D OESTE", 
                                replacement = "ALVORADA DO OESTE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "ELDORADO DO CARAJAS", 
                                replacement = "ELDORADO DOS CARAJAS")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SANT'ANA DO LIVRAMENTO", 
                                replacement = "SANT ANA DO LIVRAMENTO")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SANTA IZABEL DO PARA", 
                                replacement = "SANTA ISABEL DO PARA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "GRACHO CARDOSO", 
                                replacement = "GRACCHO CARDOSO")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "CAMACAN", 
                                replacement = "CAMACA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "QUIJINGUE", 
                                replacement = "QUINJINGUE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "OLHOS-D AGUA", 
                                replacement = "OLHOS D AGUA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SAO LUIZ DO PARAITINGA", 
                                replacement = "SAO LUIS DO PARAITINGA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "GRAO PARA", 
                                replacement = "GRAO-PARA")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "AUGUSTO SEVERO", 
                                replacement = "CAMPO GRANDE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "SEM-PEIXE", 
                                replacement = "SEM PEIXE")) %>%
  mutate(nome = str_replace_all(string = nome, 
                                pattern = "JANUARIO CICCO", 
                                replacement = "BOA SAUDE")) %>%
  left_join(votos_por_cidade, 
            by = c("nome" = "NOME_MUNICIPIO",
                   "State" = "code")) %>%
  ggplot() + geom_sf(aes(fill = propsim, col = propsim)) +
  viridis::scale_fill_viridis(option = "magma", direction = -1) +
  viridis::scale_color_viridis(option = "magma", direction = -1, guide=FALSE) +
  theme_bw() +
  labs(title = "Votos em candidatos que votaram a favor da Reforma da Previdência nas eleições de 2018",
       fill = "Votos (%)") 
png("proporcao_votos_sim.png",width=3200,height=1800,res=300)
print(p1)
dev.off()


#### comparing with population patterns

p1 <- muni_map_previ_pnud %>%   
  mutate(porchomemidoso = 100*(homem60a64+homem65a69+homem70a74+homem75a79+homens80)/homemtot,
         porcmulhidosa = 100*(mulh60a64+mulh65a69+mulh70a74+mulh75a79+mulher80)/mulhertot) %>%
  ggplot() + geom_sf(aes(fill = porcmulhidosa, col = porcmulhidosa)) +
  #  geom_sf(data = semiarido,fill = "transparent",colour = "white", size = 0.5) +
  viridis::scale_fill_viridis(option = "magma", direction = 1) +
  viridis::scale_color_viridis(option = "magma", direction = 1, guide=FALSE) +
  theme_bw()  +
  labs(title = "Proporção de mulheres acima de 60 anos",
       fill = "(%)")
png("proporcao_mulheridosa_semiarido.png",width=3200,height=1800,res=300)
print(p1)
dev.off()


p1 <- muni_map_previ_pnud %>%   
  mutate(porchomemidoso = 100*(homem60a64+homem65a69+homem70a74+homem75a79+homens80)/homemtot,
         porcmulhidosa = 100*(mulh60a64+mulh65a69+mulh70a74+mulh75a79+mulher80)/mulhertot) %>%
  ggplot() + geom_sf(aes(fill = porchomemidoso, col = porchomemidoso)) +
  #  geom_sf(data = semiarido,fill = "transparent",colour = "white", size = 0.5) +
  viridis::scale_fill_viridis(option = "magma", direction = 1) +
  viridis::scale_color_viridis(option = "magma", direction = 1, guide=FALSE) +
  theme_bw()   +
  labs(title = "Proporção de homens acima de 60 anos",
       fill = "(%)")
png("proporcao_homemidoso_semiarido.png",width=3200,height=1800,res=300)
print(p1)
dev.off()


p1 <- muni_map_previ_pnud %>%   
  mutate(porchomemidoso = 100*(homem60a64+homem65a69+homem70a74+homem75a79+homens80)/homemtot,
         porcmulhidosa = 100*(mulh60a64+mulh65a69+mulh70a74+mulh75a79+mulher80)/mulhertot,
         porcpopidosa = 100*(homem60a64+homem65a69+homem70a74+homem75a79+homens80+mulh60a64+mulh65a69+mulh70a74+mulh75a79+mulher80)/(mulhertot+homemtot)) %>%
  ggplot() + geom_sf(aes(fill = porcpopidosa, col = porcpopidosa)) +
  #  geom_sf(data = semiarido, fill = "transparent", colour = "white", size = 0.5) +
  viridis::scale_fill_viridis(option = "magma", direction = 1) +
  viridis::scale_color_viridis(option = "magma", direction = 1, guide=FALSE) +
  theme_bw()   +
  labs(title = "Proporção de habitantes acima de 60 anos",
       fill = "(%)")
png("proporcao_popidosa.png",width=3200,height=1800,res=300)
print(p1)
dev.off()


p1 <- muni_map_previ_pnud %>%   
  ggplot() + geom_sf(aes(fill = 100*pesourb/pesotot, col = 100*pesourb/pesotot)) +
  #  geom_sf(data = semiarido, fill = "transparent", colour = "white", size = 0.5) +
  viridis::scale_fill_viridis(option = "magma", direction = 1) +
  viridis::scale_color_viridis(option = "magma", direction = 1, guide=FALSE) +
  theme_bw()   +
  labs(title = "Proporção de População Urbana",
       fill = "(%)")
png("proporcao_popurb.png",width=3200,height=1800,res=300)
print(p1)
dev.off()

p1 <- muni_map_previ_pnud %>%   
  ggplot() + geom_sf(aes(fill = 100*pesorur/pesotot, col = 100*pesorur/pesotot)) +
  #  geom_sf(data = semiarido, fill = "transparent", colour = "white", size = 0.5) +
  viridis::scale_fill_viridis(option = "magma", direction = 1) +
  viridis::scale_color_viridis(option = "magma", direction = 1, guide=FALSE) +
  theme_bw()    +
  labs(title = "Proporção de População Rural",
       fill = "(%)")
png("proporcao_poprur.png",width=3200,height=1800,res=300)
print(p1)
dev.off()



p1 <- muni_map_previ_pnud %>%   
  ggplot(aes(x = 100*pesorur/pesotot, y = propsim)) + 
  geom_jitter(stat="identity")+
  theme_bw()    +
  labs(title = "População Rural vs. Proporção de votos em candidatos a favor da reforma da Previdência",
       x = "Proporção de população rural (%)",
       y = "Proporção de votos em candidatos a favor da reforma")
png("poprural_procsim.png",width=3200,height=1800,res=300)
print(p1)
dev.off()


p1 <- muni_map_previ_pnud %>%   
  mutate(porchomemidoso = 100*(homem60a64+homem65a69+homem70a74+homem75a79+homens80)/homemtot,
         porcmulhidosa = 100*(mulh60a64+mulh65a69+mulh70a74+mulh75a79+mulher80)/mulhertot,
         porcpopidosa = 100*(homem60a64+homem65a69+homem70a74+homem75a79+homens80+mulh60a64+mulh65a69+mulh70a74+mulh75a79+mulher80)/(mulhertot+homemtot)) %>%
  ggplot(aes(x = porcpopidosa, y = propsim)) + 
  geom_jitter(stat="identity")+
  theme_bw()    +
  labs(title = "População Idosa vs. Proporção de votos em candidatos a favor da reforma da Previdência",
       x = "Proporção de população acima de 60 anos (%)",
       y = "Proporção de votos em candidatos a favor da reforma")
png("popidosa_procsim.png",width=3200,height=1800,res=300)
print(p1)
dev.off()

p1 <- muni_map_previ_pnud %>%   
  ggplot(aes(x = razdep, y = propsim)) + 
  geom_jitter(stat="identity")+
  theme_bw()    +
  labs(title = "Razão de dependência vs. Proporção de votos em candidatos a favor da reforma da Previdência",
       x = "Razão de dependência (%)",
       y = "Proporção de votos em candidatos a favor da reforma")
png("popdependente_procsim.png",width=3200,height=1800,res=300)
print(p1)
dev.off()



############ ANOVA


anova_muni <- aov(propsim ~ espvida+fectot+mort1+mort5+razdep+sobre40+sobre60+t_env+e_anosestudo+t_analf11a14+t_analf15a17+t_analf15m+t_analf18a24+t_analf18m+t_analf25a29+t_analf25m+t_atraso_0_basico+t_atraso_0_fund+t_atraso_0_med+t_atraso_1_basico+t_atraso_1_fund+t_atraso_1_med+t_atraso_2_basico+t_atraso_2_fund+t_atraso_2_med+t_fbbas+t_fbfund+t_fbmed+t_fbpre+t_fbsuper+t_flbas+t_flfund+t_flmed+t_flpre+t_flsuper+t_freq0a3+t_freq11a14+t_freq15a17+t_freq18a24+t_freq25a29+t_freq4a5+t_freq4a6+t_freq5a6+t_freq6+t_freq6a14+t_freq6a17+t_freqfund1517+t_freqfund1824+t_freqfund45+t_freqmed1824+t_freqmed614+t_freqsuper1517+t_fund11a13+t_fund12a14+t_fund15a17+t_fund16a18+t_fund18a24+t_fund18m+t_fund25m+t_med18a20+t_med18a24+t_med18m+t_med19a21+t_med25m+t_super25m+corte1+corte2+corte3+corte4+corte9+gini+pind+pindcri+pmpob+pmpobcri+ppob+ppobcri+pren10ricos+pren20+pren20ricos+pren40+pren60+pren80+prentrab+r1040+r2040+rdpc+rdpc1+rdpc10+rdpc2+rdpc3+rdpc4+rdpc5+rdpct+rind+rmpob+rpob+theil+cpr+emp+p_agro+p_com+p_constr+p_extr+p_formal+p_fund+p_med+p_serv+p_siup+p_super+p_transf+ren0+ren1+ren2+ren3+ren5+renocup+t_ativ+t_ativ1014+t_ativ1517+t_ativ1824+t_ativ18m+t_ativ2529+t_des+t_des1014+t_des1517+t_des1824+t_des18m+t_des2529+theiltrab+trabcc+trabpub+trabsc+t_agua+t_banagua+t_dens+t_lixo+t_luz+agua_esgoto+parede+t_crifundin_todos+t_fora4a5+t_fora6a14+t_fundin_todos+t_fundin_todos_mmeio+t_fundin18minf+t_m10a14cf+t_m15a17cf+t_mulchefefif014+t_nestuda_ntrab_mmeio+t_ocupdesloc_1+t_rmaxidoso+t_sluz+homem0a4+homem10a14+homem15a19+homem20a24+homem25a29+homem30a34+homem35a39+homem40a44+homem45a49+homem50a54+homem55a59+homem5a9+homem60a64+homem65a69+homem70a74+homem75a79+homemtot+homens80+mulh0a4+mulh10a14+mulh15a19+mulh20a24+mulh25a29+mulh30a34+mulh35a39+mulh40a44+mulh45a49+mulh50a54+mulh55a59+mulh5a9+mulh60a64+mulh65a69+mulh70a74+mulh75a79+mulher80+mulhertot+pea+pea1014+pea1517+pea18m+peso1+peso1114+peso1113+peso1214+peso13+peso15+peso1517+peso1524+peso1618+peso18+peso1820+peso1824+peso1921+peso25+peso4+peso5+peso6+peso610+peso617+peso65+pesom1014+pesom1517+pesom15m+pesom25m+pesorur+pesotot+pesourb+pia+pia1014+pia1517+pia18m+pop+popt+i_escolaridade+i_freq_prop+idhm+idhm_e+idhm_l+idhm_r+porchomemidoso+porcmulhidosa+porcpopidosa+poprur+popurb+poprur:razdep+poprur:t_analf15m+poprur:porcpopidosa,
                  data = muni_map_previ_pnud %>% mutate(porchomemidoso = 100*(homem60a64+homem65a69+homem70a74+homem75a79+homens80)/homemtot,
                                                        porcmulhidosa = 100*(mulh60a64+mulh65a69+mulh70a74+mulh75a79+mulher80)/mulhertot,
                                                        porcpopidosa = 100*(homem60a64+homem65a69+homem70a74+homem75a79+homens80+mulh60a64+mulh65a69+mulh70a74+mulh75a79+mulher80)/(mulhertot+homemtot),
                                                        poprur = 100*pesorur/pesotot,
                                                        popurb = 100*pesourb/pesotot))
summ_anova <- summary(anova_muni)
table_summ_anova <- summ_anova[[1]]
table_summ_anova[order(abs(table_summ_anova[,3])),] %>% tail(20)