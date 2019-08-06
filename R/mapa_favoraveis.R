library(abjutils)
library(abjData)
library(tidyverse)
library(brazilmaps)
library(colorRamps)
library(ggrepel)
library(JLutils)#devtools::install_github("larmarange/JLutils")

setwd("/home/charles/GitRepos/dadoscope/previdemo/data/eleicoes/")
votos <- read.csv("../previdencia.csv")
names(votos)
pnud_muni <- abjData::pnud_muni
br_uf_map <- abjData::br_uf_map
pnud_uf <- abjData::pnud_uf
votos_por_uf <- votos %>% 
	group_by(UF,Voto) %>% 
	dplyr::summarise(parc = n()) %>% 
	ungroup() %>% 
	group_by(UF) %>% 
	dplyr::mutate(total = sum(parc)) %>% 
	ungroup() %>%
        filter(Voto == "Sim") %>% 	
	group_by(UF) %>% 
	dplyr::summarise(porc = signif(parc/total,3)*100) %>% 
	ungroup()

uf_code <- data.frame(UF = unique(votos_por_uf$UF), code = c(12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17))
votos_por_uf <- uf_code %>% left_join(votos_por_uf)
uf_map <- get_brmap("State")
uf_map <- uf_map %>% right_join(votos_por_uf, c("State" = "code"))
df_estados <- readRDS("df_muni_map_previ_pnud.rds")

# To plot figures about States

setwd("/home/charles/GitRepos/dadoscope/previdemo/figures")

p1 <- ggplot(uf_map) +
	geom_sf(data = uf_map, aes(fill = porc)) +
        geom_sf_label(data = uf_map, aes(label = porc))+	
  viridis::scale_fill_viridis(option = "magma", direction = -1) +
  viridis::scale_color_viridis(option = "magma", direction = -1, guide=FALSE) +
	theme_bw() +
	labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma nos Estados")

png("mapa_votos_previdencia.png",width=4800,height=2700,res=300)
print(p1)
dev.off()

p1<-uf_map %>% 
  right_join(df_estados  %>% ungroup() %>%
               group_by(State) %>% 
               dplyr::summarise(propsim = mean(propsim,na.rm=TRUE)), c("State" = "State")) %>%
  ggplot() +
  geom_sf(aes(fill = propsim)) +
  geom_sf_label(aes(label = signif(propsim,2)))+	
  viridis::scale_fill_viridis(option = "magma", direction = -1) +
  viridis::scale_color_viridis(option = "magma", direction = -1, guide=FALSE) +
  theme_bw() +
  labs(fill = "Votos (%)", title = "Porcentagem de votos em candidatos pró-Reforma nos Estados nas eleições de 2018")
png("mapa_proreforma_eleicoes.png",width=4800,height=2700,res=300)
print(p1)
dev.off()


df_estados  %>% ungroup() %>%
  group_by(State) %>% 
  dplyr::summarise(propsim = mean(propsim,na.rm=TRUE))

p1<-uf_map %>% 
  right_join(df_estados  %>% ungroup() %>%
               group_by(State) %>% 
               dplyr::summarise(propsim = mean(propsim,na.rm=TRUE)), c("State" = "State")) %>%
  ggplot() +
  geom_sf(aes(fill = propsim)) +
  geom_sf_label(aes(label = signif(propsim,2)))+	
  viridis::scale_fill_viridis(option = "magma", direction = -1) +
  viridis::scale_color_viridis(option = "magma", direction = -1, guide=FALSE) +
  theme_bw() +
  labs(fill = "Votos (%)", title = "Porcentagem de votos em candidatos pró-Reforma nos Estados nas eleições de 2018")
png("figures/mapa_proreforma_eleicoes.png",width=4800,height=2700,res=300)
print(p1)
dev.off()
