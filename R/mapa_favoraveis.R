library(abjutils)
library(abjData)
library(tidyverse)
library(brazilmaps)
library(colorRamps)
library(ggrepel)
library(JLutils)#devtools::install_github("larmarange/JLutils")

votos <- read.csv("data/previdencia.csv")
names(votos)
pnud_muni <- abjData::pnud_muni
br_uf_map <- abjData::br_uf_map
pnud_uf <- abjData::pnud_uf
votos_por_uf <- votos %>% 
	group_by(UF) %>% 
	count(Voto) %>% 
	ungroup() %>% 
	group_by(UF) %>% 
	mutate(total = sum(n)) %>% 
	ungroup() %>%
        filter(Voto == "Sim") %>% 	
	group_by(UF,Voto) %>% 
	summarise(porc = signif(n/total,3)*100) %>% 
	ungroup()

uf_code <- data.frame(UF = unique(votos_por_uf$UF), code = c(12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17))
votos_por_uf <- uf_code %>% left_join(votos_por_uf)
uf_map <- get_brmap("State")
uf_map <- uf_map %>% inner_join(votos_por_uf, c("State" = "code"))

palette <- colorRampPalette(c("red", "orange", "blue"))(100)

p1 <- ggplot(uf_map) +
	geom_sf(data = uf_map, aes(fill = porc)) +
        geom_sf_label(data = uf_map, aes(label = porc))+	
	scale_fill_gradientn(colors=palette,breaks=c(0,25,50,75,100),limits=c(0,100))+
	theme_bw() +
	labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma nos Estados")

png("figures/mapa_votos_previdencia.png",width=4800,height=2700,res=300)
print(p1)
dev.off()

votos_porc_part <- votos %>% group_by(Partido) %>% mutate(total=n()) %>% ungroup() %>% group_by(Partido, Voto, total) %>% summarise(votos = n()) %>% ungroup() %>% group_by(Partido, Voto, total, votos) %>% summarise(porc = 100*votos/total) %>% ungroup() %>% select(Partido, Voto, porc, total)

votos_porc_part %>% ggplot() + geom_bar(aes(y = porc, x = Partido, fill = Voto), stat = "identity")
