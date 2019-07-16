votos <- read.csv("data/previdencia.csv")
names(votos)
library(abjutils)
ls()
abjutils::tabela
devtools::install_github("abj/abjdata")
library(abjData)
ls()
pnud_muni <- abjData::pnud_muni
br_uf_map <- abjData::br_uf_map
pnud_uf <- abjData::pnud_uf
ls()
votos
library(tidyverse)
votos %>% group_by(UF)
votos %>% group_by(UF) %>% count(Voto)
votos_por_uf <- votos %>% group_by(UF) %>% count(Voto)
votos_por_uf %>% group_by(UF) %>% mutate(total = sum(n))
votos_por_uf %>% group_by(UF) %>% mutate(total = sum(n)) %>% ungroup()
votos_por_uf %>% group_by(UF) %>% mutate(total = sum(n)) %>% ungroup() %>% group_by(UF,Voto) %>% summarise(prop = n/total)
votos_por_uf %>% group_by(UF) %>% mutate(total = sum(n)) %>% ungroup() %>% group_by(UF,Voto) %>% summarise(prop = signif(n/total,2))
votos_por_uf %>% group_by(UF) %>% mutate(total = sum(n)) %>% ungroup() %>% group_by(UF,Voto) %>% summarise(prop = signif(n/total,2)*100)
votos_por_uf %>% group_by(UF) %>% mutate(total = sum(n)) %>% ungroup() %>% group_by(UF,Voto) %>% summarise(porc = signif(n/total,2)*100)
votos_por_uf %>% group_by(UF) %>% mutate(total = sum(n)) %>% ungroup() %>% group_by(UF,Voto) %>% summarise(porc = signif(n/total,3)*100)
votos_por_uf <- votos_por_uf %>% group_by(UF) %>% mutate(total = sum(n)) %>% ungroup() %>% group_by(UF,Voto) %>% summarise(porc = signif(n/total,3)*100)
ls()
library(brazilmaps)
uf_map <- get_brmap("State") %>% inner_join(votos_por_uf, c("State" = "UF"))
uf_map
uf_map$State
uf_map$nome
names(get_brmap("State"))
names(get_brmap("Region"))
names(get_brmap("City"))
names(abjData::br_uf_map)
head(abjData::br_uf_map)
head(abjData::get_states_map)
names(abjData::get_states_map)
names(abjData::get_states_map())
names(pnud_uf)
pnud_uf$uf
pnud_uf$ufn
pnud_uf$uf
pnud_uf$uf,pnud_ufn
cbind(pnud_uf$uf,pnud_ufn)
cbind(pnud_uf$uf,pnud_uf$ufn)
votos_por_uf
uf_code <- data.frame(uf = unique(votos_por_uf$UF))
uf_code
cbind(pnud_uf$uf,pnud_uf$ufn)
cbind(pnud_uf$uf,pnud_uf$ufn, row.names=c("code","uf"))
cbind(pnud_uf$uf,pnud_uf$ufn, col=c("code","uf"))
cbind(pnud_uf$uf,pnud_uf$ufn)
uf_code
cbind(pnud_uf$uf,pnud_uf$ufn)
uf_code <- data.frame(uf = unique(votos_por_uf$UF), code = c(12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17))
uf_code
votos_por_uf %>% left_join(uf_code,by=c("UF","uf"))
votos_por_uf %>% left_join(uf_code,by=c("uf"))
votos_por_uf %>% left_join(uf_code)
uf_code
votos_por_uf
uf_code <- data.frame(UF = unique(votos_por_uf$UF), code = c(12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17))
uf_code %>% left_join(votos_por_uf)
votos_por_uf <- uf_code %>% left_join(votos_por_uf)
uf_map <- get_brmap("State") %>% inner_join(votos_por_uf, c("State" = "code"))
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_viridis_c(option = 2) +theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_viridis_c(option = 2)
names(uf_map)
uf_map %>% ggplot() +geom_sf(aes(fill = porc))
uf_map %>% ggplot() +geom_sf(data = uf_map, aes(fill = porc))
ggplot(uf_map) +geom_sf(data = uf_map, aes(fill = porc))
ggplot(uf_map)
ggplot(uf_map) + geom_sf(data = uf_map)
names(uf_map)
class(uf_map)
plot(uf_map)
uf_map$geometry
names(uf_map)
ggplot(uf_map) + geom_sf(data = uf_map)
head(uf_map)
names(uf_map)
uf_map
plot(uf_map)
uf_map <- get_brmap("State") %>% inner_join(votos_por_uf %>% filter(Voto == "Sim"), c("State" = "code"))
ggplot(uf_map) + geom_sf(data = uf_map)
uf_map <- get_brmap("State") %>% inner_join(votos_por_uf %>% filter(Voto == "Sim"), c("State" = "code"))
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_viridis_c(option = 2)
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn()
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=terrain.colors(100))
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=terrain.colors(100))+theme_bw()
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=terrain.colors(100))+theme_bw() +labels(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=terrain.colors(100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=terrain.colors(100),breaks=c(0,25,50,75,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=terrain.colors(100),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=blues9(100),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=colorRamps::blue2red(100),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
library(colorRamps)
install.packages("colorRamps")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=colorRamps::blue2red(100),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=colorRampPalette(colors = c("red", "white", "blue")),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=colorRampsPalette(colors =c("red", "white", "blue")),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
colorRampPalette("red","white","blue")
colorRampPalette("#FF0000","#FFFFFF","#0000FF")
colorRampPalette("#FF0000","#FFFFFF")
colorRamp::blue2red(10)
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=colorRamps::blue2red(100),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=colorRamps::blue2red(10),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=colorRamps::blues(10),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=colorRamps::blue(10),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=rev(colorRamps::blue2red(10)),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=colorRamps::blue2red(10),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
uf_map %>% ggplot() +geom_sf(aes(fill = porc)) + scale_fill_gradientn(colors=colorRamps::blue2red(100),breaks=c(0,25,50,75,100),limits=c(0,100))+theme_bw() +labs(fill = "Votos (%)", title = "Porcentagem de deputados favoráveis à Reforma")
savehistory("mapa_favoraveis.R")
