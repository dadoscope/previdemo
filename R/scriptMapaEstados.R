library(brazilmaps)
library(tidyverse)
library(electionsBR)
library(stringr)

eleicoes2018 <- readRDS("eleicoes2018_estados.rds")
previdencia <- read.csv("../previdencia.csv")

previdencia <- previdencia %>% mutate(Deputado = toupper(Deputado))
eleicoes2018 <- eleicoes2018 %>% ungroup() %>% mutate(NOME_URNA_CANDIDATO = stringr::str_trim(NOME_URNA_CANDIDATO))
eleicoes2018 <- eleicoes2018 %>% ungroup() %>% mutate(NOME_URNA_CANDIDATO = stringr::str_squish(NOME_URNA_CANDIDATO))
eleicoes2018 <- eleicoes2018 %>% ungroup() %>% mutate(NOME_CANDIDATO = stringr::str_trim(NOME_CANDIDATO))
eleicoes2018 <- eleicoes2018 %>% ungroup() %>% mutate(NOME_CANDIDATO = stringr::str_squish(NOME_CANDIDATO))

#Fixing mismatches
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




votos_deputados_estados_parte1 <- eleicoes2018 %>% left_join(previdencia, by = c("NOME_URNA_CANDIDATO" = "Deputado","SIGLA_PARTIDO" = "Partido","SIGLA_UF" = "UF" )) %>% filter(!is.na(Voto)) %>% mutate(Deputado = NOME_URNA_CANDIDATO)
votos_deputados_estados_parte2 <- eleicoes2018 %>% left_join(previdencia, by = c("NOME_CANDIDATO" = "Deputado","SIGLA_PARTIDO" = "Partido","SIGLA_UF" = "UF" ))  %>% filter(NOME_CANDIDATO != NOME_URNA_CANDIDATO) %>% filter(!is.na(Voto)) %>% mutate(Deputado = NOME_CANDIDATO)

votos_todos_deputados <- rbind(votos_deputados_estados_parte1, votos_deputados_estados_parte2) %>% arrange(Deputado)


p1 <- votos_todos_deputados %>% group_by(SIGLA_UF, Voto) %>% summarise(total_camara = n(), total_eleicao = sum(TOTAL_VOTOS)) %>% ungroup() %>% group_by(SIGLA_UF) %>% complete(Voto, fill = list(total_camara = 0, total_eleicao = 0)) %>% ungroup() %>% group_by(SIGLA_UF) %>% mutate(total_uf_camara = sum(total_camara), total_uf_eleicao = sum(total_eleicao)) %>% ungroup() %>% filter(Voto == "Sim") %>% group_by(SIGLA_UF) %>% summarise(prop_camara = total_camara/total_uf_camara, prop_eleicao = total_eleicao/total_uf_eleicao) %>% ggplot(aes(x = prop_eleicao, y = prop_camara)) + geom_jitter(stat="identity") + xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=0.7) + geom_label(aes(x = prop_eleicao, y = prop_camara, label = SIGLA_UF)) + labs(x="Proporção de votos em deputados pró-Reforma nas Eleições 2018", y = "Proporção de deputados que votaram SIM na Câmara", title = "Análise dos Estados")
png("votos_nos_estados_eleicoes.png",width=3200,height=1800,res=300)
print(p1)
dev.off()

uf_code <- data.frame(UF = sort(unique(previdencia$UF)), 
                      code = c(12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17))
votos_todos_deputados <- votos_todos_deputados %>% inner_join(uf_code, by = c("SIGLA_UF"="UF"))

ufmap <- get_brmap("State")
ufmap <- ufmap %>% 
  inner_join(votos_todos_deputados %>% 
               group_by(code, SIGLA_UF, Voto) %>% 
               summarise(total_camara = n(), total_eleicao = sum(TOTAL_VOTOS)) %>% 
               ungroup() %>% 
               group_by(code, SIGLA_UF) %>% 
               complete(Voto, fill = list(total_camara = 0, total_eleicao = 0)) %>% 
               ungroup() %>% 
               group_by(code, SIGLA_UF) %>% 
               mutate(total_uf_camara = sum(total_camara), total_uf_eleicao = sum(total_eleicao)) %>% 
               ungroup() %>% 
               filter(Voto == "Sim") %>% 
               group_by(code, SIGLA_UF) %>% 
               summarise(prop_camara = total_camara/total_uf_camara, prop_eleicao = total_eleicao/total_uf_eleicao), 
             c("State" = "code"))
p2 <- ufmap %>%
  ggplot() +
  geom_sf(aes(fill = 100*(prop_eleicao-prop_camara)),
          colour = "black", 
          size = 0.1) +
  # muda escala de cores
  scale_fill_gradientn(colors=c(colorRamps::blue2red(10)[1:5],
                                "white",
                                colorRamps::blue2red(10)[6:10]),
                       breaks=c(-25,0,25),limits=c(-25,25))+
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Expectativa - Realidade de votos a favor da Reforma")+
  labs(fill = "Diferença (%)")
png("mapa_diferencas_expec_real.png",width=3200,height = 1800,res = 300)
print(p2)
dev.off()


p2 <- ufmap %>%
  ggplot() +
  geom_sf(aes(fill = 100*(prop_eleicao)),
          colour = "black", 
          size = 0.1) +
  # muda escala de cores
  scale_fill_gradientn(colors=c(colorRamps::blue2red(10)[1:5],
                                colorRamps::blue2red(10)[6:10]),
                       breaks=c(0,25,50,75,100),limits=c(0,100))+
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Proporção de votos em candidatos pró-Reforma")+
  labs(fill = "Votos (%)")
png("mapa_proreforma_eleicoes.png",width=3200,height = 1800,res = 300)
print(p2)
dev.off()

p2 <- ufmap %>%
  ggplot() +
  geom_sf(aes(fill = 100*(prop_camara)),
          colour = "black", 
          size = 0.1) +
  # muda escala de cores
  scale_fill_gradientn(colors=c(colorRamps::blue2red(10)[1:5],
                                colorRamps::blue2red(10)[6:10]),
                       breaks=c(0,25,50,75,100),limits=c(0,100))+
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Proporção de votos a favor da reforma na Câmara")+
  labs(fill = "Votos (%)")
png("mapa_proreforma_camara.png",width=3200,height = 1800,res = 300)
print(p2)
dev.off()

saveRDS(votos_todos_deputados, file = "dataframe_votos_deputados_reforma_eleicaonosestados.rds")
