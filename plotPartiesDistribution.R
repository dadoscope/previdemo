library(tidyverse)
library(dplyr)

previden2019 <- read.csv("data/previdencia.csv", stringsAsFactors=FALSE)

p1 <- previden2019 %>% 
  mutate(Partido = factor(Partido,
			  levels = c(
				     "PSL",
				     "CIDADANIA",
				     "DC",
				     "DEM",
				     "PODE",
				     "PTB",
				     "MDB",
				     "NOVO",
				     "PATRI",
				     "PHS",
				     "PL",
				     "PSDB",
				     "PSD",
				     "PRB",
				     "SOLIDARIEDADE",
				     "PP",
				     "PSC",
				     "AVANTE",
				     "PROS",
				     "PV",
				     "PSB",
				     "PDT",
				     "PMN",
				     "REDE",
				     "PC do B",
				     "PSOL",
				     "PT"))) %>%
  group_by(Partido, Voto) %>% 
  summarise(total = n()) %>% 
  ungroup() %>%
  complete(Partido, Voto, fill = list(total = 0)) %>%
  arrange(total) %>%
  ggplot(aes(x = Partido, y = total, fill = Voto)) +
  geom_bar(stat="identity", position = "fill") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Partidos",y="Proporção dos votos", title = "Votos de Deputados - Reforma da Previdência")

png("votos_de_partidos.png",width=3200,height=1800,res=300)
print(p1)
dev.off()

