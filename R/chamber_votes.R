library(tidyverse)
library(ggthemes)
library(nmlegisdatr)# devtools::install_github("jaytimm/nmlegisdatr")
library(wnomadds)#devtools::install_github("jaytimm/wnomadds")

seats <- function(N,M, r0=2){ 
  radii <- seq(r0, 1, len=M)
  counts <- numeric(M)
  pts = do.call(rbind,
            lapply(1:M, function(i){
              counts[i] <<- round(N*radii[i]/sum(radii[i:M]))
              theta <- seq(0, pi, len = counts[i])
              N <<- N - counts[i]
              data.frame(x=radii[i]*cos(theta), y=radii[i]*sin(theta), r=i,
                         theta=theta)}))
   pts = pts[order(-pts$theta,-pts$r),]
   pts}

election <- function(seats, counts){
  stopifnot(sum(counts)==nrow(seats))
  seats$party <- rep(1:length(counts),counts)
  seats$party <- ifelse(seats$party == 1, "Nao", "Sim")
seats}


dat <- read.csv("../data/previdencia.csv")
leg <- dat %>% group_by(Voto) %>% summarise(Count = n())
house <- election(seats(510,5), leg$Count)

p1 <- house %>% 
	ggplot() + 
	geom_point(aes(x,y, color=as.factor(party)), size = 2) + 
	ggthemes::theme_fivethirtyeight() + 
	labs(title="Votos na Reforma da Previdência na Câmara", color="") +
	coord_fixed(ratio=1) + 
	theme(axis.title.x=element_blank(),
	      axis.text.x=element_blank(),
	      axis.title.y=element_blank(),
	      axis.text.y=element_blank(),
	      plot.title = element_text(size=12) )
png("../figures/votos_camara.png", width=3200,height=1800,res=300)
print(p1)
dev.off()
