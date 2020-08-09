
#Pacotes
library(tidyverse)
library(survey)
library(haven)
library(dplyr)
library(forcats)
library(ggrepel)


#carregando a base de dados em formato SPSS
FSSR1 <- read_sav("FSSR1.sav")

FSSR1$ONDA <- as.factor(FSSR1$ONDA)
FSSR1$tratamento <- as.factor(FSSR1$tratamento)

#Criando a tabela com os resultados de cada onda
tab1 = FSSR1 %>% 
  group_by(ONDA, tratamento, op_previdência_AFAVOR) %>%
  summarise(Freq = n()) %>%
  mutate(Perc = round(Freq/sum(Freq),2)) %>%
  dplyr::filter(op_previdência_AFAVOR == 1)

#Transformando em factor pois a importação do SPSS vem com outro formato
#tab1$tratamento <- as.factor(tab1$tratamento)
#tab1$ONDA <- as.factor(tab1$ONDA)

#FSSR1 %>% 
#  mutate_at( vars(tratamento), as_factor) %>%
#  mutate_at( vars(ONDA), as.integer) %>%
# group_by(ONDA, tratamento) %>%
# summarise(N = n()) %>%
#  mutate(lower = qbeta(0.025, (N/2)+1, N-(N/2)+1)) %>%
#  mutate(upper = qbeta(0.975, (N/2+1), N-(N/2)+1)) %>%
#  mutate(IC = (upper - lower)/2)

#CRIANDO A TABELA COM OS INTERVALOS
tab0 = FSSR1 %>% 
    group_by(ONDA, tratamento) %>%
  summarise(N = n()) %>%
    mutate(lower = qbeta(0.025, (N/2)+1, N-(N/2)+1)) %>%
  mutate(upper = qbeta(0.975, (N/2+1), N-(N/2)+1)) %>%
  mutate(IC = (upper - lower)/2) %>%
  select(ONDA, tratamento, IC)
#transformando novamente em fator
#tab0$ONDA <- as.factor(tab0$ONDA)
#tab0$tratamento <- as.factor(tab0$tratamento)


#CONSOLIDANDO A TABELA 
tab2 <- left_join(tab1, tab0) %>% 
  mutate(min = Perc-IC, 
         max = Perc+IC)
levels(tab2$ONDA) <- c("Wave 1 (Dec-18)", "Wave 2 (Jan-19)", "Wave 3 (Mar-19)", "Wave 4 (Dec-19)")
levels(tab2$tratamento) <- c("Control", "Silent frame", "Salient frame")


#limpando as bases antigas
rm(tab0, tab1, FSSR1)


#GRÁFICO DE LINHA

ggplot(tab2, aes(x=ONDA, y=Perc, group = tratamento, shape=tratamento, color=tratamento))+ 
  theme_light() +
  geom_errorbar(aes(ymin=Perc-IC, ymax=Perc+IC), width=.03) +
  scale_y_continuous(limits = c(.20,.80), 
                     labels = scales::percent) +
  geom_line(size=0.2) +
  geom_point(size=4) + 
  scale_color_manual(values = c("#7e7f80","#000000", "#a8a9ab")) +
  
labs(x="", y="% Approving Pension Reform") + 
  theme(legend.position = "top", panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    geom_text_repel(
    aes(label = paste0(Perc*100,"%")), color = 'black',
    size = 3,max.iter = 5000, force = 7,
    min.segment.length = 0.95) +
  theme(legend.title = element_blank())
  
### gráfico de coluna

ggplot(tab2, (aes(x=tratamento, y=Perc, fill=ONDA))) +
  scale_fill_brewer(palette="Greys") +
  #scale_fill_grey() +
  geom_bar(stat = "identity", position=position_dodge()) +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  geom_errorbar(aes(ymin=Perc-IC, ymax=Perc+IC), width=.1,
                position=position_dodge(.9)) +
  labs(x="", y="% Approving Pension Reform") +
  geom_text(
  aes(label = paste0(Perc*100,"%")), color = 'black', size = 3,
   position = position_dodge(width = 1),
   hjust = -.2, vjust=-1)+
  theme(legend.title = element_blank(), legend.position = "top")

