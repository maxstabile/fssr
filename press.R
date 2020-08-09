library(ggplot2)
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(lubridate)
library(ggthemes)
library(xlsx)

basecompleta <- read.xlsx("dados imprensa.xlsx", 1, header=TRUE)
dfteste <- dfteste[c(3,4)]
colnames(df)[2] <- "wave"


library("dplyr") 

#criando a tabela de frequência por data
df2 <- df %>% 
  filter(wave > 0) %>%
  select(data, wave) %>%
  group_by(data) %>%
  summarize(qtde = n())

#juntando a informação de cada onda
df2 <- left_join(df2, unique(df), by = "data")

df2$wave <- case_when(
  df2$wave == 1 ~ "Wave 1",
  df2$wave == 2 ~ "Wave 2",
  df2$wave == 3 ~ "Wave 3",
  df2$wave == 4 ~ "Wave 4",
  TRUE ~ as.character(df2$wave)
)


#criando as médias por período(wave) para plotar no gráfico
dMean <- df2 %>%
  group_by(wave) %>%
  summarise(MN = mean(qtde))

#criando o gráfico
df2 %>%
  ggplot(aes(x=data, y=qtde)) +
  theme_light() +
  geom_line() + 
  xlab("Days") + ylab("Number of news articles") + 
  (scale_x_date(breaks=date_breaks("3 days"),
                minor_breaks = NULL,
                expand = c(0, 0),
                labels=date_format("%d/%b/%y"))) +
    theme(axis.text.x = element_text(angle = 90, size = 6, hjust = 1))+
    geom_hline(data = dMean, aes(yintercept = MN),linetype="dashed", color = "gray", size=1)+
    facet_wrap(~ wave, scales = "free_x", dir = "h", ncol = "4")
    

  