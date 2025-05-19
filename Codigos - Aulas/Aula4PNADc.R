library(PNADcIBGE)
library(tidyverse)
library(hutils)
cores_made <- c("#45ff66", "#eb52ff", "#3366ff","#feff41")
# Baixando a pnadc, o resultado desta função é a pnad já como objeto survey
surveyPnad <- get_pnadc(year = 2023, interview = 5)
# Transformando em Dataframe
dfPnad <- surveyPnad$variables
dfPnad <- dfPnad %>% mutate(peso = surveyPnad$pweights)
# Fazendo recoding da variável de raça e criando variável de raça e gênero
dfPnad <- dfPnad %>% 
  filter(V2010 %in% c("Parda", "Preta", "Branca")) %>%
  mutate(raca = if_else(V2010 == "Branca", "Branca", "Negra")) %>%
  mutate(raca_genero = case_when(
    (raca == "Negra" & V2007 == "Homem") ~ "Homem Negro",
    (raca == "Branca" & V2007 == "Homem") ~ "Homem Branco",
    (raca == "Branca" & V2007 == "Mulher") ~ "Mulher Branca",
    (raca == "Negra" & V2007 == "Mulher") ~ "Mulher Negra",
  ))
# Filtrando para população adulta
dfPnad <- dfPnad %>%
  filter(V2009>=18)
# Criando decis
dfPnad <- dfPnad %>%
  mutate(decis = weighted_ntile(VD5010, peso, 10))
# Base para o Gráfico
tabelaGrafico <- dfPnad %>%
  group_by(decis, raca_genero) %>%
  summarise(Total_inter = sum(peso)) %>% 
  ungroup() %>%
  group_by(decis) %>%
  mutate(Total_decil = sum(Total_inter)) %>%
  ungroup() %>%
  mutate(prop = (Total_inter/Total_decil)*100)
# Gráfico
plot <- ggplot(tabelaGrafico, aes(fill = raca_genero, x = decis, y = prop)) + 
  geom_bar(position = "stack", stat="identity") +
  scale_fill_manual(values = cores_made, name = "Grupo Demográfico") +
  ylab("Proporção (em %)") +
  xlab("Decis") +
  ggtitle("Composição Demográfica \n de Cada Decil de Renda") +
  theme(plot.title = element_text(hjust = 0.5))