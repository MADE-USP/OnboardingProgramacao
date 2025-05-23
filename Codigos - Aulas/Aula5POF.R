# Tidyverse - Manipulação de Dados
library(tidyverse)
library(this.path)
setwd(this.dir())
data_dir <- './Dados/'
tradutores_dir <- './Tradutores/'
# Bases de Despesa
base_ind <- readRDS(paste0(data_dir, 'DESPESA_INDIVIDUAL.rds'))
base_caderneta_coletiva <- readRDS(paste0(data_dir,'CADERNETA_COLETIVA.rds'))
base_coletiva <- readRDS(paste0(data_dir,'DESPESA_COLETIVA.rds'))
aluguel_estimado <- readRDS(paste0(data_dir, 'ALUGUEL_ESTIMADO.rds'))
# Morador
morador <- readRDS(paste0(data_dir, "MORADOR.rds"))
# Empilhando bases
base_empilhada <- bind_rows(base_ind, base_coletiva, base_caderneta_coletiva, aluguel_estimado)
# Para facilitar na anualização
base_empilhada <- base_empilhada %>% mutate(V9011 = replace_na(V9011, 1))
# Limitando os códigos de produtos apenas para os 5 primeiros dígitos
# A razão pela qual se faz isso é que os dois últimos dígitos, na POF, denotam apenas variações nos nomes dos produtos
base_empilhada <- base_empilhada %>% mutate(V9001 = floor(V9001/100))
# Variáveis de Despesa
# Para criar as variáveis de despesa, vamos utilizar o tradutor de despesas
tradutor_despesa <- readxl::read_excel(paste0(tradutores_dir, "Tradutor_Despesa_Geral.xls"))
# Queremos somente as despesas monetárias
base_despesa_monetaria <- base_empilhada
tradutor_despesa <- tradutor_despesa %>% select(c("Codigo", "Descricao_2", "Descricao_3"))
# Mantendo somente uma observação por código, para evitar relação many-to-many
tradutor_unico <- tradutor_despesa %>% group_by(Codigo) %>% filter(row_number(Descricao_3) == 1)
base_despesa_monetaria <- base_despesa_monetaria %>% left_join(tradutor_unico, by = c("V9001" = "Codigo")) 
# Vamos criar as variáveis de despesa agora
# Para facilitar, vamos já criar as variáveis mensais de valor
base_despesa_monetaria <- base_despesa_monetaria %>% mutate(valor = V8000_DEFLA*V9011*FATOR_ANUALIZACAO/12)
# Transporte
base_despesa_monetaria <- base_despesa_monetaria %>% group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  mutate(Transporte = sum(if_else(Descricao_3=="Transporte", valor, 0, missing = 0))) %>% 
  ungroup()
# Saúde
base_despesa_monetaria <- base_despesa_monetaria %>% group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  mutate(Saude = sum(if_else(Descricao_3=="Assistencia a saude", valor, 0, missing = 0))) %>% 
  ungroup()
# Educação
base_despesa_monetaria <- base_despesa_monetaria %>% group_by(COD_UPA, NUM_DOM, NUM_UC) %>% 
  mutate(Educacao = sum(if_else(Descricao_3=="Educacao", valor, 0, missing = 0))) %>% 
  ungroup()
# Agora, iremos agrupar a base para juntar com a base de morador
despesas_agrupadas <- base_despesa_monetaria %>% group_by(COD_UPA, NUM_DOM, NUM_UC) %>%
  summarise(Transporte=mean(Transporte),
            Saude=mean(Saude), Educacao=mean(Educacao))
# Juntando com morador
morador <- morador %>% left_join(despesas_agrupadas, by=c("COD_UPA", "NUM_DOM", "NUM_UC"))
# Filtrando para retirar empregados domésticos e parentes de empregados domésticos
morador <- morador %>% filter(!V0306 %in% 18:19)
# Criando dummy de chefe de família
morador <- morador %>% mutate(chefe = if_else(V0306==1, 1, 0))
# Filtrando para conter somente uma observação por família
base_final <- morador %>% filter(chefe == 1)
# Calculando despesas médias
educacao_media <- sum(base_final$Educacao*base_final$PESO_FINAL)/sum(base_final$PESO_FINAL)
saude_media <- sum(base_final$Saude*base_final$PESO_FINAL)/sum(base_final$PESO_FINAL)
transporte_media <- sum(base_final$Transporte*base_final$PESO_FINAL)/sum(base_final$PESO_FINAL)