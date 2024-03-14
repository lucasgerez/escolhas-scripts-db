
# ------------------------------------------------ # 

#           POF 2018 - Base consolidada

# ------------------------------------------------ # 

library(dplyr)
library(tidyr)



# diretório
rais_path <- 'E:/Drive/Projetos/Escolhas/2023/Dados 13 regiões/Dados/ENTRADA'


# painel completo da RAIS
load(file.path(rais_path, 'df_rais_painel.rdata'))


# Tabelas para Curitiba



# UF
df_rais %>% filter(UF == 41) %>% head()

# Unidade de Análise RM de Curitiba 
df_rais %>% filter(nome_regiao_metropolitana == 'Região Metropolitana de Curitiba') %>% View()

# Curitiba
df_rais %>% filter(id_municipio == 4106902)


