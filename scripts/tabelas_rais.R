
# ------------------------------------------------ # 

#           RAIS - TABELAS Consolidadas

# ------------------------------------------------ # 

library(dplyr)
library(tidyr)



# diretório
rais_path <- 'E:/Drive/Projetos/Escolhas/2023/Dados 13 regiões/Dados/ENTRADA'

# painel completo da RAIS
load(file.path(rais_path, 'df_rais_painel.rdata'))


# Tabelas para Curitiba

head(df_rais)

mun_curitiba <- 4106902

# Tabela 1: total de empregos formais e por sexo (teste Curitiba)
df_rais %>% 
  filter(id_municipio == mun_curitiba, unidade_analise == 'Capital') %>% 
  arrange(ano) %>% 
  mutate(municipio = 'Curitiba', 
         prop_emp_mulheres = emprego_foral_sexo_Feminino/emprego_foral_all,
         prop_emp_homens = emprego_foral_sexo_Masculino/emprego_foral_all,
         emp_formal = emprego_foral_all/10^6) %>%
  select(municipio, ano, emp_formal, prop_emp_mulheres, prop_emp_homens, rem_media_sexo_Feminino, rem_media_sexo_Masculino)
  

# Tabela 2: AGROALIMENTAR total de empregos formais e por sexo (teste Curitiba)
df_rais %>% 
  filter(id_municipio == mun_curitiba, unidade_analise == 'Capital_agro') %>% 
  arrange(ano) %>% 
  mutate(municipio = 'Curitiba', 
         prop_emp_mulheres = emprego_foral_sexo_Feminino/emprego_foral_all,
         prop_emp_homens = emprego_foral_sexo_Masculino/emprego_foral_all,
         emp_formal = emprego_foral_all/10^6) %>%
  select(municipio, ano, emp_formal, prop_emp_mulheres, prop_emp_homens, rem_media_sexo_Feminino, rem_media_sexo_Masculino)


# Cor/raça: tem alguma coisa zuada aqui

# Tabela 3: total de empregos formais e por cor/raça (teste Curitiba)
df_rais %>% 
  filter(id_municipio == mun_curitiba, unidade_analise == 'Capital') %>% 
  arrange(ano) %>% 
  mutate(municipio = 'Curitiba', 
         prop_emp_pretos_pardos = emprego_foral_raca_cor_Negros/emprego_foral_all,
         prop_emp_indigenas = emprego_foral_raca_cor_Indigena/emprego_foral_all,
         prop_emp_bracos = emprego_foral_raca_cor_Brancos/emprego_foral_all,
         prop_emp_amarelos = emprego_foral_raca_cor_Amarelos/emprego_foral_all,
         emp_formal = emprego_foral_all/10^6) %>%
  select(municipio, ano, emp_formal, prop_emp_pretos_pardos, prop_emp_indigenas, prop_emp_bracos, prop_emp_amarelos)


# Tabela 4: AGROALIMENTAR total de empregos formais e por cor/raça (teste Curitiba)
df_rais %>% 
  filter(id_municipio == mun_curitiba, unidade_analise == 'Capital_agro') %>% 
  arrange(ano) %>% 
  mutate(municipio = 'Curitiba', 
         prop_emp_pretos_pardos = emprego_foral_raca_cor_Negros/emprego_foral_all,
         prop_emp_indigenas = emprego_foral_raca_cor_Indigena/emprego_foral_all,
         prop_emp_bracos_amarelos = 1 - (prop_emp_pretos_pardos + prop_emp_indigenas),
         emp_formal = emprego_foral_all/10^6) %>%
  select(municipio, ano, emp_formal, prop_emp_pretos_pardos, prop_emp_indigenas, prop_emp_bracos_amarelos)








# UF
df_rais %>% filter(UF == 41) %>% head()

# Unidade de Análise RM de Curitiba 
df_rais %>% filter(nome_regiao_metropolitana == 'Região Metropolitana de Curitiba') %>% View()

# Curitiba
df_rais %>% filter(id_municipio == 4106902)


