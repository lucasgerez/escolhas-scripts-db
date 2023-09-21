# ------------------------------------------------------------------- #
#                     POF: SISTEMATIZAÇÃO DOS DADOS 
# ------------------------------------------------------------------- #

# Projeto: Instituto Escolhas
# Autor: Lucas Gerez Foratto

# Para mais informações do que é a POF:
# https://www.ibge.gov.br/estatisticas/sociais/trabalho/9050-pesquisa-de-orcamentos-familiares.html?=&t=o-que-e

# exemplo de tratamento da base
# https://rpubs.com/amrofi/microdados_pof

# Importante: A pesquisa tem como unidade de investigação o domicílio e é realizada por amostragem

# O desenho atual da amostra da POF foi estruturado de tal modo que propicia a publicação de resultados
# nos seguintes níveis: Brasil, Grandes Regiões, e também por situações urbana e rural.

# Uma vez rodado o script get_pof.R, temos os microdados brutos e RDS

# Arquivos de documentação
# F:\Drive\BASES DE DADOS BRUTOS\POF\Microdados\Documentacao_20221226

# Para mais informações sobre o dicionário das variáveis: 
# Dicionários de váriaveis.xlsx

# Para mais informações sobre os estratos: 
# Estratos POF 2017-2018.xlsx
# Para mais informações sobre os estratos: 
# Estratos POF 2017-2018.xlsx
  # ESTRATO_POF	Identifica os estratos do plano amostral da pesquisa: 
  # estratificações Geográfica e Estatística. No nível geográfico, a estratificação compreende: 
  # área urbana para o município da capital, resto da região metropolitana, resto da UF e área rural. 
  # A estratificação estatística foi realizada a partir das definições implementadas na Amostra Mestra, 
  # que utiliza informações da variável renda total do domicílio, obtida a partir dos dados do Censo 2010.  

# Nota sobre os pesos: 
# PESO: UPA a nível de domicilio
# PESO_FINAL: já com os estratos


# Para a tabela de renda per capita vamos utilizar a variável de renda monetária



# Caminho para os dados da POF: 
setwd('F:/Drive/BASES DE DADOS BRUTOS/POF/Microdados/Dados')

# pacotes
library(dplyr)
library(survey) # para mexer com os dados amostrais

arquivos <- list.files()
arquivos <- arquivos[grep('.rds', arquivos)]


# Igual a tabela3 do estudo de SP Passos que precisamos fazer -----

  # Calcular a renda per capita total (DONE)
  # Calcular o decis de renda familiar per capita (DONE)
  # Calcular despesa total mensal familiar per capita
  # Calcular despesa em consumo mensal familiar per capita
  # Calcular gasto mensal per capita com alimentação fora de casa
  # Calcular gasto mensal per capita com alimentação em casa
  # Calcular participação alimentação nas despesas de consumo
  # Calcular participação alimentação fora de casa no gasto alimentar


format_real <- function(numero) {
  
  options(warn = -1)
  return( paste0("R$ ", prettyNum(round(numero,0), big.mark = ".", small.mark = ",")) )
  
}
  
## Parâmetros para o estudo de Curitiba ----

# UF Paraná com os estratos da capital e da região metropolitana
uf <- 41; estrato_capital <- 4101:4105; estrato_rmct <- 4101:4108

# vamos fazer um teste pra sp só pra checar (para testar com o estudo de sp)
# uf <- 35; estrato_capital <- 3501:3509; estrato_rmct <- 3501:3515

# Para nível capital e RM teremos apenas na situação de área urbana
tipo_situacao_dom <- 1

# variaveis de identificação do domicílio (para as chaves)
var_dom <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "NUM_UC")


### Tabela final com os resultados
tab_final <- data.frame(decis_renda = 1:10)

## calculando a renda per capita domiciliar (primeiro nomimal, depois deflacionamos para 2023) ----

# Apenas variaveis com informacoes das UC's no arquivo 'MORADOR.rds'
# Apenas um registro por UC
morador_uc <- unique(readRDS("MORADOR.rds")[, c(var_dom, "PESO_FINAL", "PC_RENDA_MONET") ])  


# Retringindo para a região de interesse
morador_df <- morador_uc %>% 
  filter(UF == uf,
         ESTRATO_POF %in% estrato_rmct) 

# Vamos definir o dataframe como survey
survey_design <- svydesign(ids = ~1, weights = ~PESO_FINAL, data = morador_df)

# Calculate the weighted deciles
deciles <- svyquantile(~PC_RENDA_MONET, 
                       survey_design, 
                       quantiles = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))

# Extract the deciles
weighted_deciles <- deciles$PC_RENDA_MONET[,1] 




# Inserindo na tabela final 
tab_final$renda_dom_pc <- paste0("R$ ", prettyNum(round(weighted_deciles[-1],0),big.mark = ".", small.mark = ","))
tab_final$renda_dom_pc[1] <- paste0("Até ", tab_final$renda_dom_pc[1])
tab_final$renda_dom_pc[10] <- paste0("Acima de ", tab_final$renda_dom_pc[9])


survey_design$variables$decis <- cut(survey_design$variables$PC_RENDA_MONET, 
                                     breaks = weighted_deciles,
                                     labels = F, 
                                     include.lowest = T, 
                                     na.pass = TRUE)

head(survey_design$variables)


# Dados de despesa ---- 


# Calculo das despesas total ----- 


# Calculo das despesas com alimentação ----- 
caderneta_coletiva <- readRDS("CADERNETA_COLETIVA.rds")
despesa_coletiva <- readRDS("DESPESA_COLETIVA.rds") # vai quadro 6 a 19 
despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds")


## Tratamento das variáveis ---- 
caderneta_coletiva <- 
  caderneta_coletiva %>%
  mutate(Codigo = round(V9001/100), # 5 dígitos
         valor_mensal = (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12) %>%
  filter(UF == uf, 
         ESTRATO_POF %in% estrato_rmct, # filtro para a região de interesse
         Codigo < 86001 | Codigo > 89999 )  %>% 
  as.data.frame()

despesa_coletiva <- 
  despesa_coletiva %>%
  mutate(Codigo = round(V9001/100), # 5 dígitos
         valor_mensal = ifelse(QUADRO %in% c(10,19),
                               (V8000_DEFLA * V9011 * FATOR_ANUALIZACAO * PESO_FINAL)/12,
                               (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12 
                               )) %>%
  filter(UF == uf, 
         ESTRATO_POF %in% estrato_rmct)  %>% 
  as.data.frame()


despesa_individual <- 
  despesa_individual %>%
  mutate(Codigo = round(V9001/100), # 5 dígitos
         valor_mensal = ifelse(QUADRO %in% c(24,41), 
                               (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12, 
                               (V8000_DEFLA * V9011 * FATOR_ANUALIZACAO * PESO_FINAL)/12)) %>%
  filter(UF == uf, 
         ESTRATO_POF %in% estrato_rmct, # filtro para a região de interesse
         QUADRO == 24 | Codigo %in% c(41001, 48018, 49075, 49089)
         )  %>% 
  as.data.frame()

# Vamos juntar as 3 tabelas para calcular a depesa total
gastos_all <- bind_rows(caderneta_coletiva,despesa_coletiva, despesa_individual)


# Vamos então atribuir a informção do decil da renda dom per capita para cada UC
merge_decis <- survey_design$variables %>% rename(peso_df_morador = PESO_FINAL) %>% as.data.frame()

# O total de familias deve ser calculado por decil
soma_familia <- merge_decis %>% 
  group_by(decis) %>%
  summarise(soma_familia = sum(peso_df_morador)) %>%
  filter(is.na(decis) == F)
  
# Atribuição das informações de decis na tabela de despesas totais
gastos_all <- gastos_all %>% left_join(merge_decis, 
                                     by = c('UF', 'ESTRATO_POF', 'TIPO_SITUACAO_REG', 'COD_UPA', 'NUM_DOM', 'NUM_UC')) 


### Tabelas de tradutores

# Tadutores de despesa geral 
tradutor_desp_geral <- readxl::read_excel("../Tradutores_de_Tabela/Tradutor_Despesa_Geral.xls") 

# Vamos ficar com os ids unicos
tradutor_desp_geral <- tradutor_desp_geral[duplicated(tradutor_desp_geral$Codigo)==F,]


# Informação de alimentação 
tradutor_alimentacao <- readxl::read_excel("../Tradutores_de_Tabela/Tradutor_Alimentação.xls") 


# Tabela com despesa total 

# Tem alguma possível dupla contagem aqui das depesas gerais com as demais

# Gastos totais por decil
tab1 <- 
  gastos_all %>% 
  left_join(tradutor_desp_geral, by = "Codigo") %>%
  filter(Descricao_0 == 'Despesa Total') %>%
  group_by(decis) %>%
  summarise( valor_mensal = sum(valor_mensal) ) %>% 
  left_join(soma_familia, by = 'decis') %>%
  mutate( valor_mensal = format_real(valor_mensal/soma_familia)  ) %>%
  filter(is.na(decis)==F) %>%
  rename(despesa_total = valor_mensal,
         decis_renda = decis) %>%
  select(decis_renda, despesa_total)


# Para Despesas em Consumo vams aplicar filter(Descricao_2 == 'Despesas de Consumo')
tab2 <- 
  gastos_all %>% 
  left_join(tradutor_desp_geral, by = "Codigo") %>%
  filter(Descricao_2 == 'Despesas de Consumo') %>%
  group_by(decis) %>%
  summarise( valor_mensal = sum(valor_mensal) ) %>% 
  left_join(soma_familia, by = 'decis') %>%
  mutate( valor_mensal = format_real(valor_mensal/soma_familia)  ) %>%
  filter(is.na(decis)==F) %>%
  rename(despesa_consumo = valor_mensal,
         decis_renda = decis) %>%
  select(decis_renda, despesa_consumo)

# Despesas em Alimentação (acho q aqui vale fazer pelo tab_alimentação)
tab3 <- 
  gastos_all %>% 
  left_join(tradutor_alimentacao, by = "Codigo") %>%
  filter(Descricao_0 == 'Alimentacao') %>%
  filter(is.na(Descricao_1) == F) %>%
  group_by(decis) %>%
  summarise( valor_mensal = sum(valor_mensal) ) %>% 
  left_join(soma_familia, by = 'decis') %>%
  mutate( valor_mensal = format_real(valor_mensal/soma_familia)  ) %>%
  filter(is.na(decis)==F) %>%
  rename(despesa_alimentacao = valor_mensal,
         decis_renda = decis) %>%
  select(decis_renda, despesa_alimentacao)

# Alimentação fora do domicilio
tab4 <- 
  gastos_all %>% 
  left_join(tradutor_alimentacao, by = "Codigo") %>%
  filter(Descricao_1 == 'Alimentação fora do domicílio') %>%
  filter(is.na(Descricao_1) == F) %>%
  group_by(decis) %>%
  summarise( valor_mensal = sum(valor_mensal) ) %>% 
  left_join(soma_familia, by = 'decis') %>%
  mutate( valor_mensal = format_real(valor_mensal/soma_familia)  ) %>%
  filter(is.na(decis)==F) %>%
  rename(despesa_alimentacao_fora_domicilio = valor_mensal,
         decis_renda = decis) %>%
  select(decis_renda, despesa_alimentacao_fora_domicilio)


tab_final %>% 
  left_join(tab1, by = 'decis_renda') %>%
  left_join(tab2, by = 'decis_renda') %>%
  left_join(tab3, by = 'decis_renda') %>%
  left_join(tab4, by = 'decis_renda') 
   




# Tabelas requeridas pelo Pedro

# Tabela 3. Distribuição da renda familiar, despesas e gasto alimentar mensais per capita por décimos de renda familiar per capita – RMSP, 2017/2018.
# Tabela 4. Evolução da participação dos gastos alimentares nas despesas gerais e da alimentação no domicílio sobre o gasto alimentar — RMSP e Brasil, 1987/88, 1995/96, 2002/03, 2008/09 e 2017/18.
# Tabela 5. Composição do orçamento alimentar segundo grupos de alimentos e tipos de alimentação fora do domicílio, por décimos de renda familiar per capita — RMSP, 2017/18.
# Figura 8. Distribuição das despesas de consumo por grupos de despesa, segundo décimos de renda familiar per capita – RMSP, 2017/2018.
# Figura 9. Participação da alimentação fora do domicílio no gasto alimentar, segundo décimos de renda familiar per capita – RMSP, 2017/2018.
# Figura 11. Evolução na quantidade adquirida para o domicílio por grupos de alimentos — RMSP 1987/88, 1995/96, 2002/03, 2008/09 e 2017/18.
# Figura 12. Distribuição do número de aquisições, por locais de compra por e grupos de alimentos — Brasil, 2008/09.




## Tab 1 ----  
# Gasto com alimentos dentro e fora de casa, por extrato de renda familiar,em relação aos outros tipos de despesas


## Tab 2 ----  
# Gasto com alimentos dentro e fora de casa, por extrato de renda familiar, tipo de alimento

## Tab 3 ----  
# Evolução da quantidade anual per capita adquirida por grupos alimentares

## Tab 4 ----  
# Locais de compra dos produtos alimentares

## Tab 5 ----  
# Prevalência de Insegurança Alimentar e Nutricional (IAN) nos domicílios 

## Tab 6 ----  
# Aquisição alimentar domiciliar em Curitiba, de acordo com a Classificação NOVA

## Tab 7 ----  
# Média de aquisição alimentar per capita anual








