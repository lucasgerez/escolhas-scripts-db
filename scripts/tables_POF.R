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

# Caminho para exportar as tabelas
export <- 'F:/Drive/Projetos/Escolhas/2023/Consultoria_Dados/Resultados/POF'


# pacotes
library(dplyr)
library(tidyr)
library(survey) # para mexer com os dados amostrais
library(openxlsx)

arquivos <- list.files()
arquivos <- arquivos[grep('.rds', arquivos)]


# Funcao com formatação para a tabela
format_real <- function(numero) {
  
  options(warn = -1)
  return( paste0("R$ ", prettyNum(round(numero,0), big.mark = ".", small.mark = ",")) )
  
}


# Igual a tabela3 do estudo de SP Passos que precisamos fazer -----

# Calcular a renda per capita total (DONE)
# Calcular o decis de renda familiar per capita (DONE)
# Calcular despesa total mensal familiar per capita
# Calcular despesa em consumo mensal familiar per capita
# Calcular gasto mensal per capita com alimentação fora de casa
# Calcular gasto mensal per capita com alimentação em casa
# Calcular participação alimentação nas despesas de consumo
# Calcular participação alimentação fora de casa no gasto alimentar


  
## Parâmetros para o estudo de Curitiba ----


# dá pra refatorar legal esse código, depois a gente pensa na possibilidade

# Vamos definir os conjuntos de estrados para ficar mais fácil nas funções
uf                          <- 41 
estrato_uf_com_rural        <- 4101:4135
estrato_uf_sem_rural        <- 4101:4124
estrato_uf_sem_rm_sem_rural <- 4108:4124
estrato_rm                  <- 4101:4108
estrato_rm_sem_capital      <- 4106:4108
estrato_capital             <- 4101:4105

# Todos os estratos que faremos
dimensoes <- c('estrato_uf_com_rural', 'estrato_uf_sem_rural',
               'estrato_uf_sem_rm_sem_rural', 'estrato_rm',
               'estrato_rm_sem_capital', 'estrato_capital')

# Share de despesas com alimentação -----
tabela_consumo_pc_pof_f <- function(uf, estrato, tipo_situacao_dom = 1) {
  
  # variaveis de identificação do domicílio (para as chaves)
  var_dom <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
               "COD_UPA", "NUM_DOM", "NUM_UC")
  
  
  ### Tabela final com os resultados
  tab_final <- data.frame(decis_renda = 1:10)
  
  ## calculando a renda per capita domiciliar (primeiro nomimal, depois deflacionamos para 2023) ----
  
  # Apenas variaveis com informacoes das UC's no arquivo 'MORADOR.rds'
  # Apenas um registro por UC
  
  num_pessoas <- readRDS("MORADOR.rds") %>% 
    filter(TIPO_SITUACAO_REG %in% tipo_situacao_dom) %>%
    group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
             COD_UPA, NUM_DOM, NUM_UC) %>% 
    summarise(pessoas_dom = max(COD_INFORMANTE))
  
  # Atribuindo o número de pessoas da familia
  morador_uc <- unique(readRDS("MORADOR.rds")[, c(var_dom, "PESO_FINAL", "PC_RENDA_MONET") ]) %>%
    filter(TIPO_SITUACAO_REG %in% tipo_situacao_dom) %>%
    left_join(num_pessoas, by = var_dom)
  
  # Retringindo para a região de interesse
  morador_df <- morador_uc %>% 
    filter(UF == uf,
           ESTRATO_POF %in% estrato) 
  
  
  # Big numbers
  # O total de familias deve ser calculado por decil
  soma_familia <- sum(morador_df$PESO_FINAL)
  soma_pessoas <- sum(morador_df$pessoas_dom*morador_df$PESO_FINAL)
  
  # Tamanho médio por familia 
  # weighted.mean(morador_df$pessoas_dom, w = morador_df$PESO_FINAL)
  
  # Vamos definir o dataframe como survey
  survey_design <- svydesign(ids = ~1, weights = ~PESO_FINAL, data = morador_df)
  
  # Calculate the weighted deciles
  deciles <- svyquantile(~PC_RENDA_MONET, 
                         subset(survey_design, subset = !is.na(PC_RENDA_MONET))  , 
                         quantiles = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
  
  # Extract the deciles
  weighted_deciles <- deciles$PC_RENDA_MONET[,1] 
  
  
  # Inserindo na tabela final 
  tab_final$renda_dom_pc <- paste0("R$ ", prettyNum(round(weighted_deciles[-1],0),big.mark = ".", small.mark = ","))
  tab_final$renda_dom_pc[1] <- paste0("Até ", tab_final$renda_dom_pc[1])
  tab_final$renda_dom_pc[10] <- paste0("Acima de ", tab_final$renda_dom_pc[9])
  
  
  # Atribuindo o decil que a pessoa está
  survey_design$variables$decis <- cut(survey_design$variables$PC_RENDA_MONET, 
                                       breaks = weighted_deciles,
                                       labels = F, 
                                       include.lowest = T, 
                                       na.pass = TRUE)
  
  
  
  
  # Dados de despesa ---- 
  
  
  # Calculo das despesas total ----- 
  
  
  # Calculo das despesas com alimentação ----- 
  caderneta_coletiva <- readRDS("CADERNETA_COLETIVA.rds")
  despesa_coletiva <- readRDS("DESPESA_COLETIVA.rds") # vai quadro 6 a 19 
  despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds")
  aluguel <- readRDS("ALUGUEL_ESTIMADO.rds")
  
  ## Tratamento das variáveis ---- 
  caderneta_coletiva <- 
    caderneta_coletiva %>%
    mutate(Codigo = round(V9001/100), # 5 dígitos
           valor_mensal = (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12) %>%
    filter(UF == uf, 
           ESTRATO_POF %in% estrato, # filtro para a região de interesse
           TIPO_SITUACAO_REG %in% tipo_situacao_dom,
           Codigo < 86001 | Codigo > 89999 )  %>% 
    as.data.frame()
  
  despesa_coletiva <- 
    despesa_coletiva %>%
    mutate(Codigo = round(V9001/100), # 5 dígitos
           valor_mensal = (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12) %>%
  filter(UF == uf, 
         ESTRATO_POF %in% estrato,
         TIPO_SITUACAO_REG %in% tipo_situacao_dom)  %>% 
  as.data.frame()
  
  despesa_individual <- 
    despesa_individual %>%
    mutate(Codigo = round(V9001/100), # Retirar os últimos 2 dígitos
           valor_mensal = ifelse(QUADRO %in% c(44, 47, 48, 49, 50), 
                                 (V8000_DEFLA * V9011 * FATOR_ANUALIZACAO * PESO_FINAL)/12,
                                 (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12)) %>%
    filter(UF == uf, 
           ESTRATO_POF %in% estrato, # filtro para a região de interesse
           TIPO_SITUACAO_REG %in% tipo_situacao_dom
    )  %>% 
    as.data.frame()
  
  # Ainda em habitação faltam alguns detalhes possivelmente relacionados ao fator de anualização
  aluguel <- aluguel %>% 
    filter(UF == uf, 
           ESTRATO_POF %in% estrato,
           TIPO_SITUACAO_REG %in% tipo_situacao_dom) %>%
    mutate(Codigo = round(V9001/100), 
           valor_mensal = V8000_DEFLA* PESO_FINAL)
  
  
  # Vamos juntar as tabelas para calcular a depesa total
  gastos_all <- bind_rows(caderneta_coletiva, despesa_coletiva, despesa_individual, aluguel)
  
  
  # Vamos então atribuir a informação do decil da renda dom per capita para cada UC
  merge_decis <- survey_design$variables %>% rename(peso_df_morador = PESO_FINAL) %>% as.data.frame()
  
  # O total de familias deve ser calculado por decil
  soma_familia <- merge_decis %>% 
    group_by(decis) %>%
    summarise(soma_familia = sum(peso_df_morador),
              soma_pessoas = sum(pessoas_dom*peso_df_morador)) %>%
    filter(is.na(decis) == F)
  
  # Atribuição das informações de decis na tabela de despesas totais
  gastos_all <- gastos_all %>% 
    left_join(merge_decis, 
              by = c('UF', 'ESTRATO_POF', 'TIPO_SITUACAO_REG', 'COD_UPA', 'NUM_DOM', 'NUM_UC')) 
  
  
  ### Tabelas de tradutores
  # Tadutores de despesa geral 
  tradutor_desp_geral <- readxl::read_excel("../Tradutores_de_Tabela/Tradutor_Despesa_Geral.xls") 
  
  # Vamos ficar com os ids unicos
  tradutor_desp_geral <- tradutor_desp_geral[duplicated(tradutor_desp_geral$Codigo)==F,]
  
  # Informação de alimentação 
  tradutor_alimentacao <- readxl::read_excel("../Tradutores_de_Tabela/Tradutor_Alimentação.xls") 
  
  
  # Gastos totais por decil
  tab1 <- 
    gastos_all %>% 
    left_join(tradutor_desp_geral, by = "Codigo") %>%
    filter(Descricao_0 == 'Despesa Total') %>%
    group_by(decis) %>%
    summarise( valor_mensal = sum(valor_mensal) ) %>% 
    left_join(soma_familia, by = 'decis') %>%
    mutate( valor_mensal = format_real(valor_mensal/soma_pessoas)  ) %>%
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
    mutate( valor_mensal = format_real(valor_mensal/soma_pessoas)  ) %>%
    filter(is.na(decis)==F) %>%
    rename(despesa_consumo = valor_mensal,
           decis_renda = decis) %>%
    select(decis_renda, despesa_consumo)
  
  # Despesas em Alimentação no domicilio
  tab3 <- 
    gastos_all %>% 
    left_join(tradutor_alimentacao, by = "Codigo") %>%
    filter(Descricao_1 == 'Alimentação no domicílio') %>%
    filter(is.na(Descricao_1) == F) %>%
    group_by(decis) %>%
    summarise( valor_mensal = sum(valor_mensal) ) %>% 
    left_join(soma_familia, by = 'decis') %>%
    mutate( valor_mensal = format_real(valor_mensal/soma_pessoas)  ) %>%
    filter(is.na(decis)==F) %>%
    rename(despesa_alimentacao_no_domicilio = valor_mensal,
           decis_renda = decis) %>%
    select(decis_renda, despesa_alimentacao_no_domicilio)
  
  
  # Alimentação fora do domicilio
  tab4 <- 
    gastos_all %>% 
    left_join(tradutor_alimentacao, by = "Codigo") %>%
    filter(Descricao_1 == 'Alimentação fora do domicílio') %>%
    filter(is.na(Descricao_1) == F) %>%
    group_by(decis) %>%
    summarise( valor_mensal = sum(valor_mensal) ) %>% 
    left_join(soma_familia, by = 'decis') %>%
    mutate( valor_mensal = format_real(valor_mensal/soma_pessoas)  ) %>%
    filter(is.na(decis)==F) %>%
    rename(despesa_alimentacao_fora_domicilio = valor_mensal,
           decis_renda = decis) %>%
    select(decis_renda, despesa_alimentacao_fora_domicilio)
  
  tab_final <- 
    tab_final %>% 
    left_join(tab1, by = 'decis_renda') %>%
    left_join(tab2, by = 'decis_renda') %>%
    left_join(tab3, by = 'decis_renda') %>%
    left_join(tab4, by = 'decis_renda') 
  
  return(tab_final)
  
}

lst.despesas <- list()

for (d in dimensoes) {
  
  cat('\nDimensão', d, paste(Sys.time()))
  
  if (d == 'estrato_uf_com_rural') {
    
    lst.despesas[[match(d, dimensoes)]] <- tabela_consumo_pc_pof_f(uf = uf, estrato = get(d), tipo_situacao_dom = c(1:2))
    
  } else {
    
    lst.despesas[[match(d, dimensoes)]] <- tabela_consumo_pc_pof_f(uf = uf, estrato = get(d), tipo_situacao_dom = 1)
    
  }
  
}

# nomes para facilitar
names(lst.despesas) <- dimensoes


# Gastos com in natura, culinario, processado, ultraprocessado ------

# Ainda precisamos melhorar essa funcao
tabela_ultra_pc_pof_f <- function(uf, estrato, tipo_situacao_dom = 1) {
  
  
  # variaveis de identificação do domicílio (para as chaves)
  var_dom <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
               "COD_UPA", "NUM_DOM", "NUM_UC")
  
  
  ### Tabela final com os resultados
  tab_final <- data.frame(decis_renda = 1:10)
  
  ## calculando a renda per capita domiciliar (primeiro nomimal, depois deflacionamos para 2023) ----
  
  # Apenas variaveis com informacoes das UC's no arquivo 'MORADOR.rds'
  # Apenas um registro por UC
  
  num_pessoas <- readRDS("MORADOR.rds") %>% 
    group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
             COD_UPA, NUM_DOM, NUM_UC) %>% 
    summarise(pessoas_dom = max(COD_INFORMANTE))
  
  # Atribuindo o número de pessoas da familia
  morador_uc <- unique(readRDS("MORADOR.rds")[, c(var_dom, "PESO_FINAL", "PC_RENDA_MONET") ]) %>%
    left_join(num_pessoas, by = var_dom)
  
  # Retringindo para a região de interesse
  morador_df <- morador_uc %>% 
    filter(UF == uf,
           ESTRATO_POF %in% estrato) 
  
  
  # Big numbers
  # O total de familias deve ser calculado por decil
  soma_familia <- sum(morador_df$PESO_FINAL)
  soma_pessoas <- sum(morador_df$pessoas_dom*morador_df$PESO_FINAL)
  
  # Tamanho médio por familia 
  # weighted.mean(morador_df$pessoas_dom, w = morador_df$PESO_FINAL)
  
  
  # Vamos definir o dataframe como survey
  survey_design <- svydesign(ids = ~1, weights = ~PESO_FINAL, data = morador_df)
  
  # Calculate the weighted deciles
  deciles <- svyquantile(~PC_RENDA_MONET, 
                         subset(survey_design, subset = !is.na(PC_RENDA_MONET))  , 
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
  
  
  # Calculo das despesas com alimentação ----- 
  caderneta_coletiva <- readRDS("CADERNETA_COLETIVA.rds")
  despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds")
  
  ## Tratamento das variáveis ---- 
  caderneta_coletiva <- 
    caderneta_coletiva %>%
    mutate(Codigo = round(V9001/100), # 5 dígitos
           valor_mensal = (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12) %>%
    filter(UF == uf, 
           ESTRATO_POF %in% estrato, # filtro para a região de interesse
           Codigo < 86001 | Codigo > 89999 )  %>% 
    as.data.frame()
  
  
  despesa_individual <- 
    despesa_individual %>%
    mutate(Codigo = round(V9001/100), # Retirar os últimos 2 dígitos
           valor_mensal = ifelse(QUADRO %in% c(44, 47, 48, 49, 50), 
                                 (V8000_DEFLA * V9011 * FATOR_ANUALIZACAO * PESO_FINAL)/12,
                                 (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12)) %>%
    filter(UF == uf, 
           ESTRATO_POF %in% estrato, # filtro para a região de interesse
    )  %>% 
    as.data.frame()
  
 
  
  # Vamos juntar as tabelas para calcular a depesa total
  gastos_all <- bind_rows(caderneta_coletiva,despesa_individual)
  
  
  # Vamos então atribuir a informção do decil da renda dom per capita para cada UC
  merge_decis <- survey_design$variables %>% rename(peso_df_morador = PESO_FINAL) %>% as.data.frame()
  
  # O total de familias deve ser calculado por decil
  soma_familia <- merge_decis %>% 
    group_by(decis) %>%
    summarise(soma_familia = sum(peso_df_morador),
              soma_pessoas = sum(pessoas_dom*peso_df_morador)) %>%
    filter(is.na(decis) == F)
  
  # Atribuição das informações de decis na tabela de despesas totais
  gastos_all <- gastos_all %>% 
    left_join(merge_decis, 
              by = c('UF', 'ESTRATO_POF', 'TIPO_SITUACAO_REG', 'COD_UPA', 'NUM_DOM', 'NUM_UC')) 
  
  # Informação de alimentação 
  tradutor_alimentacao <- readxl::read_excel("../Tradutores_de_Tabela/Tradutor_Alimentação.xls") 
  
  
  ### Tabelas de tradutores para ultra processados 
  tradutor_ultra <- readxl::read_excel("../Tradutores_de_Tabela/Cadastro de Produtos POF 2019.xlsx", range = "A1:D8322") 
  
  # Vamos ficar apenas com os que possuem informações
  names(tradutor_ultra) <- c('quadro', 'V9001', 'Desc', 'Grupo_Processado')
  
  tradutor_ultra <- tradutor_ultra %>%
    filter(is.na(Grupo_Processado)==F) %>% 
    select(V9001, Grupo_Processado )
  
  
  # Despesas em Alimentação 
  tab1 <- 
    gastos_all %>% 
    left_join(tradutor_alimentacao, by = "Codigo") %>%
    left_join(tradutor_ultra, by = "V9001") %>%
    filter(Descricao_0 == 'Alimentacao') %>%
    filter(is.na(Descricao_1) == F, Grupo_Processado == 1) %>%
    group_by(decis) %>%
    summarise( valor_mensal = sum(valor_mensal) ) %>% 
    left_join(soma_familia, by = 'decis') %>%
    mutate( valor_mensal = format_real(valor_mensal/soma_pessoas)  ) %>%
    filter(is.na(decis)==F) %>%
    rename(despesa_in_natura = valor_mensal,
           decis_renda = decis) %>%
    select(decis_renda, despesa_in_natura)
  
  # Despesas em Alimentação 
  tab2 <- 
    gastos_all %>% 
    left_join(tradutor_alimentacao, by = "Codigo") %>%
    left_join(tradutor_ultra, by = "V9001") %>%
    filter(Descricao_0 == 'Alimentacao') %>%
    filter(is.na(Descricao_1) == F, Grupo_Processado == 2) %>%
    group_by(decis) %>%
    summarise( valor_mensal = sum(valor_mensal) ) %>% 
    left_join(soma_familia, by = 'decis') %>%
    mutate( valor_mensal = format_real(valor_mensal/soma_pessoas)  ) %>%
    filter(is.na(decis)==F) %>%
    rename(despesa_ing_culinario = valor_mensal,
           decis_renda = decis) %>%
    select(decis_renda, despesa_ing_culinario)
  
  # Despesas em Alimentação 
  tab3 <- 
    gastos_all %>% 
    left_join(tradutor_alimentacao, by = "Codigo") %>%
    left_join(tradutor_ultra, by = "V9001") %>%
    filter(Descricao_0 == 'Alimentacao') %>%
    filter(is.na(Descricao_1) == F, Grupo_Processado == 3) %>%
    group_by(decis) %>%
    summarise( valor_mensal = sum(valor_mensal) ) %>% 
    left_join(soma_familia, by = 'decis') %>%
    mutate( valor_mensal = format_real(valor_mensal/soma_pessoas)  ) %>%
    filter(is.na(decis)==F) %>%
    rename(despesa_processado = valor_mensal,
           decis_renda = decis) %>%
    select(decis_renda, despesa_processado)
  
  # Despesas em Alimentação 
  tab4 <- 
    gastos_all %>% 
    left_join(tradutor_alimentacao, by = "Codigo") %>%
    left_join(tradutor_ultra, by = "V9001") %>%
    filter(Descricao_0 == 'Alimentacao') %>%
    filter(is.na(Descricao_1) == F, Grupo_Processado == 4) %>%
    group_by(decis) %>%
    summarise( valor_mensal = sum(valor_mensal) ) %>% 
    left_join(soma_familia, by = 'decis') %>%
    mutate( valor_mensal = format_real(valor_mensal/soma_pessoas)  ) %>%
    filter(is.na(decis)==F) %>%
    rename(despesa_ultraprocessado = valor_mensal,
           decis_renda = decis) %>%
    select(decis_renda, despesa_ultraprocessado)
  
  tab_final <- 
    tab_final %>% 
    left_join(tab1, by = 'decis_renda') %>%
    left_join(tab2, by = 'decis_renda') %>%
    left_join(tab3, by = 'decis_renda') %>%
    left_join(tab4, by = 'decis_renda') 
  
  return(tab_final)
  
}

lst.ultra <- list()

for (d in dimensoes) {
  
  
  cat('\nDimensão', d, paste(Sys.time()))
  
  if (d == 'estrato_uf_com_rural') {
    
    lst.ultra[[match(d, dimensoes)]] <- tabela_ultra_pc_pof_f(uf = uf, estrato = get(d), tipo_situacao_dom = c(1:2))
    
  } else {
    
    lst.ultra[[match(d, dimensoes)]] <- tabela_ultra_pc_pof_f(uf = uf, estrato = get(d), tipo_situacao_dom = 1)
    
  }
  
}

# nomes para facilitar
names(lst.ultra) <- dimensoes



# Decomposição das despesas com alimentação por grupos de alimentos consumidos no e fora do domicílio e faixas de renda ------

tabela_share_tipo_alimento_pof_f <- function(uf, estrato, tipo_situacao_dom = 1) {
  
  
  # variaveis de identificação do domicílio (para as chaves)
  var_dom <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
               "COD_UPA", "NUM_DOM", "NUM_UC")
  
  # vamos mudar aqui pq será as colunas
  
  ### Tabela final com os resultados
  tab_final <- data.frame(decis_renda = 1:10)
  
  ## calculando a renda per capita domiciliar (primeiro nomimal, depois deflacionamos para 2023) ----
  
  # Apenas variaveis com informacoes das UC's no arquivo 'MORADOR.rds'
  # Apenas um registro por UC
  
  num_pessoas <- readRDS("MORADOR.rds") %>% 
    filter(TIPO_SITUACAO_REG %in% tipo_situacao_dom) %>%
    group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
             COD_UPA, NUM_DOM, NUM_UC) %>% 
    summarise(pessoas_dom = max(COD_INFORMANTE))
  
  # Atribuindo o número de pessoas da familia
  morador_uc <- unique(readRDS("MORADOR.rds")[, c(var_dom, "PESO_FINAL", "PC_RENDA_MONET") ]) %>%
    filter(TIPO_SITUACAO_REG %in% tipo_situacao_dom) %>%
    left_join(num_pessoas, by = var_dom)
  
  # Retringindo para a região de interesse
  morador_df <- morador_uc %>% 
    filter(UF == uf,
           ESTRATO_POF %in% estrato) 
  
  
  # Big numbers
  # O total de familias deve ser calculado por decil
  soma_familia <- sum(morador_df$PESO_FINAL)
  soma_pessoas <- sum(morador_df$pessoas_dom*morador_df$PESO_FINAL)
  
  # Tamanho médio por familia 
  weighted.mean(morador_df$pessoas_dom, w = morador_df$PESO_FINAL)
  
  
  # Vamos definir o dataframe como survey
  survey_design <- svydesign(ids = ~1, weights = ~PESO_FINAL, data = morador_df)
  
  # Calculate the weighted deciles
  deciles <- svyquantile(~PC_RENDA_MONET, 
                         subset(survey_design, subset = !is.na(PC_RENDA_MONET))  , 
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
  
  # Calculo das despesas total com alimentacao ----- 
  
  
  # Calculo das despesas com alimentação ----- 
  caderneta_coletiva <- readRDS("CADERNETA_COLETIVA.rds")
  despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds")
  
  ## Tratamento das variáveis ---- 
  caderneta_coletiva <- 
    caderneta_coletiva %>%
    mutate(Codigo = round(V9001/100), # 5 dígitos
           valor_mensal = (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12) %>%
    filter(UF == uf, 
           ESTRATO_POF %in% estrato, # filtro para a região de interesse
           TIPO_SITUACAO_REG %in% tipo_situacao_dom,
           Codigo < 86001 | Codigo > 89999 )  %>% 
    as.data.frame()
  
  despesa_individual <- 
    despesa_individual %>%
    mutate(Codigo = round(V9001/100), # Retirar os últimos 2 dígitos
           valor_mensal = ifelse(QUADRO %in% c(44, 47, 48, 49, 50), 
                                 (V8000_DEFLA * V9011 * FATOR_ANUALIZACAO * PESO_FINAL)/12,
                                 (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12)) %>%
    filter(UF == uf, 
           ESTRATO_POF %in% estrato, # filtro para a região de interesse
           TIPO_SITUACAO_REG %in% tipo_situacao_dom
    )  %>% 
    as.data.frame()
  
  
  # Vamos juntar as tabelas para calcular a depesa total com alimentacao
  gastos_all <- bind_rows(caderneta_coletiva, despesa_individual)
  
  # Vamos então atribuir a informção do decil da renda dom per capita para cada UC
  merge_decis <- survey_design$variables %>% rename(peso_df_morador = PESO_FINAL) %>% as.data.frame()
  
  # O total de familias deve ser calculado por decil
  soma_familia <- merge_decis %>% 
    group_by(decis) %>%
    summarise(soma_familia = sum(peso_df_morador),
              soma_pessoas = sum(pessoas_dom*peso_df_morador)) %>%
    filter(is.na(decis) == F)
  
  # Atribuição das informações de decis na tabela de despesas totais
  gastos_all <- gastos_all %>% 
    left_join(merge_decis, 
              by = c('UF', 'ESTRATO_POF', 'TIPO_SITUACAO_REG', 'COD_UPA', 'NUM_DOM', 'NUM_UC')) 
  
  
  # Informação de alimentação 
  tradutor_alimentacao <- readxl::read_excel("../Tradutores_de_Tabela/Tradutor_Alimentação.xls") 
  
  # Vamos criar uma subcategoria da descrição 2 que agregue em outros
  
  itens <- c('Cereais, leguminosas e oleaginosas', 
             'Farinhas, féculas e massas',
             'Tubérculos e raízes', 
             'Açúcares e derivados',
             'Legumes e verduras',
             'Frutas',
             'Carnes, vísceras, pescados',
             'Aves e ovos',
             'Leites e derivados',
             'Panificados',
             'Óleos e gorduras',
             'Bebidas e infusões',
             'Enlatados e conservas',
             'Sal e condimentos',
             'Alimentos preparados')
  
  # Classificação para os itens fora do domicilio
  itens_fora <- c('Café, leite, chocolate', 
                  'Sanduíches e salgados',
                  'Lanches', 
                  'Almoço e jantar',
                  'Bebidas',
                  'Outros')
  
  tradutor_alimentacao <- tradutor_alimentacao %>% 
    mutate( Descricao_4 = ifelse(Descricao_2 %in% itens, Descricao_2, 'Outros alimentos'),
            Descricao_5 = ifelse(Descricao_2 %in% itens_fora, Descricao_2, 'Outros alimentos'))
  
  
  # Gastos com alimentação NO domicilio
  gastos_alimentacao <-
    gastos_all %>%
    left_join(tradutor_alimentacao, by = "Codigo") %>%
    left_join(soma_familia, by = 'decis') %>% # head()
    filter(is.na(Descricao_0) == F, 
           Descricao_1 == 'Alimentação no domicílio') %>%
    group_by(decis, Descricao_4) %>%
    summarise(valor_mensal_pc = sum(valor_mensal, na.rm = T)/(mean(soma_pessoas, na.rm = T))) %>%
    pivot_wider(
      names_from = decis,
      values_from = valor_mensal_pc
    )
  
  names(gastos_alimentacao) <- c('Grupos_alimentos', paste0('decil', 1:10))
  
  
  # Outros alimentos aqui ainda está beeem estranho
  gastos_alimentacao_aux <- 
    gastos_alimentacao %>% 
    summarise(decil1 = 100 * decil1/sum(decil1, na.rm = T),
              decil2 = 100 * decil2/sum(decil2, na.rm = T),
              decil3 = 100 * decil3/sum(decil3, na.rm = T),
              decil4 = 100 * decil4/sum(decil4, na.rm = T),
              decil5 = 100 * decil5/sum(decil5, na.rm = T),
              decil6 = 100 * decil6/sum(decil6, na.rm = T),
              decil7 = 100 * decil7/sum(decil7, na.rm = T),
              decil8 = 100 * decil8/sum(decil8, na.rm = T),
              decil9 = 100 * decil9/sum(decil9, na.rm = T),
              decil10 = 100 * decil10/sum(decil10, na.rm = T))
  
  gastos_alimentacao <- cbind(data.frame(grupo_alimentos = gastos_alimentacao$Grupos_alimentos), gastos_alimentacao_aux)
  
  # FORA DO DOMICILIO
  # Gastos com alimentação NO domicilio
  gastos_alimentacao_fora <-
    gastos_all %>%
    left_join(tradutor_alimentacao, by = "Codigo") %>%
    left_join(soma_familia, by = 'decis') %>% # head()
    filter(is.na(Descricao_0) == F, 
           Descricao_1 == 'Alimentação fora do domicílio') %>%
    group_by(decis, Descricao_5) %>%
    summarise(valor_mensal_pc = sum(valor_mensal, na.rm = T)/(mean(soma_pessoas, na.rm = T))) %>%
    pivot_wider(
      names_from = decis,
      values_from = valor_mensal_pc
    )
  
  names(gastos_alimentacao_fora) <- c('Grupos_alimentos', paste0('decil', 1:10))
  
  
  # Outros alimentos aqui ainda está beeem estranho
  gastos_alimentacao_fora_aux <- 
    gastos_alimentacao_fora %>% 
    summarise(decil1 = 100 * decil1/sum(decil1, na.rm = T),
              decil2 = 100 * decil2/sum(decil2, na.rm = T),
              decil3 = 100 * decil3/sum(decil3, na.rm = T),
              decil4 = 100 * decil4/sum(decil4, na.rm = T),
              decil5 = 100 * decil5/sum(decil5, na.rm = T),
              decil6 = 100 * decil6/sum(decil6, na.rm = T),
              decil7 = 100 * decil7/sum(decil7, na.rm = T),
              decil8 = 100 * decil8/sum(decil8, na.rm = T),
              decil9 = 100 * decil9/sum(decil9, na.rm = T),
              decil10 = 100 * decil10/sum(decil10, na.rm = T))
  
  gastos_alimentacao_fora <- cbind( data.frame(grupo_alimentos = gastos_alimentacao_fora$Grupos_alimentos), gastos_alimentacao_fora_aux)
  
  
  # Vamos juntar as duas tabelas com a identificação dentro e fora do domicílio
  gastos_alimentacao <- cbind( data.frame(grupo = 'Alimentação no domicílio'),  gastos_alimentacao)
  gastos_alimentacao_fora <- cbind( data.frame(grupo = 'Alimentação fora do domicílio'),  gastos_alimentacao_fora)
  
  
  return(rbind(gastos_alimentacao, gastos_alimentacao_fora))
  
}

lst.decomp <- list()

for (d in dimensoes) {
  
  cat('\nDimensão', d, paste(Sys.time()))
  
  tipo_dom <- ifelse(d == 'estrato_uf_com_rural', c(1:2), 1)
  
  if (d == 'estrato_uf_com_rural') {
    
    lst.decomp[[match(d, dimensoes)]] <- tabela_share_tipo_alimento_pof_f(uf = uf, estrato = get(d), tipo_situacao_dom = c(1:2))
    
  } else {
    
    lst.decomp[[match(d, dimensoes)]] <- tabela_share_tipo_alimento_pof_f(uf = uf, estrato = get(d), tipo_situacao_dom = 1)
    
  }
  
  
}

# nomes para facilitar
names(lst.decomp) <- dimensoes


# Lista de share de consumo de alimento -----
pof_alimentos_f <- function(uf, estrato, tipo_situacao_dom = 1) {
  
  # Bases necessárias
  MORADOR <- readRDS("MORADOR.rds") %>%
    filter(TIPO_SITUACAO_REG %in% tipo_situacao_dom,
           UF == uf,
           ESTRATO_POF %in% estrato) %>%
    rename(M_PESO_FINAL = PESO_FINAL,
           M_PESO = PESO)
  
  CONSUMO_ALIMENTAR <- readRDS("CONSUMO_ALIMENTAR.rds") %>%
    filter(TIPO_SITUACAO_REG %in% tipo_situacao_dom,
           UF == uf,
           ESTRATO_POF %in% estrato) %>%
    rename(COD_INFORMANTE = COD_INFOR.MANTE, 
           CA_PESO_FINAL = PESO_FINAL,
           CA_PESO = PESO)
  
  
  # Código para identificar a pessoa
  var_pessoa <- c( 'UF', 'ESTRATO_POF', 'TIPO_SITUACAO_REG', 'COD_UPA', 'NUM_DOM', 'NUM_UC', 'COD_INFORMANTE', 'RENDA_TOTAL')
  
  POF_completa <- full_join( MORADOR, CONSUMO_ALIMENTAR, by = var_pessoa) 
  rm(MORADOR, CONSUMO_ALIMENTAR); gc()
  
  POF_completa <- POF_completa %>% 
    rename("Area_Habitacao"="TIPO_SITUACAO_REG",
           "Sexo"="V0404",
           "Idade"="V0403",
           "Cor"="V0405",
           "Cod_alimento"="V9001",
           "PREPARACAO"="V9016")
  
  POF_completa$Area_Habitacao <- recode(POF_completa$Area_Habitacao, '1' = "urbano", '2' = "rural")
  POF_completa$Sexo <- recode(POF_completa$Sexo, '1' = "homem", '2' = "mulher")
  POF_completa$Cor <- recode(POF_completa$Cor, '1'= "branco",'2' ="preta" ,'3'="amarela", '4'="parda", '5'="indigena", '9'="SD")
  POF_completa$INSTRUCAO <- recode(POF_completa$INSTRUCAO, '1'= "Sem instrucao",'2' ="Fundamental Incompleto" ,'3'="Fundamental Completo", '4'="Medio Incompleto", '5'="Medio Completo",'6'="Superior Incompleto",'7'="Superior Completo")
  POF_completa$Preparacao <- recode(POF_completa$PREPARACAO, '1'= "Assado",'2' ="Cozido_gord" ,'3'="Cozido_sgordura", '4'="cru", '5'="Empanado",'6'="Ensopado",'7'="Frito", '8'="Grelhado", '9'="Refogado", '99'= "NA")
  
  # Código da pessoa
  POF_completa <- transform(POF_completa, control = paste0(COD_UPA,NUM_DOM,NUM_UC,COD_INFORMANTE) )
  
  
  # Vamos juntar com a tabela de alimentos
  # Informação de alimentação 
  tradutor_alimentacao <- readxl::read_excel("../Tradutores_de_Tabela/Tradutor_Alimentação.xls") 
  
  
  # Vamos pegar o cadastro de consumo dos ultraprocessados tbm 
  ### Tabelas de tradutores para ultra processados 
  tradutor_ultra <- readxl::read_excel("../Tradutores_de_Tabela/Cadastro de Produtos POF 2019.xlsx", range = "A1:D8322") 
  
  # Vamos ficar apenas com os que possuem informações
  names(tradutor_ultra) <- c('quadro', 'Cod_alimento', 'Desc', 'Grupo_Processado')
  
  tradutor_ultra <- tradutor_ultra %>%
    filter(is.na(Grupo_Processado)==F) %>% 
    select(Cod_alimento, Grupo_Processado )
  
  tradutor_ultra$Grupo_Processado <- recode(tradutor_ultra$Grupo_Processado, 
                                            '1' = "in natura e minimamente processado",
                                            '2' = "ingrediente culinário" ,
                                            '3' = "processado",
                                            '4' = "ultraprocessado")
  
  POF_completa <- POF_completa %>%
    mutate(Codigo = round(Cod_alimento/100)) %>%  
    left_join(tradutor_alimentacao, by = "Codigo") %>%
    left_join(tradutor_ultra, by = "Cod_alimento")
  
  
  # Vamos calcular quantas pessoas temos no Brasil
  pessoas_h_m <- 
    POF_completa %>% 
    distinct(control, .keep_all = TRUE) %>%
    group_by(Sexo) %>%
    summarise(pessoas = sum(M_PESO_FINAL)) 
  
  # Consumo por tipo de processamento
  tab_ultra <- 
    POF_completa  %>%  
    group_by(control, M_PESO_FINAL, Sexo, Grupo_Processado) %>% 
    summarise(sum=sum(QTD,na.rm=TRUE)) %>% 
    group_by(Sexo, Grupo_Processado) %>% 
    summarise(mean = sum(sum*M_PESO_FINAL),
              pessoas_comem = sum(M_PESO_FINAL)) %>%
    left_join(pessoas_h_m, by = 'Sexo') %>%
    mutate(consumo = mean/pessoas,
           consumo_comem = mean/pessoas_comem) %>% 
    filter(!is.na(Grupo_Processado)) %>% 
    group_by(Sexo) %>% 
    mutate(share_pessoas_comem = round(100*pessoas_comem/pessoas,2),
           share_consumo = round(100*consumo/sum(consumo),2)) %>%
    select(Sexo, Grupo_Processado, share_pessoas_comem, share_consumo) %>%
    pivot_wider(id_cols = Grupo_Processado, names_from = Sexo, values_from = c("share_pessoas_comem", "share_consumo"))
  
  
  # Share de consumo por grupo de alimento (faz sentido bebidas e infusões representarem 50%?) 
  tab_share_consumo <- 
    POF_completa  %>%  
    group_by(control, M_PESO_FINAL, Sexo, Descricao_2) %>% 
    summarise(sum=sum(QTD,na.rm=TRUE)) %>% 
    group_by(Sexo, Descricao_2) %>% 
    summarise(mean = sum(sum*M_PESO_FINAL),
              pessoas_comem = sum(M_PESO_FINAL)) %>%
    left_join(pessoas_h_m, by = 'Sexo') %>%
    mutate(consumo = mean/pessoas,
           consumo_comem = mean/pessoas_comem) %>% 
    filter(!is.na(Descricao_2)) %>% 
    group_by(Sexo) %>% 
    mutate(share_pessoas_comem = round(100*pessoas_comem/pessoas,2),
           share_consumo = round(100*consumo/sum(consumo),2)) %>%
    select(Sexo, Descricao_2, share_pessoas_comem, share_consumo) %>%
    pivot_wider(id_cols = Descricao_2, names_from = Sexo, values_from = c("share_pessoas_comem", "share_consumo"))
  
  
  lst.result <- list()
  
  lst.result$tab_ultra <- tab_ultra
  lst.result$tab_share_consumo <- tab_share_consumo
  
  
}


lst.alimentos <- list()
lst.ultra <- list()

for (d in dimensoes) {
  
  cat('\nDimensão', d, paste(Sys.time()))
  
  if (d == 'estrato_uf_com_rural') {
    
    lst_aux <- pof_alimentos_f(uf = uf, estrato = get(d), tipo_situacao_dom = c(1:2))
    
    lst.ultra[[match(d, dimensoes)]] <- lst_aux$tab_ultra
    lst.alimentos[[match(d, dimensoes)]] <- lst_aux$tab_share_consumo
    
    
  } else {
    
    lst_aux <- pof_alimentos_f(uf = uf, estrato = get(d), tipo_situacao_dom = 1)
    
    lst.ultra[[match(d, dimensoes)]] <- lst_aux$tab_ultra
    lst.alimentos[[match(d, dimensoes)]] <- lst_aux$tab_share_consumo
    
  }
  
  
}

# nomes para facilitar
names(lst.ultra) <- dimensoes
names(lst.alimentos) <- dimensoes


# Vamos exportar todas as tabelas 

library(writexl)

leia.me <- data.frame( identificador = c('01','02', '03', '04','05', '06'),
                       nivel_geografico = c('UF incluindo rural',
                                            'UF SEM incluir rural',
                                            'Regiões da UF fora da RM SEM incluir rural',
                                            'Região Metropolitana (RM)',
                                            'RM exceto capital (Curitiba)',
                                            'Capital (Curitiba)'))

sheets <- list("leia_me" = leia.me,
               "desp_01" = lst.despesas$estrato_uf_com_rural, 
               "desp_02" = lst.despesas$estrato_uf_sem_rural,
               "desp_03" = lst.despesas$estrato_uf_sem_rm_sem_rural,
               "desp_04" = lst.despesas$estrato_rm,
               "desp_05" = lst.despesas$estrato_rm_sem_capital,
               "desp_06" = lst.despesas$estrato_capital,
               
               "tipo_proc_01" = lst.ultra$estrato_uf_com_rural,
               "tipo_proc_02" = lst.ultra$estrato_uf_sem_rural,
               "tipo_proc_03" = lst.ultra$estrato_uf_sem_rm_sem_rural,
               "tipo_proc_04" = lst.ultra$estrato_rm,
               "tipo_proc_05" = lst.ultra$estrato_rm_sem_capital,
               "tipo_proc_06" = lst.ultra$estrato_capital,
               
               "alim_dentro_fora_dom_01" = lst.decomp$estrato_uf_com_rural, 
               "alim_dentro_fora_dom_02" = lst.decomp$estrato_uf_sem_rural,
               "alim_dentro_fora_dom_03" = lst.decomp$estrato_uf_sem_rm_sem_rural,
               "alim_dentro_fora_dom_04" = lst.decomp$estrato_rm,
               "alim_dentro_fora_dom_05" = lst.decomp$estrato_rm_sem_capital,
               "alim_dentro_fora_dom_06" = lst.decomp$estrato_capital,
               
               "consumo_alim_01" = lst.alimentos$estrato_uf_com_rural, 
               "consumo_alim_02" = lst.alimentos$estrato_uf_sem_rural,
               "consumo_alim_03" = lst.alimentos$estrato_uf_sem_rm_sem_rural,
               "consumo_alim_04" = lst.alimentos$estrato_rm,
               "consumo_alim_05" = lst.alimentos$estrato_rm_sem_capital,
               "consumo_alim_06" = lst.alimentos$estrato_capital,
               
               "consumo_tipo_proc_01" = lst.alimentos$estrato_uf_com_rural, 
               "consumo_tipo_proc_02" = lst.alimentos$estrato_uf_sem_rural,
               "consumo_tipo_proc_03" = lst.alimentos$estrato_uf_sem_rm_sem_rural,
               "consumo_tipo_proc_04" = lst.alimentos$estrato_rm,
               "consumo_tipo_proc_05" = lst.alimentos$estrato_rm_sem_capital,
               "consumo_tipo_proc_06" = lst.alimentos$estrato_capital
               
               ) 

write_xlsx(sheets, "F:/Drive/Projetos/Escolhas/2023/Consultoria_Dados/Resultados/POF/pof_tabelas.xlsx")




# 
# 
# 
# 
# # Consumo alimentar médio per capita (g/dia) por grupo de alimento ----
# 
# tabela_consumo_alimento_gramas_pof_f <- function(uf, estrato, tipo_situacao_dom = 1) {
#   
#   
#   # variaveis de identificação do domicílio (para as chaves)
#   var_dom <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                "COD_UPA", "NUM_DOM", "NUM_UC")
#   
#   
#   # Bloco 1: familias e pessoas
#   num_pessoas <- readRDS("MORADOR.rds") %>% 
#     filter(TIPO_SITUACAO_REG %in% tipo_situacao_dom) %>%
#     group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
#              COD_UPA, NUM_DOM, NUM_UC) %>% 
#     summarise(pessoas_dom = max(COD_INFORMANTE))
#   
#   
#   # Atribuindo o número de pessoas da familia
#   morador_uc <- unique(readRDS("MORADOR.rds")[, c(var_dom, "PESO_FINAL", "PC_RENDA_MONET") ]) %>%
#     filter(TIPO_SITUACAO_REG %in% tipo_situacao_dom) %>%
#     left_join(num_pessoas, by = var_dom)
#   
#   
#   # Retringindo para a região de interesse
#   morador_df <- morador_uc %>% 
#     filter(UF == uf,
#            ESTRATO_POF %in% estrato) 
#   
#   
#   # Big numbers
#   # O total de familias deve ser calculado por decil
#   soma_familia <- sum(morador_df$PESO_FINAL)
#   soma_pessoas <- sum(morador_df$pessoas_dom*morador_df$PESO_FINAL)
#   
#   
#   # Consumo Alimentar 
#   consumo_alimentar <- readRDS("CONSUMO_ALIMENTAR.rds")
#   
#   ## Tratamento das variáveis ---- 
#   
#   # Tendo em vista que a frequência informada de consumo é semanal, vamos calcular por dia 
#   consumo_alimentar <- 
#     consumo_alimentar %>%
#     filter(UF == uf,
#            ESTRATO_POF %in% estrato,
#            TIPO_SITUACAO_REG %in% tipo_situacao_dom) %>%
#     mutate(Codigo = round(V9001/100), 
#            consumo_g_diario = ifelse(DIA_ATIPICO == 2, (QTD * 4 * PESO_FINAL)/30, QTD * PESO_FINAL ) ) # Dia atípico considera-se 1 vez no mês
#   
#   
#   # Informação de alimentação 
#   tradutor_alimentacao <- readxl::read_excel("../Tradutores_de_Tabela/Tradutor_Alimentação.xls") 
#   
#   # Vamos criar uma subcategoria da descrição 2 que agregue em outros
#   itens <- c('Cereais, leguminosas e oleaginosas', 
#              'Farinhas, féculas e massas',
#              'Tubérculos e raízes', 
#              'Açúcares e derivados',
#              'Legumes e verduras',
#              'Frutas',
#              'Carnes, vísceras, pescados',
#              'Aves e ovos',
#              'Leites e derivados',
#              'Panificados',
#              'Óleos e gorduras',
#              'Bebidas e infusões',
#              'Enlatados e conservas',
#              'Sal e condimentos',
#              'Alimentos preparados')
#   
#   # Classificação para os itens fora do domicilio
#   itens_fora <- c('Café, leite, chocolate', 
#                   'Sanduíches e salgados',
#                   'Lanches', 
#                   'Almoço e jantar',
#                   'Bebidas',
#                   'Outros')
#   
#   tradutor_alimentacao <- tradutor_alimentacao %>% 
#     mutate( Descricao_4 = ifelse(Descricao_2 %in% itens, Descricao_2, 'Outros alimentos'),
#             Descricao_5 = ifelse(Descricao_2 %in% itens_fora, Descricao_2, 'Outros alimentos'))
#   
#   
#   # Gastos com alimentação NO domicilio
#   alimentacao <-
#     consumo_alimentar %>%
#     left_join(tradutor_alimentacao, by = "Codigo") %>%
#     filter(is.na(Descricao_0) == F, 
#            Descricao_1 == 'Alimentação no domicílio',
#            is.na(Descricao_2) == F) %>%
#     group_by(Descricao_3) %>%
#     summarise(consumo_mensal_pc = sum(consumo_g_diario, na.rm = T)/(mean(soma_pessoas, na.rm = T)))
#   
#   
#   
#   
#   ################### ANTIGO
#   
#   # vamos mudar aqui pq será as colunas
#   
#   ### Tabela final com os resultados
#   tab_final <- data.frame(decis_renda = 1:10)
#   
#   ## calculando a renda per capita domiciliar (primeiro nomimal, depois deflacionamos para 2023) ----
#   
#   # Apenas variaveis com informacoes das UC's no arquivo 'MORADOR.rds'
#   # Apenas um registro por UC
#   
#   num_pessoas <- readRDS("MORADOR.rds") %>% 
#     filter(TIPO_SITUACAO_REG %in% tipo_situacao_dom) %>%
#     group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
#              COD_UPA, NUM_DOM, NUM_UC) %>% 
#     summarise(pessoas_dom = max(COD_INFORMANTE))
#   
#   # Atribuindo o número de pessoas da familia
#   morador_uc <- unique(readRDS("MORADOR.rds")[, c(var_dom, "PESO_FINAL", "PC_RENDA_MONET") ]) %>%
#     filter(TIPO_SITUACAO_REG %in% tipo_situacao_dom) %>%
#     left_join(num_pessoas, by = var_dom)
#   
#   # Retringindo para a região de interesse
#   morador_df <- morador_uc %>% 
#     filter(UF == uf,
#            ESTRATO_POF %in% estrato) 
#   
#   
#   # Big numbers
#   # O total de familias deve ser calculado por decil
#   soma_familia <- sum(morador_df$PESO_FINAL)
#   soma_pessoas <- sum(morador_df$pessoas_dom*morador_df$PESO_FINAL)
#   
#   # Tamanho médio por familia 
#   weighted.mean(morador_df$pessoas_dom, w = morador_df$PESO_FINAL)
#   
#   
#   # Vamos definir o dataframe como survey
#   survey_design <- svydesign(ids = ~1, weights = ~PESO_FINAL, data = morador_df)
#   
#   # Calculate the weighted deciles
#   deciles <- svyquantile(~PC_RENDA_MONET, 
#                          survey_design, 
#                          quantiles = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
#   
#   # Extract the deciles
#   weighted_deciles <- deciles$PC_RENDA_MONET[,1] 
#   
#   
#   # Inserindo na tabela final 
#   tab_final$renda_dom_pc <- paste0("R$ ", prettyNum(round(weighted_deciles[-1],0),big.mark = ".", small.mark = ","))
#   tab_final$renda_dom_pc[1] <- paste0("Até ", tab_final$renda_dom_pc[1])
#   tab_final$renda_dom_pc[10] <- paste0("Acima de ", tab_final$renda_dom_pc[9])
#   
#   
#   survey_design$variables$decis <- cut(survey_design$variables$PC_RENDA_MONET, 
#                                        breaks = weighted_deciles,
#                                        labels = F, 
#                                        include.lowest = T, 
#                                        na.pass = TRUE)
#   
#   # head(survey_design$variables)
#   
#   
#   # Calculo das despesas total ----- 
#   
#   
#   # Calculo das despesas com alimentação ----- 
#   caderneta_coletiva <- readRDS("CADERNETA_COLETIVA.rds")
#   despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds")
#   
#   ## Tratamento das variáveis ---- 
#   caderneta_coletiva <- 
#     caderneta_coletiva %>%
#     mutate(Codigo = round(V9001/100), # 5 dígitos
#            valor_mensal = (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12) %>%
#     filter(UF == uf, 
#            ESTRATO_POF %in% estrato, # filtro para a região de interesse
#            TIPO_SITUACAO_REG %in% tipo_situacao_dom,
#            Codigo < 86001 | Codigo > 89999 )  %>% 
#     as.data.frame()
#   
#   despesa_individual <- 
#     despesa_individual %>%
#     mutate(Codigo = round(V9001/100), # Retirar os últimos 2 dígitos
#            valor_mensal = ifelse(QUADRO %in% c(44, 47, 48, 49, 50), 
#                                  (V8000_DEFLA * V9011 * FATOR_ANUALIZACAO * PESO_FINAL)/12,
#                                  (V8000_DEFLA * FATOR_ANUALIZACAO * PESO_FINAL)/12)) %>%
#     filter(UF == uf, 
#            ESTRATO_POF %in% estrato, # filtro para a região de interesse
#            TIPO_SITUACAO_REG %in% tipo_situacao_dom
#     )  %>% 
#     as.data.frame()
#   
#   
#   # Vamos juntar as tabelas para calcular a depesa total
#   gastos_all <- bind_rows(caderneta_coletiva, despesa_individual)
#   
#   
#   # Vamos então atribuir a informção do decil da renda dom per capita para cada UC
#   merge_decis <- survey_design$variables %>% rename(peso_df_morador = PESO_FINAL) %>% as.data.frame()
#   
#   # O total de familias deve ser calculado por decil
#   soma_familia <- merge_decis %>% 
#     group_by(decis) %>%
#     summarise(soma_familia = sum(peso_df_morador),
#               soma_pessoas = sum(pessoas_dom*peso_df_morador)) %>%
#     filter(is.na(decis) == F)
#   
#   # Atribuição das informações de decis na tabela de despesas totais
#   gastos_all <- gastos_all %>% 
#     left_join(merge_decis, 
#               by = c('UF', 'ESTRATO_POF', 'TIPO_SITUACAO_REG', 'COD_UPA', 'NUM_DOM', 'NUM_UC')) 
#   
#   
#   # Informação de alimentação 
#   tradutor_alimentacao <- readxl::read_excel("../Tradutores_de_Tabela/Tradutor_Alimentação.xls") 
#   
#   # Vamos criar uma subcategoria da descrição 2 que agregue em outros
#   
#   itens <- c('Cereais, leguminosas e oleaginosas', 
#              'Farinhas, féculas e massas',
#              'Tubérculos e raízes', 
#              'Açúcares e derivados',
#              'Legumes e verduras',
#              'Frutas',
#              'Carnes, vísceras, pescados',
#              'Aves e ovos',
#              'Leites e derivados',
#              'Panificados',
#              'Óleos e gorduras',
#              'Bebidas e infusões',
#              'Enlatados e conservas',
#              'Sal e condimentos',
#              'Alimentos preparados')
#   
#   # Classificação para os itens fora do domicilio
#   
#   itens_fora <- c('Café, leite, chocolate', 
#                   'Sanduíches e salgados',
#                   'Lanches', 
#                   'Almoço e jantar',
#                   'Bebidas',
#                   'Outros')
#   
#   tradutor_alimentacao <- tradutor_alimentacao %>% 
#     mutate( Descricao_4 = ifelse(Descricao_2 %in% itens, Descricao_2, 'Outros alimentos'),
#             Descricao_5 = ifelse(Descricao_2 %in% itens_fora, Descricao_2, 'Outros alimentos'))
#   
#   
#   # Gastos com alimentação NO domicilio
#   gastos_alimentacao <-
#     gastos_all %>%
#     left_join(tradutor_alimentacao, by = "Codigo") %>%
#     left_join(soma_familia, by = 'decis') %>% # head()
#     filter(is.na(Descricao_0) == F, 
#            Descricao_1 == 'Alimentação no domicílio',
#            is.na(Descricao_2) == F) %>%
#     group_by(decis, Descricao_4) %>%
#     summarise(valor_mensal_pc = sum(valor_mensal, na.rm = T)/(mean(soma_pessoas, na.rm = T))) %>%
#     pivot_wider(
#       names_from = decis,
#       values_from = valor_mensal_pc
#     )
#   
#   names(gastos_alimentacao) <- c('Grupos_alimentos', paste0('decil', 1:10))
#   
#   
#   # Outros alimentos aqui ainda está beeem estranho
#   gastos_alimentacao_aux <- 
#     gastos_alimentacao %>% 
#     summarise(decil1 = 100 * decil1/sum(decil1, na.rm = T),
#               decil2 = 100 * decil2/sum(decil2, na.rm = T),
#               decil3 = 100 * decil3/sum(decil3, na.rm = T),
#               decil4 = 100 * decil4/sum(decil4, na.rm = T),
#               decil5 = 100 * decil5/sum(decil5, na.rm = T),
#               decil6 = 100 * decil6/sum(decil6, na.rm = T),
#               decil7 = 100 * decil7/sum(decil7, na.rm = T),
#               decil8 = 100 * decil8/sum(decil8, na.rm = T),
#               decil9 = 100 * decil9/sum(decil9, na.rm = T),
#               decil10 = 100 * decil10/sum(decil10, na.rm = T))
#   
#   gastos_alimentacao <- cbind(gastos_alimentacao$Grupos_alimentos, gastos_alimentacao_aux)
#   
#   
#   
#   
#   # FORA DO DOMICILIO
#   # Gastos com alimentação NO domicilio
#   gastos_alimentacao_fora <-
#     gastos_all %>%
#     left_join(tradutor_alimentacao, by = "Codigo") %>%
#     left_join(soma_familia, by = 'decis') %>% # head()
#     filter(is.na(Descricao_0) == F, 
#            Descricao_1 == 'Alimentação fora do domicílio',
#            is.na(Descricao_2) == F) %>%
#     group_by(decis, Descricao_5) %>%
#     summarise(valor_mensal_pc = sum(valor_mensal, na.rm = T)/(mean(soma_pessoas, na.rm = T))) %>%
#     pivot_wider(
#       names_from = decis,
#       values_from = valor_mensal_pc
#     )
#   
#   names(gastos_alimentacao_fora) <- c('Grupos_alimentos', paste0('decil', 1:10))
#   
#   
#   # Outros alimentos aqui ainda está beeem estranho
#   gastos_alimentacao_fora_aux <- 
#     gastos_alimentacao_fora %>% 
#     summarise(decil1 = 100 * decil1/sum(decil1, na.rm = T),
#               decil2 = 100 * decil2/sum(decil2, na.rm = T),
#               decil3 = 100 * decil3/sum(decil3, na.rm = T),
#               decil4 = 100 * decil4/sum(decil4, na.rm = T),
#               decil5 = 100 * decil5/sum(decil5, na.rm = T),
#               decil6 = 100 * decil6/sum(decil6, na.rm = T),
#               decil7 = 100 * decil7/sum(decil7, na.rm = T),
#               decil8 = 100 * decil8/sum(decil8, na.rm = T),
#               decil9 = 100 * decil9/sum(decil9, na.rm = T),
#               decil10 = 100 * decil10/sum(decil10, na.rm = T))
#   
#   gastos_alimentacao_fora <- cbind(gastos_alimentacao_fora$Grupos_alimentos, gastos_alimentacao_fora_aux)
#   
#   
#   
#   
#   
#   
#   
#   
#   return(tab_final)
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Tabelas requeridas pelo Pedro
# 
# # Tabela 3. Distribuição da renda familiar, despesas e gasto alimentar mensais per capita por décimos de renda familiar per capita – RMSP, 2017/2018.
# # Tabela 4. Evolução da participação dos gastos alimentares nas despesas gerais e da alimentação no domicílio sobre o gasto alimentar — RMSP e Brasil, 1987/88, 1995/96, 2002/03, 2008/09 e 2017/18.
# # Tabela 5. Composição do orçamento alimentar segundo grupos de alimentos e tipos de alimentação fora do domicílio, por décimos de renda familiar per capita — RMSP, 2017/18.
# # Figura 8. Distribuição das despesas de consumo por grupos de despesa, segundo décimos de renda familiar per capita – RMSP, 2017/2018.
# # Figura 9. Participação da alimentação fora do domicílio no gasto alimentar, segundo décimos de renda familiar per capita – RMSP, 2017/2018.
# # Figura 11. Evolução na quantidade adquirida para o domicílio por grupos de alimentos — RMSP 1987/88, 1995/96, 2002/03, 2008/09 e 2017/18.
# # Figura 12. Distribuição do número de aquisições, por locais de compra por e grupos de alimentos — Brasil, 2008/09.
# 
# 
# 
# 
# ## Tab 1 ----  
# # Gasto com alimentos dentro e fora de casa, por extrato de renda familiar,em relação aos outros tipos de despesas
# 
# 
# ## Tab 2 ----  
# # Gasto com alimentos dentro e fora de casa, por extrato de renda familiar, tipo de alimento
# 
# ## Tab 3 ----  
# # Evolução da quantidade anual per capita adquirida por grupos alimentares
# 
# ## Tab 4 ----  
# # Locais de compra dos produtos alimentares
# 
# ## Tab 5 ----  
# # Prevalência de Insegurança Alimentar e Nutricional (IAN) nos domicílios 
# 
# ## Tab 6 ----  
# # Aquisição alimentar domiciliar em Curitiba, de acordo com a Classificação NOVA
# 
# ## Tab 7 ----  
# # Média de aquisição alimentar per capita anual
# 
# 
# 
# 
# 
# 
# 
# 
