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

# Caminho git
git_path <- 'C:/Users/user/Projetos_GIT/escolhas-scripts-db'

# Vamos rodar as funções no Git
source(file.path( git_path, "scripts/01_criar_funcoes.R" ),encoding = 'UTF-8')

# Base de dados consolidadas
pof <- readRDS( file.path( git_path, "data/despesas_por_grupos_POF.RDS") )


# Arranjos geográficos a serem definidos
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

# PARÂMETROS A SEREM DEFINIDOS:
# NÚMERO DE PERCENTIS DA ANÁLISE
# QUAL A VARIÁVEL QUE VAI DEFINIR OS PERCENTIS
# QUAL O PESO
# QUAL ESTRATO VAI SER UTILIZADO
# QUAL O NOME DA TABELA DE OUTPUT

# Corrigindo a renda per capita monetária e não monetária
pof <- pof %>%
  mutate( v1 = if_else(is.na(PC_RENDA_MONET), 0, PC_RENDA_MONET),
          v2 = if_else(is.na(PC_RENDA_NAO_MONET), 0, PC_RENDA_NAO_MONET),
          PC_RENDA_TOTAL = v1 + v2) %>% 
  select(-v1, -v2)

lst.tab <- list()

for (d in dimensoes) {
  
  cat('\nDimensão', d, paste(Sys.time()))
  
  # Tabelas -----
  
  # Incluindo a informação de decil e já filtrando para o estrato desejado
  pof_estrato <- f_xtile_filter(pof, variavel = "PC_RENDA_DISP", var_peso = "peso_final_fam",
                                nova_var = "decis", n = 10, estrato = get(d) )  %>%
    filter(!is.na(decis))
  
  # Definindo o data.frame como objeto amostral
  pof_svy <- as_survey(svydesign(ids = ~COD_UPA, strata = ~ESTRATO_POF , weights = ~peso_final_fam, data = pof_estrato))
  
  
  # Renda domiciliar per capita disponível
  tab_renda <- f_inc_dist_filter(pof_svy, variavel = "PC_RENDA_DISP", var_peso = "peso_final_fam",
                                 nova_var = "decis", n = 10, estrato = get(d)  )
  
  
  t1 <- f_medias_2018(pof_svy, decis)
  pof_svy$variables$um <- 999 # para o total
  t1b <- f_medias_2018(pof_svy, um)
  names(t1b)[1] <- "decis"
  t1b$decis <- 'Total'
  t1 <- rbind(t1, t1b) 
  
  # Juntando tabela 1, 2 e 3
  lst.tab[[match(d, dimensoes)]] <- 
    tab_renda %>% 
    left_join(t1, by = 'decis') 
    
}

# nomes para facilitar
names(lst.tab) <- dimensoes

leia.me <- data.frame( identificador = c('01','02', '03', '04','05', '06'),
                       nivel_geografico = c('UF incluindo rural',
                                            'UF SEM incluir rural',
                                            'Regiões da UF fora da RM SEM incluir rural',
                                            'Região Metropolitana (RM)',
                                            'RM exceto capital (Curitiba)',
                                            'Capital (Curitiba)'))

sheets <- list("leia_me" = leia.me,
               "desp_01_2018" = lst.tab$estrato_uf_com_rural, 
               "desp_02_2018" = lst.tab$estrato_uf_sem_rural,
               "desp_03_2018" = lst.tab$estrato_uf_sem_rm_sem_rural,
               "desp_04_2018" = lst.tab$estrato_rm,
               "desp_05_2018" = lst.tab$estrato_rm_sem_capital,
               "desp_06_2018" = lst.tab$estrato_capital)

library(openxlsx)
write.xlsx(sheets, "F:/Drive/Projetos/Escolhas/2023/Consultoria_Dados/Resultados/POF/pof_tabelas_grandes_grupos_2018.xlsx")









