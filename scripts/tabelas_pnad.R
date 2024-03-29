# ------------------------------------------------------------------- #
#                 PNAD Contínua: Tabelas 
# ------------------------------------------------------------------- #

# Autor: Lucas Gerez Foratto
# Esse script origina as tabelas da PNAD contínua para o estudo do IPPUC



inicial <- Sys.time()


# Pacotes
load.lib <- c( "dplyr", "openxlsx", 'tidyr' )

# Carregando os pacotes e instalando o que ainda não temos
install.lib <- load.lib[ !load.lib %in% installed.packages() ]

for( lib in install.lib ) install.packages( lib, dependencies = TRUE )
sapply( load.lib, require, character = TRUE )
rm(list = c("install.lib", "lib", "load.lib"))


## Caminhos ----
result_path <- 'E:/Drive/Projetos/Escolhas/2024/Consultoria de Dados/Curitiba/PNADc' # sempre checar se o caminho se mantém

# Caminho para a pnadc tratada
save.path <- 'E:/Drive/Projetos/Escolhas/2024/Consultoria de Dados/Dados Tratados/PNADc'


## Base tratada ---- 
load(file.path(save.path, 'pnadc_tratada.rdata'))


# Regiões de análise
  # UF
  # RM 
  # Capital

uf = 'Paraná'

tables_pnad_f <- function(uf) {
  
  lst.pnad <- list()
  
  
  # Bloco 1: ocupados
  
  lst.pnad$ocupados_UF <- 
    ocupados %>% 
    filter(UF == uf, unidade_analise == 'UF')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = ocupadas, values_from = c(total, prop)) # Proporção do agroalimentar tem intersecção com os demais ocupadoses
  
  lst.pnad$ocupados_RM <- 
    ocupados %>% 
    filter(UF == uf, unidade_analise == 'RM')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = ocupadas, values_from = c(total, prop)) # Proporção do agroalimentar tem intersecção com os demais ocupadoses
  
  lst.pnad$ocupados_Cap <- 
    ocupados %>% 
    filter(UF == uf, unidade_analise == 'Capital')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = ocupadas, values_from = c(total, prop)) # Proporção do agroalimentar tem intersecção com os demais ocupadoses
  
  
  # Bloco 2: ocupados agroalimentar
  
  lst.pnad$ocupados_UF <- 
    ocupados %>% 
    filter(UF == uf, unidade_analise == 'UF')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = ocupadas, values_from = c(total, prop)) # Proporção do agroalimentar tem intersecção com os demais ocupadoses
  
  lst.pnad$ocupados_RM <- 
    ocupados %>% 
    filter(UF == uf, unidade_analise == 'RM')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = ocupadas, values_from = c(total, prop)) # Proporção do agroalimentar tem intersecção com os demais ocupadoses
  
  lst.pnad$ocupados_Cap <- 
    ocupados %>% 
    filter(UF == uf, unidade_analise == 'Capital')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = ocupadas, values_from = c(total, prop)) # Proporção do agroalimentar tem intersecção com os demais ocupadoses
  
  
  
  
  # Bloco 2: setor
  
  lst.pnad$setor_UF <- 
    setor %>% 
    filter(UF == uf, unidade_analise == 'UF')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = setor, values_from = c(rendimento, ocupados, prop)) # Proporção do agroalimentar tem intersecção com os demais setores
  
  
  lst.pnad$setor_RM <- 
    setor %>% 
    filter(UF == uf, unidade_analise == 'RM')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = setor, values_from = c(rendimento, ocupados, prop)) # Proporção do agroalimentar tem intersecção com os demais setores
  
  lst.pnad$setor_Cap <- 
    setor %>% 
    filter(UF == uf, unidade_analise == 'Capital')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = setor, values_from = c(rendimento, ocupados, prop)) # Proporção do agroalimentar tem intersecção com os demais setores
  
  
  
  
  
  head(ocupados)
  
  
  
  
  lst.pnad$agro_raca_sexo_UF <- 
    agro_raca_sexo %>% 
    filter(UF == uf, unidade_analise == 'UF')  %>%
    arrange(year) 
  
  lst.pnad$agro_raca_sexo_RM <- 
    agro_raca_sexo %>% 
    filter(UF == uf, unidade_analise == 'RM')  %>%
    arrange(year) 
  
  lst.pnad$agro_raca_sexo_Cap <- 
    agro_raca_sexo %>% 
    filter(UF == uf, unidade_analise == 'Capital')  %>%
    arrange(year) 
  
  
  
  
  
  # Tabela 1: total de empregos formais e por sexo (teste Curitiba)
  lst.rais$df_sexo <- 
    rais %>% 
    filter(id_municipio %in% mun) %>% 
    group_by(ano, sexo_str) %>% 
    summarise(empregados = sum(total_empregado)/10^3, 
              rem_media = weighted.mean(remuneracao_media,total_empregado)
    ) %>% 
    pivot_wider(id_cols = ano, names_from = sexo_str, values_from = c(empregados,rem_media))
  
  
  lst.rais$df_sexo_agro <- 
    rais %>% 
    filter(id_municipio %in% mun, setor_agroalimentar == 1) %>% 
    group_by(ano, sexo_str) %>% 
    summarise(empregados = sum(total_empregado)/10^3, 
              rem_media = weighted.mean(remuneracao_media,total_empregado)
    ) %>% 
    pivot_wider(id_cols = ano, names_from = sexo_str, values_from = c(empregados,rem_media))
  
  
  # Vamos desconsiderar os não identificados para as contas de proporção
  
  warning('Vamos desconsiderar os não identificados para as contas de proporção')
  
  # Tabela 2: cor/raça
  lst.rais$df_cor_raca <- 
    rais %>% 
    filter(id_municipio  %in% mun, raca_str  != 'nao_identificado' ) %>% 
    group_by(ano, raca_str) %>% 
    summarise(empregados = sum(total_empregado)/10^3, 
              rem_media = weighted.mean(remuneracao_media,total_empregado)
    ) %>%
    ungroup() %>%
    group_by(ano) %>% 
    mutate(prop_raca = empregados/sum(empregados)) %>%
    pivot_wider(id_cols = ano, names_from = raca_str, values_from = c(prop_raca,rem_media))
  
  
  lst.rais$df_cor_raca_agro <- 
    rais %>% 
    filter(id_municipio  %in% mun, raca_str  != 'nao_identificado', setor_agroalimentar == 1 ) %>% 
    group_by(ano, raca_str) %>% 
    summarise(empregados = sum(total_empregado)/10^3, 
              rem_media = weighted.mean(remuneracao_media,total_empregado)
    ) %>%
    ungroup() %>%
    group_by(ano) %>% 
    mutate(prop_raca = empregados/sum(empregados)) %>%
    pivot_wider(id_cols = ano, names_from = raca_str, values_from = c(prop_raca,rem_media))
  
  return(lst.pnad)
  
  
}



load(file.path(save.path, 'pnadc_tratada.rdata'))

list.files(file.path(save.path))




c('ocupados', 'setor', 'cor_raca', 'agro_raca_sexo', 'ocup_agroali', 'estados')

















