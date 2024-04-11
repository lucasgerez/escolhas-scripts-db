
# ------------------------------------------------ # 

#           RAIS - TABELAS Consolidadas

# ------------------------------------------------ # 

cat('Loading the packages .... \n')

# Pacotes
load.lib <- c( "dplyr", "tidyr", "openxlsx")

# Carregando os pacotes e instalando o que ainda não temos
install.lib <- load.lib[ !load.lib %in% installed.packages() ]

for( lib in install.lib ) install.packages( lib, dependencies = TRUE )
sapply( load.lib, require, character = TRUE )
rm(list = c("install.lib", "lib", "load.lib"))

# Diretório
rais_path <- 'E:/Drive/Projetos/Escolhas/2024/Consultoria de Dados/Dados Tratados/RAIS'
dados_brutos_path <- 'E:/Drive/BASES DE DADOS BRUTOS/MAPAS/BRASIL'
result_path <- 'E:/Drive/Projetos/Escolhas/2024/Consultoria de Dados/Curitiba/RAIS' # sempre checar se o caminho se mantém

# Funcoes ------
get_rm_f <- function(mun, br_rm){
  
  # etapa 1: identifica se possui uma região metropolitana
  
  valid <- br_rm[br_rm$id_municipio == mun,]
  
  if ( nrow(valid) != 1 ) {
    
    warning('Não temos rm')
    return(NA)
    
  } else {
    
    cat('Temos região metropolitana para a área!')
    
    rm_mun  <- br_rm[br_rm$nome_regiao_metropolitana == valid$nome_regiao_metropolitana,]
    
    return(rm_mun$id_municipio)
    
  }
  
}

tables_rais_f <- function(mun, rais) {
  
  lst.rais <- list()
  
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
  
  return(lst.rais)
  
  
}

# Base de dados ----

## Dados RAIS ----
rais <- read.csv2(file.path(rais_path, 'rais_painel_2012_2021.csv'))

## Dados regiões metropolitanas ----
br_rm <- read.csv(file.path(dados_brutos_path, 'REGIOES METROPOLITANAS/regioes_metropolitanas.csv')) %>% 
  select(nome_regiao_metropolitana, tipo, subcategoria_metropolitana, id_municipio, sigla_uf)


# Delimitações geográficas
curitiba_cod_mun <- 4106902
rm_curitiba_cod_mun <- get_rm_f(mun = mun_curitiba, br_rm = br_rm)
parana_cod_mun <- rais %>% mutate(UF = substring(id_municipio, 1,2)) %>% filter(UF %in% substring(mun_curitiba, 1,2)) %>% select(id_municipio) %>% unique() 

# Tabelas -----
curitiba <- tables_rais_f(mun = curitiba_cod_mun, rais = rais)
rm_curitiba <- tables_rais_f(mun = rm_curitiba_cod_mun, rais = rais)
parana <- tables_rais_f(mun = parana_cod_mun$id_municipio, rais = rais)


# Exportando em excel ----- 

names(curitiba) <- paste0(names(curitiba),'_Curitiba')
names(rm_curitiba) <- paste0(names(rm_curitiba),'_RM_Curitiba')
names(parana) <- paste0(names(parana),'_Parana')

export <- c(curitiba, rm_curitiba, parana)

export <- export[order(sort(names(export)))]

write.xlsx(export, file = file.path(result_path, "rais_tabelas.xlsx"))


# Looping para os demais estados ----

estados <- c('Acre', 'Amapá', 'Amazonas', 'Goiás',
             'Maranhão', 'Mato Grosso', 'Pará', 'Pernambuco',
             'Rio de Janeiro', 'Rondônia', 'Tocantins')

capitais <- c(1200401, 1600303, 1302603, 5208707,
              2111300, 5103403, 1501402, 2611606, 
              3304557, 1100205, 1721000)


df_cities <- data.frame(estados, capitais)

# Looping 
for (i in 1:nrow(df_cities)) {
  
  # Delimitações geográficas
  cod_mun <- df_cities[i,]$capitais
  rm_cod_mun <- get_rm_f(mun = cod_mun, br_rm = br_rm)
  uf_cod_mun <- rais %>% mutate(UF = substring(id_municipio, 1,2)) %>% filter(UF %in% substring(cod_mun, 1,2)) %>% select(id_municipio) %>% unique() 
  uf_cod_mun <- uf_cod_mun$id_municipio
  
  # Tabelas -----
  capital <- tables_rais_f(mun = cod_mun, rais = rais)
  
  if ( any(is.na(rm_cod_mun))) {
    
    cat('\nNão temos informações de regiões metropolitanas neste estado')
    
    rm <- NA
    
  } else {
    
    rm  <-  tables_rais_f(mun = rm_cod_mun, rais = rais)
    
  }
  
  uf <- tables_rais_f(mun = uf_cod_mun, rais = rais)
  
  
  # Exportando em excel ----- 
  
  names(capital) <- paste0(names(capital),'_Capital')
  names(rm) <- paste0(names(rm),'_RM')
  names(uf) <- paste0(names(uf),'_UF')
  
  export <- c(uf, rm, capital)
  export <- export[order(sort(names(export)))]
  
  write.xlsx(export, file = file.path(result_path, paste0(df_cities[i,]$estados,"_rais_tabelas.xlsx")))
  
  
}



