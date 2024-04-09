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


tables_pnad_f <- function(uf) {
  
  lst.pnad <- list()
  
  cat('\nBloco 1: ocupados\n')
  
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
  
  
  cat('\nBloco 2: ocupados agroalimentar\n')
  # Bloco 2: ocupados agroalimentar
  
  lst.pnad$ocup_agroali_UF <- 
    ocup_agroali %>% 
    filter(UF == uf, unidade_analise == 'UF')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = descricao, values_from = c(total, prop)) # Proporção do agroalimentar tem intersecção com os demais ocup_agroalies
  
  lst.pnad$ocup_agroali_RM <- 
    ocup_agroali %>% 
    filter(UF == uf, unidade_analise == 'RM')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = descricao, values_from = c(total, prop)) # Proporção do agroalimentar tem intersecção com os demais ocup_agroalies
  
  lst.pnad$ocup_agroali_Cap <- 
    ocup_agroali %>% 
    filter(UF == uf, unidade_analise == 'Capital')  %>%
    arrange(ano) %>%
    pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = descricao, values_from = c(total, prop)) # Proporção do agroalimentar tem intersecção com os demais ocup_agroalies
  
  cat('\nBloco 3: setor\n') 
  # Bloco 3: setor
  
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
  
  
# PAREI AQUI --------------------------------------------------------------

  cat('\nBloco 4: cor e raça \n') 
  
  # Bloco 4: cor / raça
  
  # lst.pnad$cor_raca_UF <- 
  #   cor_raca %>% 
  #   filter(UF == uf, unidade_analise == 'UF', cor_raca %in% c('Branca'))  %>%
  #   arrange(ano) %>%
  #   pivot_wider(id_cols = c(ano, unidade_analise, UF), names_from = cor_raca, values_from = c(rendimento, ocupados, prop)) # Proporção do agroalimentar tem intersecção com os demais cor_racaes
  # 
  
  return(lst.pnad)
  
  
}


parana <- tables_pnad_f(uf = 'Paraná')

write.xlsx(parana, file = file.path(result_path, 'Paraná_pnadc_tabelas.xlsx'))
















