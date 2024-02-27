# ------------------------------------------------------------------- #
#                 PNAD Contínua: SISTEMATIZAÇÃO DOS DADOS 
# ------------------------------------------------------------------- #

# Projeto: Instituto Escolhas
# Autor: Lucas Gerez Foratto

# Objetivo: sistematizar os dados da PNAD contínua 


# Pacotes
load.lib <- c( "PNADcIBGE", "dplyr", "magrittr", "survey" )

# Carregando os pacotes e instalando o que ainda não temos
install.lib <- load.lib[ !load.lib %in% installed.packages() ]

for( lib in install.lib ) install.packages( lib, dependencies = TRUE )
sapply( load.lib, require, character = TRUE )
rm(list = c("install.lib", "lib", "load.lib"))



# PNAD Continua anual ---------

# Vamos gerar o conjunto de PNAD que temos informação para download

anos   <- 2012:2023
visita <- 1:5

periodo <- expand.grid(anos, visita)
names(periodo) <- c('ano', 'visita')

# # Vamos tentar pegar o visita 5
# periodo <- periodo %>% filter(visita == 5, anos > 2015)

for (p in 1:nrow(periodo)) {
  
  t0 <- Sys.time()
  
  cat('\nTentativa de obter a base anual da PNAD contínua\n  ANO', 
      periodo[p,]$ano,  
      "\n  VISITA:", periodo[p,]$visita,
      '\n\n')
  
  
  pnad_lst <- get_pnadc(year = periodo[p,]$ano,
                         interview = periodo[p,]$visita) 
  
  
  pnad_file <- pnad_lst$variables 
  
  pnad.path <- 'F:/Drive/BASES DE DADOS BRUTOS/PNAD_CONTÍNUA_IBGE/Anual/Raw_data_R'
  
  # salvando o arquivo de cada entrevista anual
  saveRDS(pnad_file,
          file.path(pnad.path, 
                    paste0('pnad_anual_', 
                           periodo[p,]$ano, '_visita',
                           periodo[p,]$visita,
                           ".RDS"))) 
  
  rm(list = c('pnad_lst', 'pnad_file'));gc()
  
  t1 <- Sys.time() 
  
  cat(paste(t1 - t0))
  
  
}






