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

lst.pnadc <- list()


for (p in 1:nrow(periodo)) {
  
  t0 <- Sys.time()
  
  cat('Tentativa de obter a base anual da PNAD contínua\nANO', 
      periodo[p,]$ano,  
      "\nVISITA:", periodo[p,]$visita,
      '\n\n')
  
  
  pnad_lst <- get_pnadc(year = periodo[p,]$ano,
                         interview = periodo[p,]$visita) 
  
  
  pnad_file <- pnad_lst$variables 
  
  
  save(pnad_file, 
       file = paste0('F:/Drive/BASES DE DADOS BRUTOS/PNAD_CONTÍNUA_IBGE/Anual/Raw_data_R/',
                     'pnad_anual_', periodo[p,]$ano, '_visita' ,periodo[p,]$visita, '.rdata'), 
       version = 2, compress = T)
  
  
  t1 <- Sys.time() 
  
  
  
  
}



# PNAD Continua trimestral ---------



# Teste com um trimestre



survey_design_df <- svydesign(
  
  id = ~UPA,
  strata = ~Estrato,
  weights = ~V1027,
  data = pnad_file$variables
  
)





