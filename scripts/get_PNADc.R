# ------------------------------------------------------------------- #
#                 PNAD Contínua: SISTEMATIZAÇÃO DOS DADOS 
# ------------------------------------------------------------------- #

# Projeto: Instituto Escolhas
# Autor: Lucas Gerez Foratto

# Objetivo: sistematizar os dados da PNAD contínua 


# Pacotes
load.lib <- c( "PNADcIBGE", "dplyr", "magrittr", "survey", 'tidyr' )

# Carregando os pacotes e instalando o que ainda não temos
install.lib <- load.lib[ !load.lib %in% installed.packages() ]

for( lib in install.lib ) install.packages( lib, dependencies = TRUE )
sapply( load.lib, require, character = TRUE )
rm(list = c("install.lib", "lib", "load.lib"))


# Caminho dos scripts
scripts.path <- 'C:/Users/user/Documents/Projetos_github/escolhas-scripts-db/scripts'

# Caminho dos arquivos
pnad.path <- 'E:/Drive/BASES DE DADOS BRUTOS/PNAD_CONTÍNUA_IBGE/Anual/Raw_data_R'

# Caminho para a pnadc tratada
save.path <- 'E:/Drive/Projetos/Escolhas/2024/Consultoria de Dados/Dados Tratados/PNADc'

# Etapa 1: pegar a base completa da PNADc -----

# Vamos gerar o conjunto de PNAD que temos informação para download
anos   <- 2012:2023
visita <- 1:5

periodo <- expand.grid(anos, visita)
names(periodo) <- c('ano', 'visita')

# # Vamos tentar pegar o visita 1 por enquanto
# periodo <- periodo %>% filter(visita == 5, ano == 2023)

for (p in 1:nrow(periodo)) {
  
  t0 <- Sys.time()
  
  cat('\nTentativa de obter a base anual da PNAD contínua\n  ANO', 
      periodo[p,]$ano,  
      "\n  VISITA:", periodo[p,]$visita,
      '\n\n')
  
  
  pnad_lst <- get_pnadc(year = periodo[p,]$ano,
                         interview = periodo[p,]$visita) 
  
  
  pnad_file <- pnad_lst$variables 
  
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


# Etapa 2: tratar a base para o intuito do projeto ------

# funcoes necessárias para o tratamento da base 
source(file.path(scripts.path, 'funcoes_pnadc.R'), encoding = 'UTF-8')


## Vamos então fazer o looping para tratar cada um dos anos ----

# A princípio 2016 está dando algum problema com a variável sexo, checar

# Vamos fazer uma lista para cada dimensão da pnad
lst.ocupados       <- list()
lst.ocup_agroali   <- list()
lst.setor          <- list()
lst.cor.raca       <- list()
lst.agro_raca_sexo <- list()

# Está salvo até 2019, temos que identificar o que mudou depois
for (y in years) {
  
  cat('\n\n\n\nPNADc ano:', y, paste0(Sys.time()), '\n\n\n')
  
  # parte 1: abrindo a PNAD do ano
  pnad <- trat_pnad(ano = y, visita = 1); gc()
  
  
  for (state in estados) {
    
    cat('\n\n Estado:', state)
    
    # Bloco 1: estatísticas da UF
    
    cat('\n  * Bloco 1: estatísticas da UF de', state)
    
    pnad_svy <- pnad %>% 
      filter(UF == state) 
    
    bloco_uf <- tabelas_pnad(estado = state, 
                             unidade_analise = "UF", 
                             pnad_data = pnad_svy,
                             year = y )
    
    # Linha de referência nos resultados
    linha <- df_resultados %>% filter(year == y, estado == state, regiao == 'UF') 
    
    lst.ocupados[[linha$num_row]]       <- bloco_uf$ocupadas
    lst.setor[[linha$num_row]]          <- bloco_uf$setor_rendimento
    lst.cor.raca[[linha$num_row]]       <- bloco_uf$setor_sexo
    lst.agro_raca_sexo[[linha$num_row]] <- bloco_uf$agro_raca_sexo
    lst.ocup_agroali[[linha$num_row]]   <- bloco_uf$ocup_agroali
    
    rm(pnad_svy); gc()
    
    # Bloco 2: estatísticas da RM
    
    cat('\n  * Bloco 2: estatísticas da RM')
    
    pnad_svy <- pnad %>% 
      filter(UF == state,
             unidade_geo %in% c('Capital', 'Resto da RM (Região Metropolitana, excluindo a capital)' ) ) 
    
    bloco_rm <- tabelas_pnad(estado = state, 
                             unidade_analise = "RM", 
                             pnad_data = pnad_svy,
                             year = y)
    
    # Linha de referência nos resultados
    linha <- df_resultados %>% filter(year == y, estado == state, regiao == 'RM') 
    
    lst.ocupados[[linha$num_row]]       <- bloco_rm$ocupadas
    lst.setor[[linha$num_row]]          <- bloco_rm$setor_rendimento
    lst.cor.raca[[linha$num_row]]       <- bloco_rm$setor_sexo
    lst.agro_raca_sexo[[linha$num_row]] <- bloco_rm$agro_raca_sexo
    lst.ocup_agroali[[linha$num_row]]   <- bloco_rm$ocup_agroali
    
    rm(pnad_svy); gc()
    
    # Bloco 3: estatísticas da Capital
    
    cat('\n  * Bloco 3: estatísticas da Capital')
    
    pnad_svy <- pnad %>% 
      filter(UF == state,
             unidade_geo %in% c('Capital') ) 
    
    bloco_cap <- tabelas_pnad(estado = state, 
                              unidade_analise = "Capital", 
                              pnad_data = pnad_svy,
                              year = y)
    
    # Linha de referência nos resultados
    linha <- df_resultados %>% filter(year == y, estado == state, regiao == 'Capital') 
    
    lst.ocupados[[linha$num_row]]       <- bloco_cap$ocupadas
    lst.setor[[linha$num_row]]          <- bloco_cap$setor_rendimento
    lst.cor.raca[[linha$num_row]]       <- bloco_cap$setor_sexo
    lst.agro_raca_sexo[[linha$num_row]] <- bloco_cap$agro_raca_sexo
    lst.ocup_agroali[[linha$num_row]]   <- bloco_cap$ocup_agroali
    
    rm(pnad_svy); gc()
    
  }
  
  rm(pnad); gc()
  
}

ocupados       <- bind_rows(lst.ocupados)
ocup_agroali   <- bind_rows(lst.ocup_agroali)
setor          <- bind_rows(lst.setor)
cor_raca       <- bind_rows(lst.cor.raca)
agro_raca_sexo <- bind_rows(lst.agro_raca_sexo)


# Feito isso vamos salvar esses dados e depois tratar as tabelas para exportar

save(list = c('ocupados', 'setor', 'cor_raca', 'agro_raca_sexo', 'ocup_agroali', 'estados'), 
     file = file.path(save.path, 'pnadc_tratada.rdata') )  

final <- Sys.time()

final - inicial
















