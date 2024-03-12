
# ------------------------------------------------------------------------- # 

# DADOS DA POF: tabelas consolidadas a nível Brasil

# ------------------------------------------------------------------------- # 

# RM, Curitiba e demais regiões

# Script consolidado com os dados da POF

inicial <- Sys.time()

## Funções necessárias para as tabelas ----

result_path       <- 'E:/Drive/Projetos/Escolhas/2023/Consultoria_Dados/Resultados/POF' # sempre checar se o caminho se mantém
estratos_path     <- 'E:/Drive/BASES DE DADOS BRUTOS/POF/POF 2017-2018/Microdados/Documentacao_20221226/Estratos POF 2017-2018.xls' # sempre checar se o caminho se mantém
dados_brutos_path <- 'E:/Drive/BASES DE DADOS BRUTOS'

# Caminho dos scripts na máquina local
git_path <- 'C:/Users/user/Documents/Projetos_github/escolhas-scripts-db/'

# Vamos rodar as funções no Git
source(file.path( git_path, "scripts/funcoes_pof.R" ), encoding = 'UTF-8')

## POF 2018 ----
# Base de dados consolidadas
pof_2018 <- readRDS( file.path( git_path, "data/pof_fam_wide_2018.RDS") )


##  Estratos do Brasil ----
estrato_COM_rural  <- df_estratos[is.na(df_estratos$estratos)==F,]$estratos  
estrato_SEM_rural  <- df_estratos[is.na(df_estratos$estratos)==F & df_estratos$regiao != 'rural_ref' ,]$estratos  
dimensoes <- c('estrato_COM_rural', 'estrato_SEM_rural')

sheets_br <- compila_tabelas_pof_f(dimensoes = dimensoes, df_pof_2002_2008 = pof_res, df_pof_2018 = pof_2018)
write.xlsx(sheets_br, file = file.path(result_path, "Brasil_pof_tabelas.xlsx"))


## Macrorregioes do Brasil ----- 

for (r in unique(df_estratos$macrorregiao)) {
  
  cat('\n\nMacrorregião:', r,"\n\n")
  
  estrato_COM_rural  <- df_estratos[is.na(df_estratos$estratos)==F & df_estratos$macrorregiao == r,]$estratos  
  estrato_SEM_rural  <- df_estratos[is.na(df_estratos$estratos)==F & df_estratos$macrorregiao == r & df_estratos$regiao != 'rural_ref' ,]$estratos  
  dimensoes <- c('estrato_COM_rural', 'estrato_SEM_rural')
  
  sheets <- compila_tabelas_pof_f(dimensoes = dimensoes, df_pof_2002_2008 = pof_res, df_pof_2018 = pof_2018)
  write.xlsx(sheets, file = file.path(result_path, paste0(r,'_pof_tabelas.xlsx')))
  
  
}

final <- Sys.time()





