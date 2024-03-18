# ------------------------------------------------------------------- #
#               OBTENÇÃO DA BASE DA RAIS NÃO IDENTIFICADA
# ------------------------------------------------------------------- #

# Autor: Lucas Gerez Foratto

# Vamos usaro pacote base de dados para extrair os dados da RAIS


cat('Loading the packages .... \n')

# Pacotes
load.lib <- c( "dplyr", "basedosdados")

# Carregando os pacotes e instalando o que ainda não temos
install.lib <- load.lib[ !load.lib %in% installed.packages() ]

for( lib in install.lib ) install.packages( lib, dependencies = TRUE )
sapply( load.lib, require, character = TRUE )
rm(list = c("install.lib", "lib", "load.lib"))


# Caminho para os arquivos salvos
rais_path <- 'E:/Drive/Projetos/Escolhas/2024/Consultoria de Dados/Dados Tratados/RAIS'


# Defina o seu projeto no Google Cloud
cat("Enter your set_billing_id: ")
billing_id <- readline(prompt = "")
set_billing_id(billing_id)


### IMPORTANTE: ESSE SCRIPT JÁ FOI REALIZADO, MAS FOI PERDIDO NA FORMATAÇÃO
### OS OUTPUTS NÓS TEMOS SALVO, MAS PRECISAMOS RESGATÁ-LO PARA REPLICAÇÃO



# Looping para pegar os dados ---------------------------------------------


# Teste com microdados (deu certo)
# período de interesse

anos <- 2012:2021

# Looping para obter os dados (rodar apenas uma vez)
for (y in anos) {
  
  cat('Ano', y, paste0(Sys.time()),'...\n')
  
  # Construct the query
  query <- paste0('WITH cte_agro AS (
                     SELECT 
                       id_municipio,
                       ano,
                       sexo,
                       raca_cor,
                       cnae_2,
                       valor_remuneracao_media,
                       tempo_emprego,
                       CASE WHEN cnae_2 IN (\'01113\', \'01121\', \'01130\', \'01148\', \'01156\', \'01199\', \'01229\', \'01334\', \'01342\', \'01351\', \'01326\', \'01334\', \'01229\', \'01512\', \'10112\', \'01547\', \'10121\', \'01598\', \'46613\', \'01636\', \'01628\', \'01709\', \'46231\', \'46362\', \'47113\', \'56112\', \'56121\', \'56112\', \'56201\', \'01555\', \'03124\', \'10112\', \'28623\', \'10520\', \'10716\', \'10813\', \'10333\', \'10520\', \'11119\', \'11224\', \'20932\', \'12107\', \'12204\') THEN 1 ELSE 0 END AS setor_agroalimentar 
                     FROM basedosdados.br_me_rais.microdados_vinculos
                     WHERE ano = ',as.numeric(y),') 
                  SELECT 
                     id_municipio,
                     ano,
                     sexo,
                     raca_cor,
                     setor_agroalimentar,
                     COUNT(*) AS total_empregado,
                     AVG(valor_remuneracao_media) AS remuneracao_media,
                     AVG(tempo_emprego) AS tempo_emprego 
                  FROM cte_agro 
                  GROUP BY id_municipio, ano, sexo, raca_cor, setor_agroalimentar')
  
  
  download(query = query, path = file.path(rais_path, paste0('rais_',y, '.csv' )))
  
}



# Sistematizando a base em um painel --------------------------------------

# Para carregar o dado direto no R
# query <- bdplyr("br_me_rais.dicionario")
# dic <- bd_collect(query)

# Vamos salvar o dicionário 
# write.csv2(x = dic,file.path(rais_path, 'dicionario_rais.csv'))

# Vamos então fazer uma tabela com o de-para de raca/cor
dic <- read.csv2(file.path(rais_path, 'dicionario_rais.csv'))

dic[dic$id_tabela == "microdados_vinculos" & dic$nome_coluna == 'raca_cor',]

df.raca.cor <- data.frame(raca_cor = c(1,2,4,6,8,9,-1),
                          raca_str = c('indigena', 
                                       'branca',
                                       'preta',
                                       'amarela',
                                       'parda',
                                       'nao_identificado',
                                       'ignorado'))

dic[dic$id_tabela == "microdados_vinculos" & dic$nome_coluna == 'sexo',]

df.sexo <- data.frame(sexo = c(1,2,-1), 
                      sexo_str = c('masculino',
                                   'feminino',
                                   'ignorado'))


# Temos que ver agr a questão de cor e raça (com o dicionário)

# Vamos empilhar os dados em um painel de 2012 a 2016

lst.rais.painel <- list()

for (y in anos) {
  
  cat('Ano', y, '..\n')
  
  # Para ver como ficou a base 
  data <- read.csv(file.path(rais_path, paste0('rais_',y, '.csv' )))
  
  data <- data %>% 
    left_join(df.raca.cor, by = 'raca_cor') %>%
    left_join(df.sexo, by = 'sexo') 
  
  lst.rais.painel[[match(y, anos)]] <- data
  
  rm(data); gc()
  
}

rais.painel <- bind_rows(lst.rais.painel)
  
write.csv2(x = rais.painel,file.path(rais_path, 'rais_painel_2012_2021.csv'))





