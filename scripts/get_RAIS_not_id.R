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


# Defina o seu projeto no Google Cloud

cat("Enter your set_billing_id: ")
billing_id <- readline(prompt = "")

set_billing_id(billing_id)


### IMPORTANTE: ESSE SCRIPT JÁ FOI REALIZADO, MAS FOI PERDIDO NA FORMATAÇÃO
### OS OUTPUTS NÓS TEMOS SALVO, MAS PRECISAMOS RESGATÁ-LO PARA REPLICAÇÃO


# Para carregar o dado direto no R
query <- bdplyr("br_me_rais.dicionario")
dic <- bd_collect(query)


unique(dic[dic$id_tabela == "microdados_estabelecimentos",]$nome_coluna)

# Teste com microdados (deu certo)

query <- "with 
cte_agro as (

       SELECT 
        id_municipio
        ,ano
        ,sexo
        ,raca_cor
        ,cnae_2
        ,valor_remuneracao_media
        ,tempo_emprego
       ,CASE
        WHEN cnae_2 IN ('01113', '01121', '01130', '01148', '01156', '01199', '01229', '01334', '01342', '01351', '01326', '01334', '01229', '01512', '10112', '01547', '10121', '01598', '46613', '01636', '01628', '01709', '46231', '46362', '47113', '56112', '56121', '56112', '56201', '01555', '03124', '10112', '28623', '10520', '10716', '10813', '10333', '10520', '11119', '11224', '20932', '12107', '12204')
              THEN 1
              ELSE 0
       END AS setor_agroalimentar
       FROM `basedosdados.br_me_rais.microdados_vinculos`
       WHERE ano = 2021 

) 

select id_municipio
       ,ano
       ,sexo
       ,raca_cor
       ,setor_agroalimentar
       ,count(*) as total_empregado
       ,avg(valor_remuneracao_media) as remuneracao_media
       ,avg(tempo_emprego) as tempo_emprego 
from cte_agro 
group by id_municipio, ano, sexo, raca_cor, setor_agroalimentar
limit 10"

download(query = query, path = 'C:/Users/user/Downloads/test.csv')
data <- read.csv('C:/Users/user/Downloads/test.csv')


# período de interesse

anos <- 2012:2021

for (y in anos) {
  
  cat('Ano', y, paste0(Sys.time()),'...\n')
  
  
  query <- "with 
            cte_agro as (
            
                   SELECT 
                    id_municipio
                    ,ano
                    ,sexo
                    ,raca_cor
                    ,cnae_2
                    ,valor_remuneracao_media
                    ,tempo_emprego
                   ,CASE
                    WHEN cnae_2 IN ('01113', '01121', '01130', '01148', '01156', '01199', '01229', '01334', '01342', '01351', '01326', '01334', '01229', '01512', '10112', '01547', '10121', '01598', '46613', '01636', '01628', '01709', '46231', '46362', '47113', '56112', '56121', '56112', '56201', '01555', '03124', '10112', '28623', '10520', '10716', '10813', '10333', '10520', '11119', '11224', '20932', '12107', '12204')
                          THEN 1
                          ELSE 0
                   END AS setor_agroalimentar
                   FROM `basedosdados.br_me_rais.microdados_vinculos`
                   WHERE ano = ",`y''," 
            
            ) 
            
            select id_municipio
                   ,ano
                   ,sexo
                   ,raca_cor
                   ,setor_agroalimentar
                   ,count(*) as total_empregado
                   ,avg(valor_remuneracao_media) as remuneracao_media
                   ,avg(tempo_emprego) as tempo_emprego 
            from cte_agro 
            group by id_municipio, ano, sexo, raca_cor, setor_agroalimentar
            limit 10"
  
  download(query = query, path = 'C:/Users/user/Downloads/test.csv')
  data <- read.csv('C:/Users/user/Downloads/test.csv')
  
  
  
  
  
}




# agr temos que acertar a query


# Query de teste que está no base de dados (precisamos fazer o id_mun na jogada)

query_old <- "with 
cte_agro as (

       SELECT ano
       ,sexo
       ,raca_cor
       ,cnae_2
       ,valor_remuneracao_media
       ,tempo_emprego
       ,CASE
       WHEN cnae_2 IN ('01113', '01121', '01130', '01148', '01156', '01199', '01229', '01334', '01342', '01351', '01326', '01334', '01229', '01512', '10112', '01547', '10121', '01598', '46613', '01636', '01628', '01709', '46231', '46362', '47113', '56112', '56121', '56112', '56201', '01555', '03124', '10112', '28623', '10520', '10716', '10813', '10333', '10520', '11119', '11224', '20932', '12107', '12204')
              THEN 1
              ELSE 0
       END AS setor_agroalimentar
       FROM `basedosdados.br_me_rais.microdados_vinculos`
       WHERE ano = 2021 
       ---LIMIT 10

)"


