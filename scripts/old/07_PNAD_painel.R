
# Tabelas PNADc 

# Pacotes
load.lib <- c( "tidyr", "dplyr", "openxlsx", "survey", "srvyr" )

# Carregando os pacotes e instalando o que ainda não temos
install.lib <- load.lib[ !load.lib %in% installed.packages() ]

for( lib in install.lib ) install.packages( lib, dependencies = TRUE )
sapply( load.lib, require, character = TRUE )
rm(list = c("install.lib", "lib", "load.lib"))


# Caminho da PNAD contínua
pnad.path <- "F:/Drive/BASES DE DADOS BRUTOS/PNAD_CONTÍNUA_IBGE/Anual/Raw_data_R"


# CNAE de interesse para o projeto do escolhas
# cnae <- c()


# Vamos tentar abrir uma PNAD contínua e fazer as estatísticas descritivas

pnad <- readRDS( file.path( pnad.path, "pnad_anual_2012_visita1.RDS") )

# Vamos usar a documentação de 2012 aqui

# Etapa 1: vamos ficar apenas com as variáveis de interesse para diminuir o consumo de memória

# Essas variáveis podem mudar de ano para ano
variaveis <- c("UF", "Capital", "RM_RIDE", "UPA", "Estrato",
               "V1008", # Número de seleção do domicílio
               "V1014", # Painel
               "V1022", # Situação do domicílio
               "V1023", # Tipo de área (identifica se é capital, resto da RM, resto do estado)
               "V1032", # Peso do domicílio e das pessoas com calibração
               "V2001", # Número de pessoas no domicílio
               "V2005", # Condição no domicílio (responsável é o um)
               "V2007", # Sexo (1= MASC; 2 = FEM)
               "V2009", # idade do morador
               "V2010", # Cor ou raça
               "V4013", # CNAE ativ principal
               "VD3004", # Escolaridade 
               "VD4001", # PEA
               "VD4002", # Ocupadas e não ocupadas
               "VD4004", # Subocupadas
               "VD4005", # Desalentadas
               "VD4008", # Tipo de ocupação
               "VD4009", # Posição na ocupação
               "VD4010", # Atividade do Estabelecimento
               "VD4011", # Atividade dos trabalhadores
               "VD4013", # horas trabalhadas (todos os trabalhos)
               "VD4016", # Rendimento mensal habitual do trabalho principal
               "VD4019", # Rendimento mensal habitual todos os trabalhos 
               "VD5007", # Renda dom total (habitual)
               "VD5008" # Renda dom pc (habitual)
               )

# Tal como na POF, vamos montar a nossa base a nível de domicílio e a partir disso fazer as contas

pnad <- pnad[variaveis]; gc()


pnad <- pnad %>% 
  rename(unidade_geo = V1023, 
         num_pessoas_dom = V2001,
         cor_raca = V2010,
         sexo = V2007,
         cond_dom = V2005,
         escolaridade = VD3004,
         peso_dom = V1032,
         cnae_trab_princ = V4013,
         idade = V2009,
         PEA = VD4001,
         ocupadas = VD4002,
         subocupadas = VD4004,
         desalentadas = VD4005,
         tipo_ocupacap = VD4008,
         atividade_estabelecimentos = VD4010,
         atividade_trabalhadores = VD4011,
         renda_trab_princ = VD4016,
         renda_trab_todas_fontes = VD4019,
         renda_dom_total = VD5007,
         renda_dom_pc = VD5008,
         horas_trabalhadas = VD4013
         ) 

pnad <- pnad %>%
  mutate(setor = case_when( 
    atividade_estabelecimentos %in% c( 'Alojamento e alimentação', 
                                    'Serviços domésticos', 
                                    'Transporte, armazenagem e correio', 
                                    'Construção', 
                                    'Educação, saúde humana e serviços sociais', 
                                    'Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas', 
                                    'Atividades mal definidas' ) ~ 'Serviços',
    atividade_estabelecimentos %in% c( 'Comércio, reparação de veículos automotores e motocicletas' ) ~ 'Comércio',
    atividade_estabelecimentos %in% c( 'Indústria geral') ~ 'Indústria',
    atividade_estabelecimentos %in% c( 'Agricultura, pecuária, produção florestal, pesca e aquicultura') ~ 'Agricultura',
    atividade_estabelecimentos %in% c( 'Administração pública, defesa e seguridade social') ~ 'Adm. Pública'
  )); gc() 

# Passo 2: definir como um objeto do tipo survey


# Restringindo a POF para a região de interesse

head(pnad$UF)
head(pnad$Estrato)


# Testes para Curitiba 

pnad_svy <- pnad %>% 
  filter(UF == 'Paraná',
         unidade_geo == 'Capital') 

pnad_svy <- as_survey(svydesign(ids = ~UPA, 
                                strata = ~Estrato,
                                weights = ~peso_dom, 
                                data = pnad_svy, 
                                check.strata = TRUE))


# Estatísticas

# Proporção de pessoas ocupadas
pnad_svy %>%  
  filter(!is.na(ocupadas)) %>%
  group_by(ocupadas) %>%
  summarise(survey_prop()) %>%
  select(-(ends_with("_se"))) 
  


# Evolução da ocupação e do salário médio por setores na Região Metropolitana 

# Se a gente souber o que constitui o agroalimentar é só acrescentar aqui
pnad_svy %>% 
  filter(!is.na(setor)) %>%
  group_by(setor) %>%
  summarise(prop_ocupados = survey_prop(),
            rendimento = survey_mean(renda_trab_princ, na.rm = T)) %>%
  select(-(ends_with("_se"))) 


# Parcipação dos brancos e das mulheres no setor agroalimentar 
# precisamos saber o que é o setor agroalimentar (pode ser agricultura + alimentos)

pnad_svy %>% 
  filter(ocupadas == 'Pessoas ocupadas') %>%
  group_by(sexo) %>%
  summarise(sexo_ocupados = survey_prop(),
            sexo_rendimento = survey_mean(renda_trab_princ, na.rm = T)) %>%
  select(-(ends_with("_se"))) 










