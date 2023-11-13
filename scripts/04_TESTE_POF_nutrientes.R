

# Será que vale a última tentativa com os dados de consumo?
cat('Tratamento dos dados de ingestão de alimentos da POF a nível de familia')

# pacotes
library(dplyr)
library(tidyr)
library(survey) # para mexer com os dados amostrais
library(srvyr)

microdados_path <- 'F:/Drive/BASES DE DADOS BRUTOS/POF/POF 2017-2018/Microdados/Dados'

# variaveis de identificação do domicílio (para as chaves)
var_dom <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "NUM_UC")

num_pessoas <- readRDS( file.path(microdados_path, "MORADOR.rds") ) %>%
  group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
           COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(pessoas_dom = max(COD_INFORMANTE))

# Atribuindo o número de pessoas da familia
morador_df <- unique( readRDS(file.path(microdados_path, 
                                        "MORADOR.rds"))[, c(var_dom,
                                                           "PESO_FINAL",
                                                           "RENDA_TOTAL", 
                                                           "PC_RENDA_DISP",
                                                           "PC_RENDA_MONET", 
                                                           "PC_RENDA_NAO_MONET") ]) %>%
  left_join(num_pessoas, by = var_dom)

# Retringindo para a região de interesse

# Big numbers
# O total de familias deve ser calculado por decil
soma_familia <- sum(morador_df$PESO_FINAL)
morador_df$peso_final_fam <- morador_df$pessoas_dom * morador_df$PESO_FINAL # @RCCV criando o peso por domicílio porque a forma padrão de obter os decis com as rendas per capita é usando o peso total do domicílio
# ex.: se você tem 10 pessoas, essas 10 pessoas vão integrar os 10% mais pobres, e não um domicílio vai integrar os 10 domicílios mais pobres
soma_pessoas <- sum(morador_df$peso_final_fam)

# Vamos atribuir o percentil da renda domiciliar per capita por habitante 
# Caminho git
git_path <- 'C:/Users/user/Projetos_GIT/escolhas-scripts-db'

# Vamos rodar as funções no Git
source(file.path( git_path, "scripts/01_criar_funcoes.R" ),encoding = 'UTF-8')

# Base de dados consolidadas
pof <- readRDS( file.path( git_path, "data/despesas_por_grupos_POF.RDS") )

pof <- pof %>%
  mutate( v1 = if_else(is.na(PC_RENDA_MONET), 0, PC_RENDA_MONET),
          v2 = if_else(is.na(PC_RENDA_NAO_MONET), 0, PC_RENDA_NAO_MONET),
          PC_RENDA_TOTAL = v1 + v2) %>% 
  select(-v1, -v2)



# Queremos pegar a base consumo alimentar colocar o consumo total em formato wide na base morador_df

CONSUMO_ALIMENTAR <- readRDS(file.path(microdados_path, "CONSUMO_ALIMENTAR.rds")) %>%
  rename(COD_INFORMANTE = COD_INFOR.MANTE, 
         CA_PESO_FINAL = PESO_FINAL,
         CA_PESO = PESO)

# Vamos juntar essa base com as informações de tipo de processamento

### Tabelas de tradutores para ultra processados (v2 atualizado pela Elizandra)
tradutor_ultra <- readxl::read_excel( file.path(microdados_path, "../Tradutores_de_Tabela/Cadastro de Produtos POF 2019-V2.xlsx"), range = "A1:D8322") 

# Vamos ficar apenas com os que possuem informações
names(tradutor_ultra) <- c('quadro', 'V9001', 'Desc', 'Grupo_Processado')

tradutor_ultra <- tradutor_ultra %>%
  filter(is.na(Grupo_Processado)==F) %>% 
  select(V9001, Grupo_Processado ) %>% 
  mutate(Codigo = round(V9001/100))


# Informação de alimentação 
tradutor_alimentacao <- readxl::read_excel( file.path(microdados_path, "../Tradutores_de_Tabela/Tradutor_Alimentação.xls"))
tradutor_alimentacao <- tradutor_alimentacao %>% left_join(tradutor_ultra, by = 'Codigo')

# Agora vamos incorporar essas informações na base de Consumo Alimentar
CONSUMO_ALIMENTAR <- CONSUMO_ALIMENTAR %>% left_join(tradutor_alimentacao, by = 'V9001')


# TESTE 1 DE CALCULO: consumo de calorias por familia (KCAL/dia) 


df_consumo <- 
  CONSUMO_ALIMENTAR %>% 
  group_by(across(all_of(var_dom))) %>%
  summarise( ENERGIA_KCAL_pc = sum(ENERGIA_KCAL) ) %>% 
  left_join(morador_df, by = var_dom) %>%
  mutate(ENERGIA_KCAL_pc = ENERGIA_KCAL_pc/pessoas_dom)

pof_svy <- as_survey(svydesign(ids = ~COD_UPA, strata = ~ESTRATO_POF , weights = ~peso_final_fam, data = df_consumo))

pof_svy %>%
  summarise(ENERGIA_KCAL_pc = survey_mean( ENERGIA_KCAL_pc,  na.rm = TRUE)) %>%
  as.data.frame()

# Consumo Por Grupo de alimentos

df_consumo <- 
  CONSUMO_ALIMENTAR %>% 
  group_by(across(all_of(c(var_dom, 'Grupo_Processado')))) %>%
  summarise( ENERGIA_KCAL_pc = sum(ENERGIA_KCAL) ) %>%
  filter(!is.na(Grupo_Processado)) %>% 
  pivot_wider( id_cols =  var_dom, 
               names_from = Grupo_Processado, 
               names_prefix = 'grupo_proc_',  
               values_from = ENERGIA_KCAL_pc) %>%
  left_join(morador_df, by = var_dom) %>%
  mutate(ENERGIA_KCAL_pc_natura = grupo_proc_1/pessoas_dom,
         ENERGIA_KCAL_pc_ing_culin = grupo_proc_2/pessoas_dom,
         ENERGIA_KCAL_pc_process = grupo_proc_3/pessoas_dom,
         ENERGIA_KCAL_pc_ultra = grupo_proc_4/pessoas_dom)

# De-para dos grupos: 

#          Grupo_Processado = recode(Grupo_Processado,
#                                    '1' = 'In natura e minimanente processado',
#                                    '2' = 'Ingrediente culinário',
#                                    '3' = 'Processado',
#                                    '4' = 'Ultraprocessado'))

pof_svy <- as_survey(svydesign(ids = ~COD_UPA, 
                               strata = ~ESTRATO_POF,
                               weights = ~peso_final_fam,
                               data = df_consumo))

tab <- 
  pof_svy %>%
  summarise(ENERGIA_KCAL_pc_natura = survey_mean( ENERGIA_KCAL_pc_natura,  na.rm = TRUE),
            ENERGIA_KCAL_pc_ing_culin = survey_mean( ENERGIA_KCAL_pc_ing_culin,  na.rm = TRUE),
            ENERGIA_KCAL_pc_process = survey_mean( ENERGIA_KCAL_pc_process,  na.rm = TRUE),
            ENERGIA_KCAL_pc_ultra = survey_mean( ENERGIA_KCAL_pc_ultra,  na.rm = TRUE)) %>%
  select(-(ends_with("_se"))) %>%
  as.data.frame()


# essa conta seria sensacional: $ dispendido por tipo de alimento e Kcal convertida por tipo de alimento
tab/sum(tab[1,])




# Vamos agregar esse dados a nível da familia









