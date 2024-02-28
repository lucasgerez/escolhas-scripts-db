
# ------------------------------------------------ # 

#           POF 2018 - Base consolidada

# ------------------------------------------------ # 

library(dplyr)
library(tidyr)

# Vamos agregar no nível dos moradores: 
# Despesas por grupos
# Consumo em kcal por tipo de alimentos

# Passos:
# 1. Abrir a base de moradores da POF e criar como referência contendo o
# número de moradores, o peso de cada morador daquele domicílio, o peso do domicílio

# 2. Abrir cada uma das bases de despesas (caderneta coletiva, despesas coletivas,
# despesas individuais, serviços não monetários 2, serviços não monetários 4 e aluguel
# estimado) e separar despesas totais, monetárias e não monetárias e por grupos


# Diretório com as bases brutas
setwd('F:/Drive/BASES DE DADOS BRUTOS/POF/POF 2017-2018/Microdados/Dados')


# 1. Tabelas de referência --------------------------------------

# Vamos ter uma tabela com todos de-para de despesa geral, alimentos e consumo de alimentos

# Tradutor de despesas
tradutor_desp <- readxl::read_excel("../Tradutores_de_Tabela/Tradutor_Despesa_Geral.xls", range = "A1:O5319") %>%
  filter(Variavel == "V8000_DEFLA") %>%
  mutate(check_merge = 1,
         Codigo = as.integer(Codigo)) %>%
  select(-Nivel_4, -Descricao_4, -Nivel_5, -Descricao_5)


# Acrescentando as subclassificações de alimentação
tradutor_alimentacao <- readxl::read_excel( "../Tradutores_de_Tabela/Tradutor_Alimentação.xls" )

tradutor_alimentacao <- 
  tradutor_alimentacao %>% 
  mutate(Codigo = as.integer(Codigo)) %>%
  select(-Nivel_0, -Descricao_0) %>% 
  rename(Nivel_4 = Nivel_1, Descricao_4 = Descricao_1,
         Nivel_5 = Nivel_2, Descricao_5 = Descricao_2,
         Nivel_6 = Nivel_3, Descricao_6 = Descricao_3)


### Tabelas de tradutores para ultra processados (v2 atualizado pela Elizandra)
tradutor_ultra <- readxl::read_excel( "../Tradutores_de_Tabela/Cadastro de Produtos POF 2019-V2.xlsx", range = "A1:D8322") 

# Vamos ficar apenas com os que possuem informações
names(tradutor_ultra) <- c('quadro', 'V9001', 'Desc', 'Grupo_Processado')

tradutor_ultra <- tradutor_ultra %>%
  filter(is.na(Grupo_Processado)==F) %>% 
  select(V9001, Grupo_Processado ) %>% 
  mutate(Codigo = as.integer(round(V9001/100)))

# Regra de decisão: A nível do Código vamos pegar o maior nível de processamento
tradutor_ultra <- tradutor_ultra %>% 
  group_by(Codigo) %>% 
  summarise(Grupo_Processado = max(Grupo_Processado))

# Feito isso vamos colocar na tabela de despesas gerais
tradutor_desp <- tradutor_desp %>% 
  left_join(tradutor_alimentacao, by = 'Codigo') %>%
  left_join(tradutor_ultra, by = 'Codigo')

rm(list = c('tradutor_ultra', 'tradutor_alimentacao')); gc()


# 2. Juncao Tabelas Despesas ----------------------------------------------

# Bases de dados necessárias:
caderneta_coletiva <- readRDS("CADERNETA_COLETIVA.rds")
despesa_coletiva   <- readRDS("DESPESA_COLETIVA.rds") # vai quadro 6 a 19 
despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds")
serv_non_mon2      <- readRDS("SERVICO_NAO_MONETARIO_POF2.rds")
serv_non_mon4      <- readRDS("SERVICO_NAO_MONETARIO_POF4.rds")
aluguel            <- readRDS("ALUGUEL_ESTIMADO.rds")

# Vamos juntar todas tabelas 
despesas_gerais <- bind_rows(caderneta_coletiva, despesa_coletiva, despesa_individual,
                             serv_non_mon2, serv_non_mon4, aluguel)

rm(list = c('caderneta_coletiva', 'despesa_coletiva', 'despesa_individual',
            'serv_non_mon2', 'serv_non_mon4', 'aluguel')); gc()

# Função para estimar valor total:
f_valor_mensal <- function(df, nome_var) {
  
  if (!("V9011" %in% names(df))) {
    df$V9011 <- 1
  }
  
  df$V9011[is.na(df$V9011) & !is.na(df$V8000_DEFLA)] <- 1
  
  df[[nome_var]] <- (df$V8000_DEFLA * df$FATOR_ANUALIZACAO * df$V9011) /12
  
  
  df <- df %>%
    mutate(
      Codigo = as.integer(floor(V9001/100))) %>%
    left_join(tradutor_desp, by = "Codigo") %>%
    mutate(monetaria = if_else(V9002 <= 6, 1, 0),
           grupo_consumo = case_when(
             Descricao_3=="Alimentacao" ~ 1,
             Descricao_3=="Habitacao" ~ 2,
             Descricao_3=="Vestuario" ~ 3,
             Descricao_3=="Transporte" ~ 4, 
             Descricao_3=="Higiene e cuidados pessoais" ~ 5,
             Descricao_3=="Assistencia a saude" ~ 6,
             Descricao_3=="Educacao" ~ 7,
             Descricao_3=="Recreação e cultura" ~ 8,
             Descricao_3=="Fumo" ~ 9,
             Descricao_3=="Serviços pessoais" ~ 10, 
             Descricao_3=="Despesas diversas" ~ 11,
             Descricao_3=="Serviços bancários" ~ 12,
             Descricao_3=="Pensões, mesadas e doações" ~ 13,
             Descricao_3=="Previdência privada" ~ 14,
             Descricao_3=="Contribuições trabalhistas" ~ 15,
             Descricao_3=="Impostos" ~ 16,
             Descricao_3=="Outras" ~ 17,
             Descricao_2=="Prestação de imóvel" ~ 18,
             Descricao_2=="Empréstimo" ~ 19,
             Descricao_2=="Imóvel (reforma)" ~ 20,
             Descricao_2=="Imóvel (aquisição)" ~ 21,
             Descricao_2=="Outros investimentos" ~ 22
           ),
           monetaria = factor(monetaria, levels = c(0,1), labels = c("nao_monetaria", "monetaria")),
           grupo_consumo = factor(grupo_consumo, levels = 1:22, labels = c("Alimentacao",
                                                                           "Habitacao", "Vestuario", "Transporte", "Higiene",
                                                                           "Saude", "Educacao", "Lazer_e_cult", "Fumo", "Serv_pessoais",
                                                                           "diversos", "serv_bancos", "pensoes_doacoes", "previdencia_priv",
                                                                           "cont_trab", "impostos", "outros", "prestacao_imovel", "emprestimo", "imovel_reforma",
                                                                           "imovel_aquis", "outros_inv")),
           grupo_ali_fora = case_when(
             Descricao_4=="Alimentação no domicílio" ~ 1,
             Descricao_4=="Alimentação fora do domicílio" ~ 2
           ), 
           grupo_ali_fora = factor(grupo_ali_fora, levels = 1:2, labels = c('Alimentacao_no_dom', 'Alimentacao_fora_dom')),
           
           grupo_alim_tipo = case_when(
             Descricao_5=="Cereais, leguminosas e oleaginosas" ~ 1,
             Descricao_5=="Tubérculos e raízes" ~ 2,
             Descricao_5=="Alimentos preparados" ~ 3,
             Descricao_5=="Farinhas, féculas e massas" ~ 4,
             Descricao_5=="Bebidas e infusões" ~ 5,
             Descricao_5=="Legumes e verduras" ~ 6,
             Descricao_5=="Sal e condimentos" ~ 7,
             Descricao_5=="Frutas" ~ 8,
             Descricao_5=="Açúcares e derivados" ~ 9,
             Descricao_5=="Carnes, vísceras e pescados" ~ 10,
             Descricao_5=="Enlatados e conservas" ~ 11,
             Descricao_5=="Aves e ovos" ~ 12,
             Descricao_5=="Leites e derivados" ~ 13,
             Descricao_5=="Panificados" ~ 14,
             Descricao_5=="Óleos e gorduras" ~ 15,
             Descricao_5=="Outros" ~ 16
           ), 
           
           grupo_alim_tipo = factor(grupo_alim_tipo, levels = 1:16, labels = c('cereais','tuberculos_raizes','alimentos_preparados',
                                                                               'farinhas_e_massas','bebidas_e_infusoes','legumes_e_verduras',
                                                                               'sal','frutas','acucares_e_derivados',
                                                                               'carnes_e_pescados','enlatados','aves_e_ovos',
                                                                               'leites_e_derivados','panificados','oleos_e_gorduras','outros') ),
           
           grupo_alim_fora_tipo = case_when(
             Descricao_5=="Almoço e jantar" ~ 1,
             Descricao_5=="Outras" ~ 2,
             Descricao_5=="Sanduíches e salgados" ~ 3,
             Descricao_5=="Alimentação light e diet" ~ 4,
             Descricao_5=="Café, leite, café/leite e chocolate" ~ 5,
             Descricao_5=="Refrigerantes e outras bebidas não alcoólicas" ~ 6,
             Descricao_5=="Cervejas, chopes e outras bebidas alcoólicas" ~ 7,
             Descricao_5=="Lanches" ~ 8
           ), 
           grupo_alim_fora_tipo = factor(grupo_alim_fora_tipo, levels = 1:8, labels = c('almoco_e_jantar','outras','sanduiches_e_salgados',
                                                                                        'alimentacao_light_diet','cafe_leite_choco',
                                                                                        'refri_e_outras_nao_alcooicas', 
                                                                                        'cervejas_e_outras_alcoólicas','lanches')), 
           grupo_alim_tipo_process = factor(Grupo_Processado, levels = 1:4, labels = c('in_natura',
                                                                                       'ing_culinario',
                                                                                       'processado',
                                                                                       'ultraprocessado'))
    ) %>%
    as.data.frame()
  
  return(df)
  
  
}

# Aplicar as funcoes de classificacao das despesas
despesas_gerais <- f_valor_mensal(despesas_gerais, "desp"); gc()


# variaveis de identificação do domicílio (para as chaves)
var_dom <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "NUM_UC")

## 2.1. Despesas a nível de grupo ----
desp.nivel.3.all <- 
  despesas_gerais %>%
  filter(!is.na(grupo_consumo)) %>%
  group_by(across(all_of(c(var_dom, 'grupo_consumo')))) %>%
  summarise(desp_grupo = sum(desp, na.rm = TRUE))  %>%
  ungroup() %>% 
  pivot_wider(names_from = grupo_consumo,
              values_from = desp_grupo,
              names_prefix = "desp_nivel3_") %>%
  rowwise() %>%
  mutate(sum_desp_nivel3 = sum(c_across(starts_with("desp_nivel3_")), na.rm = TRUE)); gc()

## 2.2. Despesas monetárias a nível de grupo ----
desp.nivel.3.monet <- 
  despesas_gerais %>%
  filter(!is.na(grupo_consumo)) %>%
  filter(monetaria == 'monetaria') %>%
  group_by(across(all_of(c(var_dom, 'grupo_consumo'))))%>%
  summarise(desp_grupo = sum(desp, na.rm = TRUE))  %>%
  ungroup() %>% 
  pivot_wider(names_from = grupo_consumo, 
              values_from = desp_grupo,
              names_prefix = "desp_nivel3_monet_") %>%
  rowwise() %>%
  mutate(sum_desp_nivel3_monet = sum(c_across(starts_with("desp_nivel3_monet_")), na.rm = TRUE)); gc()


## 2.3. Despesas não monetárias a nível de grupo ----
desp.nivel.3.nao.monet <- 
  despesas_gerais %>%
  filter(!is.na(grupo_consumo)) %>%
  filter(monetaria == 'monetaria') %>%
  group_by(across(all_of(c(var_dom, 'grupo_consumo'))))%>%
  summarise(desp_grupo = sum(desp, na.rm = TRUE))  %>%
  ungroup() %>% 
  pivot_wider(names_from = grupo_consumo, 
              values_from = desp_grupo,
              names_prefix = "desp_nivel3_nao_monet_") %>% 
  rowwise() %>%
  mutate(sum_desp_nivel3_nao_monet = sum(c_across(starts_with("desp_nivel3_nao_monet_")), na.rm = TRUE)); gc()


# Depesas com alimentos dentro e fora do domicilio
desp.nivel.4 <- 
  despesas_gerais %>%
  filter(!is.na(grupo_ali_fora)) %>%
  group_by(across(all_of(c(var_dom, 'grupo_ali_fora'))))%>%
  summarise(desp_grupo = sum(desp, na.rm = TRUE))  %>%
  ungroup() %>% 
  pivot_wider(names_from = grupo_ali_fora, 
              values_from = desp_grupo,
              names_prefix = "desp_nivel4_") %>%
  rowwise() %>%
  mutate(sum_desp_nivel4 = sum(c_across(starts_with("desp_nivel4_")), na.rm = TRUE)); gc()


# Tipo de alimento dentro do dom
desp.nivel.5.dentro.dom <- 
  despesas_gerais %>%
  filter(!is.na(grupo_alim_tipo)) %>%
  group_by(across(all_of(c(var_dom, 'grupo_alim_tipo'))))%>%
  summarise(desp_grupo = sum(desp, na.rm = TRUE))  %>%
  ungroup() %>% 
  pivot_wider(names_from = grupo_alim_tipo,
              values_from = desp_grupo,
              names_prefix = "desp_nivel5_dentro_dom_") %>%
  rowwise() %>%
  mutate(sum_desp_nivel5_dentro_dom = sum(c_across(starts_with("desp_nivel5_dentro_dom_")), na.rm = TRUE)); gc()


# Fora do domicilio
desp.nivel.5.fora.dom <- 
  despesas_gerais %>%
  filter(!is.na(grupo_alim_fora_tipo)) %>%
  group_by(across(all_of(c(var_dom, 'grupo_alim_fora_tipo'))))%>%
  summarise(desp_grupo = sum(desp, na.rm = TRUE))  %>%
  ungroup() %>% 
  pivot_wider(names_from = grupo_alim_fora_tipo,
              values_from = desp_grupo,
              names_prefix = "desp_nivel5_fora_dom_") %>%
  rowwise() %>%
  mutate(sum_desp_nivel5_fora_dom = sum(c_across(starts_with("desp_nivel5_fora_dom_")), na.rm = TRUE)); gc()


## 2.4 Despesas por tipo de processamento ----
desp.nivel.6.tipo.proc <- 
  despesas_gerais %>%
  filter(!is.na(grupo_alim_tipo_process)) %>%
  group_by(across(all_of(c(var_dom, 'grupo_alim_tipo_process'))))%>%
  summarise(desp_grupo = sum(desp, na.rm = TRUE))  %>%
  ungroup() %>% 
  pivot_wider(names_from = grupo_alim_tipo_process,
              values_from = desp_grupo,
              names_prefix = "desp_nivel6_tipo_") %>%
  rowwise() %>%
  mutate(sum_desp_nivel6_tipo_proc = sum(c_across(starts_with("desp_nivel6_tipo_")), na.rm = TRUE)); gc()


## 2.5 Junção de todas as bases de despesas ---
desp_df <- desp.nivel.3.all %>% 
  full_join(desp.nivel.3.monet, by = var_dom) %>%
  full_join(desp.nivel.3.nao.monet, by = var_dom) %>%
  full_join(desp.nivel.4, by = var_dom) %>% 
  full_join(desp.nivel.5.dentro.dom, by = var_dom) %>% 
  full_join(desp.nivel.5.fora.dom, by = var_dom) %>%
  full_join(desp.nivel.6.tipo.proc, by = var_dom) 

rm(list = c('desp.nivel.3.all', 'desp.nivel.3.monet', 'desp.nivel.3.nao.monet',
            'desp.nivel.4', 'desp.nivel.5.dentro.dom', 'desp.nivel.5.fora.dom',
            'desp.nivel.6.tipo.proc', 'despesas_gerais')); gc()


# 3. Base de moradores --------------------------------------

# Base de moradores
MORADOR <- readRDS("MORADOR.rds")

# Base apenas com as pessoas responsáveis do domicílio
resp.dom <- MORADOR %>% 
  filter(V0306 == 1) %>% 
  mutate(chefe_dom_over_65 = ifelse(!is.na(V0403) & V0403 >= 65, 1, 0 ),
         chefe_dom_mulher = ifelse(!is.na(V0404) & V0404 == 2, 1, 0 ),
         chefe_dom_negro = ifelse(!is.na(V0405) & V0405 %in% c(2, 4), 1, 0 ),
         chefe_dom_analf = ifelse(!is.na(V0414) & V0414 == 2, 1, 0 ),
         chefe_dom_EF = ifelse(!is.na(V0425) & V0425 %in% c(1:8) , 1, 0 ),
         chefe_dom_EM = ifelse(!is.na(V0425) & V0425 %in% c(9:11), 1, 0 )) %>% 
  select(var_dom, chefe_dom_over_65, 
         chefe_dom_mulher, chefe_dom_negro, 
         chefe_dom_analf, chefe_dom_EF, chefe_dom_EM)

# Número de pessoas no domicilio
num_pessoas <- readRDS("MORADOR.rds") %>%
  group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
           COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(pessoas_dom = max(COD_INFORMANTE))

# Atribuindo o número de pessoas da familia e as características dos chefe de familia
morador_df <- unique(readRDS("MORADOR.rds")[, c(var_dom, "PESO_FINAL", "RENDA_TOTAL", "PC_RENDA_DISP", "PC_RENDA_MONET", "PC_RENDA_NAO_MONET") ]) %>%
  left_join(num_pessoas, by = var_dom) %>%
  left_join(resp.dom, by = var_dom) %>%
  mutate(peso_final_fam = pessoas_dom * PESO_FINAL); gc()

# Se quisermos fazer o join das despesas gerais com a de pessoas vamos precisar da base MORADOR.rds e do id var_pessoa
pof_2018 <- full_join(desp_df, morador_df, by = var_dom) %>% as.data.frame()

# Vamos calcular todas essas variáveis a nível per capita do domicílio

# Extract variables with prefix 'desp_'
desp_vars <- grep("^desp_", names(pof_2018), value = TRUE)

# Create new variables with the prefix 'pc_desp_'
for (var in desp_vars) {
  
  new_var_name <- paste0("pc_", var)
  
  cat('\n',new_var_name, match(var, desp_vars), "de", length(desp_vars),'...')
  
  pof_2018[[new_var_name]] <- pof_2018[[var]] / pof_2018$pessoas_dom
  
}

rm(list = c('desp_df', 'morador_df', 'num_pessoas',
            'resp.dom', 'MORADOR')); gc()

# 4. Consumo Nutrientes ------------------------------------------------------

# Informações sobre o consumo de alimentos
CONSUMO_ALIMENTAR <- readRDS( "CONSUMO_ALIMENTAR.rds" ) %>%
  rename(COD_INFORMANTE = COD_INFOR.MANTE, 
         CA_PESO_FINAL = PESO_FINAL,
         CA_PESO = PESO) %>%
  mutate(Codigo = round(V9001/100))

# Agora vamos incorporar essas informações na base de Consumo Alimentar
CONSUMO_ALIMENTAR <- CONSUMO_ALIMENTAR %>% 
  left_join(tradutor_desp, by = 'Codigo')

consumo_nutrientes <- CONSUMO_ALIMENTAR %>% 
  filter(!is.na(ENERGIA_KCAL)) %>%
  group_by(across(all_of(c(var_dom))))%>%
  summarise(consumo_kcal = sum(ENERGIA_KCAL, na.rm = TRUE),
            consumo_kj = sum(ENERGIA_KJ, na.rm = TRUE),
            consumo_gramas = sum(GRAMATURA1, na.rm = TRUE))  

# Vamos calcular o consumo pelos grupos da descricao 5
consumo_kg_grupos <- CONSUMO_ALIMENTAR %>% 
  mutate(
    grupo_alim_tipo = case_when(
    Descricao_5=="Cereais, leguminosas e oleaginosas" ~ 1,
    Descricao_5=="Tubérculos e raízes" ~ 2,
    Descricao_5=="Alimentos preparados" ~ 3,
    Descricao_5=="Farinhas, féculas e massas" ~ 4,
    Descricao_5=="Bebidas e infusões" ~ 5,
    Descricao_5=="Legumes e verduras" ~ 6,
    Descricao_5=="Sal e condimentos" ~ 7,
    Descricao_5=="Frutas" ~ 8,
    Descricao_5=="Açúcares e derivados" ~ 9,
    Descricao_5=="Carnes, vísceras e pescados" ~ 10,
    Descricao_5=="Enlatados e conservas" ~ 11,
    Descricao_5=="Aves e ovos" ~ 12,
    Descricao_5=="Leites e derivados" ~ 13,
    Descricao_5=="Panificados" ~ 14,
    Descricao_5=="Óleos e gorduras" ~ 15,
    Descricao_5=="Outros" ~ 16
  )) %>% 
  mutate(
    
    grupo_alim_tipo = factor(grupo_alim_tipo, levels = 1:16, labels = c('cereais','tuberculos_raizes','alimentos_preparados',
                                                                        'farinhas_e_massas','bebidas_e_infusoes','legumes_e_verduras',
                                                                        'sal','frutas','acucares_e_derivados',
                                                                        'carnes_e_pescados','enlatados','aves_e_ovos',
                                                                        'leites_e_derivados','panificados','oleos_e_gorduras','outros') )
    
  ) %>% 
  mutate(
    
    grupo_alim_fora_tipo = case_when(
      Descricao_5=="Almoço e jantar" ~ 1,
      Descricao_5=="Outras" ~ 2,
      Descricao_5=="Sanduíches e salgados" ~ 3,
      Descricao_5=="Alimentação light e diet" ~ 4,
      Descricao_5=="Café, leite, café/leite e chocolate" ~ 5,
      Descricao_5=="Refrigerantes e outras bebidas não alcoólicas" ~ 6,
      Descricao_5=="Cervejas, chopes e outras bebidas alcoólicas" ~ 7,
      Descricao_5=="Lanches" ~ 8
    ), 
    grupo_alim_fora_tipo = factor(grupo_alim_fora_tipo, levels = 1:8, labels = c('almoco_e_jantar','outras','sanduiches_e_salgados',
                                                                                 'alimentacao_light_diet','cafe_leite_choco',
                                                                                 'refri_e_outras_nao_alcooicas', 
                                                                                 'cervejas_e_outras_alcoólicas','lanches')), 
    grupo_alim_tipo_process = factor(Grupo_Processado, levels = 1:4, labels = c('in_natura',
                                                                                'ing_culinario',
                                                                                'processado',
                                                                                'ultraprocessado'))
    
  ) 
  

# Feito isso vamos calcular o consumo em kg para os grupos 

consumo_kg_desc5 <- consumo_kg_grupos %>% 
  filter(!is.na(ENERGIA_KCAL)) %>%
  group_by(across(all_of(c(var_dom, 'grupo_alim_tipo'))))%>%
  summarise(consumo_gramas = sum(GRAMATURA1, na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = grupo_alim_tipo,
              values_from = consumo_gramas,
              names_prefix = "consumo_gr_dom_") %>%
  rowwise() 
  

# Aqui temos a soma de gramas de alimentos processados
consumo_kg_tipo_proces <- consumo_kg_grupos %>% 
  filter(!is.na(ENERGIA_KCAL)) %>%
  group_by(across(all_of(c(var_dom, 'grupo_alim_tipo_process'))))%>%
  summarise(consumo_gramas = sum(GRAMATURA1, na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = grupo_alim_tipo_process,
              values_from = consumo_gramas,
              names_prefix = "consumo_gr_tipo_proc_") %>%
  rowwise() 

# Aqui temos a soma de kcal de alimentos processados
consumo.tipo.proc <-  
  CONSUMO_ALIMENTAR %>%
  filter(!is.na(Grupo_Processado)) %>%
  mutate(grupo_alim_tipo_process = factor(Grupo_Processado, levels = 1:4, 
                                          labels = c('in_natura',
                                                     'ing_culinario',
                                                     'processado',
                                                     'ultraprocessado'))
  ) %>%
  group_by(across(all_of(c(var_dom, 'grupo_alim_tipo_process'))))%>%
  summarise(consumo_grupo = sum(ENERGIA_KCAL, na.rm = TRUE))  %>%
  ungroup() %>% 
  pivot_wider(names_from = grupo_alim_tipo_process,
              values_from = consumo_grupo,
              names_prefix = "consumo_nivel6_tipo_") %>%
  rowwise() %>%
  mutate(sum_consumo_nivel6_tipo = sum(c_across(starts_with("consumo_nivel6_tipo_")), na.rm = TRUE)); gc()


pof_2018 <- pof_2018 %>% 
  full_join(consumo_nutrientes, by = var_dom) %>%
  full_join(consumo.tipo.proc, by = var_dom) %>% 
  full_join(consumo_kg_desc5, by = var_dom) %>%
  full_join(consumo_kg_tipo_proces, by = var_dom) 

consumo_vars <- grep("^consumo_", names(pof_2018), value = TRUE)

# Create new variables with the prefix 'pc_consumo_'
for (var in consumo_vars) {
  
  new_var_name <- paste0("pc_", var)
  
  cat('\n',new_var_name, match(var, consumo_vars), "de", length(consumo_vars),'...')
  
  pof_2018[[new_var_name]] <- pof_2018[[var]] / pof_2018$pessoas_dom
  
}

rm(list = c('CONSUMO_ALIMENTAR', 'consumo_nutrientes', 'consumo.tipo.proc')); gc()


# 5. Insegurança alimentar ------

# base Condições de vida

CONDICOES_VIDA <- readRDS( "CONDICOES_VIDA.RDS" ) 

# Definicação de segurança alimentar (SA): 
# A condição de Segurança Alimentar (SA) reflete o 
# pleno acesso dos moradores dos domicílios aos alimentos,
# tanto em quantidade suficiente como em qualidade adequada, 
# de tal modo que a pessoa entrevistada sequer relata preocupação 
# ou iminência de sofrer qualquer restrição alimentar no futuro próximo


# Insegurança Alimentar Leve (IA leve)
# há preocupação com o acesso aos alimentos no futuro e já se 
# verifica comprometimento da qualidade da alimentação, ou os 
# adultos da família assumem estratégias para manter uma quantidade
# mínima de alimentos disponível aos seus integrantes

# Insegurança Alimentar Moderada (IA moderada)
# Nos domicílios com Insegurança Alimentar Moderada (IA moderada), 
# os moradores, em especial os adultos, passaram a conviver com restrição
# quantitativa de alimentos no período de referência

# Insegurança Alimentar Grave (IA grave)
# O nível de Insegurança Alimentar Grave (IA grave) significa que houve 
# ruptura nos padrões de alimentação resultante da falta de alimentos 
# entre todos os moradores, incluindo, quando presentes, as crianças.

CONDICOES_VIDA <- CONDICOES_VIDA %>% 
  mutate(
    inseguranca_alimentar = case_when(
      V6108 == 2 & V6109 == 2 & V6110 == 2 & V6111 == 2 & 
        V6112 %in% c(NA, 2) & V6113 %in% c(NA, 2) & V6114 %in% c(NA, 2) & V6115 %in% c(NA, 2) &
        V6116 %in% c(NA, 2) & V6117 %in% c(NA, 2) & V6118 %in% c(NA, 2) & V6119 %in% c(NA, 2) & 
        V6120 %in% c(NA, 2) & V6121 %in% c(NA, 2) ~ 1,
      ( V6108 == 1 | V6109 == 1 | V6110 == 1 | V6111 == 1 | V6113 == 1 |  V6116 == 1) & 
        V6112 %in% c(NA, 2) & V6114 %in% c(NA, 2) & V6115 %in% c(NA, 2) &
        V6116 %in% c(NA, 2) & V6117 %in% c(NA, 2) & V6118 %in% c(NA, 2) & V6119 %in% c(NA, 2) & 
        V6120 %in% c(NA, 2) & V6121 %in% c(NA, 2) ~ 2,
      ( V6108 == 1 | V6109 == 1 | V6110 == 1 | V6111 == 1 | V6112 == 1 | V6113 == 1 |
          V6114 == 1 | V6116 == 1 | V6117 == 1 | V6118 == 1 | V6119 == 1) &
        V6115 %in% c(NA, 2) & V6120 %in% c(NA, 2) & V6121 %in% c(NA, 2) ~ 3,
      ( V6108 == 1 | V6109 == 1 | V6110 == 1 | V6111 == 1 | V6112 == 1 | V6113 == 1 |
          V6114 == 1 | V6116 == 1 | V6117 == 1 | V6118 == 1 | V6119 == 1 |
          
          
          V6115 == 1 | V6120 == 1 | V6121 == 1) ~ 4)
  ) %>%
  mutate(
    inseguranca_alimentar = factor(inseguranca_alimentar, 1:4, 
                                   labels = c('SA',
                                              'IA_leve',
                                              'IA_moderada',
                                              'IA_grave'))) %>% 
  select(var_dom, inseguranca_alimentar)


# Tabela a nível de familia finalizada
pof_2018 <- pof_2018 %>% 
  full_join(CONDICOES_VIDA, by = var_dom) 


# 6. Tabela Final ------------------------------------------------------------

git.path <- 'C:/Users/user/Projetos_GIT/escolhas-scripts-db/data'

saveRDS(pof_2018, file.path(git.path, "pof_fam_wide_2018.RDS")) 


