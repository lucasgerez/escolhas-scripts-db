# Ricardo Campante Cardoso Vale
# Criando a Base da POF com despesas totais, monetárias e não monetárias
# 28 de outubro de 2023 (YYYYMMDD = 20231028)


# Script N1: tratamento dos dados

# Passos:
# 1. Abrir a base de moradores da POF e criar como referência contendo o
# número de moradores, o peso de cada morador daquele domicílio, o peso do domicílio

# 2. Abrir cada uma das bases de despesas (caderneta coletiva, despesas coletivas,
# despesas individuais, serviços não monetários 2, serviços não monetários 4 e aluguel
# estimado) e separar despesas totais, monetárias e não monetárias e por grupos



# Preâmbulo -------------------------------
# Limpar dados da área de trabalho
rm(list = ls())

# Caminho para os dados da POF: 
setwd('C:/Users/ricar/Documents/consultoria/gerez/dados pof')

# Caminho para exportar as tabelas
export <- 'C:/Users/ricar/Documents/consultoria/gerez/resultados'


# pacotes
library(dplyr)
library(tidyr)
library(survey) # para mexer com os dados amostrais
library(openxlsx)

arquivos <- list.files()
arquivos <- arquivos[grep('.rds', arquivos)]

# Base de morador --------------------

# Base de referência

num_pessoas <- readRDS("MORADOR.rds") %>%
  group_by(UF, ESTRATO_POF, TIPO_SITUACAO_REG,
           COD_UPA, NUM_DOM, NUM_UC) %>% 
  summarise(pessoas_dom = max(COD_INFORMANTE))


# variaveis de identificação do domicílio (para as chaves)
var_dom <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
             "COD_UPA", "NUM_DOM", "NUM_UC")

# Atribuindo o número de pessoas da familia
morador_df <- unique(readRDS("MORADOR.rds")[, c(var_dom, "PESO_FINAL", "RENDA_TOTAL", "PC_RENDA_DISP", "PC_RENDA_MONET", "PC_RENDA_NAO_MONET") ]) %>%
  left_join(num_pessoas, by = var_dom)

# Big numbers
# O total de familias deve ser calculado por decil
soma_familia <- sum(morador_df$PESO_FINAL)
morador_df$peso_final_fam <- morador_df$pessoas_dom * morador_df$PESO_FINAL # @RCCV criando o peso por domicílio porque a forma padrão de obter os decis com as rendas per capita é usando o peso total do domicílio
# ex.: se você tem 10 pessoas, essas 10 pessoas vão integrar os 10% mais pobres, e não um domicílio vai integrar os 10 domicílios mais pobres
soma_pessoas <- sum(morador_df$peso_final_fam)

rm(num_pessoas)

# Tamanho médio por familia 
# weighted.mean(morador_df$pessoas_dom, w = morador_df$PESO_FINAL)

# Vamos definir o dataframe como survey
survey_design <- svydesign(ids = ~1, weights = ~peso_final_fam, data = morador_df)

# Calculate the weighted deciles
deciles0 <- svyquantile(~PC_RENDA_MONET,
                       subset(survey_design, subset = !is.na(PC_RENDA_MONET))  , 
                       quantiles = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))

# Extract the deciles
weighted_deciles0 <- deciles0$PC_RENDA_MONET[,1]

# Atribuindo o decil que a pessoa está
morador_df$decis0 <- cut(morador_df$PC_RENDA_DISP, 
                                     breaks = weighted_deciles0,
                                     labels = F, 
                                     include.lowest = T, 
                                     na.pass = TRUE)

# Mesma coisa mas para Renda disponível (dá para fazer função depois, mas tempo não compensa agora)
# Calculate the weighted deciles
deciles1 <- svyquantile(~PC_RENDA_DISP,
                        subset(survey_design, subset = !is.na(PC_RENDA_DISP))  , 
                        quantiles = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))

# Extract the deciles
weighted_deciles1 <- deciles1$PC_RENDA_DISP[,1]

# Atribuindo o decil que a pessoa está
morador_df$decis1 <- cut(morador_df$PC_RENDA_DISP, 
                         breaks = weighted_deciles1,
                         labels = F, 
                         include.lowest = T, 
                         na.pass = TRUE)
rm(list = c("deciles0", "deciles1", "weighted_deciles0", "weighted_deciles1"))

# testar consist^encia dos decis
#teste <- morador_df$decis0 - morador_df$decis1
# summary(teste)


# Cálculo das despesas totais e classificação delas -------------------------------------------------

# Bases de dados necessárias:
caderneta_coletiva <- readRDS("CADERNETA_COLETIVA.rds")
despesa_coletiva <- readRDS("DESPESA_COLETIVA.rds") # vai quadro 6 a 19 
despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds")
serv_non_mon2 <- readRDS("SERVICO_NAO_MONETARIO_POF2.rds")
serv_non_mon4 <- readRDS("SERVICO_NAO_MONETARIO_POF4.rds")
aluguel <- readRDS("ALUGUEL_ESTIMADO.rds")


# Tradutor de despesas
tradutor_desp <- readxl::read_excel("Tradutor_Despesa_Geral.xls", range = "A1:O5319") %>%
  filter(Variavel == "V8000_DEFLA") %>%
  mutate(check_merge = 1,
         Codigo = as.integer(Codigo))

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
           grupo_consumo = factor(grupo_consumo, levels = 1:22, labels = c("Alimetacao",
                            "Habitacao", "Vestuario", "Transporte", "Higiene",
                            "Saude", "Educacao", "Lazer_e_cult", "Fumo", "Serv_pessoais",
                            "diversos", "serv_bancos", "pensoes_doacoes", "previdencia_priv",
                            "cont_trab", "impostos", "outros", "prestacao_imovel", "emprestimo", "imovel_reforma",
                            "imovel_aquis", "outros_inv")
                            )
           ) %>%
    as.data.frame()
    
  return(df)
  
}

# Aplicar as funcoes de classificacao das despesas
caderneta_coletiva <- f_valor_mensal(caderneta_coletiva, "desp_cc")
despesa_coletiva <- f_valor_mensal(despesa_coletiva, "desp_dc")
despesa_individual <- f_valor_mensal(despesa_individual, "desp_di")
serv_non_mon2 <- f_valor_mensal(serv_non_mon2, "desp_snm2")
serv_non_mon4 <- f_valor_mensal(serv_non_mon4, "desp_snm4")
aluguel <- f_valor_mensal(aluguel, "desp_alu")

#  Criar despesa total por domicilio:
b1 <- caderneta_coletiva %>%
                      group_by(across(all_of(var_dom))) %>%
                      summarise(
                        desp_cc = sum(desp_cc, na.rm = TRUE)
                      ) %>%
                    ungroup()

b2 <- despesa_coletiva %>%
  group_by(across(all_of(var_dom))) %>%
  summarise(
    desp_dc = sum(desp_dc, na.rm = TRUE)
  ) %>%
  ungroup()

b3 <- despesa_individual %>%
  group_by(across(all_of(var_dom))) %>%
  summarise(
    desp_di = sum(desp_di, na.rm = TRUE)
  )  %>%
  ungroup()

b4 <- serv_non_mon2 %>%
  group_by(across(all_of(var_dom))) %>%
  summarise(
    desp_snm2 = sum(desp_snm2, na.rm = TRUE)
  )  %>%
  ungroup()

b5 <- serv_non_mon4 %>%
  group_by(across(all_of(var_dom))) %>%
  summarise(
    desp_snm4 = sum(desp_snm4, na.rm = TRUE)
  )  %>%
  ungroup()

b6 <- aluguel %>%
  group_by(across(all_of(var_dom))) %>%
  summarise(
    desp_alu = sum(desp_alu, na.rm = TRUE)
  )  %>%
  ungroup()
  


 
morador_df1 <- morador_df %>%
  left_join(b1, by = var_dom) %>%
  left_join(b2, by = var_dom) %>%
  left_join(b3, by = var_dom) %>%
  left_join(b4, by = var_dom) %>%
  left_join(b5, by = var_dom) %>%
  left_join(b6, by = var_dom) %>%
  mutate(
    desp_total_tudo = across(starts_with("desp_")) %>% rowSums(na.rm = TRUE),
    desp_total_no_alu = desp_total_tudo - desp_alu
  )

morador_df1$desp_total_no_alu[is.na(morador_df1$desp_alu)] <- morador_df1$desp_total_tudo[is.na(morador_df1$desp_alu)]

rm(list = c("b1", "b2", "b3", "b4", "b5", "b6"))


# Despesas monetarias totais ---------------------------------------------------

b1 <- caderneta_coletiva %>%
  filter(monetaria == "monetaria") %>% 
  group_by(across(all_of(var_dom))) %>%
  summarise(
    desp_mon_cc = sum(desp_cc, na.rm = TRUE)
  ) %>%
  ungroup()

b2 <- despesa_coletiva %>%
  filter(monetaria == "monetaria") %>% 
  group_by(across(all_of(var_dom))) %>%
  summarise(
    desp_mon_dc = sum(desp_dc, na.rm = TRUE)
  ) %>%
  ungroup()

b3 <- despesa_individual %>%
  filter(monetaria == "monetaria") %>% 
  group_by(across(all_of(var_dom))) %>%
  summarise(
    desp_mon_di = sum(desp_di, na.rm = TRUE)
  ) %>%
  ungroup()

# tem que dar zero
# b4 <- serv_non_mon2 %>%
#   filter(monetaria == "monetaria") %>% 
#   group_by(across(all_of(var_dom))) %>%
#   summarise(
#     desp_mon_snm2 = sum(desp_snm2)
#   ) %>%
#   ungroup()

# b5 <- serv_non_mon4 %>%
#   filter(monetaria == "monetaria") %>%
#   group_by(across(all_of(var_dom))) %>%
#   summarise(
#     desp_mon_snm4 = sum(desp_snm4)
#   )
# 
# b6 <- aluguel %>%
#   filter(monetaria == "monetaria") %>%
#   group_by(across(all_of(var_dom))) %>%
#   summarise(
#     desp_mon_alu = sum(desp_alu)
#   )
# de fato d'a zero
  
morador_df2 <- morador_df1 %>%
  left_join(b1, by = var_dom) %>%
  left_join(b2, by = var_dom) %>%
  left_join(b3, by = var_dom) %>%
  mutate(
    desp_mon_total = across(starts_with("desp_mon_")) %>% rowSums(na.rm = TRUE)
  )

rm(list = c("b1", "b2", "b3"))  


# Despesas por grupo de consumo ------------------------------------------------

var_dom1 <- var_dom
var_dom1[7] <- "grupo_consumo"

b1 <- caderneta_coletiva %>%
  group_by(across(all_of(var_dom1))) %>%
  summarise(
    desp_cc = sum(desp_cc, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
  pivot_wider(names_from = grupo_consumo, values_from = desp_cc,
              names_prefix = "desp_cc_g")


b2 <- despesa_coletiva %>%
  group_by(across(all_of(var_dom1))) %>%
  summarise(
    desp_dc = sum(desp_dc, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
  pivot_wider(names_from = grupo_consumo, values_from = desp_dc,
              names_prefix = "desp_dc_g")

b3 <- despesa_individual %>%
  group_by(across(all_of(var_dom1))) %>%
  summarise(
    desp_di = sum(desp_di, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
  pivot_wider(names_from = grupo_consumo, values_from = desp_di,
              names_prefix = "desp_di_g")

b4 <- serv_non_mon2 %>%
  group_by(across(all_of(var_dom1))) %>%
  summarise(
       desp_snm2 = sum(desp_snm2, na.rm = TRUE)
     ) %>%
     ungroup() %>%
  mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
  pivot_wider(names_from = grupo_consumo, values_from = desp_snm2,
              names_prefix = "desp_snm2_g")

b5 <- serv_non_mon4 %>%
  group_by(across(all_of(var_dom1))) %>%
  summarise(
    desp_snm4 = sum(desp_snm4, na.rm = TRUE)
   ) %>%
  ungroup() %>%
  mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
  pivot_wider(names_from = grupo_consumo, values_from = desp_snm4,
              names_prefix = "desp_snm4_g")

 b6 <- aluguel %>%
   group_by(across(all_of(var_dom1))) %>%
   summarise(
     desp_alu = sum(desp_alu, na.rm = TRUE)
   )  %>%
   ungroup() %>%
   mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
   pivot_wider(names_from = grupo_consumo, values_from = desp_alu,
               names_prefix = "desp_alu_g")
 
 
 morador_df3 <- morador_df2 %>%
   left_join(b1, by = var_dom) %>%
   left_join(b2, by = var_dom) %>%
   left_join(b3, by = var_dom) %>%
   left_join(b4, by = var_dom) %>%
   left_join(b5, by = var_dom) %>%
   left_join(b6, by = var_dom) %>%
   mutate(
     desp_g1 = across(ends_with("_g1")) %>% rowSums(na.rm = TRUE),
     desp_g2 = across(ends_with("_g2")) %>% rowSums(na.rm = TRUE),
     desp_g3 = across(ends_with("_g3")) %>% rowSums(na.rm = TRUE),
     desp_g4 = across(ends_with("_g4")) %>% rowSums(na.rm = TRUE),
     desp_g5 = across(ends_with("_g5")) %>% rowSums(na.rm = TRUE),
     desp_g6 = across(ends_with("_g6")) %>% rowSums(na.rm = TRUE),
     desp_g7 = across(ends_with("_g7")) %>% rowSums(na.rm = TRUE),
     desp_g8 = across(ends_with("_g8")) %>% rowSums(na.rm = TRUE),
     desp_g9 = across(ends_with("_g9")) %>% rowSums(na.rm = TRUE),
     desp_g10 = across(ends_with("_g10")) %>% rowSums(na.rm = TRUE),
     desp_g11 = across(ends_with("_g11")) %>% rowSums(na.rm = TRUE),
     desp_g12 = across(ends_with("_g12")) %>% rowSums(na.rm = TRUE),
     desp_g13 = across(ends_with("_g13")) %>% rowSums(na.rm = TRUE),
     desp_g14 = across(ends_with("_g14")) %>% rowSums(na.rm = TRUE),
     desp_g15 = across(ends_with("_g15")) %>% rowSums(na.rm = TRUE),
     desp_g16 = across(ends_with("_g16")) %>% rowSums(na.rm = TRUE),
     desp_g17 = across(ends_with("_g17")) %>% rowSums(na.rm = TRUE),
     desp_g18 = across(ends_with("_g18")) %>% rowSums(na.rm = TRUE),
     desp_g19 = across(ends_with("_g19")) %>% rowSums(na.rm = TRUE),
     desp_g20 = across(ends_with("_g20")) %>% rowSums(na.rm = TRUE),
     desp_g21 = across(ends_with("_g21")) %>% rowSums(na.rm = TRUE),
     desp_g22 = across(ends_with("_g22")) %>% rowSums(na.rm = TRUE)
   )
 
 morador_df3 <- morador_df3 %>%
   select(- (desp_cc_g1:desp_alu_g2))

 # check <- morador_df3 %>%
 #   select(desp_total_tudo, desp_g1:desp_g22) %>%
 #   mutate(teste = across(desp_g1:desp_g22)  %>% rowSums(na.rm = TRUE) ) %>%
 #   select(desp_total_tudo, teste, desp_g1:desp_g22)
 # 
 # rm(check)
 # fixed after na.rm = TRUE was inserted in all the commands
 
 
 rm(list = c("b1", "b2", "b3", "b4", "b5", "b6"))
 
 
 # Despesas monetarias por grupo de consumo ------------------------------------
 
 b1 <- caderneta_coletiva %>%
   filter(monetaria == "monetaria") %>% 
   group_by(across(all_of(var_dom1))) %>%
   summarise(
     desp_cc = sum(desp_cc, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
   pivot_wider(names_from = grupo_consumo, values_from = desp_cc,
               names_prefix = "desp_cc_mon_g")
 
 
 b2 <- despesa_coletiva %>%
   filter(monetaria == "monetaria") %>%
   group_by(across(all_of(var_dom1))) %>%
   summarise(
     desp_dc = sum(desp_dc, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
   pivot_wider(names_from = grupo_consumo, values_from = desp_dc,
               names_prefix = "desp_dc_mon_g")
 
 b3 <- despesa_individual %>%
   filter(monetaria == "monetaria") %>% 
   group_by(across(all_of(var_dom1))) %>%
   summarise(
     desp_di = sum(desp_di, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
   pivot_wider(names_from = grupo_consumo, values_from = desp_di,
               names_prefix = "desp_di_mon_g")
 
 b4 <- serv_non_mon2 %>%
   filter(monetaria == "monetaria") %>% 
   group_by(across(all_of(var_dom1))) %>%
   summarise(
     desp_snm2 = sum(desp_snm2, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
   pivot_wider(names_from = grupo_consumo, values_from = desp_snm2,
               names_prefix = "desp_snm2_mon_g")
 
 b5 <- serv_non_mon4 %>%
   filter(monetaria == "monetaria") %>% 
   group_by(across(all_of(var_dom1))) %>%
   summarise(
     desp_snm4 = sum(desp_snm4, na.rm = TRUE)
   ) %>%
   ungroup() %>%
   mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
   pivot_wider(names_from = grupo_consumo, values_from = desp_snm4,
               names_prefix = "desp_snm4_mon_g")
 
 b6 <- aluguel %>%
   filter(monetaria == "monetaria") %>%
   group_by(across(all_of(var_dom1))) %>%
   summarise(
     desp_alu = sum(desp_alu, na.rm = TRUE)
   )  %>%
   ungroup() %>%
   mutate(grupo_consumo = as.integer(grupo_consumo)) %>%
   pivot_wider(names_from = grupo_consumo, values_from = desp_alu,
               names_prefix = "desp_alu_mon_g")
 
 
 morador_df4 <- morador_df3 %>%
   left_join(b1, by = var_dom) %>%
   left_join(b2, by = var_dom) %>%
   left_join(b3, by = var_dom) %>%
   left_join(b4, by = var_dom) %>%
   left_join(b5, by = var_dom) %>%
   left_join(b6, by = var_dom) %>%
   mutate(
     desp_mon_g1 = across(ends_with("_mon_g1")) %>% rowSums(na.rm = TRUE),
     desp_mon_g2 = across(ends_with("_mon_g2")) %>% rowSums(na.rm = TRUE),
     desp_mon_g3 = across(ends_with("_mon_g3")) %>% rowSums(na.rm = TRUE),
     desp_mon_g4 = across(ends_with("_mon_g4")) %>% rowSums(na.rm = TRUE),
     desp_mon_g5 = across(ends_with("_mon_g5")) %>% rowSums(na.rm = TRUE),
     desp_mon_g6 = across(ends_with("_mon_g6")) %>% rowSums(na.rm = TRUE),
     desp_mon_g7 = across(ends_with("_mon_g7")) %>% rowSums(na.rm = TRUE),
     desp_mon_g8 = across(ends_with("_mon_g8")) %>% rowSums(na.rm = TRUE),
     desp_mon_g9 = across(ends_with("_mon_g9")) %>% rowSums(na.rm = TRUE),
     desp_mon_g10 = across(ends_with("_mon_g10")) %>% rowSums(na.rm = TRUE),
     desp_mon_g11 = across(ends_with("_mon_g11")) %>% rowSums(na.rm = TRUE),
     desp_mon_g12 = across(ends_with("_mon_g12")) %>% rowSums(na.rm = TRUE),
     desp_mon_g13 = across(ends_with("_mon_g13")) %>% rowSums(na.rm = TRUE),
     desp_mon_g14 = across(ends_with("_mon_g14")) %>% rowSums(na.rm = TRUE),
     desp_mon_g15 = across(ends_with("_mon_g15")) %>% rowSums(na.rm = TRUE),
     desp_mon_g16 = across(ends_with("_mon_g16")) %>% rowSums(na.rm = TRUE),
     desp_mon_g17 = across(ends_with("_mon_g17")) %>% rowSums(na.rm = TRUE),
     desp_mon_g18 = across(ends_with("_mon_g18")) %>% rowSums(na.rm = TRUE),
     desp_mon_g19 = across(ends_with("_mon_g19")) %>% rowSums(na.rm = TRUE),
     desp_mon_g20 = across(ends_with("_mon_g20")) %>% rowSums(na.rm = TRUE),
     desp_mon_g21 = across(ends_with("_mon_g21")) %>% rowSums(na.rm = TRUE),
     desp_mon_g22 = across(ends_with("_mon_g22")) %>% rowSums(na.rm = TRUE)
   )
 
 morador_df4 <- morador_df4 %>%
   select(- (desp_cc_mon_g1:desp_di_mon_g22))
 
 # check <- morador_df4 %>%
 #    select(desp_mon_total, desp_mon_g1:desp_mon_g22) %>%
 #    mutate(teste = across(desp_mon_g1:desp_mon_g22)  %>% rowSums(na.rm = TRUE) ) %>%
 #    select(desp_mon_total, teste, desp_mon_g1:desp_mon_g22)
 # summary(check$desp_mon_total[check$desp_mon_total != check$teste] - check$teste[check$desp_mon_total != check$teste])
 # # rm(check)
 
 
 # Criar variaveis per capita
 morador_df4$desp_total_pc <- morador_df4$desp_total_tudo / morador_df4$pessoas_dom
 morador_df4$desp_total_mon_pc <- morador_df4$desp_mon_total / morador_df4$pessoas_dom
 
 
 # Criar percentuais por grupo de consumo total e monetário
 for (i in 1:22) {
   morador_df4[[paste0("desp_pc_g", i)]] <- morador_df4[[paste0("desp_g", i)]] / morador_df4[["pessoas_dom"]]
 }
 
 for (i in 1:22) {
   morador_df4[[paste0("desp_mon_pc_g", i)]] <- morador_df4[[paste0("desp_mon_g", i)]] / morador_df4[["pessoas_dom"]]
 }
 
 
 for (i in 1:22) {
   morador_df4[[paste0("perc_g", i)]] <- morador_df4[[paste0("desp_g", i)]] / morador_df4[["desp_total_tudo"]] * 100
 }
 
 
 for (i in 1:22) {
   morador_df4[[paste0("perc_mon_g", i)]] <- morador_df4[[paste0("desp_mon_g", i)]] / morador_df4[["desp_mon_total"]] * 100
 }

 saveRDS(morador_df4, "despesas_por_grupos_POF.RDS") 
 