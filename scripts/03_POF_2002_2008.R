
library(dplyr)
library(janitor)

# fonte
# https://github.com/CaoBittencourt/Data-Science-Python/blob/main/pof_2008.rar

# testinho aqui (pode dar bem bom esse aquivo aqui!)

pof_2002 <- read.csv('F:/Drive/BASES DE DADOS BRUTOS/POF/POF 2002-2003/raw/pof_2002.csv') %>% clean_names()
pof_2008 <- read.csv('F:/Drive/BASES DE DADOS BRUTOS/POF/POF 2008-2009/raw/pof_2008.csv') %>% clean_names()

pof_2002$ano <- 2002
pof_2008$ano <- 2008


pof_2008[, c(
  "tipo_reg"                                                               
  ,"num_ext_renda"                                                          
  ,"fator_expansao1"                                                        
  ,"fator_expansao2"                                                        
  ,"perd_cod_p_visit_realm_em"                                              
  ,"qtd_morador_domc"                                                       
  ,"qtd_uc"                                                                 
  ,"qtd_familia"                                                            
  ,"cod_tipo_domc") 
] %>% head() 


pof <- bind_rows( pof_2002, pof_2008 )

# rm(pof_2002, pof_2008); gc()


# Criação da variável estrato igual em 2018

pof <- pof %>% 
  mutate( estrato_pof = case_when(
    ano == 2002 & estrato < 10 ~ paste0(uf, "0", estrato),
    ano == 2002 & estrato >= 10 ~ paste0(uf, estrato),
    ano == 2008 & cod_domc < 10 ~ paste0(cod_uf, "0", cod_domc), # perd_cod_p_visit_realm_em 
    ano == 2008 & cod_domc >= 10 ~ paste0(cod_uf, cod_domc))
  ) %>% 
  mutate(estrato_pof = as.numeric(estrato_pof))
  
head(pof$estrato_pof)
  
unique(pof[pof$cod_uf == 41,]$estrato_pof)  
  
# Vamos fazer a conta das despesas totais 

pof %>% group_by(ano) %>% summary()


# Nessa primeira etapa vamos fazer para o ano de 2022 q sabemos onde fica os stratos

pof %>% filter(ano == 2008) %>% select(cod_uf, perd_cod_p_visit_realm_em, estrato_pof) %>% head()


pof %>% 
  # filter(cod_uf == 41 | uf == 41) %>%
  filter(estrato_pof %in% 4101:4135) %>% 
  group_by(ano) %>% 
  summarise(sum(n_morador*fator)/10^6,
            sum(qtd_morador_domc*fator_expansao1)/10^6) 



pof %>% group_by(ano) %>%  summarise(sum(estrato)) 

pof %>% 
  filter(estrato %in% 4101:4108) %>% 
  mutate(peso_fam = ifelse(ano == 2002, n_morador*fator, qtd_morador_domc*fator_expansao2 ) ) %>% 
  group_by(ano) %>% 
  summarise(pessoas_milhoes = sum(peso_fam)/10^6, 
            desp_total = weighted.mean( despesas_mensais_totais_per_capita, w = peso_fam ),
            desp_alimentos = weighted.mean( despesas_mensais_alimentacao, w = peso_fam ),
            desp_hab = weighted.mean( despesas_mensais_moradia_per_capita, w = peso_fam ),
            desp_transporte = weighted.mean( despesas_mensais_transporte_per_capita, w = peso_fam )
            )
  
  






