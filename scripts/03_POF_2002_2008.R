
# Tabelas para as POFs de 2002 e 2008

library(dplyr)
library(janitor)
library(survey)
library(srvyr)


# fonte
# https://github.com/CaoBittencourt/Data-Science-Python/blob/main/pof_2008.rar

# testinho aqui (pode dar bem bom esse aquivo aqui!)

pof_2002 <- read.csv('F:/Drive/BASES DE DADOS BRUTOS/POF/POF 2002-2003/raw/pof_2002.csv') %>% clean_names()

cat('\nTabela wide da Pof 2002:', nrow(pof_2002), 'linhas...')

pof_2008 <- read.csv('F:/Drive/BASES DE DADOS BRUTOS/POF/POF 2008-2009/raw/pof_2008.csv') %>% clean_names()

cat('\nTabela wide da Pof 2008:', nrow(pof_2008), 'linhas...')

# Adicionando a variável de ano
pof_2002$ano <- 2002
pof_2008$ano <- 2008

# Vamos renomear algumas variaveis de 2008 para facilitar

pof_2008 <- pof_2008 %>% 
  rename(uf = cod_uf, 
         estrato = cod_domc,
         n_morador = qtd_morador_domc,
         fator = fator_expansao2,
         domcl = cod_tipo_domc,
         quant_uc = qtd_uc)



# Juntando as duas POFs
pof <- bind_rows( pof_2002, pof_2008 )

# Ficando apenas com o painel completo 
rm(pof_2002, pof_2008); gc()


# Criação da variável estrato igual em 2018
pof <- pof %>% 
  mutate( estrato_pof = case_when(
    estrato < 10 ~ paste0(uf, "0", estrato),
    estrato >= 10 ~ paste0(uf, estrato)
  )) %>% 
  mutate(estrato_pof = as.numeric(estrato_pof),
         peso_fam = n_morador*fator, 
         COD_UPA = paste0(uf, estrato_pof, tipo_reg, domcl, id_dom, quant_uc)
         ) 
  
  

# Vamos ficar apenas com as variáveis que fazer sentido
pof_res <- pof %>% select(uf, estrato_pof, tipo_reg, domcl, id_dom, quant_uc,
                          peso_fam, COD_UPA,
                          despesas_mensais_totais_per_capita, 
                          despesas_mensais_alimentacao_per_capita,
                          despesas_mensais_moradia_per_capita,
                          despesas_mensais_transporte_per_capita, 
                          despesas_mensais_emprestimos_per_capita,
                          despesas_mensais_saude_per_capita,
                          renda_total_per_capita)




# Transformando o arquivo em survey  
pof_2002_2008_svy <- as_survey(svydesign(ids = ~COD_UPA, strata = ~estrato_pof , weights = ~peso_fam, data = pof_res))

# Decil de renda


# Função para a POF de 2002 a 2008

# Calculo da renda pc 
f_inc_dist_filter_2002_2008 <- function(pof_svy, variavel, var_peso, nova_var, n, estrato) {
  
  options(warn = -1)
  
  # Cálculo da Renda dom pc total
  
  aux <- pof_svy %>%
    summarise(renda_dom_pc_disp = survey_mean( PC_RENDA_DISP,  na.rm = TRUE)) %>%
    select(renda_dom_pc_disp) %>%
    as.data.frame()
  
  df <- pof_svy$variables
  
  x <- 1 / n
  
  df <- filter(df, ESTRATO_POF %in% estrato)
  
  percentis <-  wtd.quantile( df[[variavel]], weights = df[[var_peso]], probs = seq(0,1,x) )
  
  tab <- data.frame(decis = 1:(n+1), renda_dom_pc_disp = c(percentis[-1], aux$renda_dom_pc_disp))
  
  tab$renda_dom_pc_disp <- paste0("R$ ", prettyNum(round(tab$renda_dom_pc_disp,0), big.mark = ".", small.mark = ","))
  tab$renda_dom_pc_disp[1] <- paste0("Até ", tab$renda_dom_pc_disp[1])
  tab$renda_dom_pc_disp[10] <- paste0("Acima de ", tab$renda_dom_pc_disp[9])
  tab$decis[11] <- 'Total'
  
  return(tab)
  
}






# Vamos fazer a conta das despesas totais 
pof %>% 
  filter(estrato_pof %in% 4101:4105) %>% 
  mutate(peso_fam = ifelse(ano == 2002, n_morador*fator, qtd_morador_domc*fator_expansao2 ) ) %>% 
  group_by(ano) %>% 
  summarise(pessoas_milhoes = sum(peso_fam)/10^6, 
            desp_total = weighted.mean( despesas_mensais_totais_per_capita, w = peso_fam ),
            desp_alimentos = weighted.mean( despesas_mensais_alimentacao_per_capita, w = peso_fam ),
            desp_hab = weighted.mean( despesas_mensais_moradia_per_capita, w = peso_fam ),
            desp_transporte = weighted.mean( despesas_mensais_transporte_per_capita, w = peso_fam )
  )



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


  
  






