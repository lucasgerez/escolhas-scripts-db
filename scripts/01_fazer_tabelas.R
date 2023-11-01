# Rodar esse script depois de criar as funções, selecionando o estrato de interesse
# e escolhendo qual variável define os decis e o peso
#Alterar o nome da planilha a ser exportada


# PARÂMETROS A SEREM DEFINIDOS:
  # NÚMERO DE PERCENTIS DA ANÁLISE
  # QUAL A VARIÁVEL QUE VAI DEFINIR OS PERCENTIS
  # QUAL O PESO
  # QUAL ESTRATO VAI SER UTILIZADO
  # QUAL O NOME DA TABELA DE OUTPUT

# Aplica as funções ------------------------------------------------------------

# Carrega a base tratada
pof <- readRDS("despesas_por_grupos_POF.RDS")

estrato_uf_com_rural        <- 4101:4135
estrato_uf_sem_rural        <- 4101:4124
estrato_uf_sem_rm_sem_rural <- 4108:4124
estrato_rm                  <- 4101:4108
estrato_rm_sem_capital      <- 4106:4108
estrato_capital             <- 4101:4105

pof <- pof %>%
  mutate( v1 = if_else(is.na(PC_RENDA_MONET), 0, PC_RENDA_MONET),
          v2 = if_else(is.na(PC_RENDA_NAO_MONET), 0, PC_RENDA_NAO_MONET),
          PC_RENDA_TOTAL = v1 + v2) %>% 
      select(-v1, -v2)

rmsp <- 3501:3515

nome_tabela <- "/pof_tabelas_curitiba.xlsx"

pof_sp <- f_xtile_filter(pof, variavel = "PC_RENDA_DISP", var_peso = "peso_final_fam",
                         nova_var = "decis", n = 10, estrato = estrato_capital )  %>%
  filter(!is.na(decis))
#nrow(pof_sp) só perde 1 obs do Paraná com o filtro
pof_sp_svy <- as_survey(svydesign(ids = ~COD_UPA, strata = ~ESTRATO_POF , weights = ~peso_final_fam, data = pof_sp))


t1 <- f_medias(pof_sp_svy, decis)
pof_sp_svy$variables$um <- 999 # para o total
t1b <- f_medias(pof_sp_svy, um)
names(t1b)[1] <- "decis"
t1 <- rbind(t1, t1b)

t2 <- f_medias_mon(pof_sp_svy, decis)
t2b <- f_medias_mon(pof_sp_svy, um)
names(t2b)[1] <- "decis"
t2 <- rbind(t2, t2b)

t3 <- f_medias_g(pof_sp_svy, decis)
t3b <- f_medias_g(pof_sp_svy, um)
names(t3b)[1] <- "decis"
t3 <- rbind(t3, t3b)


t4 <- f_perc_medios_g(pof_sp_svy, decis)
t4b <- f_perc_medios_g(pof_sp_svy, um)
names(t4b)[1] <- "decis"
t4 <- rbind(t4, t4b)

t5 <- f_totais_g(pof_sp_svy, decis)
t5b <- f_totais_g(pof_sp_svy, um)
names(t5b)[1] <- "decis"
t5 <- rbind(t5, t5b)

t6 <- f_medias_mon_g(pof_sp_svy, decis)
t6b <- f_medias_mon_g(pof_sp_svy, um)
names(t6b)[1] <- "decis"
t6 <- rbind(t6, t6b)

t7 <- f_perc_mon_medios_g(pof_sp_svy, decis)
t7b <- f_perc_mon_medios_g(pof_sp_svy, um)
names(t7b)[1] <- "decis"
t7 <- rbind(t7, t7b)

t8 <- f_totais_mon_g(pof_sp_svy, decis)
t8b <- f_totais_mon_g(pof_sp_svy, um)
names(t8b)[1] <- "decis"
t8 <- rbind(t8, t8b)




# Escreve Excel ---------------------------------------------------------

sheets <- list(
  "Medias" = t1,
  "Medias_mon" = t2,
  "Medias_grupos" = t3,
  "Percentuais_medios_grupos" = t4,
  "Totais_grupos" = t5,
  "Medias_mon_grupos" = t6,
  "Percentuais_mon_medios_grupos" = t7,
  "Totais_mon_grupos" = t8
)


write.xlsx(sheets, paste0(export, nome_tabela))

# Ns dos estratos de Curitiba --------------------------------------------------
estratos <- list(estrato_capital, estrato_rm, estrato_rm_sem_capital, estrato_uf_com_rural,
     estrato_uf_sem_rm_sem_rural, estrato_uf_sem_rural)

tab <- 1:6
i = 0
for (j in  estratos) {
  
  i = i+1
  tab[i] <- nrow(filter(pof, ESTRATO_POF %in% j))
  
}

nums <- as.table(cbind(c("estrato_capital", "estrato_rm", "estrato_rm_sem_capital",
                "estrato_uf_com_rural", "estrato_uf_sem_rm_sem_rural", "estrato_uf_sem_rural"),
              tab)
)
