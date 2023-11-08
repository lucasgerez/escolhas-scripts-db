
library(dplyr)
library(janitor)

# fonte
# https://github.com/CaoBittencourt/Data-Science-Python/blob/main/pof_2008.rar

# testinho aqui (pode dar bem bom esse aquivo aqui!)

pof_2002 <- read.csv('F:/Drive/BASES DE DADOS BRUTOS/POF/POF 2002-2003/raw/pof_2002.csv') %>% clean_names()
pof_2008 <- read.csv('F:/Drive/BASES DE DADOS BRUTOS/POF/POF 2008-2009/raw/pof_2008.csv') %>% clean_names()

pof_2002$ano <- 2002
pof_2008$ano <- 2008

pof <- bind_rows( pof_2002, pof_2008 )

rm(pof_2002, pof_2008); gc()


# Criação da variável estrato igual em 2018

pof$estrato <- paste0(pof$uf, 
                      ifelse(pof$estrato < 10, 
                             paste0("0",pof$estrato), 
                             pof$estrato)) %>% as.numeric()










