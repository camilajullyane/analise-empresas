library(tidyr)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(BatchGetSymbols)
library(purrr)

# Informações das Empresas

df_info <- get_info_companies()
names(df_info)
print(df_info)

qtd_empresasAtv <- df_info %>%
  filter(SIT_REG=="ATIVO") %>%
  count(SETOR_ATIV) %>%
  arrange(desc(n))


energia_eletrica <- df_info %>%
  filter(SIT_REG=="ATIVO" & SETOR_ATIV=="Energia Elétrica" & CATEG_REG=="Categoria A" & TP_MERC=="BOLSA")

resultados <- energia_eletrica %>%
  split(.$CD_CVM) %>% 
  map(~ search_company(.)) 
