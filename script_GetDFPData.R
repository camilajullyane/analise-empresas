library(tidyr)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(BatchGetSymbols)

# Informações das Empresas

df_info <- get_info_companies()
names(df_info)
print(df_info)

# Qtd Empresas Ativas, Canceladas e Suspensas

df_info %>%
  count(SIT_REG)

# Qtd Empresas por Setor e Segmentos de Governança Corporativas

df_info %>%
  count(SETOR_ATIV) %>%
  arrange(desc(n))

qtd_setAtv <- df_info %>%
  count(SETOR_ATIV)
  arrange(desc(n))

  
# Selecionar Empresas Ativas de um Determinado Setor

df_info %>%
  group_by(SETOR_ATIV) %>%
  filter(SIT_REG == "ATIVO" & SETOR_ATIV == "Energia Elétrica")

qtd_energiaAtv <- df_info %>%
  group_by(SETOR_ATIV) %>%
  filter(SIT_REG == "ATIVO" & SETOR_ATIV == "Energia Elétrica" & CATEG_REG == "Categoria A")

qtd_mineracaoAtv <- df_info %>%
  group_by(SETOR_ATIV) %>%
  filter(SIT_REG == "ATIVO" & SETOR_ATIV == "Extração Mineral" & CATEG_REG == "Categoria A")

#Bancos

qtd_bancosAtv <- df_info %>%
  group_by(SETOR_ATIV) %>%
  filter(SIT_REG == "ATIVO" & SETOR_ATIV == "Bancos" &  CATEG_REG == "Categoria A")

# Pesquisando Informações Básicas de uma Empresa

df_search <- search_company('SER EDUCACIONAL')
print(df_search)

# Baixando Dados DFP com Base no Código CVM da Empresa - CD_CVM
## Set opções

id_companies <- 23221
first_year <- 2021
last_year <- 2023

# Download Dados

## AENTçÂO
# Símbolo do tipo de documento financeiro a ser retornado. 
# Definições: ’*’ = retorna todos documentos, ‘BPA’ = Ativo, 
# ‘BPP’ = passivo, ‘DRE’ = demonstrativo de resultados do exercício, 
# ‘DFC_MD’ = fluxo de caixa pelo metodo direto, 
# ‘DFC_MI’ = fluxo de caixa pelo metodo indireto, 
# ‘DMPL’ = mutacoes do patrimonio liquido, 
# ‘DVA’ = demonstrativo de valor agregado.

l_dfp <- get_dfp_data(companies_cvm_codes = id_companies,
                      type_docs = '*', # pegar todos
                      type_format = 'con', # consolidade
                      first_year = first_year,
                      last_year = last_year)

str(l_dfp)

# Salvar BPA em df
fr_assests <- l_dfp$`DF Consolidado - Balanço Patrimonial Ativo`

# Verificando
print(fr_assests)
glimpse(fr_assests)
str(fr_assests)
View(fr_assests)

l_dfp_grandene <- get_dfp_data(companies_cvm_codes = 19615,
                      type_docs = c('BPA'),
                      type_format = 'con',
                      first_year = 2019,
                      last_year = 2020)


l_dfp_grandene <- as.data.frame(l_dfp_grandene)
View(l_dfp_grandene)

# Salvar BPP em df

fr_passiv <- l_dfp$`DF Consolidado - Balanço Patrimonial Passivo`

# Verificando

print(fr_passiv)
glimpse(fr_passiv)
str(fr_passiv)
View(fr_passiv)

# Ajustando df do Ativo para Cálculo de Índice
fr_assests_1 <- fr_assests %>%
  select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)

At_total <- fr_assests_1[1,4]
At_circulante <- fr_assests_1[2,4]

# Ajustando df do Passivo para Cálculo de Índice
fr_passiv_1 <- fr_passiv %>%
  select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)

Ps_toal <- fr_passiv_1[1,4]
Ps_circulante <- fr_passiv_1[2,4]

# Índice de Liquez
At_circulante/Ps_circulante
