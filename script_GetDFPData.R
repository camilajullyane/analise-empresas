library(tidyr)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(BatchGetSymbols)


Indice_liquidez <- function(ATCIRCULANTE,PSCIRCULANTE) {
  ILiquidez = ATCIRCULANTE/PSCIRCULANTE
  
  return(ILiquidez)
  
}

Giro_estoque <- function(Custo_Venda, At_estoque){
  GIestoque = Custo_Venda*-1/At_estoque
  
  return(GIestoque)
}

Marg_Lucro <- function(Receita_financeiro, Receita_Venda){
  MARGlucro = Receita_financeiro/Receita_Venda
  
  return(MARGlucro)
}

df_info <- get_info_companies()

companies_ids <- c(4170, 21610, 7617, 2437)  # Lista de IDs das empresas
first_year <- 2021
last_year <- 2022

for (id_company in companies_ids) {
  company_name <- df_info %>%
    filter(id_company == CD_CVM) %>%
    select(DENOM_SOCIAL) %>%
    pull()
  
  # Baixando dados DFP
  l_dfp <- get_dfp_data(companies_cvm_codes = id_company,
                        type_docs = '*',  # pegar todos os tipos de documentos
                        type_format = 'con',  # consolidado
                        first_year = first_year,
                        last_year = last_year)
  
  fr_ativ <- l_dfp$'DF Consolidado - Balanço Patrimonial Ativo' %>%
    select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
  At_circulante <- unlist(fr_ativ[2, 4])
  At_estoque <- unlist(fr_ativ[16, 4])
  At_total <- unlist(fr_ativ[1, 4])
  
  fr_passiv <- l_dfp$'DF Consolidado - Balanço Patrimonial Passivo' %>%
    select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
  Ps_circulante <- unlist(fr_passiv[2, 4])
  
  fr_dre <- l_dfp$'DF Consolidado - Demonstração do Resultado' %>%
    select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
  Custo_Venda <- unlist(fr_dre[2, 4])
  Receita_financeiro <- unlist(fr_dre[53, 4])
  Receita_Venda <- unlist(fr_dre[1, 4])
  
  cat("Empresa ID:", id_company, "\n")
  cat("Nome da Empresa:", company_name, "\n")
  cat("Indice de liquides: ", Indice_liquidez(At_circulante, Ps_circulante), "\n")
  cat("Giro de estoque: ", Giro_estoque(Custo_Venda, At_estoque), "\n")
  cat("Margem de lucro: ", Marg_Lucro(Receita_financeiro, Receita_Venda))
}

