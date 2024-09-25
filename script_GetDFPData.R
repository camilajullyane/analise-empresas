library(tidyr)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(BatchGetSymbols)
library(purrr)


minha_funcao <- function(cod_cvm, first_year, last_year) {
  l_dfp <- get_dfp_data(companies_cvm_codes = cod_cvm,
                        type_docs = '*', 
                        type_format = 'con', 
                        first_year = first_year,
                        last_year = last_year)
  
  # Verifique se os dados foram retornados corretamente
  if (is.null(l_dfp) || is.null(l_dfp$`DF Consolidado - Balanço Patrimonial Ativo`)) {
    return(NA)  # Retorna NA se não houver dados
  }
  
  fr_assests <- l_dfp$`DF Consolidado - Balanço Patrimonial Ativo`
  fr_assests_1 <- fr_assests %>%
    select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
  
  At_total <- fr_assests_1[1, 4]
  At_circulante <- fr_assests_1[2, 4]
  
  # Aqui, você deve ter a variável fr_passiv definida
  # Certifique-se de que está obtendo os dados do passivo corretamente
  fr_passiv <- l_dfp$`DF Consolidado - Balanço Patrimonial Passivo`  # Exemplo
  
  if (is.null(fr_passiv)) {
    print(2)
    return(NA)  # Retorna NA se não houver dados do passivo
  }
  
  fr_passiv_1 <- fr_passiv %>%
    select(DT_REFER, CD_CONTA, DS_CONTA, VL_CONTA)
  
  Ps_total <- fr_passiv_1[1, 4]
  Ps_circulante <- fr_passiv_1[2, 4]
  
  # Índice de Liquidez
  liquidez <- At_circulante / Ps_circulante
  
  print(liquidez)
  
  return(liquidez[1,1])  # Retorna o resultado
}


df_info <- get_info_companies()

empresas_industria <- df_info %>%
  filter(SIT_REG=="ATIVO" & TP_MERC=="BOLSA" & SETOR_ATIV=="Serviços Médicos")


# Aplicando a função para cada empresa
resultado_liquidez <- empresas_industria %>%
  mutate(map(CD_CVM, ~minha_funcao(.x, 2022, 2023)))
