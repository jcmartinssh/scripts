## Avaliacao INFORM

library(data.table)
library(readxl)

inform_excel <-
  "W:\\DGC_ACERVO_CGEO\\PROJETOS_EM_ANDAMENTO\\Cemaden\\BOLSISTAS\\Joaquim\\Seminarios2022\\segunda_parte\\INFORM_Risk_Mid2023_v066.xlsx"

inform_cols_title <-
  as.character(read_excel(
    inform_excel,
    sheet = "INFORM Risk 2023 (a-z)",
    n_max = 1,
    col_names = FALSE
  ))

# inform_cols_unit <- as.character(read_excel(
#   inform_excel,
#   sheet = "INFORM Risk 2023 (a-z)",
#   n_max = 1,
#   col_names = TRUE
# ))                         

inform_cols <-
#   paste0(inform_cols_title,inform_cols_unit)
  inform_cols_title

# cols_to_num <- (Inequality,  Children U5, Access to health care, )

inform <- (read_excel(inform_excel,
                     sheet = "INFORM Risk 2023 (a-z)",
                     skip = 3,
                     col_names = inform_cols)
           [,  replace(.SD, .SD == "x", NA)]
           [, ])

cplp <-
  c(
    "AGO",
    "BRA",
    "CPV",
    "GNQ",
    "GNB",
    "MOZ",
    "PRT",
    "STP",
    "TLS")

inform_cplp <- 
  inform[ISO3 %in% cplp, replace(.SD, .SD == "x", NA)]
