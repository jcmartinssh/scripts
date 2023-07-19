library(readr)

library(sf)

scanfwf <- fwf_empty("C:\\ACELERADOR\\Lote5\\Arquivos_DI\\V_gQF_SUDESTE (ES-RJ).csv")

pastaRaiz <- "C:\\ACELERADOR\\Lote5\\"

lista_arq <- list.files(paste(pastaRaiz, "Arquivos_DI\\", sep = ""))

arquivo_saida <- paste(pastaRaiz, "_GPKG\\lote5_faces.gpkg", sep = "" )

# lista_arq <- lista_arq[c(1, 4)]

for(i in lista_arq) {
  
  nome_arq <- i
  
  arquivo_entrada <- paste(pastaRaiz, "Arquivos_DI\\", nome_arq, sep = "")
  
  tabela_faces <- read_fwf(arquivo_entrada, col_positions = scanfwf, col_types = "cccccccnnnnnnnnnnnnnnnnnnnnnnnn")
  
  write_sf(tabela_faces, arquivo_saida, layer = "dados_faces_lote5", append = TRUE)
  
}


verificacao <- read_sf(arquivo_saida,
                       query = "SELECT count(*) FROM dados_faces_lote5")

columns <- read_sf(arquivo_saida,
                   query = "SELECT * FROM dados_faces_lote5 LIMIT 50")

UFs <- read_sf(arquivo_saida,
               query = "SELECT DISTINCT SUBSTR(X1, 1, 2) FROM dados_faces_lote5")


