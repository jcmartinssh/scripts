# <- isso indica um comentário

## protótipo do script de automatização 
##  - pré-processamento dos dados da DI,
##  - processamento com bases 
##  - criação dos kits para delimitação da BATER

## carrega a bibioteca "Read Rectangular Text Data" para leitura de tabelas e dados matriciais
library(readr)

## carrega a biblioteca "Simple Features" para geoprocessamento.
## ela utiliza algumas bibliotecas consagradas, como o GDAL, e é útil até para abrir arquivos de formatos variados
library(sf)

## LIGA / DESLIGA o modo de teste (TRUE ou FALSE)
teste <- TRUE

## roda o comando readr::fwf_empty para detectar automaticamente as quebras das colunas a partir de um único arquivo.
## esse arquivo deve garantir a existência de pelo menos um valor em cada coluna. Não utilize o Distrito Federal.
## o parâmetro do comando é o endereço e nome do arquivo
scanfwf <- fwf_empty("C:\\ACELERADOR\\Lote5\\Arquivos_DI\\V_gQF_SUDESTE (ES-RJ).csv")

## endereço da pasta raiz onde estão as subpastas com os arquivos de entrada e de saída
pastaRaiz <- ifelse(teste == TRUE, "C:\\ACELERADOR\\Lote5\\teste\\", "C:\\ACELERADOR\\Lote5\\")

## cria a lista de arquivos lendo o conteúdo da pasta dos arquivos originais.
## o parâmetro pattern permite definir um string para restringir os arquivos de interesse.
## nesse caso, todos os arquivos de interesse, e somente eles, devem conter .csv no nome
lista_arq <- list.files(paste(pastaRaiz, "Arquivos_DI\\", sep = ""), pattern = ".csv")

## endereço do geopackage onde serão salvos as camadas e tabelas
arquivo_saida <- paste(pastaRaiz, "_GPKG\\lote5_faces.gpkg", sep = "" )

## reduz a lista de arquivos de entrada para rodar o teste
lista_arq <- ifelse(teste == TRUE, lista_arq[c(1, 2)], lista_arq)

## loop de processamento dos arquivos da DI
for(i in lista_arq) {
  
  # pega o nome do arquivo da lista
  nome_arq <- i
  
  # constrói o endereço do arquivo de entrada
  arquivo_entrada <- paste(pastaRaiz, "Arquivos_DI\\", nome_arq, sep = "")
  
  # lê o arquivo da DI de largura fixa, utilizando os parâmetros computados anteriormente
  # define os tipos das colunas usando a notação compacta: c = character; n = numeric
  tabela_faces <- read_fwf(arquivo_entrada, col_positions = scanfwf, col_types = "cccccccnnnnnnnnnnnnnnnnnnnnnnnn")
  
  # acrescenta a tabela na camada indicada dentro do gpkg, sem apagar o que já existe
  write_sf(tabela_faces, arquivo_saida, layer = "dados_faces_lote5", append = TRUE)
  
}

## conta o número de feições na camada produzida usando SQL
total_faces <- read_sf(arquivo_saida,
                       query = "SELECT count(*) FROM dados_faces_lote5")

## carrega os primeiros 50 elementos da camada produzida para verificação
amostra_faces <- read_sf(arquivo_saida,
                   query = "SELECT * FROM dados_faces_lote5 LIMIT 50")

## cria a lista de Municípios a partir do geocódigo das faces do arquivo
lista_mun <- read_sf(arquivo_saida,
               query = "SELECT DISTINCT SUBSTR(X1, 1, 7) FROM dados_faces_lote5")


