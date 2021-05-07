# Autor: Péricles S. G. Marques
# Email: pericles.marques@outlook.com
# Versão: 1.0


## coletar as informações dos jogos de tabuleiro ##
ludopedia <- function(slug) {
  # bibliotecas
  library('stringr') #
  library('rvest') #html_text()
  library('dplyr') #data manipulation


  # criar dataframe com 30 colunas
  dfx <- function(x, colunas) {
    
    DF = data.frame(
      V1 = x[1],
      V2 = x[2],
      V3 = x[3],
      V4 = x[4],
      V5 = x[5],
      V6 = x[6],
      V7 = x[7],
      V8 = x[8],
      V9 = x[9],
      V10 = x[10],
      V11 = x[11],
      V12 = x[12],
      V13 = x[13],
      V14 = x[14],
      V15 = x[15],
      V16 = x[16],
      V17 = x[17],
      V18 = x[18],
      V19 = x[19],
      V20 = x[20],
      V21 = x[21],
      V22 = x[22],
      V23 = x[23],
      V24 = x[24],
      V25 = x[25],
      V26 = x[26],
      V27 = x[27],
      V28 = x[28],
      V29 = x[29],
      V30 = x[30]
    )
    
    col_2 = NULL
    for (i in 1:30) {
      col_1 = paste(colunas, i, sep = '')
      col_2 = cbind(col_2, col_1)
    }
    
    colnames(DF) = col_2
    return(DF)
  }
  
  
  # junta todas as informações em uma única string
  concat = function(vetor) {
    #vetor = categoria
    vetor = vetor[ , (is.na(vetor)) == 0]
    vetor = as.character(vetor)
    
    if(identical(vetor, character(0))) {
      vetor = ''
    } else {
      if(length(vetor) != 1) {
        for (i in 2:length(vetor)) {
          vetor[1] = paste0(vetor[1], ', ', vetor[i])
        }
      }
    }
    
    vetor = vetor[1]
    return(vetor)
  }


  # coletar dados da página 'creditos'
  pag_creditos <- function(slug) {
    
    ## carregando a página de créditos 
    link = paste('https://www.ludopedia.com.br/jogo/', slug, '?v=creditos', sep = '')
    page = read_html(link)
    
    
    ## manipular estrutura da página créditos
    creditos = page %>% html_nodes('#creditos' ) %>% html_text() %>% str_trim(side = "both")
    creditos = gsub("\r\n", "", creditos, fixed = TRUE) 
    creditos = gsub("\tEditora", "#Editora", creditos, fixed = TRUE)
    creditos = gsub("\tArtista", "#Artista", creditos, fixed = TRUE)
    creditos = gsub("\tDomínio", "#Domínio", creditos, fixed = TRUE)
    creditos = gsub("\tMecânica", "#Mecânica", creditos, fixed = TRUE)
    creditos = gsub("\tCategoria", "#Categoria", creditos, fixed = TRUE)
    creditos = gsub("., Ltd.", ". Ltd.", creditos, fixed = TRUE)
    creditos = gsub("\t", "", creditos, fixed = TRUE) %>% str_trim()
    
    designer = str_extract(creditos, "(?<=Designer)[^#]+") %>% unlist() %>% strsplit(", ") %>% as.data.frame() %>% t %>% dfx('designer')
    designer = concat(designer)
    artista = str_extract(creditos, "(?<=Artista)[^#]+") %>% unlist() %>% strsplit(", ") %>% as.data.frame() %>% t() %>% dfx('artista')
    artista = concat(artista)
    editora = str_extract(creditos, "(?<=Editora)[^#]+") %>% unlist() %>% strsplit(", ") %>% as.data.frame() %>% t() %>% dfx('editora')
    #editora = concat(editora)
    dominio = str_extract(creditos, "(?<=Domínio)[^#]+") %>% unlist() %>% strsplit(", ") %>% as.data.frame() %>% t() %>% dfx('dominio')
    dominio = concat(dominio)
    mecanica = str_extract(creditos, "(?<=Mecânica)[^#]+") %>% unlist() %>% strsplit(", ") %>% as.data.frame() %>% t() %>% dfx('mecanica')
    mecanica = concat(mecanica)
    categoria = str_extract(creditos, "(?<=Categoria)[^#]+") %>% unlist() %>% strsplit(", ") %>% as.data.frame() %>% t() %>% dfx('categoria')
    categoria = concat(categoria)
    
    dados_creditos = data.frame(
      'editora' = editora[1,1],
      'dominio' = dominio,
      'categoria' = categoria,
      'mecanica' = mecanica,
      'designer' = designer,
      'artista' = artista
    )
    

    return(dados_creditos)
    
  }
  

  # coletar dados da página 'resumo'
  pag_resumo <- function(slug) {
    
    link = paste('https://www.ludopedia.com.br/jogo/', slug, sep = '')
    page = read_html(link)
    
    dados = page %>% html_nodes('.pad-all') %>% html_text() %>% str_trim(side = "both")
    dados = gsub("\r\n", "", dados[2], fixed = TRUE)
    dados = gsub("\tTemas", "#Temas", dados, fixed = TRUE)
    dados = gsub("\t", "", dados, fixed = TRUE)
    
    tema = str_extract(dados, "(?<=#Temas)[^#]+") %>% unlist() %>% strsplit(", ") %>% as.data.frame() %>% t %>% dfx('tema')
    tema = concat(tema)
    
    page %>% html_nodes('.text-sm:nth-child(1) a' ) %>% html_text()
    
    ## data frame 
    resumo = data.frame(
      'nome' = page %>% html_nodes('h3 > a') %>% html_text(),
      'descricao' = page %>% html_nodes('#bloco-descricao-sm p') %>% html_text(),
      'componentes' = page %>% html_nodes('#bloco-componentes-sm .col-sm-9 p') %>% html_text(),
      'sleeves' = page %>% html_nodes('.bg-gray-light p') %>% html_text() %>% str_trim(side = "both"),
      'dep_idioma' = page %>% html_nodes('#bloco-descricao-sm .pad-all') %>% html_text() %>% 
        str_extract("(?<=Idioma)[^D]+") %>% str_trim(side = "both")
    )
    
    ## dados de: 'idade_mim', 'tempo_jogo', 'no_jogadores'
    dados_j = page %>% html_nodes('#page-content .list-inline') %>% html_text() %>% str_trim(side = "both")
    dados_j = gsub("Idade ", "", dados_j, fixed = TRUE)
    dados_j = gsub("+", "", dados_j, fixed = TRUE)
    dados_j = gsub("\r\n", ",", dados_j, fixed = TRUE)
    dados_j = gsub("\t", "", dados_j, fixed = TRUE) %>% unlist() %>% strsplit(",") %>% as.data.frame %>% t() %>% str_trim() %>% dfx('dados')
    dados_j = dados_j[,1:3]
    dados_j[1,1] = paste0(dados_j[1,1], ' anos')
    colnames(dados_j) = c('idade_mim', 'tempo_jogo', 'no_jogadores')
    
    dados_resumo = cbind(resumo, dados_j, tema, row.names = NULL)
    return(dados_resumo)
  }
  
  # mega data frame com os dados compilados
  dados_ludopedia = cbind(slug, pag_resumo(slug), pag_creditos(slug))
  
  # cria salva informações em csv
  dir.create('ludopedia', showWarnings = FALSE)
  write.csv(dados_ludopedia, paste0('ludopedia/', slug, '.csv'), fileEncoding = 'UTF-8')
  
  # alarmes
  print(paste0('Arquivo criado: ', slug, '.csv'))
  
  # return
  return(dados_ludopedia)
}


## como usar ##
ludopedia('terra-mystica')
ludopedia('zombicide-black-plague')
ludopedia('pandemic-reign-of-cthulhu')