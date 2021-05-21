# Autor: Péricles S. G. Marques
# Email: pericles.marques@outlook.com
# Versão: 1.0


## coletar as informações da súmulas eletônicas da CBF ##
sumula = function(url) {
  ## bibliotecas #########################################################################################################
  library('magrittr') # %>%
  library('pdftools') # pdf_text
  library('readr') # read_lines()
  library('tidyr') # drop_na()
  library('stringr') # str_extract()
  library('dplyr')


  ## coleta as informações do pdf (texto)
  pdf = pdf_text(url)


  ## checa se há um arquivo de apoio (opcional)
  if(file.exists('padronizacao_nomes.csv')) {
    # gerando data frame com 3 versões: 'Clube / UF', 'Clube/UF' e 'Clube'
    cbf_clubes = function() {

      confronto = str_extract_all(pdf, '(?<=Jogo:).*(?=\r\n)')[[1]][2] %>% str_trim()
      clubes_v1 = as.data.frame(strsplit(confronto, ' X '))
      clubes_v2 = gsub(' / ', '/', confronto)
      clubes_v2 = as.data.frame(strsplit(clubes_v2, ' X '))
      clubes_v3 = rbind(strsplit(clubes_v1[1,1], ' / ' )[[1]][[1]], strsplit(clubes_v1[2,1], ' / ' )[[1]][[1]]) %>% as.data.frame()
      uf = rbind(strsplit(clubes_v1[1,1], ' / ' )[[1]][[2]], strsplit(clubes_v1[2,1], ' / ' )[[1]][[2]]) %>% as.data.frame()

      clubes = cbind(clubes_v1, clubes_v2, clubes_v3, uf)
      colnames(clubes) = c('V1', 'V2', 'V3', 'UF')

      # carregando csv com nomes padronizados
      nome_clubes = read.csv('padronizacao_nomes.csv', fileEncoding = 'UTF-8')

      # padronizado
      clubes = left_join(clubes, nome_clubes %>% select(padronizado, padronizado2, sumula), by = c('V2' = 'sumula'))

      return(clubes)
    }
    df_clubes = cbf_clubes()
  }


  ## quando não há arquivo de apoio ou o nome do clube não se encontra na lista
  if(is.na(df_clubes$padronizado[1])) {
    df_clubes$padronizado[1] = df_clubes$V3[1]
    df_clubes$padronizado2[1] = df_clubes$V2[1]
  }

  if(is.na(df_clubes$padronizado[2])) {
    df_clubes$padronizado[2] = df_clubes$V3[2]
    df_clubes$padronizado2[2] = df_clubes$V2[2]
  }


  ## dados da partida
  cbf_partida = function() {

    ## preparando o data frame - cbf_partida #######################################################################
    cbf_competicao = str_extract(pdf, '(?<=Campeonato: ).*(?=Rodada:)')[1] %>% strsplit('/')
    cbf_ano = cbf_competicao[[1]][2] %>% str_trim()
    cbf_competicao = cbf_competicao[[1]][1] %>% str_trim()

    cbf_rodada = str_extract(pdf, '(?<=Rodada:).*(?=\r\n)') %>% as.data.frame() %>% drop_na() %>% str_trim()

    if(str_detect(pdf, 'Resultado Penalti:')[1] == TRUE) {
      cbf_placar = str_extract(pdf, '(?<=Resultado Final: ).*(?=Resultado Penalti:)')[[1]]
    } else {
      cbf_placar = str_extract(pdf, '(?<=Resultado Final: ).*(?=\r\n)')[[1]]
    }

    cbf_placar = gsub('x', 'X', cbf_placar) %>% strsplit(' X ')
    cbf_placar_penaltis = str_extract(pdf, '(?<=Resultado Penalti: ).*(?=\r\n)')[[1]]

    cbf_horario = str_extract(pdf, '(?<=Horário:).*(?=Estádio:)')[1] %>% str_trim()
    cbf_data = str_extract(pdf, '(?<=Data:).*(?=Horário)')[1] %>% str_trim() %>% strsplit('/')
    cbf_data = paste0(cbf_data[[1]][3], '-', cbf_data[[1]][2], '-', cbf_data[[1]][1])

    cbf_local = str_extract(pdf, '(?<=Estádio:).*(?=\r\n)') %>% strsplit('/')
    cbf_local = cbf_local[[1]][1] %>% str_trim()


    ## dataframe ###################################################################################################
    cbf_partida = data.frame(
      'ano' = cbf_ano,
      'id_jogo' = paste0(cbf_data, '_', df_clubes$padronizado2[1], '_', df_clubes$padronizado2[2]),
      'competicao' = cbf_competicao,
      'data' = cbf_data,
      'horario' = cbf_horario,
      'fase' = NA,
      'grupo' = NA,
      'rodada' = cbf_rodada,
      'mandante' = df_clubes$padronizado2[1],
      'gol1' = cbf_placar[[1]][1],
      'gol2' = cbf_placar[[1]][2],
      'visitante' = df_clubes$padronizado2[2],
      'local' = cbf_local,
      'confronto1' = paste0(df_clubes$padronizado2[1], '_', df_clubes$padronizado2[2]),
      'confronto2' = paste0(df_clubes$padronizado2[2], '_', df_clubes$padronizado2[1]),
      'penaltis' = cbf_placar_penaltis,
      'uf1'= df_clubes$UF[1],
      'uf2'= df_clubes$UF[2]
    )
    return(cbf_partida)
  }
  df_partida = cbf_partida()


  ## arbitragem
  cbf_arbitragem = function() {

    ## preparando o data frame - cbf_arbitragem ####################################################################
    cbf_arbitro = str_extract(pdf, '(?<=Arbitro:).*(?=\r\n)')[[1]] %>% strsplit('\\(|\\)')
    cbf_arbitro = cbf_arbitro[[1]][1] %>% str_trim()

    cbf_asst1 = str_extract(pdf, '(?<=Arbitro Assistente 1:).*(?=\r\n)')[[1]] %>% strsplit('\\(|\\)')
    cbf_asst1 = cbf_asst1[[1]][1] %>% str_trim()

    cbf_asst2 = str_extract(pdf, '(?<=Arbitro Assistente 2:).*(?=\r\n)')[[1]] %>% strsplit('\\(|\\)')
    cbf_asst2 = cbf_asst2[[1]][1] %>% str_trim()

    cbf_qrt_arbt = str_extract(pdf, '(?<=Quarto Arbitro:).*(?=\r\n)')[[1]] %>% strsplit('\\(|\\)')
    cbf_qrt_arbt = cbf_qrt_arbt[[1]][1] %>% str_trim()

    cbf_an_campo = str_extract(pdf, '(?<=Analista de Campo:).*(?=\r\n)')[[1]] %>% strsplit('\\(|\\)')
    cbf_an_campo = cbf_an_campo[[1]][1] %>% str_trim()


    ## dataframe ###################################################################################################
    cbf_arbitragem = data.frame(
      'id_jogo' = df_partida$id_jogo,
      'arbritro' = cbf_arbitro,
      'assistente_1' = cbf_asst1,
      'assistente_2' = cbf_asst2,
      'quarto_arbitro' = cbf_qrt_arbt,
      'analista_de_campo' = cbf_an_campo
    )
    return(cbf_arbitragem)
  }
  df_arbitragem = cbf_arbitragem()


  ## aritilharia
  cbf_artilharia <- function() {

    pdf2 = pdf %>% read_lines() %>% str_trim()

    if(as.integer(df_partida$gol1) + as.integer(df_partida$gol2) == 0) {

      cbf_artilheiros <- data.frame(
        'id_jogo' = NA,
        'nome_completo' = NA,
        'clube' = NA,
        'ano' = NA,
        'gol' = NA,
        'tempo' = NA,
        'min_45' = NA,
        'min_90' = NA,
        'acrescimo' = NA
      )

      cbf_artilheiros$id_jogo = df_partida$id_jogo

    } else {
      # encontrar a "tabela" Gol

      gols = as.integer(df_partida$gol1) + as.integer(df_partida$gol2)
      f_ = which(pdf2 == "Cartões Amarelos") - 2
      i_ = f_ - gols + 1

      z = f_ - i_ + 1

      # #i_ = which(pdf2 == "Gols")
      # i_ = which(pdf2 == "Gols") #
      # f_ = which(pdf2 == "Cartões Amarelos")
      #
      # z = f_-i_-2

      # extração dos artilheiros
      cbf_artilheiros = pdf2[i_:f_] #%>%
      #readr::read_lines()

      # tratamento
      cbf_artilheiros = str_replace_all(cbf_artilheiros, fixed('  '), "!")
      cbf_artilheiros = str_replace_all(cbf_artilheiros, fixed('! '), "!")
      #artilheiros2 = str_replace_all(artilheiros2, fixed(' - PE'), "")
      cbf_artilheiros = str_replace_all(cbf_artilheiros, fixed(':00'), "")
      cbf_artilheiros = strsplit(cbf_artilheiros, "!+")

      # transformar lista em dataframe
      cbf_artilheiros = as.data.frame(cbf_artilheiros)
      cbf_artilheiros = t(cbf_artilheiros)

      rownames(cbf_artilheiros) = NULL

      if(ncol(cbf_artilheiros) == 6) {
        colnames(cbf_artilheiros) = c('min_45', 'tempo', 'no', 'tipo', 'jogador', 'clube')
        cbf_artilheiros = as.data.frame(cbf_artilheiros)
      } else if(ncol(cbf_artilheiros) == 4) {
        colnames(cbf_artilheiros) = c('min_45', 'tempo', 'tipo', 'clube')
        cbf_artilheiros = as.data.frame(cbf_artilheiros)
        cbf_artilheiros$jogador = '-'
        cbf_artilheiros$no = '-'
      }

      #cbf_artilheiros = as.data.frame(cbf_artilheiros)

      cbf_artilheiros$acresc = NA
      cbf_artilheiros$min_90 = NA

      # verificar se há acréscimos
      for (i in 1:z) {
        if (grepl('+', cbf_artilheiros$min_45[i], fixed = TRUE) == TRUE) {
          cbf_artilheiros$acresc[i] <- as.numeric(cbf_artilheiros$min_45[i])
          cbf_artilheiros$min_45[i] <- 45
        } else { cbf_artilheiros$acresc[i] <- 0 }
      }

      # verificar se o gol foi feito no 1º ou no 2º tempo
      for (i in 1:z) {
        if (cbf_artilheiros$tempo[i] == '2T') {
          cbf_artilheiros$min_90[i] <- 45 + as.integer(cbf_artilheiros$min_45[i])
        } else {
          cbf_artilheiros$min_90[i] <- 0 + as.integer(cbf_artilheiros$min_45[i])
        }
      }

      cbf_artilheiros = left_join(cbf_artilheiros, df_clubes %>% select(V2, padronizado2), by = c("clube" = "V2"))
      cbf_artilheiros$tipo_gol = NA

      # verificar se o gol foi contra
      for (i in 1:z) {
        if (cbf_artilheiros$tipo[i] == 'CT') {
          cbf_artilheiros$tipo_gol[i] <- 'Contra'
        } else if (cbf_artilheiros$tipo[i] == 'NR') {
          cbf_artilheiros$tipo_gol[i] <- 'Normal'
        } else if (cbf_artilheiros$tipo[i] == 'FT') {
          cbf_artilheiros$tipo_gol[i] <- 'Falta'
        } else if (cbf_artilheiros$tipo[i] == 'PN') {
          cbf_artilheiros$tipo_gol[i] <- 'Pênalti'
        }
      }

      # reorganizando o data frame
      cbf_artilheiros <- data.frame(
        'id_jogo' = NA,
        'nome_completo' = cbf_artilheiros$jogador,
        'clube' = cbf_artilheiros$padronizado2,
        'ano' = NA,
        'gol' = cbf_artilheiros$tipo_gol,
        'tempo' = cbf_artilheiros$tempo,
        'min_45' = cbf_artilheiros$min_45,
        'min_90' = cbf_artilheiros$min_90,
        'acrescimo' = cbf_artilheiros$acresc

      )

      cbf_artilheiros$id_jogo = df_partida$id_jogo
      cbf_artilheiros$ano = df_partida$ano
    }

    return(cbf_artilheiros)

  }
  df_artilharia = cbf_artilharia()


  ## excessão (arquivo mal formatado)
  if(url == 'https://conteudo.cbf.com.br/sumulas/2018/4100142se.pdf') {
    df_artilharia$nome_completo[1] = 'Jose Romario Silva de Souza'
    df_artilharia$nome_completo[2] = 'Denilson Pereira Junior'
    df_artilharia$nome_completo[3] = 'Yago Felipe da Costa Rocha'
    df_artilharia$clube[1] = 'Globo/RN'
    df_artilharia$clube[2] = 'Vitória/BA'
    df_artilharia$clube[3] = 'Vitória/BA'
    df_artilharia$clube[4] = 'Vitória/BA'
    df_artilharia$gol[1] = 'Normal'
    df_artilharia$gol[2] = 'Normal'
    df_artilharia$gol[3] = 'Pênalti'
    df_artilharia$tempo[1] = '1T'
    df_artilharia$tempo[2] = '1T'
    df_artilharia$tempo[3] = '2T'
    df_artilharia$min_45[1] = 8
    df_artilharia$min_45[2] = 19
    df_artilharia$min_45[3] = 34
    df_artilharia$min_90[1] = 8
    df_artilharia$min_90[2] = 19
    df_artilharia$min_90[3] = 34+45
  }


  ## compilado de todos os data frames
  cbf_sumula = list(
    'partidas' = df_partida,
    'arbitragem' = df_arbitragem,
    'artilharia' = df_artilharia,
    'clubes' = df_clubes
  )


  ## módulo para gravação em csv
  options(warn=-1)
  dir.create(paste0('csv'))
  dir.create(paste0('csv/partidas'))
  dir.create(paste0('csv/arbitragem'))
  dir.create(paste0('csv/artilharia'))


  #
  csv_clubes = cbf_sumula$clubes
  csv_partida = cbf_sumula$partidas
  csv_arbitragem = cbf_sumula$arbitragem
  csv_artilharia = cbf_sumula$artilharia


  ## gravar em formato csv
  write.csv(
    csv_partida,
    paste0('csv/partidas/', csv_partida$data, '_', csv_clubes$padronizado[1], '_', csv_clubes$padronizado[2], '_partida.csv'),
    fileEncoding = 'UTF-8',
    row.names = FALSE
  )
  write.csv(
    csv_arbitragem,
    paste0('csv/arbitragem/', csv_partida$data, '_', csv_clubes$padronizado[1], '_', csv_clubes$padronizado[2], '_arbitragem.csv'),
    fileEncoding = 'UTF-8',
    row.names = FALSE
  )
  write.csv(
    csv_artilharia,
    paste0('csv/artilharia/', csv_partida$data, '_', csv_clubes$padronizado[1], '_', csv_clubes$padronizado[2], '_artilharia.csv'),
    fileEncoding = 'UTF-8',
    row.names = FALSE
  )


  ##
  return(cbf_sumula)

}


## como usar ##
sumula('https://conteudo.cbf.com.br/sumulas/2014/4100161se.pdf')
sumula('https://conteudo.cbf.com.br/sumulas/2020/142372se.pdf')
