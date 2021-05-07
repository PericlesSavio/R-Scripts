# R-Scripts
Personal scripts made in R

### Ludopedia ###
Ludopedia é uma comunidade e um portal que pretende ser a fonte definitiva de jogos de tabuleiro em português.

Website: https://www.ludopedia.com.br/

A função ludopedia(`slug`) permite extrair as seguintes informações do site:

* Nome do jogo
* Slug
* Descrição
* Componentes
* Informações de sleeves
* Dependencia do Idioma
* Idade mínima
* Temo de jogo
* Número de jogadores
* Tema
* Editora
* Domínio
* Categoria
* Mecânicas
* Designers
* Artistas

#### Exemplos ####
Terra Mystica: https://www.ludopedia.com.br/jogo/terra-mystica
```R
ludopedia('terra-mystica')

```

Zombicide: Black Plague: https://www.ludopedia.com.br/jogo/zombicide-black-plague
```R
ludopedia('zombicide-black-plague')

```

Pandemic: Reign of Cthulhu: https://www.ludopedia.com.br/jogo/pandemic-reign-of-cthulhu
```R
ludopedia('pandemic-reign-of-cthulhu')

```

A função irá coletar e salvar as informações em um arquivo .csv na pasta `ludopedia`.