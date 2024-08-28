import requests
from bs4 import BeautifulSoup
import pandas as pd
import time

def paginas(i):
	url = f'https://ludopedia.com.br/search_jogo?pagina={i}'
	response = requests.request("GET", url)
	soup = BeautifulSoup(response.text, 'html.parser')
	lista_links_bruto = soup.find_all(class_='full-link')
	nome_jogo = soup.find_all('h4', class_='mar-no')
	return lista_links_bruto, nome_jogo

elementos_slug = []
elementos_nome = []
for i in range(1, 2600): #2599
    start = time.time()
    elementos_slug.extend(paginas(i)[0])
    elementos_nome.extend(paginas(i)[1])
    print(f"Página {i} -> {(time.time() - start):.2f} segundos.")

slugs = []
jogos = []
for i in elementos_slug:
    slugs.append(i.get('href').replace('https://ludopedia.com.br/jogo/', ''))
for i in elementos_nome:
    jogos.append(i.text.split('(')[0].strip())

pd.DataFrame({
	'jogo': jogos,
	'slug': slugs
}).to_csv('slugs_ludopedia.csv', index=False, sep=';')
