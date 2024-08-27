import requests
from bs4 import BeautifulSoup
import pandas as pd
import time

def paginas(i):
	url = f'https://ludopedia.com.br/search_jogo?pagina={i}'
	response = requests.request("GET", url)
	soup = BeautifulSoup(response.text, 'html.parser')
	lista_links_bruto = soup.find_all(class_='full-link')
	return lista_links_bruto

elementos = []
for i in range(1, 4): #2599
    start = time.time()
    elementos.extend(paginas(i))
    print(f"Página {i} -> {(time.time() - start):.2f} segundos.")

slugs = []
for i in elementos:
    slugs.append(i.get('href').replace('https://ludopedia.com.br/jogo/', ''))

pd.DataFrame(slugs, columns=['slug']).to_csv('slugs_ludopedia.csv', index=False, sep=';')
