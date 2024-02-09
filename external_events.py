from striprtf.striprtf import rtf_to_text
from nltk.corpus import stopwords
from role import role
import pandas as pd
import asyncio
import aiohttp
import json
import os
import numpy as np
from constants import *
from dotenv import load_dotenv

load_dotenv()

#STEP 1: Extract a list of all the articles from 
# the rtf files
def extract_data():
    all_articles=[]
    for i in range(1,13):

        with open(f"unicef_events_data/external{i}.rtf") as infile:
            content = infile.read()
            text = rtf_to_text(content)
            articles = text.split('End of Document')
            all_articles.extend(articles)
    return all_articles

#STEP 2: clean the article to decrease amount of tokens
# perform operaions concurrently
async def extract_articles(article):
    #remove stopwords
    filtered_article = ' '.join([word for word in article.split(' ') if word not in stopwords.words('dutch')])
    #replace newline char
    filtered_article = filtered_article.replace("\n", "")
    #remove unnecessary characters in the beginning
    try:
        filtered_article = filtered_article[filtered_article.index("Body")+4:]
    except:
        filtered_article = filtered_article[100:]
    content = f"""Dit is het artikel: {filtered_article}"""
    return content

#STEP 3: Send articles concurrently to Chat GPT
async def call_chatgpt(prompt):
    #get the api key
    #TODO: add your key in a .env file
    api_key = os.getenv('api_key')
    #define system role
    role_gpt = role()
    #prepare asyncio objects
    sem = asyncio.Semaphore(200)
    session=aiohttp.ClientSession()
    #call chatgpt with the given prompt and role definition
    try:
            async with sem, session.post(
            url='https://api.openai.com/v1/chat/completions',
            headers={"Content-Type": "application/json", "Authorization": f"Bearer {api_key}"},
            json={
        'model': "gpt-3.5-turbo",
        'messages': [
            {"role": "system", "content": role_gpt},
            {"role": "user", "content": prompt},
        ]
    }) as resp:
                response = await resp.json()
                if "error" in response:
                    print(f"OpenAI request failed with error {response['error']}")
                await session.close()
                await asyncio.sleep(15)
                return response['choices'][0]['message']['content']
    except:
        print("Request failed.")
        await session.close()
        await asyncio.sleep(5)
    finally:
        sem.release()

#collect the asynchronous operations in 1 function
async def main(all_articles):
    list_of_prompts=[]
    list_of_responses=[]
    for article in all_articles:
        result = await extract_articles(article)
        list_of_prompts.append(result)
    for prompt in list_of_prompts:
        result = await call_chatgpt(prompt)
        try:
            list_of_responses.append(json.loads(result))
        except:
            pass
        if len(list_of_responses)%10==0:
            print('Length of the extracted articles list:', len(list_of_responses))
            print(f'Still {len(all_articles)-len(list_of_responses)} left to extract')
    return list_of_responses

def clean_the_dataset(df):
    # drop potential duplicates

    #clean the countries list
    df['land']=df['land'].replace(["Groot-Brittannië", "Tsjechoslowakije", "Nederlands-Indië", 
                                   "Beiroet", "VS", "oekraïne", "Texas", "nederland", "duitsland", 
                                   "België Duitsland", "java", "Molukken", "italië","noorwegen","britse kanaaleiland jersey", 
                                   "indonesië", "Amerikaanse staat californië", "Amerikaanse staat colorado", "Amerikaanse canadese",
                                   "Amerikaanse zuidkust", "Australie", "Bahama’s", "Belgie","Brazilie", "Californië",
                                   "Charkov", "Congo", "Divers", "Diverse landen in europa (belgië, denemarken, duitsland, frankrijk, groot-brittannië, hongarije, italië, polen, portugal, spanje, tsjechië)",
                                   "Diversen", "Ethiopië, kenia, somalië", "Florida","Florida, verenigde staten", "Frankrijk griekenland sardinië",
                                   "Hoorn afrika", "Hoorn afrika midden-oosten", "India bangladesh", "India en bangladesh", "India, bangladesh, Indonesië",
                                   "Indonesië en maleisië", 
                                   "Iraakse", "Israel", "Italie", "Kirgizië, tadzjikistan", "Latijns-amerika", "Latijns-amerikaanse landen",
                                   "Madagascar", "Michigan", "Mozambique, malawi, zimbabwe", "Nederlands-indië", "New York", "Noord- zuid-korea", "Noordpoolgebied",
                                   "Noordwest-italië zuidoost-frankrijk", "Oekraine", "Oekraïne rusland turkse stad istanbul turkije verenigde naties", "Oost-aziê",
                                   "Oost-europa", "Portugal, spanje, frankrijk, marokko", "Venetië", "Zuid-amerika", "Zuid-europa", "Zuid-frankrijk", "Grote oceaan", "India, bangladesh", "New york"], 
                                   ["Verenigd Koninkrijk", "Tsjechië", "Indonesië", "Libanon", 
                                   "Verenigde Staten","Oekraïne", "Verenigde Staten", "Nederland", "Duitsland", 
                                   "Europa","Java", "Indonesië", "Italië", "Noorwegen","Kaaimaneilanden", "Indonesië", 
                                   "Amerika", "Amerika", "Amerika", "Amerika", "Australië", "Bahama's", "België","Brazilië",
                                   "Amerika", "Oekraïne", "Democratische republiek congo", "World", "Europa","World", "Afrika", "Amerika", "Amerika", 
                                   "Europa", "Afrika", "Afrika", "India", "India","India", "Indonesië", "Irak", "Israël","Italië", "Kirgizië","Amerika", "Amerika",
                                   "Madagaskar", "Amerika", "Mozambique", "Suriname", "Amerika", "Zuid-korea", "World", "Europa","Oekraïne", "Oekraïne","Azië", 
                                   "Europa", "Europa", "Italië", "Amerika", "Europa", "Europa", "Azië", "India", "Amerika"])
    #make necessary data cleaning & feature engineering
    df['land'] = df.Land.combine_first(df.land)
    df['type'] = df.Type.combine_first(df.type)
    df['slachtoffers'] = df.Slachtoffers.combine_first(df.slachtoffers)
    df['datum'] = df.Datum.combine_first(df.datum)
    df['datum'] = df.Datum.combine_first(df.publicatie_datum)
    df['publicatie_datum'] = df.Publicatie_datum.combine_first(df.publicatie_datum)
    df['land'] = df['land'].str.strip()
    df['land'] = df['land'].str.capitalize()
    df['type'] = df['type'].str.strip()
    df['type'] = df['type'].str.lower()
    df['datum'] = df['datum'].str.strip()
    df['publicatie_datum'] = df['publicatie_datum'].str.strip()
    df=df.drop(['Land', 'Datum', 'Publicatie_datum', 'Type', 'Slachtoffers'], axis=1)
    df = df[df['land'].isna()==False]
    df = df[df['land']!='Null']
    df = df[df['land']!='Onbekend']


    #prepare the country list
    import constants
    european_countries=list(map(str.capitalize, constants.european_countries))
    africa_countries=list(map(str.capitalize, constants.africa_countries))
    america_countries=list(map(str.capitalize, constants.america_countries))
    asia_countries=list(map(str.capitalize, constants.asia_countries))
    man_made=list(map(str.lower, constants.man_made))
    
    #create proximities
    df['Nederland_ind'] = np.where(df['land']=='Nederland', 1, 0)
    df['Europe_ind'] = np.where(df['land'].isin(european_countries), 1, 0)
    df['World_ind'] = np.where(df['Nederland_ind']==1, 0,np.where(df['Europe_ind']==1,0,1))
    df['type'] = df["type"].str.lower()
    df['man_made'] = np.where(df['type'].isin(man_made), 1, 0)
    df['Marokko_ind'] = np.where(df['land']=='Marokko', 1, 0)
    df['Turkije_ind'] = np.where(df['land']=='Turkije', 1, 0)
    df['Belgie_ind'] = np.where(df['land']=='België', 1, 0)
    df['Duitsland_ind'] = np.where(df['land']=='Duitsland', 1, 0)
    df['Indonesie_ind'] = np.where(df['land']=='Indonesië', 1, 0)
    df['Polen_ind'] = np.where(df['land']=='Polen', 1, 0)
    df['Suriname_ind'] = np.where(df['land']=='Suriname', 1, 0)
    df['Oceanie_ind'] = np.where(df['land'].isin(oceanie_countries), 1, 0)
    df['Afrika_ind'] = np.where(df['land'].isin(africa_countries), 1, 0)
    df['Amerika_ind'] = np.where(df['land'].isin(america_countries), 1, 0)
    df['Asie_ind'] = np.where(df['land'].isin(asia_countries), 1, 0)

    #clean the dates
    df['datum']= pd.to_datetime(df['datum'], format="mixed")
    return df

if __name__ == "__main__":
    #articles = extract_data()
    #articles = list(set(articles))
    #results = asyncio.run(main(articles))
    df = pd.DataFrame(results)
    df_final = clean_the_dataset(df) 
    
    #save the file as csv
    df_final.to_csv('external_events_data.csv', index=False)

    #check that produced results are what is exected
    list(df_final.groupby(['land']).count().index)
    df_final[['land', 'Nederland_ind']].groupby(['land', 'Nederland_ind']).count()
    df_final[['land', 'World_ind']].groupby(['land', 'World_ind']).count()
    df_final[['land', 'Europe_ind']].groupby(['land', 'Europe_ind']).count()
    df_final[['land', 'Marokko_ind']].groupby(['land', 'Marokko_ind']).count()
    df_final[['land', 'Turkije_ind']].groupby(['land', 'Turkije_ind']).count()
    df_final[['land', 'Suriname_ind']].groupby(['land', 'Suriname_ind']).count()
    df_final[['land', 'Oceanie_ind']].groupby(['land', 'Oceanie_ind']).count()
    df_final[['land', 'Afrika_ind']].groupby(['land', 'Afrika_ind']).count()

    df_final['datum'].max()
    df_final['datum'].min()


