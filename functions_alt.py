import pandas as pd
import requests
import math
import json
import re
import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

# Twitter

def tweeters_metadata(fields_to_retrieve, df, client):
    '''
    This function retrieves specific metadata of Twitter accounts
    '''
    
    tw = pd.DataFrame()
    
    for i in range(math.ceil(df.shape[0]/100)):
        print('Total: '+str(round(100*i/math.ceil(df.shape[0]/100),2))+' %', end='\r')
        try:
            user = client.lookup_users(user_ids=df.iloc[(i*100):((i*100)+99)]['Outlet or Author'].tolist())
            tw_aux = pd.json_normalize(user)[fields_to_retrieve]
            tw = pd.concat([tw, tw_aux])
        except:
            pass
    
    return(tw)


def tweets_metadata(fields_to_retrieve, df, client):
    '''
    This function retrieves specific metadata of tweets
    '''
    
    tw = pd.DataFrame()
    
    for i in range(math.ceil(df.shape[0]/100)):
        print('Total: '+str(round(100*i/math.ceil(df.shape[0]/100),2))+' %', end='\r')
        try:
            tweets = client.statuses_lookup(id_=df.iloc[(i*100):((i*100)+99)]['External Mention ID'].tolist())
            tw_aux = pd.json_normalize(tweets)[fields_to_retrieve]
            tw = pd.concat([tw, tw_aux])
        except:
            pass
    
    tw['is_retweet'] = False
    tw.loc[~tw['retweeted_status.id'].isna(), 'is_retweet'] = True
    
    return(tw)


def twitter_bots(tw_id, blt_twitter):
    '''
    This function returns the Botometer scores of Twitter accounts
    '''
    
    df_botometer = pd.DataFrame()
    
    for i in tw_id:
        try:
            result = blt_twitter.check_account(i)
            df_aux = pd.DataFrame({'user_id':[i],'cap_eng':[result['cap']['english']], 'cap_univ':[result['cap']['universal']],
                                   'score_eng':[result['display_scores']['english']['overall']],
                                   'score_over':[result['display_scores']['universal']['overall']]})
            df_botometer = pd.concat([df_botometer, df_aux])
        except:
            pass
        
    return(df_botometer)


def news_data(url, header, domains):
    '''
    This function is for querying Site Traffic API
    '''
    
    df_news = pd.DataFrame()
    
    for domain in domains:
        print('Total: '+str(round(df_news.shape[0]/math.ceil(len(domains)/100),2))+' %', end='\r')
        params = {'domain':domain}
        response = requests.get(url, headers=header, params=params).json()
        df_aux = pd.json_normalize(response)
        
        try:
            top_c = [x for x in response['TopCountryShares'][0].values()][0]
            top_v = [x for x in response['TopCountryShares'][0].values()][1]
        except:
            top_c = None
            top_v = None
            
        df_aux_2 = pd.DataFrame({'topcountry_name':top_c, 'topcountry_views':top_v}, index=[0])

        df_aux = pd.concat([df_aux, df_aux_2], axis=1)
        df_aux['id'] = domain

        df_news = pd.concat([df_news, df_aux])
        
    return(df_news)


def full_url(short_url):
    '''
    This function transforms short URL into a full URL
    '''
    
    try:
        new_url = requests.head(short_url).headers['location']
        return(new_url)
    except:
        return(short_url)

    
def wiki_ores(df_ores):
    '''
    This function is for querying Wikipedia ORES
    '''
    
    df_res = pd.DataFrame(columns=['wiki', 'revid', 'score', 'main_topic', 'major_topic'])
    
    for i in range(df_ores.shape[0]):
        lang = df_ores['Wikipedia_lg'][i]+'wiki'
        revids = df_ores['Wikipedia_rev'][i]
        
        ores_url = 'https://ores.wikimedia.org/v3/scores/'+lang+'/?revids='+revids
        query_ores = requests.get(ores_url, verify=False, headers = {'User-Agent':'Mozilla/5.0 (Windows NT 10.0; Win64; x64)'})
        response_ores = json.loads(query_ores.text)
        try:
            quality_score = response_ores[lang]['scores'][revids]['articlequality']['score']['prediction']
        except:
            quality_score = None
        try:
            topic_score = response_ores[lang]['scores'][revids]['articletopic']['score']['prediction'][0]
            major_topic = topic_score.split('.')[0]
        except:
            topic_score = None
            major_topic = None
        
        df_res = pd.concat([df_res,
                            pd.DataFrame({'wiki':df_ores['Wikipedia_lg'][i],
                                          'revid':revids,
                                          'score':quality_score,
                                          'main_topic':topic_score,
                                          'major_topic':major_topic}, index=[0])])
    return(df_res.reset_index(drop=True))

def wikipedia_data(wikipedia_urls):
    '''
    This function returns specific metadata of Wikipedia pages
    '''
    
    count=0
    
    wikidata = pd.DataFrame(columns=['url', 'watchers', 'revisions', 'minor_edits', 'editors',
                                     'author', 'created_at', 'assessment',
                                     'characters', 'words', 'references', 'unique_references', 'sections'])
    
    for i in wikipedia_urls:
        count+=1
        print(round(100*count/len(wikipedia_urls),2), end='\r')

        try:
            url_info = 'https://xtools.wmflabs.org/api/page/articleinfo/' + i
            query_info = requests.get(url_info, verify=False)
            response_info = json.loads(query_info.text)

            url_prose = 'https://xtools.wmflabs.org/api/page/prose/' + i
            query_prose = requests.get(url_prose, verify=False)
            response_prose = json.loads(query_prose.text)

            if(response_info['assessment']):
                assessment = response_info['assessment']['value']
            else:
                assessment = None

            wikidata.loc[len(wikidata)] = pd.Series({'url':i,
                                                     'watchers':response_info['watchers'],
                                                     'revisions':response_info['revisions'],
                                                     'minor_edits':response_info['minor_edits'],
                                                     'editors':response_info['editors'],
                                                     'author':response_info['author'],
                                                     'created_at':response_info['created_at'],
                                                     'assessment':assessment,
                                                     'characters':response_prose['characters'],
                                                     'words':response_prose['words'],
                                                     'references':response_prose['references'],
                                                     'unique_references':response_prose['unique_references'],
                                                     'sections':response_prose['sections']})
        except:
            print('Error: '+ i)
        
    return(wikidata)
  
    
def wikipedia_views(wikipedia_urls):
    '''
    This function returns pageviews of Wikipedia pages
    '''
    
    df_views = pd.DataFrame({'page_title':wikipedia_urls})
    
    count=0
    
    for i in wikipedia_urls:
        count+=1
        print(round(100*count/len(wikipedia_urls),2), end='\r')

        try:
            url_views = 'https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/' + re.sub('\..*', '', i) + '.wikipedia/all-access/user/' + re.sub('^.*org/', '', i) + '/monthly/2015010100/2023020100'
            query_views = requests.get(url_views, verify=False, headers = {'User-Agent':'Mozilla/5.0 (Windows NT 10.0; Win64; x64)'})
            response_views = json.loads(query_views.text)

            for x in response_views['items']:
                df_views.loc[df_views.page_title==i,x['timestamp']] = x['views']

        except:
            print('Error: '+ i)
            
    return(df_views)
