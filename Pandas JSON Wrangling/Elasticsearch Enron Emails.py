#Import packages
import pandas as pd
import numpy as np
import json
import time
from pandas.io.json import json_normalize
from tabulate import tabulate
from elasticsearch import Elasticsearch, helpers

"""
# Connect to Enron index in ES and retrieve all messages.
# Save as pandas dataframe and pickle for later use 
es=Elasticsearch('http://enron:spsdata@129.105.88.91:9200')  
query={"query" : {"match_all" : {}}}    
scanner=helpers.scan(client= es, query=query, scroll= "100m", index="", doc_type="email", timeout="100m")
selectdocs = [msg['_source'] for msg in scanner]
emails = pd.DataFrame(selectdocs)
emails.to_pickle('./Desktop/Northwestern/420 Databases/GrEX4/emails.pkl')
"""

#Retrieve pickled pandas dataframe and check size to see if download successful
#Expect 250000 rows (emails).
emails = pd.read_pickle('./Desktop/Northwestern/420 Databases/GrEX4/emails.pkl')
print emails.shape

#Investigate the dict for potential Ken Lay email addresses
#'_source': { 'headers': { 'From': , 'To' } 
#Create a list that will hold dicts of pairs of msgID's and email addresses
dictList = []
KEcount = 0
TEcount = 0
# Create a loop that will dict list 
for msg in emails['headers']:
    
    try: 
        dictList.append({'msgID':msg['Message-ID'],'From':msg['From']})
        
        #Split required as emails can be sent to multiple people
        msgTo= msg['To'].split(',')
        
        for mTo in msgTo:
            dictList.append({'msgID':msg['Message-ID'],'To':mTo.encode('utf-8').strip()})
    
    except KeyError:
        KEcount = KEcount + 1
    
    except TypeError:
        TEcount = TEcount + 1
       
#Change dictList to a pandas dataframe and print error count
to_from = pd.DataFrame(dictList)
print to_from.shape
print KEcount, "Key Errors encountered"
print TEcount, "Type Errors encountered" 

#Data manipulation to obtain list of Ken Lay email addresses
tolist = to_from['To']
fromlist = to_from['From']
biglist = tolist.append(fromlist)
bigdf = pd.DataFrame({'address': biglist})
bigdf = bigdf.drop_duplicates()
bigdf = bigdf.dropna()
df1 = bigdf[bigdf['address'].str.contains('lay')]
df2 = bigdf[bigdf['address'].str.contains('ken')]
df3 = bigdf[bigdf['address'].str.contains('chairman')]
bigdf = df1.append(df2.append(df3))
bigdf = bigdf.drop_duplicates()

#Manually filtering was required in excel. 
bigdf.to_csv('./Desktop/Northwestern/420 Databases/GrEX4/alias.csv')
aliasdf = pd.read_csv('./Desktop/Northwestern/420 Databases/GrEX4/alias1.csv')

#Further filtering was required by checking speculative emails by 
#reviewing body of emails using emails['body']ix[x]
emails.to_csv('./Desktop/Northwestern/420 Databases/GrEX4/emails.csv')
layfrom = to_from[to_from['From'].isin(aliasdf['address'])]
layfrom = layfrom.drop_duplicates('From')
layfrom.to_csv('./Desktop/Northwestern/420 Databases/GrEX4/layfrom.csv')

    



