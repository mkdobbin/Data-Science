#Import packages
import pandas as pd
import numpy as np
import json
from pandas.io.json import json_normalize
import glob
from tabulate import tabulate

#Read in file, using with closes file after reading.
path = './Desktop/Northwestern/420 Databases/GrEX3/'
with open(path + 'data2.json') as input_file:
    jsondat = json.load(input_file)

#Create dataframe with all columns from reviews and rename column headings.
df = json_normalize(jsondat['Reviews'])
cm = list(df)
Rating_names = []
new_names = [cm[0], cm[1], cm[2], cm[3], 'Business_service', 'Front_desk', 
             'Cleanliness', 'Location', 'Overall', 'Rooms', 'Service', 
             'Sleep_quality', 'Value', cm[13], cm[14]]
Ratings = ['Business_service', 'Front_desk', 'Cleanliness', 'Location', 
           'Overall', 'Rooms', 'Service', 'Sleep_quality', 'Value']
df.columns = [new_names]




import rhinoscriptsyntax as rs
import json

#prompt the user for a file to import
filter = "JSON file (*.json)|*.json|All Files (*.*)|*.*||"
filename = rs.OpenFileName("data2", filter)

#Read JSON data into the datastore variable
if filename:
    with open(filename, 'r') as f:
        datastore = json.load(f)

#Use the new datastore datastructure
print datastore["office"]["parking"]["style"]







#Check for duplicates, duplicate would be defined as entire row (all columns) 
#data exactly the same. No duplicates found.
df = df.drop_duplicates()

#Change ratings data type to numeric and date to datetime and 
df[Ratings] = df[Ratings].apply(pd.to_numeric)
df['Date'] = pd.to_datetime(df['Date'])
print df.dtypes

#Calculate min, max and mean values for each rating types.
ratings_df = df[Ratings]
stats = {'min': ratings_df.min(),'mean': ratings_df.mean(),'max': ratings_df.max()}
stats_df = pd.DataFrame(stats)
print stats_df

#Save content, date as text indexed by name.
text = ['Author', 'Date', 'Content']
text_df = df[text]
text_df = text_df.sort_values(['Author', 'Date'])

#Printing of dataframes. The dataframes contain long strings.
#Maximum column width has been limited to 25 to show segment.
pd.options.display.max_colwidth = 25
print text_df.head()

#Part 2
#Read in all JSON files in this directory location and store in pandas dataframe. 
path2 = './Desktop/Northwestern/420 Databases/GrEX3/*.json'
hotel_df = pd.DataFrame()
for name in glob.glob(path2):
    with open(name) as input_file:
      jsondat = json.load(input_file)
    temphotel_df = json_normalize(jsondat['HotelInfo'])
    hotel_df =  hotel_df.append(temphotel_df, ignore_index = True) 

#The address column still has html script. Code to remove html script.
hotel_df['Address'] = hotel_df['Address'].str.replace('<[^<]+?>', '')
hotel_df['Address'] = hotel_df['Address'].str.replace('c/', '') 

#Check for duplicates, duplicate would be defined as same HotelID.
hotel_df = hotel_df.drop_duplicates('HotelID')
print hotel_df.head(5).iloc[:, :3]
print hotel_df.head(5).iloc[:, 3:6]

#Pickle the dataframes for future use.
#Can read them back in using df = pd.read_pickle('filepath').
df.to_pickle('./Desktop/Northwestern/420 Databases/GrEX3/df.pkl')
ratings_df.to_pickle('./Desktop/Northwestern/420 Databases/GrEX/ratings_df3.pkl')
stats_df.to_pickle('./Desktop/Northwestern/420 Databases/GrEX/stats_df3.pkl')
text_df.to_pickle('./Desktop/Northwestern/420 Databases/GrEX3/text_df.pkl')
hotel_df.to_pickle('./Desktop/Northwestern/420 Databases/GrEX3/hotel_df.pkl')