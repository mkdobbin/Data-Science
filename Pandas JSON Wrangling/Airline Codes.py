#PRED420 Section 59 GrEX 1 Matthew Dobbin 02/04/2017
#Import numpy and pandas packages.
from tabulate import tabulate
import numpy as np
import pandas as pd

#Create column headings for each dataframe.                                              
lines_col_names = ['airline_ID', 'name', 'alias', 'IATA', 'ICAO',
                   'callsign', 'country', 'active']
port_col_names = ['airport_ID', 'name', 'city', 'country', 'IATA/FAA', 'ICAO',
                  'latitude', 'longitude', 'altitude', 'timezone', 'DST', 'tz']
routes_col_names = ['airline', 'airline_ID', 'source_airport', 'source_airport_ID',
                    'dest_airport', 'dest_airport_ID', 'codeshare', 
                    'stops', 'equipment']

#Read data files into pandas dataframes.
airlinesdf = pd.read_csv('./Desktop/Northwestern/420 Databases/GrEX/airlines.dat', 
                         header = None, names = lines_col_names)
airportsdf = pd.read_csv('./Desktop/Northwestern/420 Databases/GrEX/airports.dat', 
                         header = None, names = port_col_names)
routesdf = pd.read_csv('./Desktop/Northwestern/420 Databases/GrEX/routes.dat', 
                       header = None, names = routes_col_names)

#Print first three rows of each dataframe.
print tabulate(airlinesdf.iloc[[0, 1, 2],:], headers = 'keys', tablefmt = 'psql')
print tabulate(airportsdf.iloc[[0, 1, 2],[0 , 1, 2, 3, 4, 5]], 
                headers = 'keys', tablefmt = 'psql')
print tabulate(airportsdf.iloc[[0, 1, 2],[6, 7, 8, 9, 10, 11]], 
                headers = 'keys', tablefmt = 'psql')
print tabulate(routesdf.iloc[[0, 1, 2],[0 , 1, 2, 3, 4,]], 
                headers = 'keys', tablefmt = 'psql')
print tabulate(airportsdf.iloc[[0, 1, 2],[5, 6, 7, 8]], 
                headers = 'keys', tablefmt = 'psql')

#Create new column in dataframe and return True if a duplicate.
#Each airport and airline should have unique name.
#Route is unique to airline travelling from source to destination 
airlinesdf['is_duplicated'] = airlinesdf.duplicated(['name'])
airportsdf['is_duplicated'] = airportsdf.duplicated(['name'])
routesdf['is_duplicated'] = routesdf.duplicated(['airline', 'source_airport',
                                                 'dest_airport', 'stops'])

#Prints the count of duplicate results.
print airlinesdf['is_duplicated'].sum(), 'airline duplicates in data.'
print airportsdf['is_duplicated'].sum(), 'airport duplicates in data.'
print routesdf['is_duplicated'].sum(), 'route duplicates in data.'

#Data types of the columns in airlines dataset.
print airlinesdf.dtypes

#Data types of the columns in airports dataset.
print airportsdf.dtypes

#Data types of the columns in routes dataset.
print routesdf.dtypes

#Defunct airlines are airlines that are no longer active. 
#OpenFlights states this field is not reliable as airlines that stopped 
#flying may not have had their IATA code reassigned and will incorrectly show as 'Y'.
#Ideally the dataset would have a time based column with date of last flight
#however it is not included in the dataset. I have defined defunct as airlines 
#that don't have routes.
nodupairlinesdf = airlinesdf.drop_duplicates('name')
notdefunctdf = nodupairlinesdf[nodupairlinesdf['IATA'].isin(routesdf['airline']) |
                               nodupairlinesdf['ICAO'].isin(routesdf['airline'])]
defunct = len(nodupairlinesdf) - len(notdefunctdf)
print 'There are', defunct, 'defunct airlines.'

#Determine number of routes that don't originate from an airport. 
#Additional information states that speciali value \N is
#used to indicate no value was available/missing data.  
nowheredf = routesdf[routesdf['source_airport'].isin(['\N'])]
print 'There are', len(nowheredf), 'routes that do not originate from an airport.'

#Pickle the dataframes for future use.
#Can read them back in using df = pd.read_pickle('filepath')
airlinesdf.to_pickle('./Desktop/Northwestern/420 Databases/GrEX/airlinesdf.pkl')
airportsdf.to_pickle('./Desktop/Northwestern/420 Databases/GrEX/airportsdf.pkl')
routesdf.to_pickle('./Desktop/Northwestern/420 Databases/GrEX/routesdf.pkl')