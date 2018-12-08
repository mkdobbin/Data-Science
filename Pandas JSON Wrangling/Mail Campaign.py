#PRED420 Section 59 GrEX2 Matthew Dobbin 19/04/2017
#Import required packages.
from tabulate import tabulate
import numpy as np
import pandas as pd
import sqlite3

#Read csv files into pandas dataframes. First row in .csv are column headings.
customerdf = pd.read_csv('./Desktop/Northwestern/420 Databases/GrEX2/customer.csv')              
maildf = pd.read_csv('./Desktop/Northwestern/420 Databases/GrEX2/mail.csv') 
itemdf = pd.read_csv('./Desktop/Northwestern/420 Databases/GrEX2/item.csv') 
                      
#Print first 5 records of the mail dataframe. Number of columns too large to fit
#on one page so printed in three different tables.
print tabulate(maildf.head(5).iloc[:, :6], headers = 'keys', tablefmt = 'psql')
print tabulate(maildf.head(5).iloc[:, 6:12], headers = 'keys', tablefmt = 'psql')
print tabulate(maildf.head(5).iloc[:, 12:], headers = 'keys', tablefmt = 'psql')

#Count the number of duplicates in 'acctno' column. Each customer should have a
#unique customer account number. 
cust_du = customerdf['acctno'].duplicated().sum()
print 'There are', cust_du, 'customer duplicate records.'

#Find the number of account numbers in mail and item that are not in customer.
i_isin = itemdf[itemdf['acctno'].isin(customerdf['acctno'])]
icount = len(itemdf) - len(i_isin)
m_isin = maildf[maildf['acctno'].isin(customerdf['acctno'])]
mcount = len(maildf) - len(m_isin)
print 'There are', icount, 'account numbers in item that are not in customer.'
print 'There are', mcount, 'account numbers in mail that are not in customer.'

#Create a connection to sqlite3 and create three new tables in the database.
conn = sqlite3.connect('GrEX2')
cur = conn.cursor()
cur.execute("DROP TABLE IF EXISTS customer")
cur.execute("DROP TABLE IF EXISTS mail")
cur.execute("DROP TABLE IF EXISTS item")
cur.execute("DROP TABLE IF EXISTS mail_focus")
customerdf.to_sql("customer", conn, if_exists='append', index=False)
maildf.to_sql("mail", conn, if_exists='append', index=False)
itemdf.to_sql("item", conn, if_exists='append', index=False)

#Verify records have been included in database. Checked the names of tables in
#the database and the number of rows in each table. Compared against len(dataframe).
query = "SELECT name FROM sqlite_master WHERE type = 'table'"
tables = pd.read_sql_query(query, conn)
custcount = pd.read_sql_query("SELECT COUNT(acctno) from customer", conn)
mailcount = pd.read_sql_query("SELECT COUNT(acctno) from mail", conn)
itemcount = pd.read_sql_query("SELECT COUNT(acctno) from item", conn)
print tables, custcount, itemcount, mailcount
print len(customerdf), len(itemdf), len(maildf)

#Create new column in mail that calculates the total number of mail sent to customer.
cur.execute("ALTER TABLE mail ADD COLUMN sum_mail int")
query = """
        UPDATE mail
        SET sum_mail = mail_1 + mail_2 + mail_3 + mail_4 + mail_5 + mail_6 + mail_7 + 
                       mail_8 + mail_9 + mail_10 + mail_11 + mail_12 + mail_13 + 
                       mail_14 + mail_15 + mail_16
        """
cur.execute(query)

#Create new table that selects customers who received more than 6 mail campaigns.
#Table is selecting data from two different tables, mail and customer. 
#"acctno" is the link between the two tables.
query = """
        CREATE TABLE mail_focus 
        AS SELECT customer.acctno, mail.sum_mail, customer.ytd_transactions_2009, 
                  customer.ytd_sales_2009, customer.zhomeent, customer.zmobav, 
                  customer.zcredit, customer.zhitech   
           FROM customer, mail
           WHERE customer.acctno = mail.acctno AND mail.sum_mail > 6           
        """
cur.execute(query)

#Create pandas dataframe from the database table mail_focus. Create four
#calculated columns with a 1 for 'Y' and 0 for 'U'. Missing data 
#in dataframe filled with 'NaN'. Export dataframe to .csv file.
df = pd.read_sql_query("SELECT * FROM mail_focus", conn)
cat_codes = {'Y':1, 'U':0}
df['zhomeent01'] = df['zhomeent'].map(cat_codes)
df['zmobav01'] = df['zmobav'].map(cat_codes)
df['zcredit01'] = df['zcredit'].map(cat_codes)
df['zhitech01'] = df['zhitech'].map(cat_codes)
df = df.fillna('NaN')     
df.to_csv('./Desktop/Northwestern/420 Databases/GrEX2/mail_focus.csv')
print tabulate(df.head(10).iloc[:, :4], headers = 'keys', tablefmt = 'psql')
print tabulate(df.head(10).iloc[:, 4:8], headers = 'keys', tablefmt = 'psql')
print tabulate(df.head(10).iloc[:, 8:], headers = 'keys', tablefmt = 'psql')

#Create cross-tabulation table that computes frequencies for the
#z columns and calculated numeric categories.
print pd.crosstab(df.zhomeent, df.zhomeent01, margins = True)
print pd.crosstab(df.zmobav, df.zmobav01, margins = True)
print pd.crosstab(df.zcredit, df.zcredit01, margins = True)
print pd.crosstab(df.zhitech, df.zhitech01, margins = True)

#Save the database changes and close the connection
conn.commit()
conn.close()

#Pickle the dataframes for future use.
#Can read them back in using df = pd.read_pickle('filepath')
customerdf.to_pickle(r'./Desktop/Northwestern/420 Databases'
                     r'/GrEX2/customerdf.pkl')
maildf.to_pickle('./Desktop/Northwestern/420 Databases/GrEX2/maildf.pkl')
itemdf.to_pickle('./Desktop/Northwestern/420 Databases/GrEX2/itemdf.pkl')