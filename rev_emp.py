import pandas as pd
import urllib as u
from bs4 import BeautifulSoup as bs

"""
First visit www.Finviz.com and get the base url for the quote page.
example: http://finviz.com/quote.ashx?t=aapl

Then write a simple function to retrieve the desired ratio. 
In this example I'm grabbing Price-to-Book (mrq) ratio
"""
symbol='pcln'
def get_price2book( symbol ):
    try:
        url = r'http://finviz.com/quote.ashx?t={}'.format(symbol.lower())
        html = u.request.urlopen(url).read()
        soup = bs(html, 'lxml')
        # Change the text below to get a diff metric
        revenue =  soup.find(text = r'Sales')
        revenue_ = revenue.find_next(class_='snapshot-td2').text
        if 'B' in revenue_:
            revenue_ = float(revenue_.replace('B',''))*1000
        elif 'M' in revenue_:
            revenue_ = float(revenue_.replace('M','')) 
        employees =  soup.find(text = r'Employees')
        employees_ = int(employees.find_next(class_='snapshot-td2').text)
        print( '{} revenue per employee = {}'.format(symbol, revenue_/employees_) )
        return revenue_/employees_
    except Exception as e:
        print(e)

"""
Construct a pandas series whose index is the list/array
of stock symbols of interest.

Run a loop assigning the function output to the series
"""
stock_list = ['BABA','FB','TWTR','CMG','BWLD','CAKE','EBAY']
p2b_series = pd.Series( index=stock_list )

for sym in stock_list:
	p2b_series[sym] = get_price2book(sym)

