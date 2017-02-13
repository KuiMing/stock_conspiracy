import pandas as pd
x=pd.read_json('tmp.json')
overbought=x.iloc[0:10,range(1,15,2)]
oversold=x.iloc[0:10,range(2,15,2)]
overbought.to_excel('overbought.xls')
oversold.to_excel('oversold.xls')