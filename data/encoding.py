# %%
# !pip install chardet
# %%
import chardet

# look at the first ten thousand bytes to guess the character encoding
with open("test.csv", 'rb') as rawdata:
    result = chardet.detect(rawdata.read(10000))

# check what the character encoding might be
print(result)

#%%

import pandas as pd

df = pd.read_csv('data_by_artist.csv', encoding='ISO-8859-1', encoding_errors='replace')
# %%
