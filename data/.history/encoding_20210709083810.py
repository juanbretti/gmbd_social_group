# %%
!pip install chardet
# %%
import chardet

# look at the first ten thousand bytes to guess the character encoding
with open("data_by_artist.csv", 'rb') as rawdata:
    result = chardet.detect(rawdata.read(10000))

# check what the character encoding might be
print(result)

#%%