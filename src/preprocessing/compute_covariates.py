import pandas as pd
from old20 import old20
from read_resources import read_subtlex, read_target_words

SUBTLEX_PATH = "../../Resources/SUBTLEX/SUBTLEX-US.txt" # modify to the correct path
TARGETS_PATH = "../../data/targetWords.json"

target_words = read_target_words(TARGETS_PATH)
subtlex_words = read_subtlex(SUBTLEX_PATH)

old20_subtlex = old20(target_words, subtlex_words)

data = []
for (w, old) in zip(target_words, old20_subtlex):
    data.append([w, old, len(w)])

df = pd.DataFrame(data, columns=['Word', 'OLD20', 'len'])

df.to_csv("../../data/word_OLD_len.csv",
          index=False, sep='\t')

