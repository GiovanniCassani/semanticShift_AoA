import json
import pandas as pd


def read_subtlex(path):

    subtlex = pd.read_csv(path, sep='\t', header=0)
    return {w for w in subtlex['Word'] if isinstance(w, str)}


def read_target_words(path):

    return set(json.load(open(path, 'r')))
