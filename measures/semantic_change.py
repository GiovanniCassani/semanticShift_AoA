import itertools
import numpy as np
from scipy.spatial.distance import cosine

avg_m1 = 0
avg_m2 = 0
initialized_avgs = False


def initialize_avgs(m1, m2):
    global avg_m1, avg_m2, initialized_avgs
    avg_m1 = np.average(m1[m1.wv.vocab], axis=0)
    avg_m2 = np.average(m2[m2.wv.vocab], axis=0)
    initialized_avgs = True


def get_neighbors_set(word, slice_year, topn):
    return set([k[0] for k in slice_year.wv.most_similar(word, topn=topn)])


def c_measure(word, slices):
    word_vectors = [slice[word] for slice in slices]

    combs = itertools.combinations(word_vectors, 2)

    collect_c = []
    for a, b in combs:
        collect_c.append(1 - cosine(a, b))
    return collect_c


def lncs2_setted(word, m1, m2, topn):
    """
    https://www.aclweb.org/anthology/D16-1229/
    :param word:
    :param m1:
    :param m2:
    :param topn:
    :return:
    """

    global avg_m1, avg_m2, initialized_avgs

    words_m1 = list(get_neighbors_set(word, m1, topn))
    words_m2 = list(get_neighbors_set(word, m2, topn))

    all_words = set(words_m1 + words_m2)

    vec_1 = []
    vec_2 = []
    mean = False or initialized_avgs

    for inner_word in all_words:
        if inner_word in m1.wv.vocab:
            vec_1.append(1 - cosine(m1.wv[word], m1.wv[inner_word]))
        else:
            if not mean:
                avg_m1 = np.average(m1[m1.wv.vocab], axis=0)
                mean = True
            vec_1.append(1 - cosine(m1.wv[word], avg_m1))

    mean = False or initialized_avgs

    for inner_word in all_words:
        if inner_word in m2.wv.vocab:
            vec_2.append(1 - cosine(m2.wv[word], m2.wv[inner_word]))
        else:
            if not mean:
                avg_m2 = np.average(m2[m2.wv.vocab], axis=0)
                mean = True
            vec_2.append(1 - cosine(m2.wv[word], avg_m2))

    return 1 - cosine(vec_1, vec_2)

