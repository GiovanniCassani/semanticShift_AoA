===================================
Semantic Shift & Age of Acquisition
===================================

This repository contains code detailing the analyses performed for the CogSci paper 'Words with consistent diachronic usage patterns are learned earlier. A computational analysis using temporally aligned word embeddings'.


Content
=======

The project code has been separated in three different folders:
All the necessary datasets are provided in the folder **data/**, whereas the folder **src/** contains Python code to read relevant resources to compute OLD20 for the target words as well as R code to preprocess raw data to generate input files for the statistical analyses, run linear models and generate the plots included in the paper.
Finally, the code to compute the semantic change measures is available under the **measures** folder.

Temporal Embeddings with A Compass
==================================

To create the aligned embeddings, it is necessary to obtain the `CoHA`_ corpus. Then, the  `TWEC`_ embedding alignment algorithm can be used to aling
the slices. It is enough to split the COHA data in 5 sets: 1800-1840, 1840-1880, 1880-1920, 1920-1960,
1960-2000. You should manually pre-process the text before using TWEC (we used spacy to do this).

Follow the instruction on the `TWEC`_ to install the tool.

Requirements
============

+ pandas
+ scipy
+ numpy
+ cade (refer to the `TWEC`_ page)
+ old20 (https://github.com/stephantul/old20)
+ SUBTLEX-US (https://www.ugent.be/pp/experimentele-psychologie/en/research/documents/subtlexus/subtlexus2.zip)
+ Age of Acquisition norms by Kuperman et al (http://crr.ugent.be/archives/806)
+ concreteness norms by Brysbaert et al (http://crr.ugent.be/papers/Concreteness_ratings_Brysbaert_et_al_BRM.txt)


Authors
=======

+ Giovanni Cassani, Tilburg University
+ Federico Bianchi, Bocconi University
+ Marco Marelli, University of Milano-Bicocca

.. _CoHA: https://www.corpusdata.org/coha_full_text.asp
.. _TWEC: https://github.com/vinid/cade
