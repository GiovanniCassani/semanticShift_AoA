from string import punctuation
import spacy

class SpacyCleaner:
    """
    Loads a simple instance of Spacy Core Web LG to provide data cleaning
    """
    def __init__(self):
        self.nlp = spacy.load("en_core_web_lg", disable=["parser", "ner"])
        self.nlp.max_length = 23621305

    def clean(self, text):
        text = " ".join(text.split())

        doc = self.nlp(text)

        tokens = [(token.lemma_) for token in doc if
                  not token.is_stop
                  and not self.nlp.vocab[token.lemma_].is_stop
                  and not token.is_punct
                  and not token.is_digit
                  ]
        text = " ".join(tokens)

        return text.lower()
