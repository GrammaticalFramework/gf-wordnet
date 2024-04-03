from setuptools import setup

def read(fname):
    import os
    return open(os.path.join(os.path.dirname(__file__), fname),encoding="utf8").read()

setup(
    name = "gf-wordnet",
    version = "0.0.17",
    author = "Krasimir Angelov",
    author_email = "kr.angelov@gmail.com",
    description = ("An API to the GF WordNet."),
    license = "BSD",
    keywords = "GF WordNet",
    url = "https://github.com/GrammaticalFramework/gf-wordnet",
    packages=['wordnet'],
    install_requires=['pgf-majestic','daison'],
    long_description=read("README.md"),
    long_description_content_type="text/markdown",
    zip_safe=False
)
