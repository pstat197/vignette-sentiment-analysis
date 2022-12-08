Vignette on Sentiment Analysis and its accuracy when applied to IMBD review data; created as a class project for PSTAT 197A in Fall 2022.

Contributors:
Shannon Rumsey
Piero Trujillo
Raymond Lee
Vardan Martirosyan
Yutong Wang


Abstract:
This vignette covers topics of sentiment analysis, a way of classifying words with specific emotional value.  The document utilizes four major lexicon libraries (NRC, Loughran, AFINN, and Bing) and their different sentiment labels. The example data we will be using is the IMDB dataset found on Kaggle https://www.kaggle.com/datasets/lakshmi25npathi/imdb-dataset-of-50k-movie-reviews. Using Logistic Regression, the goal is to see how accurate the four lexicon libraries are at labeling and predicting the true sentiment values of these reviews. The purpose of this vignette as a whole is to further explore the world of Natural Language Processing and its implications.


Repository Contents:
root directory
|-- data
    |-- IMDB_raw.csv
    |-- IMDB-clean.csv
|-- scripts
    |-- drafts
    |-- vignette-script.R
|-- vignette.qmd
|-- vignette.html
|-- README.md
|-- LICENSE
|-- vignette_files
|-- vignette-sentiment-analysis.Rproj


Reference List:
https://www.kaggle.com/datasets/lakshmi25npathi/imdb-dataset-of-50k-movie-reviews
https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
https://www.tidytextmining.com/tidytext.html (Chapter 2)

AFINN Dataset:  Finn Ärup Nielsen (2011), “A new ANEW: Evaluation of a word list for sentiment analysis in microblogs”, Proceedings of the ESWC2011 Workshop on 'Making Sense of Microposts': Big things come in small packages (2011) 93-98.

https://github.com/fnielsen/afinn


Bing Dataset: Minqing Hu and Bing Liu, “Mining and summarizing customer reviews.”, Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery & Data Mining (KDD-2004), 2004.

https://emilhvitfeldt.github.io/textdata/reference/lexicon_bing.html

NRC Dataset: Saif Mohammad and Peter Turney. (2013), “Crowdsourcing a Word-Emotion Association Lexicon.” Computational Intelligence, 29(3): 436-465.

http://saifmohammad.com/WebPages/lexicons.html

Loughran Dataset: Loughran, T. and McDonald, B. (2011), “When Is a Liability Not a Liability? Textual Analysis, Dictionaries, and 10-Ks.” The Journal of Finance, 66: 35-65.

https://emilhvitfeldt.github.io/textdata/reference/lexicon_loughran.html


Instructions: 
For navigation of main report and findings, see vignette.html. For source code, see scripts/vignette-script.R.