Capstone_presentation
========================================================
author: Willem Hogewoning
date: 16-5-2021
autosize: true

Project Overview
========================================================

This project is the Capstone project of the course Data Science.
The challenge is to predict the next word in a typed sentence.

I am not going to interpret the whole sentence, only look at the last five words.
I compare these five words with occurrence in datasets and lookup the next following word.
So the highest used frequencies of phrases will be shown as predictions.

This type of predictions is often used in keyboards on mobile devices, or in an autofill in on online forms.


Project steps
========================================================

* Use a dataset, reflecting the use (e.g. used in online typing).
  For this we were presented 3 datasets of sentences originating from Twitter, blogs and news.
  Made available by Swiftkey; https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
  (available in multiple languages, here only english was used)
* The next steps are cleaning up the sentences with punctuation, URLs, numbers, capital letters, ect.
  These characters have no predictive value.
* Then I create NGRAM models, filtering out most used phrases of word combination from 1 to 5 words.
  (source: https://en.wikipedia.org/wiki/N-gram.
* With the NGRAM datasets I (and some help from google) was able to create a functions for predictions of the next word.
* Finally the result was put into a Shinny app for the show case. (https://www.shinyapps.io/ by Rstudio).


Limitations
========================================================

During the project I struggeld with quite some limitations.

* Limited computer power to process the datasets.
  First my model used a matrix which turns into a too big of a memory use.
  Later the Ngram model needed too much time to process all.
  This resulted into only using a small percentage (5%) sample size of the dataset. Which I preferred to be much bigger.
  
* I also struggled with some R packages, not working together, not existing anymore, and other problems.
  Next to that also the shinnyapps websites have limitations, not be able to all the packages needed.

Result
========================================================

I was able to get a result; however not the quickest or most perfect one.
But it is a good proof of perfect.

* The app is available at: https://willemhogewoning.shinyapps.io/Capstone/

* The data repository is at: https://github.com/WillemHogewoning/Capstone

* The presentation is at: https://rpubs.com/WillemHogewoning/Capstone
