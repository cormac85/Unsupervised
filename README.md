# Unsupervised
Learning how to do some unsupervised learning based on various tutorials.

## Resources
Mainly based on [this RPubs tutorial](https://rpubs.com/williamsurles/310847).

I applied what I'd learned to the [Wine dataset](https://rdrr.io/cran/rattle.data/man/wine.html) in gmm_wine.R.

## Conclusions
It seems what works best in each case is heavily dependent on the character of the data, much like supervised learning. K-means clustering in particular has some very strong assumptions but often performs very well in spite of this.

Also, using PCA prior to clustering did not yield better performance for the datasets I had, though there might be other reasons to use PCA such as dimensionality reduction in more complex datasets.
