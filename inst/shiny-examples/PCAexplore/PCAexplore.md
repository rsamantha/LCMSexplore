### Exploring the data matrix with Principal Component Analysis (PCA)

This demo allows you to explore the RUBUS dataset (available from Metabolights with ID: MTBLS333). The dataset contains the untargeted measure of 13 red rubus samples and 12 yellow rubus samples. The data are log10 transformed and scaled to unit variance before computing the PCA. The directions in a PCA are chosen in order to maximize the dataset variance (thus it is useful for identifying dominant sources of variability in the dataset such as outliers, analytical trends, batch effects, etc). 

Some question to orient your discussion ...

* What is the meaning of the plots? 
* Does the percentage of missing value affect the final output?
* Does the imputing strategy affect the final output?
* Which is the best strategy for handling missing values?
* With regards to the experimental design, what can we learn from the PCA? 
