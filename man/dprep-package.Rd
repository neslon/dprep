\name{dprep-package}
\alias{dprep-package}
\alias{dprep}
\docType{package}
\title{
Data Preprocessing for supervised classification
}
\description{
Functions for normalization, treatment of missing values,discretization, outlier detection, feature selection, and visualization}
\details{
Dprep has been developed by Professor Edgar Acuna and his students at the CASTLE research Group of the University of Puerto Rico-Mayaguez.
This is a library of R functions for normalization, handling of missing values, discretization, outlier detection, feature selection, and 
data visualization classification. Most of the methods handle datasets with numerical and categorical attributes.

Normalization methods: Score Normalization, Min-Max Normalization, Decimal scale, Sigmoidal Normalization, 
Softmax normalization.

Missing values methods: Imputation by mean, median and mode (categorical features), K-nn Imputation (categorical and numerical data).

Discretization Methods: Equal width bins, Equal Frequency bins, Holte's One R, chiMerge, Entropy Discretization with MDL stopping rule.

Feature Selection Methods: ReliefF, LVF, Finco, Sequential Forward Selection, Sequential Floating Forward Selection.

Outlier Detection Methods: Mahaout, Robout, Bay's algorthm, LOF.

Crossvalidation estimation error: LDA, Naive Bayes, Logistic, Knn, Rpart, Neural Networks.

}
\author{
Maintainer: Edgar Acuna <edgar.acuna@upr.edu>}
\references{
See website academic.uprm.edu/eacuna/dprep.html
}
\keyword{ package }

