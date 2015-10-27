## KNN-Classification 

implementation of general knn-classification with euclidean and tangent distance metric.

  * Tangent distance was originally implemented in C and authored by Daniel Keysers (This programe is free software) and to R by Volodya Vovk

Main Knn function takes five arguments

  - Train object
  - test object
  - label column number
  - k - nearest neighbours (Default 1)
  - distance metric (euclidean or tangent) (Defualt "euclidean")
  
and returns all the predicted classification. 