if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("randomForest")) install.packages("randomForest")
setwd("~/Documents/Cursos/DataScience/2ndTerm/HomeworkMachineLearning/Project/code/RandomForest")

#source("RF_XValidation.R")
source("RF_XValidationBuckets.R")


#Load data
rawdata <- read.csv("../../data/Kaggle_Covertype_training.csv")
numcol <- ncol(rawdata)
numrow <- nrow(rawdata)
rawdata[,numcol] <- factor(rawdata[,numcol]) #Classes as factors

features <- rawdata[,-c(1,numcol)]
labels <- factor(rawdata[,numcol])
# labels <- rawdata[,numcol]
# labels[labels==5] <- 2
# labels[labels==4] <- 3
# labels <- factor(labels)

mtry <- 4
ntree <- 10
RFout <- randomForest(features,
                      labels,
                      mtry=mtry,
                      ntree=ntree)

RFoutImportanceSorted <- RFout$importance[order(RFout$importance[,1]),]
names(RFoutImportanceSorted[RFoutImportanceSorted>100])

###Importance
soil_type_15  0.000000e+00
soil_type_7   7.731201e-03
soil_type_25  9.803830e-01
soil_type_8   1.183877e+00
soil_type_36  1.336591e+00
soil_type_28  3.552696e+00
soil_type_27  4.092074e+00
soil_type_5   5.315286e+00
soil_type_18  5.579560e+00
soil_type_9   6.581805e+00
soil_type_14  8.264457e+00
soil_type_19  1.100779e+01
soil_type_37  1.158568e+01
soil_type_16  1.255876e+01
soil_type_34  1.340954e+01
soil_type_26  1.550652e+01
soil_type_20  1.839450e+01
soil_type_21  1.873903e+01
soil_type_1   2.155436e+01
soil_type_3   2.539205e+01
soil_type_6   3.253234e+01
soil_type_17  3.409454e+01
soil_type_35  3.555092e+01
soil_type_31  4.249350e+01
soil_type_11  4.715030e+01
soil_type_29  5.106942e+01
soil_type_24  5.643894e+01
soil_type_13  5.696920e+01
soil_type_33  6.211290e+01
soil_type_32  6.745354e+01
soil_type_30  7.283344e+01
soil_type_40  1.604961e+02
wild_area_2   1.687201e+02
soil_type_38  2.170311e+02
soil_type_12  2.262627e+02
soil_type_39  2.277536e+02
hill_9am      2.286461e+02
soil_type_2   2.319443e+02
hill_3pm      2.440985e+02
slope         2.597320e+02
soil_type_23  2.714494e+02
hill_noon     2.731609e+02
ver_dist_hyd  2.777735e+02
aspect        2.865364e+02
wild_area_1   2.922268e+02
hor_dist_hyd  2.930544e+02
soil_type_10  3.406635e+02
soil_type_22  3.536402e+02
soil_type_4   3.680527e+02
wild_area_3   3.993113e+02
wild_area_4   6.432594e+02
hor_dist_road 6.436267e+02
hor_dist_fire 6.748079e+02
elevation     3.355948e+03


mtry <- 20
ntree <- 10
RFout <- randomForest(features,
                      labels,
                      mtry=mtry,
                      ntree=ntree)

t(t(RFout$importance[order(RFout$importance[,1]),]))
#Importance
soil_type_7      0.0000000
soil_type_15     0.0000000
soil_type_36     0.1948718
soil_type_8      0.2637846
soil_type_18     0.3323880
soil_type_25     1.1711680
soil_type_9      3.4130080
soil_type_28     4.0126795
soil_type_26     4.3055012
soil_type_14     5.7808379
soil_type_34     7.7935629
soil_type_5      8.1086268
soil_type_37     8.8898891
soil_type_1     10.5668581
soil_type_27    17.6153368
soil_type_6     19.8867016
soil_type_21    23.3840180
soil_type_16    28.3258129
soil_type_35    35.2066984
soil_type_19    38.9614928
soil_type_17    55.1862929
soil_type_3     65.0533757
soil_type_20    65.4143511
soil_type_30    67.9144206
soil_type_40    75.9120924
soil_type_11    77.5684982
soil_type_13    80.0615998
soil_type_24   122.1002121
soil_type_31   124.4021434
soil_type_12   133.2221332
soil_type_33   133.3338101
wild_area_2    145.4683783
soil_type_10   145.7373256
soil_type_29   174.6384040
soil_type_32   207.4293446
soil_type_39   215.3741479
soil_type_38   226.7735386
soil_type_2    267.7950873
wild_area_1    288.3310086
soil_type_4    305.1703591
soil_type_23   351.1552309
wild_area_3    450.3714964
soil_type_22   470.0983291
slope         1023.8758937
hill_9am      1259.4711780
hill_3pm      1307.7893753
wild_area_4   1313.0465575
aspect        1424.0652214
hill_noon     1451.5458569
ver_dist_hyd  1639.5974199
hor_dist_hyd  1893.1404811
hor_dist_road 3303.8042603
hor_dist_fire 3347.3853716
elevation     8450.1191588

mtry <- 20
ntree <- 100
RFout <- randomForest(features,
                      labels,
                      mtry=mtry,
                      ntree=ntree)
# Call:
#   randomForest(x = features, y = labels, ntree = ntree, mtry = mtry) 
# Type of random forest: classification
# Number of trees: 100
# No. of variables tried at each split: 20
# 
# OOB estimate of  error rate: 10.75%
# Confusion matrix:
#       1     2    3   4   5    6    7 class.error
# 1 15801  2202    1   0  10    2   85  0.12706480
# 2  1476 22880  107   0  44   71   13  0.06957830
# 3     0   144 2728  14   4  136    0  0.09847984
# 4     0     0   51 138   0   11    0  0.31000000
# 5    16   350   17   0 426    1    0  0.47407407
# 6     4   134  241  10   1 1121    0  0.25810721
# 7   214    16    0   0   0    0 1531  0.13060761


t(t(RFout$importance[order(RFout$importance[,1]),]))

#Importance
soil_type_15  0.000000e+00
soil_type_7   3.670833e-02
soil_type_36  8.846257e-01
soil_type_8   9.383480e-01
soil_type_18  2.273337e+00
soil_type_25  2.975640e+00
soil_type_28  3.060113e+00
soil_type_9   3.751077e+00
soil_type_14  4.332483e+00
soil_type_26  6.099979e+00
soil_type_34  9.898994e+00
soil_type_37  1.005797e+01
soil_type_5   1.071352e+01
soil_type_1   1.093251e+01
soil_type_21  1.811024e+01
soil_type_27  1.899382e+01
soil_type_19  3.060659e+01
soil_type_16  3.301089e+01
soil_type_6   3.335996e+01
soil_type_35  4.039225e+01
soil_type_40  5.411411e+01
soil_type_3   5.691844e+01
soil_type_17  5.845752e+01
soil_type_11  5.931334e+01
soil_type_20  6.463367e+01
soil_type_30  7.734214e+01
soil_type_13  7.789583e+01
soil_type_24  1.224735e+02
soil_type_33  1.272294e+02
wild_area_2   1.294879e+02
soil_type_31  1.363725e+02
soil_type_38  1.439388e+02
soil_type_12  1.611382e+02
soil_type_29  1.660939e+02
soil_type_39  1.724958e+02
soil_type_32  2.332813e+02
soil_type_10  2.659475e+02
soil_type_2   2.676036e+02
soil_type_23  3.170532e+02
soil_type_4   3.505125e+02
wild_area_3   3.835729e+02
wild_area_1   3.984888e+02
soil_type_22  4.162870e+02
slope         9.920320e+02
wild_area_4   1.169768e+03
hill_3pm      1.233559e+03
hill_9am      1.303552e+03
hill_noon     1.429335e+03
aspect        1.452522e+03
ver_dist_hyd  1.686282e+03
hor_dist_hyd  1.872187e+03
hor_dist_fire 3.252642e+03
hor_dist_road 3.327504e+03
elevation     8.644933e+03