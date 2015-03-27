This documents comments the structure of Machine Slave's 
GitHub folder and file structure. It should explain what bits of code
need to be run in what order to reproduce our results. 

#### Folders ####

1. Bibliography - Contains material used for inspiration and research on
the models used.

2. Report - Contains files that are used in the generation of the report
These include graphs, rmd files, word files and spreadsheets.

3. Final_classifier - Contains the file and data that is necessary to 
reproduce our best classifier.

4. Data - Contains original and scaled data and code necessary to achieve 
this.

5. Code - Contains all the code we have written for the competition, this
is the primary location that was being updated as we were developing our
models. It is divided by the folders relating to the classifiers we have
used in the competition. 

##### Instructions to reproduce the results #####

1. Download the Final_classifier folder.
2. Go to Final_classifier/code/ .
2. Set the folder Final_classifier/code/ folder as your working directory.
3. Run Optimal_kNN.R

The code is expected to run in an 8 core machine.

#### Instructions to get results from each model ####

### SVM ###

- The majority of model comparison and selection is done in `svmAws1.R`
- `svmAws.R` and multiSVM.R are earlier versions of `svmAws1.R`
- `allData.R` has instructions to run the best SVM model and compute 
relevant predictions on the entire training set
- `funStuffSVM.R` has code relating to graphs about the SVM performance

### Random Forest classifier ###

- The systematic optimization of parameters was done in `RFValidation_AllFeatures_Systematic.R`
- The performance of preprocessing was evaluated in `RFValidation_PCA_AllFeatures.R`, `RFValidation_PCARescaled_AllFeatures.R`, and `RFValidation_SVD.R`.
- The rest of the files contain other validation and prediction trials.

### k-NN classifier ###

- 