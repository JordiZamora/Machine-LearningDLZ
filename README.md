ReadMe <- this documents comments the structure of Machine Slave's 
GitHub folder and file structure. It should explain what bits of code
need to be run in what order to reproduce our results. 

######### Folders ######

1. Bibliography - contains material used for inspiration and research on
the models used

2. Report - contains files that are used in the generation of the report
These include graphs, rmd files, word files and spreadsheets

3. Final_classifier - contains the file and data that is necessary to 
reproduce our best classifier

4. Data -contains original and scaled data and code necessary to achieve 
this 

5. Code - contains all the code we have written for the competition, this
is the primary location that was being updated as we were developing our
models. It is divided by the folders relating to the classifiers we have
used in the competition. 

######### Instructions to reproduce results of each model #########

######## SVM ############

- the majority of model comparison and selection is done in svmAws1.R
- svmAws.R and multiSVM.R are earlier versions of svmAws1.R
- allData.R has instructions to run the best SVM model and compute 
relevant predictions on the entire training set
- funStuffSVM.R has code relating to graphs about the SVM performance

###### Random Forest classifier #######

######## k-NN classifier #########