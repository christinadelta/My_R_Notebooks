# My R Notebooks

A personal repo with notebook tutorials about anything and everything that is R related. 

For simplicity, in all the notebooks I will be using the same dataset (or datasets). 

Briefly, the main dataset  that I will use comes from a simple Reaction Times (RTs) task, were 24 participants were asked to report whether the stimulus presented was **living** or **non-living** by a key press.

### Some useful information about this dataset:
* Particpants: 24 (12 males, 12 females)
*  Stimuli: 48 total 
*  Classes: 2 (living vs non-living)
*  Categories: 4 (scenes, obejcts, people, animals)
*  Total trials: 960
*  Total blocks: 8
*  Numebr of repetitions per stimulus: 20
*  Number of trials per block: 120

## ANOVA contrasts 

At least one notebook tutorial that will explain with examples what contrasts are in factorial analyses and why they are important. 

The folder of the Anova notebooks contains:

* One .Rmd file
* One .md file
* One .html file
* One .R script

Note that all the above contain the same information: they explain the various contrasts  in linear models and run analyses using different ANOVA functions 

A .csv file with the dataset is also included in the folder. 

Before running the script or the Rmd file, change the line below to your own path:

```data = read.csv("~/Desktop/rt_data/allcategoryrts.csv", header=FALSE, sep=";")```

#### Note: the analyses and explanations in this folder are not yet complete. 

## Hypothesis testing 

A brief explanation of what is hypothesis testing is provided in an .md file. 

### Hypothesis testing with permutations 
* Independent T-tests with permutation using the RTs data
* Paired T-tests with permutation using the RTs data

## Bayesian statistics with R and the stan package 

## Visualisation in R

My Favourite Libraries:

* ggplot
* plotly 
* and more
