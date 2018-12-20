# Data and analysis for the paper "Morality in the Time of Cognitive Famine
... by Panos Mitkidis, Jonas Kristoffer Lindeløv, and others.

The folder `experiment` contains the files used to run the experiment using PsychoPy 1.83.00. This should run well on newer PsychoPy2 versions, but could fail on PsychoPy3.


The folder `analysis` contains two markdown files. One for the Public Goods Game](https://github.com/lindeloev/morality/blob/master/analysis/morality_notebook_pgg.md (experiment 1 and 2) and [one for the Dots task](https://github.com/lindeloev/morality/blob/master/analysis/morality_notebook_dots.md) (experiment 3 and 4). Do do the analysis from the raw data files, do this:

1. Unzip the zip file with the raw csvs
2. Run the python scripts in the `misc` folder to merge them into the `pgg_all.csv` and `dots_all.csv`. This is needed because there were some formatting errors and changes at different stages in the data collection.
3. Run the preprocessing on these which generate `pgg.Rda` and `dots.Rda`. These R-objects are needed to handle the matrix columns.
4. Run the notebooks!
