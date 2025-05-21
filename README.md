# Predicting the plant microbiome based on its phylogeny

This repository gives access to an implementation of the data science procedure
presented in the paper submission “Predicting the plant microbiome based on its phylogeny”, 
as well as all data and code that was used for the application study.

This repository is structured as follows:

``` 
    ├── code/                    # All implementation and analysis scripts                       
    ├── data/                    # Location where all input, output and meta data for the application study is stored
    ├── results/                 # Location where all results of the model benchmark and detailed knn results are stored
    ├── figures/                 # Location where all figures for the manuscript are stored
    ├── LICENSE
    └── README.md               
```



## Reproduce Results

Steps to reproduce the results of the manuscript:

1.  Install and load all required packages by running `code/required_packages.R` and run script `code/helper_functions.R`.
  
2.  Preprocessing of data: Run script `code/data_preparation.R`

3.  Model benchmark including tuning via nested cross-valation: Run script `code/benchmark.R`

4.  To reproduce figures and tables of Section for of the manuscript, run the scripts `code/analysis_sec_4_1_statistics.R`, `code/analysis_sec_4_2_comp.R`, and `code/analysis_sec_4_3_knn.R`. Figures produced within the script are stored in `figures/`.


