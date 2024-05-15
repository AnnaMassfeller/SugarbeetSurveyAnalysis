This repository contains the analysis for the paper "Field observation and verbal exchange as different types of peer effects in farmers' technology adoption decisions".
In the folder "Rfiles", all necessary files are collected to run the analysis. 
The raw data can not be made publicly available as it contains geo-locations of individual farmers' fields.
In the pre-processing file this raw data is processed such that the geo-locations are converted into spatial distances. 
Further postal codes and any other geo-specific information are dummy-coded leading to the data set of "SampleIV" that is then used for the further analysis and is publicly available.
The order of files is as follows:

1. Processing_new: the raw data is pre-processed as explained above, including data cleaning and variable creation, own API needed
2. zensus_api: zensus data from german statistics (DESTATIS) is retreived, own credentials needed
3. read_processed_data: fiel to read and combine all pre-processed data
4. Analysis_clean: analysis of the data including IV approach
5. RQ3_clean: Analysis of research question 3
6. Lasso_dooubleselection_applied: analysis via LASSO double selection
7. Loop_efficient: Loop to run LASSO with different specifications for model sensitivity analysis and comparison
8. Graphs: file to create plots for publication
9. Maps_new: file to create maps based on collected data

the processed data (SampleIV) can be downloaded.
