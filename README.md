# Repository goal
This repository consists of files that were used by Marleen Haubitz, Anastasia Khomenko, Nikilesh Jagan, and Inno Loor for analysis of Unicef data as a part of FEM11153: Seminar Case Studies in Data Science and Marketing Analytics. Master's program DSMA.

This repository is part of the course, the main output of which was a final report. Therefore, this repository cannot be regarded separate from this report, where a lot of choices made for coding are explained from the literatire standpoint.

# Effect of external events and social proximities on donations

## Description

This folder consists of all the files used to generate the results for the Team 4 research of _"Effect of external events and proximities on donations"_. 

To optimize its donor acquisition strategies and ensure efficient resource allocation, our research aims to investigate the return on investment on face-to-face and online fundraising channels for UNICEF. Face-to-face fundraising is quite expensive and the dynamics driving online donations are less understood, particularly how these two channels influence each other. A key focus of our study is to unravel the carry-over effect: quantifying the extent to which door-to-door fundraising efforts contribute to subsequent online donations. 
Our approach involves a detailed analysis of the characteristics and preferences of the donor groups. By profiling these donors, we aim to identify segments which are more responsive to certain types of fundraising. Additionally, we will explore the impact of external events, such as natural disasters or humanitarian crises, on the patterns of donations. Thus, we arrive at our research question.
**How do donor characteristics and external events influence the preference for immediate versus delayed donations in response to face-to-face fundraising efforts?**


## Scripts

The data files used are the following:

1. _external events.py_ ensures that the external events dataset is created. This script expects several rtf files as input. For current research, the a batch of data was downloaded from Lexis Nexis database for 2018-2022 (see unicef events data for reference). _This file requires following packages to run:striprtf, nltk, pandas, asyncio, aiohttp, json, numpy, python-dotenv, statsmodels_. 

2. _constants.py_ The file consists of all constants used for external_events script.

3. _role.py_ This script defines the system message passed to chatGPT model. It is very important to define the prompt properly as this defines the success of the model. Poorly-defined prompt will lead to bad quality data. The best practices are listed [here](https://platform.openai.com/docs/guides/prompt-engineering/strategy-write-clear-instructions).

4. _ETL.R_ This script ensures that all data cleaning and joins are performed. As output, it produces 1 dataset (cleaned_joined_data.csv) that can be used for ML. In principle, any ML model can then be applied to this dataset. On top of it, in order to be able to compare multiple models on exactly the same data, we have performed train/test split in this script. These datasets are also saved in datasets folder.

5. _Data description.R_ This script produces plots and summaries used to describe the obtained data and get more insights into the dataset used for ML.


6. _Linear Regression.R_ This script consists of the implementation, training, and evaluating the Linear Regression model.

7. _Random Forest.R_ This script consists of the implementation, training, hyperparametertuning, evaluating of the Random Forest model.

8. _Posthoc analysis.R_ This script consists of Post-hoc analysis of the Random forest, namely ALE Plots and Shapley values.

9. _days to donate.R_ This script allows to build the histogram of amount of days that passed between pledge & online donations used in the methodology section of report.

10. _shapley.R_ Code used to produce shapley values for the presentation

## Recommendations on continuity

In this section, we will provide advice on how to ensure continuity of this project.

1. **External events**

In order to continue collecting external events, one needs to access a particular news API. We were able to find [news API](https://newsapi.org/). This API provides data from the biggest newspapers across the world, including Dutch ones. On top of it, free version includes access to all the articles up to 30 days ago. This means that assuming monthly updates,this data could be acquired for free. However, paid version removes a lot of limits on amount of calls and allows to extract data from 5 years. The API allows usage of multiple keywords that would allow to filter out relevant data. Once the data source is changed, one can continue using the same pipeline as presented in external_events.py.

2. **ChatGPT**

Undoubtedly, ChatGPT API plays an important role in this project. Therefore, it is important to invest into a developers account and set up the correct api key in the python script. The cost highly depends on how long the input articles are and can be found on [openai website](https://openai.com/pricing). Price of extracting data on 300 articles was below 1.5 USD.

3. **Data Cleaning**

The following cleaning steps are performed (ETL scripts):

1. Remove whitespaces, irrelevant/erroneous entries, aggregate to PC4 (instead of PC6);
2. Remove Postcode where no pledge has occured in the past 5 years, since we believe that these are remote areas;
3. Merge online donations with pledges using a full join. Online donations are a match with a pledge if they occured in the same Postcode and happened within 14 days after a pledge occured;
4. Join external events. They are added if they occured 7 days before pledge date (if present). Or, otherwise, 7 days before online donation date.
5. Join demographics based on Postcode and year.
6. Aggregate the data on PC4, Year, Week level. 

The output dataset of data is in datasets/cleaned_joined_data.csv
We have additionally performed train and test split. These datasets are called full_data_train & full_data_test and are located in the folder datasets.+


### A note on programming languages

**Python** was used to make asynchronous calls to the ChatGpt API as it significantly increases the speed of the data retrieval as instead of making each call one by one, we could retreive up to 500 calls per minute. Therefore, the data extraction for external datasets was setup fully in python. 

However, because **R** was part of the curriculum for our master's programm, students were mostly familiar with this language. Therefore, this language was further applied throughout the research.
