# Predicting Fire Incident Impact in Toronto: Financial Losses and Casualties

## Overview

This repo analyzed fire incident data from Toronto to explore relationships between fire occurrences and various independent variables. The research demonstrated that these variables have significant predictive power in assessing the likelihood and impact of fire incidents, including potential financial damage and casualties. Key factors identified as predictive of fire incidents include the condition of buildings, the impact on adjacent businesses, the functionality of fire alarm systems, methods of fire control, and the presence of smoke alarms at the incident's origin. Additionally, the study highlighted the importance of response times, noting that both the time it takes to control a fire after arrival and the time elapsed from the alarm to arrival are crucial predictors of fire outcomes. The findings enhance the current understanding of fire dynamics and have substantial implications for public safety and preventive strategies. 


## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data and the simulated data as obtained from Alberta government.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `models` contains three fitted models. 
-   `others` contains details about LLM chat interactions and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper.
-   `scripts` contains the R scripts used to simulate, download, clean and test data. It also contains the R code used to generate models.


## Statement on LLM usage

Aspects of the code were written with the help of the ChatGPT, and the entire chat history is available in others/llm/llm.txt. The abstract and introduction were not written with any help of chat tools or auto-complete tools. Grammarly was used to check grammar. 