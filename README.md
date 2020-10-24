# Coronavirus in the São Paulo State: Cases, Deaths and Population

This project explored the still-growing cases of Covid-19 in Brazil. It allowed us to visualise the estimate how coronavirus cases are distributed according to the population of each city. While we are at the verge of municipal elections, these maps allowed us to have a sense of which cities were dealing the best with the Covids-19 pandemic.

[The complete data analysis is displayed here](https://github.com/pedroafleite/coronavirus/blob/main/coronavirus.md).

In this data analysis, we predicted that the city of Barretos was among the cities that were having the most number of cases and deaths of Covid-19. Later on, the State government disclaimed that the city was to enter a new curfew (https://bit.ly/3dV1vuV).

We used the coronavirus dataset provided by the Brazilian Ministry of Health to apply concepts of Spatial Data Analysis.

A few steps were required to be made prior to the data analysis and are not explicitly shown here:
- Data mining: Finding the pathways for the datasets on government sites.
- Data cleaning: most of which was made using Excel to assure that both coronavirus dataset and the census dataset had the same typography, especially in what regards the cities names.

Data focussed on the São Paulo State, but I also used the city of São José dos Campos to test the `geobr`package.

Unfortunately, the Ministry of Health gave up on disclosing Covid-19 data by neightborhoods, which hindered the details of fine-grained spatial analysis by neightborhoods.
