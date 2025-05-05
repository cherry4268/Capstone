# Capstone - Cost of Living Predictor (U.S.)
DATS 4001 Capstone - Cherry Kuruppacherry

## Overview
This project develops a predictive model for individual cost of living by analyzing key expense categories including housing, transportation, grocery, and a few insurance costs. The analysis uses multi-variable techniques to understand how different factors contribute to living expenses across different regions in the United States.

## Project Objectives
- Develop predictive models for cost of living expenses - based on years 2015-2022
- Analyze regional and geographic trends in living costs
- Create Benchmarks for location-based decision-making
- Heatmap interactive display

## Data Sources
### Cost of Living Indicator (real Personal Consumption Expenditure per Capita)
- Bureau of Economic Analysis; real Personal Consumption Expenditure per Capita by state
   - https://www.bea.gov/news/2024/real-personal-consumption-expenditures-state-and-real-personal-income-state-and

### Housing
- Zillow Home Value Index (ZHVI); measures typical home values and market changes, covers 35th-65th percentile range, seasonally adjusted, measured by state
   - https://www.zillow.com/research/data/

### Transportation
- National Association of Insurance Commissioners; "Auto Insurance Database Report" both for 2017/2018 and 2022 to cover years 2015-2022, measures combined average premium
   - https://content.naic.org/article/news-release-naic-releases-2017-2018-auto-insurance-database-report
   - https://content.naic.org/article/naic-releases-latest-auto-insurance-database-average-premium-supplement
- Bureau of Labor Statistics - Gasoline by census region (Midwest, Northeast, South, West)
   - https://data.bls.gov/dataViewer/view/timeseries/CXU470111LB1103M
   - https://data.bls.gov/dataViewer/view/timeseries/CXU470111LB1102M
   - https://data.bls.gov/dataViewer/view/timeseries/CXU470111LB1104M
   - https://data.bls.gov/dataViewer/view/timeseries/CXU470111LB1105M

### Grocery
- Map the Meal; includes average cost of a meal per state,  Weighted weekly $ needed by FI, and  Weighted Annual Food Budget Shortfall
   - https://map.feedingamerica.org/

### Utilities
- US Energy Information Administration State Energy Data System (SEDS)
   - https://www.eia.gov/state/seds/seds-data-complete.php?sid=US

## Methodology
### Data Collection & Preparation

   -  Data aggregation from multiple sources
   -  Dataset standardization and normalization
   -   Comprehensive dataset creation


### Exploratory Data Analysis

 -    Expense distribution visualization
 -    Variable correlation analysis
 -    Initial expense categorization


### Feature Engineering

-   Expense ratio creation
-  Significant variable identification


### Predictive Modeling

 -    Machine learning model development
 -    Regression-based cost prediction
 -    Cross-validation implementation


### Geospatial Analysis

-  Regional cost variation mapping
 -    Demographic impact analysis
- Interactive visualization creation

## Timeline
- Data Collection & Automation (3 weeks)
- Feature Engineering & Selection (3 weeks)
- Predictive Modeling (4 weeks)
- Geospatial Analysis & Visualization (2 weeks)
- Results Compilation (1 week)
- Report Writing (1 week)
- Final Presentation (1 week)


