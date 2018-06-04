# TradeShiny
ShinyDashboard for Global Commodity Trade Statistics
----------------
### 
The Global Commodity data from the Kaggle contain over a million rows of export/import information from hundreds of countries around the world. Our ShinyDashboard mainly deals with different visualization techniques and how we can use them to explore meaningful insights from the data. 

## Content
This dataset contains three files:
1. tradeSub.rds
   * Year 2014-2016 (subset of kaggle orginal version)
2. country.csv
3. group.csv

All monetary values are in terms of millions of US dollars.

## Motivation

1. *What is the economical trend of inflows and outflows for various countries?*
2. *How does the export and import trend varies with time?*
3. *What countries send to most commodities abroad?*
4. *Try to compare Terms of Trade ratio [Export/Import] for different countries?*

## Run the Dashboard;

'load shinydashboard' directly from github ,

    library(shiny)
    runGitHub("TradeShiny", "ramamet")

-------------------
## Dashboard1
![p1](https://user-images.githubusercontent.com/16385390/40892882-e4e5f164-679c-11e8-9abe-e72475d6de95.png)

## Dashboard2
![p2](https://user-images.githubusercontent.com/16385390/40892896-3bd4c81a-679d-11e8-95e3-efb90b939988.png)

![p3](https://user-images.githubusercontent.com/16385390/40892907-6ff29e7e-679d-11e8-98ac-0831e7d3c8ec.png)

## Dashboard3
![p4](https://user-images.githubusercontent.com/16385390/40892912-8dd62cd0-679d-11e8-8f21-81c5da4d11e1.png)

## DataTable
![p5](https://user-images.githubusercontent.com/16385390/40892932-cc4ca728-679d-11e8-9788-137e45f4f328.png)
