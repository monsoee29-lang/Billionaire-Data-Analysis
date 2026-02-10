# 💰 Billionaire Data Analysis & Interactive Dashboard
Developed by: Ei Mon Soe

Major: Statistics and Data Science (Parami University)

Course: Data Communication and Ethics

📌 Project Overview
This project provides a comprehensive Exploratory Data Analysis (EDA) and an interactive web dashboard focused on the 2023 Billionaires Statistics Dataset. Beyond just listing the wealthiest individuals, this project explores the intersection of industry, geography, and gender to understand the modern landscape of global wealth.

It is designed as an ideal reference for students looking to practice data cleaning (handling missing values), descriptive statistics, and building interactive dashboards using R Shiny.

📊 Data Science Workflow (R-Based)
1. Data Preprocessing & Integrity
Real-world datasets require careful cleaning before analysis. I implemented a robust preprocessing pipeline:

Dimensionality Reduction: Dropped high-null columns (organization, title, state, residenceStateRegion) that could bias the analysis.

Numeric Imputation: Implemented a for loop to automatically detect numeric columns and fill missing values with the mean, ensuring statistical consistency.

Categorical Cleaning: Replaced missing "City" values with the mode (New York) and handled character-based NAs to maintain a clean dataset for visualization.

2. Analytical Deep Dives
The project answers 10 core questions, including:

Gender Imbalance: A critical look at the stark gap between male and female billionaires (only 337 females out of 2,640).

Wealth Origins: Comparing Self-Made vs. Inherited wealth, revealing that 1,812 billionaires are self-made.

Age Demographics: Analyzing how average net worth correlates with age groups, from "Young Adults" to "Elderly."

🌐 Interactive Dashboard Features
The R Shiny dashboard transforms the static analysis into a dynamic tool using bslib for styling and plotly for interactivity:

Global Filtering: Users can filter the entire dashboard by country to see localized insights.

Dynamic Ranking: Adjustable sliders to view the Top 5, 10, or 15 industries or countries.

Variable Coloring: A unique feature allowing users to change the "fill color" of charts based on different categories (Gender, Self-Made status, Industry) to spot visual patterns instantly.

Interactive Tooltips: Hovering over bars provides precise net worth and count data.

🚀 Tech Stack
Language: R

Libraries: * tidyverse (dplyr, ggplot2, readr) — Data manipulation & static plotting.

shiny & bslib — Web framework and UI styling.

plotly — Converting static charts into interactive visuals.

Deployment: GitHub & Shinyapps.io

📊 Dataset Source
The data is sourced from the Billionaires Statistics Dataset (2023) via Kaggle.

Origin: Forbes Billionaires List.

Scope: 2,640 individuals globally.

Key Indicators: Final Worth, Category, Country, Age, Gender, and Self-made status.
