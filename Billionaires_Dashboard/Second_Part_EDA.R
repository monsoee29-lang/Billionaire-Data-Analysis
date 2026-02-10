#Load necessary libraries 
library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

BSD <- read_csv('Billionaires Statistics Dataset.csv')

#I will do the same process as midterm EDA when checking missing values and for data cleaning. 

#----checking missing values-------
sum(is.na(BSD))

colSums(is.na(BSD))
#When I checked numbers of column by column which has missing values, there are 4 columns 
#that I should drop because those columns has big portion amount of missing values which can make inaccurate analysis.  
#Those columns are "organization" with 2315 missing values, "title" with 2301 missing values, "state" with 1887 missing values and "residenceStateRegion" with 1893 missing values. 
#Even remained columns has respective missing values, they are very few amount. 

rowSums(is.na(BSD))
#When I check missing values of each row of the data set, there are a lot of missing values, but they are a very small portion of overall. So, I won't drop any rows. 

#remove columns with missing values using minus(-)
BSD <- BSD %>% select(-organization, -title, -state, -residenceStateRegion) 
dim(BSD)
#Now, the data set has 31 columns after dropping 4 columns. 

#----------Replacing missing values-------------
#Loop through all columns in BSD
for (col in names(BSD)) {
  #check numeric columns
  if (is.numeric(BSD[[col]])) { 
    BSD[[col]][is.na(BSD[[col]])] <- mean(BSD[[col]], na.rm = TRUE) #calculate mean ignoring na and replace all NA values
  }
}

#Check any NA is still remained in numeric columns
colSums(is.na(BSD))

#I will replace all missing values in "city" column with mode. 
#Before I replace, I will check the most frequent city (mode).
mode_value <- names(sort(table(BSD$city), decreasing = TRUE)) [1] 
print(mode_value)

#Now, I will replace with "New York" as it is frequent city. 
BSD $city[is.na(BSD$city)] = "New York"

#check again after replacing NA values at "city" column with "New York"
colSums(is.na(BSD))

#Now, there are only four columns with missing values. They are "country", "birthDate", "firstName", "gdp_country."
#I will check their data type.

#c is combine or concatenate
sapply(BSD[c("country", "birthDate", "firstName", "gdp_country")], is.character)

#I will ignore those remained four columns "country", "birthDate", "firstName", "gdp_country" which all are character data type. 
#Because the portion of missing values is very small amount, and I believe it won't affect any consequences. 

#After pre processing the data set, I will check summary of the cleaned data set and any missing values. 
summary(BSD)
is.na(BSD)

#Now, the data set is ready to analyze and build R shiny dashboard.

#I chose the most 3 interesting questions in my point of view based on the questions of EDA midterm project. 

#(1)What are highest 5 industries that hit most billionaires?
#(2)Who are individuals with highest net worth within the highest industry with most billionaires? 
#Question2 based on Q1 and my original question which I addressed in Midterm is "Who are individuals with highest network within highest industries?". 
#Here, I changed it slightly because I want to explore on only one highest industry not highest industries. 

#(3)Which countries have the highest number of billionaires?
#========================================================================================================

#Define UI 

ui <- fluidPage( #to create floating sidebar layout 
  titlePanel("Billionaire Statistics 2023 Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Insights from Billionaires Data"), #for smaller subheading inside sidebar
      selectInput( #For first question,
        "top_n_industries", "Top N Industries by Number of Billionaires:",
        choices = c(5, 10, 15), selected = 5 #Default is 5
      ),
      selectInput( #For second question
        "top_n_individuals", "Number of Individuals to Show in Top Industry:",
        choices = c(5, 10, 15), selected = 5
      ),
      selectInput( #For third question
        "top_n_countries", "Top N Countries by Number of Billionaires:",
        choices = c(5, 10, 15), selected = 5
      ),
      selectInput( #To choose which variable will be used for individuals chart
        "fill_var", "Color bars by:", #
        choices = c("industries", "category", "gender", "selfMade", "country", "city"), #Dropdown options
        selected = "industries" #Default selected value, we can change 
      ),
      selectInput( #I chose "country" over other columns to filter all data because I will get a chance to explore all countries with billionaires in the dataset
        "country_filter", "Filter by country:",
        choices = c("All", unique(BSD$country)),
        selected = "All"
      )
    ),
    mainPanel(
      plotlyOutput("top_industries_plot"),
      br(), #br() is to add space between charts. If not, the dashboard will be a bit messy. 
      plotlyOutput("top_individuals_plot"),
      br(),
      plotlyOutput("top_countries_plot"),
      br()
    )
  )
)

#Define server logic
server <- function(input, output, session) {
  filtered_data <- reactive({ 
    if (input$country_filter == "All") { #If the user selects "All", it will return whole data set
      BSD
    } else { #If not, filter the data set for country only
      BSD %>% filter(country == input$country_filter)
    }
  })
  
  #Plot 1
  output$top_industries_plot <- renderPlotly({
    df <- filtered_data() %>% 
      count(industries, sort = TRUE) %>% #count how many billionaires are there in each industry and sort
      slice_head(n = as.numeric(input$top_n_industries)) #take top N industries (N is number that user will select)
    
    
    p <- ggplot(df, aes(x = reorder(industries, n), y = n, fill = industries)) +
      geom_col() + #create bar
      coord_flip() + #As ggplot2 does not have direct horizontal bar chat function, I use coord_flip() to flip x and y axes
      labs(
        title = paste("Top", input$top_n_industries, "Industries by Number of Billionaires"),
        x = "Industry",
        y = "Number of Billionaires"
      ) +
      theme_minimal() +
      theme(legend.position = "none") #remove legend 
    
    ggplotly(p) #change to interactive plot
  })
  
  #Plot 2
  output$top_individuals_plot <- renderPlotly({
    req(input$top_n_individuals, input$fill_var) #I want to make sure that required inputs exist
    #filter industry with most billionaires
    top_industry <- filtered_data() %>%
      count(industries) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(industries)
    #filter top richest individuals within industry with most billionaires
    top_individuals <- filtered_data() %>%
      filter(industries == top_industry) %>%
      arrange(desc(finalWorth)) %>%
      slice_head(n = as.numeric(input$top_n_individuals))
    
    #Now, plot question 2
    p <- ggplot(top_individuals, aes(x = reorder(personName, finalWorth), y = finalWorth, fill = !!sym(input$fill_var))) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Top", input$top_n_individuals, "Individuals in Industry with Most Billionaires:", top_industry),
        x = "Individual",
        y = "Net Worth (in millions USD)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  #Plot 3
  output$top_countries_plot <- renderPlotly({
    req(filtered_data()) #Just make sure the date is ready
    
    df <- filtered_data() %>%
      count(country, sort = TRUE) %>%
      slice_head(n = as.numeric(input$top_n_countries))
    req(nrow(df) > 0) #handle error if no data is returned
    
    p <- ggplot(df, aes(x = reorder(country, n), y = n, fill = country)) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Top", input$top_n_countries, "Countries by Number of Billionaires"),
        x = "Country",
        y = "Number of Billionaires"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
}

#Run app
shinyApp(ui=ui,server=server)












