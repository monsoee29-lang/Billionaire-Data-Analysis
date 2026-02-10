library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

#Load the dataset
BSD <- read_csv("Billionaires Statistics Dataset.csv")

#-------------------EDA process------------------

head(BSD) #first six rows
View (BSD)
dim(BSD) #number of rows and columns
summary(BSD) #min, 1st Qu, median, mean, 3rd Qu, max
str(BSD) #structure overview

#sapply: a function to elements in a list and returns the results in a vector or a list
colnames(BSD)[sapply(BSD,is.numeric)]
colnames(BSD)[sapply(BSD,is.character)]
colnames(BSD)[sapply(BSD,is.logical)] #(TRUE/FALSE) columns

#----checking missing values-------
sum(is.na(BSD))

colSums(is.na(BSD))
#When I checked numbers of column by column which has missing values, there are 4 columns that I should drop because those columns has big portion amount of missing values which can make inaccurate analysis.  
#Those columns are "organization" with 2315 missing values, "title" with 2301 missing values, "state" with 1887 missing values and "residenceStateRegion" with 1893 missing values. 
#Even remained columns has respective missing values, they are very few amount. 

rowSums(is.na(BSD))
#when I check missing values of each row of the data set, there are a lot of missing values, but they are a very small portion of overall. So, I won't drop any rows. 

#remove columns with missing values using minus(-)
BSD <- BSD %>% select(-organization, -title, -state, -residenceStateRegion) 
dim(BSD)
#Now, the data set has 31 columns after dropping 4 columns. 

#----------Replacing missing values-------------
# Loop through all columns in BSD
for (col in names(BSD)) {
#check numeric columns
  if (is.numeric(BSD[[col]])) { 
    BSD[[col]][is.na(BSD[[col]])] <- mean(BSD[[col]], na.rm = TRUE) #calculate mean ignoring na and replace all NA values
  }
}

# Check any NA is still remained in numeric columns
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
sapply(BSD_clean[c("country", "birthDate", "firstName", "gdp_country")], is.character)

#I will ignore those remained four columns "country", "birthDate", "firstName", "gdp_country" which all are character data type. 
#Because the portion of missing values is very small amount, and I believe it won't affect any consequences. 

#After preprocessing the data set, I will check summary of the cleaned data set and any missing values. 
summary(BSD)
is.na(BSD)

#Now, it is ready to analyze data and plot analytical questions. 

#-------------------Analytical Questions-----------------------
#------------Descriptive Questions (summarise)--------------
#Descriptive Questions (summarise)
#(1) Which countries have the highest number of billionaires?
#--------------Top 10 Countries with Most Billionaires----------------
#The dataset has 2640 rows and I want to show only top 10 rows based on "country" column.
#I will sort the countries in descending order. 

country_counts <- BSD %>%
  count(country) %>% 
  arrange(desc(n)) %>% #sort the countries in descending order
  top_n(10, n) #10 rows based on n column

ggplot(country_counts, aes(x = reorder(country, n), y = n)) + #reorer is to make bars appear in order
  geom_bar(stat = "identity", fill = "blue") + #build the bar #stat= tell ggplot to use the actual values of n instead of counting rows. 
  geom_text(aes(label = n), hjust = -0.01, size = 3) +  #geom_text is to add text labels to the plot. 
  labs(title = "Top 10 Countries with Most Billionaires", x = "Country",y = "Number of Billionaires") +
  theme_classic() + #Here, I can use theme_minimal() as well, but I chose theme_classic which remove gray background and grid lines, keeps x and y axis lines and ticks, axis titles and plot title. 
  coord_flip() #This is to flip x and y axes of plot because we have long country names. 

#As a result, United States is the country that has most billionaires in the world. 
#Well, China has also really high number which is 523 billionaires. 
#Surprisingly, four countries among top ten countries which are China, India, Hong Kong, and Singapore are from Asia. 

#(2) What are common sources of wealth that made billionaires?
#------------------Top 6 Sources of Wealth------------------
top_sources <- BSD %>%
  count(source) %>%
  arrange(desc(n)) %>%
  top_n(6, n)

ggplot(top_sources, aes(x=reorder(source, n), y=n)) +
  geom_bar(stat="identity", fill="darkred") +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  labs(title="Top 6 Sources of Wealth", x="Source", y="Count") +
  theme_classic() +
  coord_flip()
#So, it concludes that most billionaires are real estate.
#Secondly, they do investments. 
#Here, diversified means that they are taking role of multiples tasks and responsibilities in different fields rather than doing one job. (The dataset did not mention what roles are exactly for diversified.)
#For fourth wealth, it is pharmaceuticals that is medical industry which produces, manufactures, discover, develops medicine and medical devices. It seems they are really great job which makes a lot of money. 



#(3) How does wealth reflect gender imbalance?
#-----------------Gender Distribution Among Billionaires--------------------
gender_dist <- BSD %>%
  count(gender)

ggplot(gender_dist, aes(x=gender, y=n)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(aes(label = n), vjust = -0.7, size = 3) + #vjust= move text upward (vertical justification)/ negative value moves text above the bar. Size=3 is for the size of data points on the labels.
  labs(title="Gender Distribution of Billionaires", subtitle = "F = Female, M = Male", x="Gender", y="Count") +
  theme_classic() 

#First, I added subtitle that explains "F" stands for female and "M" stand for male. 
#In data set, it originally uses F and M instead of female and male. Even almost everyone can be familiar with those short word "F and M", I added long forms considering every single one of audience. 
#Secondly, I did not expect that female billionaires will be that much low among 2640 people. There are only 337 female billionaires. 
#The rest billionaires are male persons hit to 2303 amount. That is really huge number. 

#(4) 
#After knowing the number of male and female billionaires, I am very curious to know who are top female billionaires, and how much are they rich? 
#(4) Who are the most wealthiest female billionaires, and how much are they rich? 
#-----------------Top 5 Female Billionaires------------------

top5_females <- BSD %>%
  filter(gender == "F") %>%        #Filter for female
  arrange(desc(finalWorth)) %>%    #Sort in descending order of finalWorth
  top_n(5, finalWorth) %>%         #select top 5 by finalWorth
  
  select(personName, finalWorth) %>% #Select personName and finalWorth columns
  mutate( #And, I use mutate function to create new column which includes female billionaires name and their net worth. 
    worth_in_millions = finalWorth, 
    name_label = personName 
  )

ggplot(top5_females, aes(x = reorder(name_label, finalWorth), y = finalWorth)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = paste0(finalWorth, " M")), hjust = 1.2, size = 3) + #paste0 is function to combine "finalWorth" which is a number and " M" character string into a single string like "6000 M".    
  labs(title = "Top 5 Female Billionaires", subtitle = "Sorted by Net Worth (in Million USD)", x = "Billionaire", y = "Net Worth") +
  theme_classic() +
  coord_flip()  

#Francoise Bettencourt Meyers & family is the wealthiest female billionaires having net worth of $80,500 millions. 
#When it comes to second wealthiest, net worth goes to big gap from wealthiest having $59,000 million. 
#But, second and third rank did not make big difference wet worth. 
#Among five, 3 has family. It seems they are married and maybe they have kids. Even I am curious, there is no information about single or married status in the data set. 
#To be honest, I am not familiar with all these female billionaires. However, this is really worth knowing.


#(5) After Top 5 Female Billionaires, I want to know male.
#(5) Who are the most wealthiest male billionaires? 
#Who are Top 5 Male Billionaires? 
top5_male <- BSD %>%
  filter(gender == "M") %>%
  arrange(desc(finalWorth)) %>%
  top_n(5, finalWorth)

ggplot(top5_male, aes(x = reorder(personName, finalWorth), y = finalWorth)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  geom_text(aes(label = finalWorth), hjust = 1.2, size = 3) +
  labs(title = "Top 5 Male Billionaires by Net Worth", x = "Billionaire", y = "Net Worth (in millions USD)") +
  theme_classic() +
  coord_flip() 

#As of the data, the world's wealthiest male billionaires are led by Bernard Arnault & family. 
#Bernard Arnault & family is followed closely by Elon Musk, and Jeff Bezos. 
#In fourth place, it is Larry Ellison while Warren Buffett rounds out the top five. 


#----------------------------Univariate Analysis (analyze one variable at a time)-----------------------------
#Univariate Analysis (analyze one variable at a time)
#(6) How many billionaires are self-made or are they inherited?
#-------------------Self-Made vs Inherited Billionaires----------------
#In my data set, there is already been selfMade column. The values in that column are TRUE and FALSE.
#If it is TRUE, it is selfMade. FALSE means the wealth is inherited. Here, I am not testing logical conditions, but analyzing one variable "selfMade". 

selfmade_dist <- BSD %>%
  count(selfMade)

ggplot(selfmade_dist, aes(x = selfMade, y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +  
  labs(title = "Self-Made vs Inherited Billionaires", x = "Self Made", y = "Count") +
  theme_classic()
#TRUE, which is self-made, has 1812 amount and FALSE, where wealth is inherited, is 828 amount. 
#It reflects the reality of our society that people who are trying harder become more successful. 



#(7) How does net worth vary by age group?
#---------------Average Net Worth by Age Group--------------
#First, I have to create age group. 

#Below code is the first code I used and I got NA bin when plotting. 
#mutate(age_group = case_when( 
#  age >= 20 & age < 40 ~ "Young Adult",
#  age >= 40 & age < 60 ~ "Middle-Aged",
#  age >= 60 & age < 80 ~ "Senior Adult",
#  age >= 80 & age <= 100 ~ "Elderly"
#))

#I have to drop rows where age is NA. 
billionaires <- BSD %>%
  filter(!is.na(age)) %>%  #drop rows where age is NA.
  mutate(age_group = case_when( #create age group using mutate and case_when (assign values at multiple conditions).
    age < 20 ~ "Teenager",
    age >= 20 & age < 40 ~ "Young Adult",
    age >= 40 & age < 60 ~ "Middle-Aged",
    age >= 60 & age < 80 ~ "Senior Adult",
    age >= 80 & age <= 100 ~ "Elderly",
    age > 100 ~ "Senior Elderly"
  ))

#Now, I will calculate average net-worth by age group. 
networth_by_age <- billionaires %>%
  group_by(age_group) %>% #groups the data based on age_group
  summarise(avg_networth = mean(finalWorth, na.rm = TRUE)) %>% #just calculate average net worth based on finalWorth column
  filter(!is.na(age_group)) #check again no NA groups left

ggplot(networth_by_age, aes(x = age_group, y = avg_networth)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(avg_networth, 1)), vjust = -0.5, size = 3) +
  labs(title = "Average Net Worth by Age Group", x = "Age Group", y = "Average Net Worth (in millions)") +
  theme_classic()

#I have to drop rows where age is NA. 
billionaires <- BSD %>%
  filter(!is.na(age)) %>% 
  mutate(age_group = case_when(
    age < 20 ~ "Teenager",
    age >= 20 & age < 40 ~ "Young Adult",
    age >= 40 & age < 60 ~ "Middle-Aged",
    age >= 60 & age < 80 ~ "Senior Adult",
    age >= 80 & age <= 100 ~ "Elderly",
    age > 100 ~ "Senior Elderly"
  ))

#Now, I will calculate average net-worth by age group. 
networth_by_age <- billionaires %>%
  group_by(age_group) %>% #groups the data based on age_group
  summarise(avg_networth = mean(finalWorth, na.rm = TRUE)) %>% 
  filter(!is.na(age_group)) #check again no NA groups left

ggplot(networth_by_age, aes(x = age_group, y = avg_networth)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(avg_networth, 1)), 
            vjust = -0.5, size = 3) +
  labs(title = "Average Net Worth by Age Group", 
       x = "Age Group",
       y = "Average Net Worth (in millions)") +
  theme_classic()

#For Elderly group aged between 80 and 100, average net worth is approximately $6,234 million. Experience and time really does seem pay off proving that wealth often grows with long-term investments.  
#For young adult group, who are between 20 and 40 years old, $5,180 million is average wealth. This is very impressive. I think it shows that a new generation of ultra-rich is emerging earlier than ever.
#Senior adult and middle-aged groups have $4,505.4 million and $4,054.1 million respectively. 
#It is very surprisingly that teenage billionaires exist. 
#By looking at those average net worth, while older age groups dominate in average wealth, 
#the rise of young billionaires suggests the wealth landscape is shifting.
#Legacy wealth and fast-tracked fortunes are now coexisting in fascinating ways.




#------------------------Bivariate Analysis (analyzing two variables)----------------
#Bivariate Analysis
#(8) What is top 5 industries with most billionaires? 
#First, I have to count billionaires per industry and select the top 5 industries. 
top5_industries <- BSD %>%
  count(industries, sort = TRUE) %>%
  top_n(5, n) 

ggplot(top5_industries, aes(x = reorder(industries, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), hjust = 1.5, size = 3) +
  labs(title = "Top 5 Industries with Most Billionaires", x = "Industry",y = "Number of Billionaires") +
  theme_classic()+
  coord_flip()  

#Now, it is confirmed that Finance & Investments industry has most billionaires hit to 372. 
#Manufacturing industry has second highest billionaires having 324. 
#Technology is at third most position resulting 314, followed by Fashion & Retail at 266. 
#At top five, Food & Beverage accounts for 212.
#Traditional sectors like finance and manufacturing continue to dominate billionaire creation. 
#However, technology remains a powerful driver of modern wealth. 


#(9) What is highest net worth by industry?

networth_by_industry <- BSD %>%
  group_by(industries) %>%
  summarise(max_networth = max(finalWorth, na.rm = TRUE)) %>%
  arrange(desc(max_networth)) %>%
  top_n(6, max_networth)

ggplot(networth_by_industry, aes(x = reorder(industries, max_networth), y = max_networth)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = paste0(max_networth, " M")),hjust = 1.3, size = 3) + #paste0 is function to combine two strings parts which is max_networth and M. 
  labs(title = "Highest Net Worth by Industry", x = "Industry", y = "Highest Net Worth") +
  theme_classic() +
  coord_flip()

#According to the data, Fashion & Retail dominates the leaderboard with an eye-popping $211,000 million. 
#This makes the fashion not only stylish but staggeringly profitable.
#Next up is Automotive, revving up at $180,000 million, likely fueled by game-changers like Elon Musk. 
#Technology follows close behind with peaks of $114,000 million, making the huge wealth created by innovation. 
#Media & Entertainment hits $94,500 million, proving that influence and storytelling can be just as lucrative.
#And finally, Telecom rings in at $93,000 million, highlighting the ongoing power of global connectivity.
#This analysis shows that while tech continues to be a powerhouse, 
#traditional industries like fashion and automotive still hold incredible influence. 


#After the highest net worth by industry, I want to know individuals with highest network within those industries. 
#(10) Who are individuals with highest networth within highest industry? 
top_industries <- BSD %>%
  group_by(industries) %>%
  summarise(max_networth = max(finalWorth, na.rm = TRUE)) %>%
  top_n(6, max_networth)

top_billionaires_by_industry <- BSD %>%
  filter(industries %in% top_industries$industries) %>%
  group_by(industries) %>%
  filter(finalWorth == max(finalWorth)) %>%
  select(industries, personName, finalWorth) %>%
  arrange(desc(finalWorth))

print(top_billionaires_by_industry)

#Bernard Arnault & family top the chart with an astounding $211,000 million, 
#showing how luxury brands and global retail empires can translate into unmatched fortune.

#Elon Musk, known for Tesla and SpaceX, leads this category with $180,000 million, 
#reflecting how innovation and electrification are driving wealth in the modern transportation world.

#Jeff Bezos, founder of Amazon, holds $114,000 million proving that tech giants continue to dominate the financial landscape.

#Warren Buffett, the legendary investor, sits at $106,000 million, a testament to the power of long-term investing. 


#Michael Bloomberg brings in $94,500 million, illustrating how media empires and data services can be highly profitable. 

#Carlos Slim Helu & family command $93,000 million, showing that communication networks remain crucial and profitable in a connected world.

















