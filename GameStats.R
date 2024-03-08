# Game Stats
# CSE 310 Class
# 28 March 2024
# Christopher Slack
# Data Set From: https://www.kaggle.com/datasets/gregorut/videogamesales

#Load Libraries
library(dplyr)
library(tidyr)

# Welcomes user to program
  chr_var <- "Welcome to Game Stats Breakdown!"
  print(paste(chr_var))


# Gets the CSV File
  # This asks the user to enter the file path to their CSV file
  file_path <- readline(prompt = "Please enter a file path to your CSV file: ")

  # This code loads the data
  games <- read.csv(file_path)


# Prepares the data
  # View the first few rows of the data
  head(games)

  # This cleans data and remove N/A values
  games_clean <- na.omit(games)

# Provides Basic information about the CSV File
  # Stores column names as a factor
  column_names <- factor(colnames(games))

  # Prints column names
  print("The Columns in the CSV are: ")
  for (name in levels(column_names)) 
  {
    print(name)
  }

  # Gets the number of games and tells the user
  int_games <- nrow(games_clean)
  print(paste("The number of games in this data is: ", int_games))

  # Calculate the total global sales
  total_sales <- sum(games_clean$Global_Sales, na.rm = TRUE)

  # Print the total global sales
  print(paste("The Total Global Sales (in Millions) are:", total_sales))


# Games per Year Breakdown
 # Create a table of the number of games released each year
  year_counts <- table(games_clean$Year)

  # Plot data with colors
  bar_colors <- rep(c("blue", "green"), length.out=length(year_counts))
  barplot(year_counts, main = "Number of Video Game Releases by Year", xlab = "Year", ylab = "Number of Games", col=bar_colors)


# Total Sales per Year
  # Create a table of the total sales in dollars by year
  sales_by_year <- games_clean %>%
    group_by(Year) %>%
    summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%

    # Sort by Year in ascending order
    arrange(Year)  
  
  # Print the table
  print(sales_by_year)

  # Saves the table to a CSV file
  write.csv(sales_by_year, file = "GlobalSalesPerYear.csv", row.names = FALSE)

  # Plot data with colors
  bar_colors <- rainbow(length(year_counts))
  barplot(sales_by_year$Total_Sales, names.arg=sales_by_year$Year, xlab="Year", ylab="Total Sales (in millions)", main="Total Sales by Year", col=bar_colors)


# Games per Platfom Breakdown
  # Create a table of the number of games released per platform
  platform_counts <- table(games_clean$Platform)

  # Plot data with colors
  bar_colors <- rep(c("red", "purple"), length.out=length(platform_counts))
  barplot(platform_counts, main = "Number of Video Game Releases by Platform", xlab = "Platform", ylab = "Number of Games", col=bar_colors)


# Games per Genre Breakdown
  # Create a table of the number of games released each year
  genre_counts <- table(games_clean$Genre)

  # Plot data with colors
  bar_colors <- rainbow(length(genre_counts))
  barplot(genre_counts, main = "Number of Video Game Releases per Genre", xlab = "Genre", ylab = "Number of Games", col=bar_colors)

  # This creates a table of the number of games in each genre
  genre_counts_table <- games_clean %>%
    group_by(Genre) %>%
    summarise(Total_Games = n()) %>%

    # Sort by Total_Games in descending order
    arrange(desc(Total_Games))  

  # Print the table
  print(genre_counts_table)  

  # Saves the table to a CSV file
  write.csv(genre_counts_table, file = "GamesPerGenre.csv", row.names = FALSE)


# Total Sales per Genre
  # Create a table of the total sales in dollars by genre
    sales_by_genre <- games_clean %>%
      group_by(Genre) %>%
      summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%

      # Sort by Year in ascending order
      arrange(Genre)  
    
    # Print the table
    print(sales_by_genre)

    # Saves the table to a CSV file
    write.csv(sales_by_genre, file = "GlobalSalesByGenre.csv", row.names = FALSE)

    # Plot data with colors
    bar_colors <- rainbow(length(genre_counts))
    barplot(sales_by_genre$Total_Sales, names.arg=sales_by_genre$Genre, xlab="Genre", ylab="Total Sales (in millions)", main="Total Sales by Genre", col=bar_colors)


# Sum up the total sales and print to screen - num