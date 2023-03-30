## Data Transformation part
library(tidyverse)
library(sqldf)
library(lubridate)

df <- read.csv("Copy of book_master_2023-03-24.csv")
glimpse(df)

## filter Na value
df %>%
  filter_at(vars(Title,Book_category,Star_rating,Price),
            all_vars(!is.na(.)))

## remove Quantity = 0
df2 <- df %>%
  select(Quantity) %>%
  filter(Quantity <= 0) %>%
  unlist() %>%
  as.vector("numeric") 

df[-df2,]

## check column that unable to contain duplicated rows
df %>%
  filter(duplicated(select(.,Title)))

## delete duplicated rows
df <- df[!duplicated(df$Title), ]

## rename column from สำนักพิมพ์ to Publisher
glimpse(df)

df <- df %>%
  rename(Publisher = สำนักพิมพ.)

## rename value from thai to english

sort(table(df$Book_category))
df$Book_category[df$Book_category == 'กวี'] <- "Poetry"

## change date format

as.Date(df$PublishedDate, format = "%Y-%m-%d")

df$PublishedDate[c(664,665,666)] <- c("2022-09-25","2022-09-25","2022-09-25")

View(df)

## drop Na column
df <- df %>% select(-add_date)

## convert Star rating value from string to numeric value
unique(df$Star_rating)
df$Star_rating <- ifelse(df$Star_rating %in% c("One","Two","Three","Four","Five"),
       c(1,2,3,4,5),"Na")
df$Star_rating <- as.integer(df$Star_rating)

library(tidyverse)
library(sqldf)
library(lubridate)

df <- read.csv("Copy of book_master_2023-03-24.csv")
glimpse(df)

## filter Na value
df %>%
  filter_at(vars(Title,Book_category,Star_rating,Price),
            all_vars(!is.na(.)))

## remove Quantity = 0
df2 <- df %>%
  select(Quantity) %>%
  filter(Quantity <= 0) %>%
  unlist() %>%
  as.vector("numeric") 

df[-df2,]

## check column that unable to contain duplicated rows
df %>%
  filter(duplicated(select(.,Title)))

## delete duplicated rows
df <- df[!duplicated(df$Title), ]

## rename column from สำนักพิมพ์ to Publisher
glimpse(df)

df <- df %>%
  rename(Publisher = สำนักพิมพ.)

## rename value from thai to english

sort(table(df$Book_category))
df$Book_category[df$Book_category == 'กวี'] <- "Poetry"

## change date format

as.Date(df$PublishedDate, format = "%Y-%m-%d")

df$PublishedDate[c(664,665,666)] <- c("2022-09-25","2022-09-25","2022-09-25")

View(df)

## drop Na column
df <- df %>% select(-add_date)

## convert Star rating value from string to numeric value
unique(df$Star_rating)
df$Star_rating <- ifelse(df$Star_rating %in% c("One","Two","Three","Four","Five"),
       c(1,2,3,4,5),"Na")
df$Star_rating <- as.integer(df$Star_rating)


# ----------------------------------------------------------------------------------


## data analysis part
library(tidyverse)

glimpse(df)

## 1. find the summary stats of price of each book category
df %>%
  group_by(Book_category) %>%
  summarise(n = n(),avg_price = mean(Price),sum_price = sum(Price),
            med_price = median(Price),max_price = max(Price),
            min_price = min(Price)) %>%
  arrange(desc(n),desc(avg_price))

## 2. count the amount of books published by each publisher
df %>%
  group_by(Publisher) %>%
  summarise(n = n())

## 3. find fiction books with 4 and 5 stars rating 
top_rating_fiction_book <- df %>% 
  select(Title,Book_category,Star_rating) %>%
  filter(Book_category == "Fiction" & Star_rating >= 4) %>%
  arrange(Star_rating) %>%
  View()

## 4. find the books cost under $20 with 4-5 stars rating
df %>%
  select(Title,Book_category,Star_rating,Price) %>%
  filter(Star_rating >= 4 & Price < 20) %>%
  arrange(Star_rating) %>%
  View()
  
## 5. which publisher sell the most expensive book (in average)?
df %>%
  select(Publisher,Price) %>%
  group_by(Publisher) %>%
  summarise(avg_price = mean(Price)) %>%
  arrange(desc(avg_price))


## 6. find the portion(%) of business books' average price compare to 
## average price of all books (To find out how reasonable the price is)
df %>%
  select(Book_category,Price) %>%
  filter(Book_category == "Business") %>%
  summarise(avg_price_of_business_book = mean(Price),
            avg_price_of_all_books = mean(df$Price),
            bus_book_portion = (avg_price_of_business_book / avg_price_of_all_books) * 100)


## 7. which books has 1-2 star rating but still expensive (> $30)
df %>%
  select(Title,Star_rating,Price) %>%
  filter(Star_rating <= 2 & Price > 30) %>%
  View()

## 8. run regression model to predict Star_rating
library(caret)

# split data
set.seed(42)
n <- nrow(df)
id <- sample(1:n, size = n*0.8)
train_df <- df[id,]
test_df <- df[-id,]

# train model
set.seed(21)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = T)

(lm_model <- (train(Star_rating ~ Price + Publisher + Quantity,
                  data = train_df,
                  method = "lm",
                  metric = "RMSE",
                  trControl = ctrl)))

(knn_model <- (train(Star_rating ~ Price + Publisher + Quantity,
                    data = train_df,
                    method = "knn",
                    tuneLength = 5,
                    metric = "RMSE",
                    preProcess = c("center","scale"),
                    trControl = ctrl)))
lm_model
knn_model

# test model
p_knn <- predict(knn_model,newdata = test_df)
p_lm <- predict(lm_model,newdata = test_df)

# evaluate model
RMSE(p_knn,test_df$Star_rating)
RMSE(p_lm,test_df$Star_rating)

## 9. bar chart (star rating vs. book category)
ggplot(df, aes(Star_rating,Book_category)) +
  geom_col(fill = "salmon3",alpha = 0.9) +
  theme_minimal() +
  labs(
    title = "Bar Chart Shows Relationship Between Book Category and
    Star Rating",
    x = "Total Star Rating",
    y = "Book Category"
  )


## 10. Density chart with facet function
ggplot(df, aes(Price,fill = Publisher)) +
  geom_density(alpha = 0.8) +
  theme_minimal() +
  facet_grid(Star_rating ~ Publisher) +
  labs(
    title = "Density Chart Shows Density of Price facet by
    Star Rating and Publisher",
    x = "Price",
    y = "Density"
  )
















  
  


  
  


  







  


































  
  


  







  
































