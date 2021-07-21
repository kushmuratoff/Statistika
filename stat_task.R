library("readxl")

my_data <- read_excel("villa2.xls", sheet = 2)
my_data

fit <- lm(Price ~ ., data=my_data)
summary(fit)