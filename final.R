getwd()
setwd("C:/Users/Mardeen/Desktop/businessanalytics")

install.packages('mlogit')
install.packages("rbibutils")

library(dplyr)
library(mlogit)
library(ggplot2)
library(tidyr)

data <- read.csv2("CBC_cellphone_data.csv")
head(data)
str(data)
# ok the data is in a wide format, each row represent a question, so we have more columns


for (col in 1:ncol(data)) {
  cat("Unique values in column", colnames(data)[col], ":\n")
  print(unique(data[[col]]))
  cat("\n")  
}

summary(data)
data %>%
  select_if(~!is.numeric(.)) %>%
  summarise_all(~length(unique(.)))
xtabs(choice ~ Brand, data=data)
xtabs(choice ~ Price, data=data)
xtabs(choice ~ RAMGB, data=data)
xtabs(choice ~ Foldable, data=data)
xtabs(choice ~ CameraQuality, data=data)

sum(is.na(data))

# 300 respondents
length(unique(data$resp.id))

# 4500 questions
length(unique(data$ques))


# Numeric
data %>%
  select_if(is.numeric) %>%
  gather() %>%
  ggplot2::ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~key, scales = "free") +
  theme_minimal()

###
table(data$alt[data$choice == 1]) # inclinazione verso il centro
table(data$alt[data$choice == 0]) 

# verifica se la distribuzione delle scelte ? significativamente diversa da una distribuzione uniforme 
chisq.test(table(data$alt[data$choice == 1])) 

df <- data
##### transformations
df$Price <- factor(df$Price, levels = c("Budget","LowerMid-range",
                                            "Mid-range","UpperMid-range","Premium"))
df$Brand <- factor(df$Brand)
df$Foldable <- factor(df$Foldable,
                     levels = c("Yes","No"))
df$CameraQuality <- factor(df$CameraQuality,
                        levels = c("Low","Medium","High"))
df$RAMGB <- factor(df$RAMGB, levels=c("Low","LowerMid-range","Mid-range","High-end"))

####

df %>%
  select_if(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_bar(fill = "skyblue", color = "black") +
  facet_wrap(~key, scales = "free") +
  geom_text(stat = "count", aes(label = ..count..), vjust = 1.5, color = "white", size = 4) +
  xlab("") +
  ylab("Count") +
  theme_minimal()

################ MODELLI ##############################
# Create the design matrix
data_mlogit <- dfidx(df, idx = list(c("ques", "resp.id"), "alt"))
data_mlogit

## Le frequenze sono abbastanza bilanciate tra le 4 alternative

model1 <- mlogit(choice ~ Price + Brand + RAMGB +
                   Foldable + CameraQuality,
                 data = data_mlogit)
summary(model1)

# Fit the model without intercept parameters
model2 <- mlogit(choice ~ Price + Brand + RAMGB +
                   Foldable + CameraQuality | -1, data = data_mlogit)
summary(model2)

# Test the restriction on the intercepts by comparing the two models
# through a likelihood ratio test
lrtest(model2, model1)

# modello con price numerica
data_mlogit$Price_num <- as.numeric(factor(data_mlogit$Price, 
                                           levels = c("Budget", "LowerMid-range", "Mid-range", "UpperMid-range", "Premium")))

table(data_mlogit$Price_num)

# Fit the model without intercept parameters and with price as a quantitative variable
model3 <- mlogit(choice ~ Price_num + Brand + RAMGB +
                   Foldable + CameraQuality | -1, data = data_mlogit)
summary(model3)
lrtest(model3, model2)

# Compute the willingness to pay
coefs <- summary(model3)$coefficients
price_coef <- coefs[0:1] #price num
wtp <- coefs / abs(price_coef)
wtp
