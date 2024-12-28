getwd()
setwd("C:/Users/marti/Desktop/labCustomer/finalProject")
library(dplyr)
install.packages('mlogit')
library(mlogit)
library(ggplot2)
library(tidyr)

data <- read.csv2("CBC_cellphone_data.csv")
head(data)
str(data)
# ok the data is in a wide format, each row represent a question, so we have more columns


for (col in 2:ncol(data)) {
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
  select_if(\is.numeric) %>%
  gather() %>%
  ggplot2::ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~key, scales = "free") +
  theme_minimal()

# Non- Numeric
data %>%
  select_if(~!is.numeric(.)) %>% # Seleziona solo colonne non numeriche
  gather(key = "Variable", value = "Value") %>% # Trasforma i dati in formato lungo
  count(Variable, Value) %>% # Conta le occorrenze per ogni combinazione di variabile e valore
  ggplot(aes(x = Value, y = n, fill = Variable)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~Variable, scales = "free") + # Un grafico per ciascuna variabile
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Ruota i nomi sull'asse x

###
table(data$alt[data$choice == 1]) # inclinazione verso il centro
table(data$alt[data$choice == 0]) 

# verifica se la distribuzione delle scelte è significativamente diversa da una distribuzione uniforme 
chisq.test(table(data$alt[data$choice == 1])) 

##### transformations
data$Price <- as.factor(data$Price)
data$Brand <- factor(data$Brand,
              levels = c("Xiaomi","Huawei","Poco","OnePlus"))
data$Foldable <- factor(data$Foldable,
                     levels = c("Yes","No"))
data$CameraQuality <- factor(data$CameraQuality,
                        levels = c("Low","Medium","High"))
data$RAMGB <- as.factor(data$RAMGB)


