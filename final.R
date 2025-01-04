getwd()
setwd("C:/Users/Mardeen/Desktop/businessanalytics")
#setwd("/home/sav/Desktop/labcust/businessanalytics")

library(dplyr)
library(mlogit)
library(ggplot2)
library(tidyr)

data <- read.csv2("CBC_cellphone_data.csv")
price_mapping <- c("Budget" = 200, 
                   "LowerMid-range" = 400, 
                   "Mid-range" = 600, 
                   "UpperMid-range" = 800, 
                   "Premium" = 1000)

data$Price_num <- price_mapping[data$Price]


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
xtabs(choice ~ Price_num, data=data)
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

#### ============================================== 
# Transformations
#### ============================================== 
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

#### ============================================== 
# Models
#### ============================================== 
# Create the design matrix
data_mlogit <- dfidx(df, idx = list(c("ques", "resp.id"), "alt"))
data_mlogit

## Le frequenze sono abbastanza bilanciate tra le 4 alternative
## Ho tenuto entrambi i modelli per vedere se ci sono variazioni trattando il prezzo come numerico vs factor

model1 <- mlogit(choice ~ Price_num + Brand + RAMGB +
                   Foldable + CameraQuality,
                 data = data_mlogit)

model1_price_fac <- mlogit(choice ~ Price + Brand + RAMGB +
                   Foldable + CameraQuality,
                 data = data_mlogit)

summary(model1)
summary(model1_price_fac)

# Fit the model without intercept parameters
model2 <- mlogit(choice ~ Price_num + Brand + RAMGB +
                   Foldable + CameraQuality | -1, data = data_mlogit)
model2_price_fac <- mlogit(choice ~ Price + Brand + RAMGB +
                   Foldable + CameraQuality | -1, data = data_mlogit)
summary(model2)
summary(model2_price_fac)

# Test the restriction on the intercepts by comparing the two models
# through a likelihood ratio test
lrtest(model2, model1)
lrtest(model2_price_fac, model1_price_fac)

# Fit the model without intercept parameters and with price as a quantitative variable
model3 <- mlogit(choice ~ Price_num + Brand + RAMGB +
                   Foldable + CameraQuality | -1, data = data_mlogit)

model3_price_fac <- mlogit(choice ~ Price + Brand + RAMGB +
                   Foldable + CameraQuality | -1, data = data_mlogit)
summary(model3)
summary(model3_price_fac)
lrtest(model3, model2)
lrtest(model3_price_fac, model2_price_fac)

#### ============================================== 
# WTP
#### ============================================== 

# Compute the willingness to pay
coefs <- summary(model1)$coefficients
price_coef <- coefs["Price_num"]
wtp <- -coefs / price_coef
wtp

# Create a data frame for better visualization
wtp_df <- data.frame(
  Attribute = names(wtp),
  WTP = wtp
)


# Remove Price_num from results since it's our reference
wtp_df <- wtp_df[wtp_df$Attribute != "Price_num", ]

# Sort by absolute WTP value
wtp_df <- wtp_df[order(abs(wtp_df$WTP), decreasing = TRUE), ]

# Print formatted results
print(wtp_df)

ggplot(wtp_df, aes(x = reorder(Attribute, WTP), y = WTP)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Willingness to Pay by Attribute",
    x = "Attribute",
    y = "Willingness to Pay (Price Units)"
  )

#### ============================================== 
# Predictions
#### ============================================== 

# Prediction function for MNL
predict.mnl <- function(model, data) {
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  logitUtility <- data.model %*% model$coef
  share <- exp(logitUtility) / sum(exp(logitUtility))
  cbind(share, data)
}

# Define attributes based on your data
attributes <- list(
  Price = levels(df$Price),
  Brand = levels(df$Brand),
  RAMGB = levels(df$RAMGB),
  Foldable = levels(df$Foldable),
  CameraQuality = levels(df$CameraQuality)
)

# Create all possible designs
allDesign <- expand.grid(attributes)
# Add Price_num column to allDesign
allDesign$Price_num <- price_mapping[allDesign$Price]
# Select a subset of designs for prediction
new.data <- allDesign[c(1, 5, 10, 15), ]  # Adjust indices as needed
# Predict probabilities
predictions <- predict.mnl(model2, new.data)
# Convert predictions to a data frame for visualization
predictions_df <- as.data.frame(predictions)
print(predictions)


#### ============================================== 
# Modelling: Mixed MNL model
#### ============================================== 

########################################
#### 3- Modelling: Mixed MNL model ####
########################################

# Define the random parameter structure
# Set all parameters to have random effects with normal distribution
coef_names <- names(model2$coef)
model2.rpar <- rep("n", length = length(coef_names))
names(model2.rpar) <- coef_names

# Fit the Mixed MNL model with uncorrelated random effects
model2.mixed <- mlogit(
  choice ~ Price_num + Brand + RAMGB + Foldable + CameraQuality | -1,
  data = data_mlogit,
  panel = TRUE,
  rpar = model2.rpar,
  correlation = FALSE
)
summary(model2.mixed)
# Visualize the distribution of random effects to understand heterogeneity
plot(model2.mixed)

########################################
#### Analyze Random Effects for Price ####
########################################

names(rpar(model2.mixed))
# Random effect for PriceLowerMid-range
# PriceLowerMid.distr <- rpar(model2.mixed, "PriceLowerMid-range")
# summary(PriceLowerMid.distr)
# mean(PriceLowerMid.distr)
# plot(PriceLowerMid.distr)
# 
# # Random effect for PricePremium
# PricePremium.distr <- rpar(model2.mixed, "PricePremium")
# summary(PricePremium.distr)
# mean(PricePremium.distr)
# plot(PricePremium.distr)


########################################
#### Add Correlated Random Coefficients ####
########################################

# Update the model to include correlations between random effects
model2.mixed_corr <- update(model2.mixed, correlation = TRUE)
summary(model2.mixed_corr)
# Analyze the correlation matrix of random parameters
summary(vcov(model2.mixed_corr, what = "rpar", type = "cor"))
cor_matrix <- vcov(model2.mixed_corr, what = "rpar", type = "cor")
cor_df <- as.data.frame(as.table(cor_matrix))
# Plot the heatmap using ggplot2
ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix of Random Parameters (Mixed MNL Model)",
       x = "",
       y = "")


#############
# Function to get features with strong correlations
get_strong_correlations <- function(corr_mat, threshold = 0.7) {
  # Find indices of correlations above threshold or below -threshold
  high_corr_indices <- which(abs(corr_mat) > threshold, arr.ind = TRUE)
  
  # Filter out self-correlations (where row index equals column index)
  high_corr_indices <- high_corr_indices[high_corr_indices[, 1] != high_corr_indices[, 2], ]
  
  # Get the unique feature names from the row and column indices
  features <- unique(c(rownames(corr_mat)[high_corr_indices[, 1]], 
                       colnames(corr_mat)[high_corr_indices[, 2]]))
  
  return(features)
}
# Extract the correlation matrix of random parameters
corr_mat <- vcov(model2.mixed_corr, what = "rpar", type = "cor")
# Get features with strong correlations
strongly_correlated_features <- get_strong_correlations(corr_mat, threshold = 0.7)
print(strongly_correlated_features)

# Update the model to include partially correlated random effects
# Specify correlation only for the strongly correlated features
model2.mixed_strong <- update(model2.mixed, correlation = strongly_correlated_features)

# Compare models using likelihood ratio tests
# Fixed effects vs. uncorrelated random effects
lrtest(model2, model2.mixed)
# Uncorrelated random effects vs. all correlated random effects
lrtest(model2.mixed, model2.mixed2)
# Partially correlated random effects vs. all correlated random effects
lrtest(model2.mixed, model2.mixed_strong) 


########################################
#### Simulating preference shares ####
########################################

#todo

########################################
#### Sensitivity Chart ####
########################################


