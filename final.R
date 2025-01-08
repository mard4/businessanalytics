getwd()
setwd("C:/Users/Mardeen/Desktop/businessanalytics")
setwd("/home/lorenzo/Desktop/labcust/businessanalytics")

library(dplyr)
library(mlogit)
library(ggplot2)
library(tidyr)
library(MASS)


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
# data %>%
#   select_if(is.numeric) %>%
#   gather() %>%
#   ggplot2::ggplot(aes(value)) +
#   geom_histogram(bins = 30, fill = "skyblue", color = "black") +
#   facet_wrap(~key, scales = "free") +
#   theme_minimal()

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

#### Frequencies

df %>%
  select_if(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_bar(fill = "lightblue", color = "black") +
  facet_wrap(~key, scales = "free") +
  geom_text(stat = "count", aes(label = ..count..), vjust = 1.5, color = "white", size = 4) +
  xlab("") +
  ylab("Count") +
  theme_minimal()


ggplot(data, aes(x = factor(alt), y = choice)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Alternative (alt)", y = "Choice Counts", title = "Distribution of Choices by Alternative") +
  geom_text(aes(label = choice), vjust = 1.5, color = "white", size = 4) +
  theme_minimal()


#### ============================================== 
# Models
#### ============================================== 
# Create the design matrix
data_mlogit <- dfidx(df, idx = list(c("ques", "resp.id"), "alt"))
data_mlogit

## Le frequenze sono abbastanza bilanciate tra le 4 alternative
## Ho tenuto entrambi i modelli per vedere se ci sono variazioni trattando il prezzo come numerico vs factor

model1_price_fac <- mlogit(choice ~ Price + Brand + RAMGB +
                             Foldable + CameraQuality,
                           data = data_mlogit)

model1 <- mlogit(choice ~ Price_num + Brand + RAMGB +
                   Foldable + CameraQuality,
                 data = data_mlogit)


summary(model1_price_fac)
summary(model1)

calculate_mlogit_bic <- function(model) {
  ll <- as.numeric(model$logLik)
  
  k <- length(model$coefficients)
  
  n <- length(model$fitted.values)
  
  bic <- -2 * ll + k * log(n)
  
  cat("Log-Likelihood:", ll, "\n")
  cat("Number of Parameters (k):", k, "\n")
  cat("Number of Observations (n):", n, "\n")
  cat("BIC:", bic, "\n")
  
  return(bic)
}

AIC(model1)
AIC(model1_price_fac)
bic_model1_fact <- calculate_mlogit_bic(model1_price_fac)
bic_model1 <- calculate_mlogit_bic(model1)

# Fit the model without intercept parameters
model2 <- mlogit(choice ~ Price_num + Brand + RAMGB +
                   Foldable + CameraQuality | -1, data = data_mlogit)
model2_price_fac <- mlogit(choice ~ Price + Brand + RAMGB +
                   Foldable + CameraQuality | -1, data = data_mlogit)
summary(model2)
summary(model2_price_fac)


AIC(model2)
AIC(model2_price_fac)
bic_model2_fact <- calculate_mlogit_bic(model2_price_fac)
bic_model2 <- calculate_mlogit_bic(model2)
# Test the restriction on the intercepts by comparing the two models
# through a likelihood ratio test
lrtest(model2, model1)
lrtest(model2_price_fac, model1_price_fac)
  
bic_model2_fact <- calculate_mlogit_bic(model2_price_fac)
bic_model2 <- calculate_mlogit_bic(model2)

AIC(model2)
AIC(model2_price_fac)

aic_bic_df <- data.frame(
  model = character(0), 
  AIC = numeric(0), 
  BIC = numeric(0),
  stringsAsFactors = FALSE
)

aic_bic_df <- rbind(aic_bic_df, data.frame(
  model = "model1_price_fac",
  AIC = AIC(model1_price_fac),
  BIC = calculate_mlogit_bic(model1_price_fac)
))

aic_bic_df <- rbind(aic_bic_df, data.frame(
  model = "model1",
  AIC = AIC(model1),
  BIC = calculate_mlogit_bic(model1)
))

aic_bic_df <- rbind(aic_bic_df, data.frame(
  model = "model2_price_fac",
  AIC = AIC(model2_price_fac),
  BIC = calculate_mlogit_bic(model2_price_fac)
))

aic_bic_df <- rbind(aic_bic_df, data.frame(
  model = "model2",
  AIC = AIC(model2),
  BIC = calculate_mlogit_bic(model2)
))

# Print the resulting dataframe
print(aic_bic_df)
#### ============================================== 
# WTP
#### ============================================== 

# Compute the willingness to pay
coefs <- summary(model2)$coefficients
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
# We have a softmax function that picks the "winner"
new.data <- allDesign[c(1, 179, 278, 400), ]  # Adjust indices as needed
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
# Vorrei visualizzare le distribuzioni per ogni coefficiente ma devo vedere come fare per bene
par(mar = c(4, 4, 2, 2))
par(mfrow = c(1,1))
plot(model2.mixed)
random_coefs <- model2.mixed$coefficients
random_price_num <- model2.mixed$rpar$Price_num
random_price_num_mean <- random_price_num$mean
random_price_num_sigma <- random_price_num$sigma

random_coefs_df <- as.data.frame(random_coefs)

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
# Function to get features with strong correlations for easier visualization
get_strong_correlations_df <- function(corr_mat, threshold) {
  # Find indices of correlations above threshold or below -threshold
  high_corr_indices <- which(abs(corr_mat) > threshold, arr.ind = TRUE)
  
  # Filter out self-correlations (where row index equals column index)
  high_corr_indices <- high_corr_indices[high_corr_indices[, 1] != high_corr_indices[, 2], ]
  
  # Create a dataframe to store variable pairs and their correlation values
  correlations_df <- data.frame(
    Feature1 = rownames(corr_mat)[high_corr_indices[, 1]],
    Feature2 = colnames(corr_mat)[high_corr_indices[, 2]],
    Correlation = corr_mat[high_corr_indices]
  )
  return(correlations_df)
}
# Extract the correlation matrix of random parameters
corr_mat <- vcov(model2.mixed_corr, what = "rpar", type = "cor")
# Get features with strong correlations
strongly_correlated_features_07 <- get_strong_correlations_df(corr_mat, threshold = 0.7)
strongly_correlated_features_06 <- get_strong_correlations_df(corr_mat, threshold = 0.6)

print(strongly_correlated_features_07)
print(strongly_correlated_features_06)


# Function to get features with strong correlations to create the model

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
# Get features with strong correlations
strongly_correlated_features_07 <- get_strong_correlations(corr_mat, threshold = 0.7)
strongly_correlated_features_06 <- get_strong_correlations(corr_mat, threshold = 0.6)

print(strongly_correlated_features_07)
print(strongly_correlated_features_06)

# Update the model to include partially correlated random effects
# Specify correlation only for the strongly correlated features
model2.mixed_strong <- update(model2.mixed, correlation = strongly_correlated_features_07)

# Compare models using likelihood ratio tests
# Fixed effects vs. uncorrelated random effects
lrtest(model2, model2.mixed)
# Uncorrelated random effects vs. all correlated random effects
lrtest(model2.mixed, model2.mixed_corr)
# Partially correlated random effects vs. all correlated random effects
lrtest(model2.mixed_strong, model2.mixed) 

# model2.mixed_strong is the better model (notice how the chi squared improves at each test)

########################################
#### Simulating preference shares ####
########################################
predict.mixed.mnl <- function(model, data, nresp=1000) {
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- model$coef[1:dim(coef.Sigma)[1]]
  draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
  dim(data.model)
  dim(draws)
  for (i in 1:nresp) {
    utility <- data.model%*%draws[i,]
    share = exp(utility)/sum(exp(utility))
    shares[i,] <- share
  }
  result <- cbind(colMeans(shares), data)
  return(result)
}

predictions_mixed_corr <- predict.mixed.mnl(model2.mixed_corr, data = new.data)

predict.mixed.mnl.strong <- function(model, data, nresp=1000) {
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  
  # Get random parameters names
  rpar_names <- names(model$rpar)
  # Get indices of random parameters in coefficient vector
  idx <- which(names(model$coefficients) %in% rpar_names)
  
  # Extract correct covariance matrix and coefficients
  coef.Sigma <- vcov(model)[idx, idx]
  coef.mu <- model$coefficients[idx]
  
  draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
  
  for (i in 1:nresp) {
    utility <- data.model %*% draws[i,]
    share = exp(utility)/sum(exp(utility))
    shares[i,] <- share
  }
  
  result <- cbind(colMeans(shares), data)
  return(result)
}
predictions_mixed_strong <- predict.mixed.mnl.strong(model2.mixed_strong, data = new.data)


########################################
#### Sensitivity Chart ####
########################################
sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mixed.mnl(model, data)[1,1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mixed.mnl(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}
# base.data should contain the baseline level of our model
# doing summary(model2.mixed_strong) shows that baseline is: Price_num low, Huawei, Low Ram, Foldable Yes, Camera quality low
base.data <- new.data[1,]
competitor.data <- new.data[-1,]
tradeoff <- sensitivity.mnl(model2.mixed_corr, attributes, base.data, competitor.data)
tradeoff$labels <- paste0(rep(names(attributes), sapply(attributes, length)),
                          "\n", tradeoff$level)


sensitivity.mnl.strong <- function(model, attrib, base.data, competitor.data) {
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mixed.mnl.strong(model, data)[1,1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mixed.mnl.strong(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}
tradeoff_strong <- sensitivity.mnl.strong(model2.mixed_strong, attributes, base.data, competitor.data)
tradeoff_strong$labels <- paste0(rep(names(attributes), sapply(attributes, length)),
                          "\n", tradeoff$level)

barplot(tradeoff_strong$increase, horiz=FALSE, names.arg=tradeoff$labels, las=2, 
        ylab="Change in Share for the Planned Product Design", 
        ylim=c(-0.1, 0.4), cex.names=0.7)
grid(nx=NA, ny=NULL)
barplot(tradeoff$increase, horiz=FALSE, names.arg=tradeoff$labels, las=2, 
        ylab="Change in Share for the Planned Product Design", 
        ylim=c(-0.1, 0.4), cex.names=0.7)
grid(nx=NA, ny=NULL)