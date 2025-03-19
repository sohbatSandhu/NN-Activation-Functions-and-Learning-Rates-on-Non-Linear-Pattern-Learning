nn.data <- read.csv("nn-data.txt", sep="")
head(nn.data)

# factorize the three variables
nn.data$pattern <- as.factor(nn.data$pattern)
nn.data$activation <- as.factor(nn.data$activation)
nn.data$learning.rate <- as.factor(nn.data$learning.rate)

library(MASS)
# Perform ANOVA to analyze the impact of the factors on the test error
anova_results <- aov(
  test.error ~ pattern * activation + activation*learning.rate + learning.rate*pattern, data = nn.data
)

summary(anova_results)

#use boxcox to see if transformation is needed
par(mar=c(5,5,1,1), cex=0.8)
boxcox(aov(test.error ~ pattern,data=nn.data))
boxcox(anova_results) 

# Create the Q-Q plot
qqnorm(residuals(anova_results))
qqline(residuals(anova_results))

# Check for homogeneity of variances using a plot of residuals vs. fitted values
plot(fitted(anova_results), residuals(anova_results), 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")


# Create an interaction plot using interaction.plot()
interaction.plot(x.factor = nn.data$pattern, 
                 trace.factor = nn.data$activation, 
                 response = nn.data$test.error,
                 type = "b",                 
                 col = c("red", "green", "blue", "purple"), 
                 pch = c(19, 17, 15, 16),
                 xlab = "Pattern",          
                 ylab = "Test Error",         
                 main = "Interaction Plot: Pattern vs Test Error by Activation")

# Calculate the mean for each activation function and round to two digits
activation_means <- aggregate(test.error ~ activation, data = nn.data, FUN = mean)

# Round the means to 2 decimal places
activation_means$test.error <- round(activation_means$test.error, 2)

activation_means


