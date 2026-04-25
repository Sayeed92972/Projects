#Libraries

library(MASS)
library(pscl)
library(knitr)
nmes_old <- read.table("F:/Study Materials/Statistical Models/Final Lab/nmes_old.dt", quote="\"", comment.char="")
colnames(nmes_old) <- c("OFP", "OFNP", "OPP", "OPNP", "EMR", "HOSP", "EXCLHLTH", "POORHLTH", "NUMCHRON", "ADLDIFF", "NOREAST", "MIDWEST", "WEST", "AGE", "BLACK", "MALE", "MARRIED", "SCHOOL", "FAMINC", "EMPLOYED", "PRIVINS", "MEDICAID")

View(nmes_old)
summary(nmes_old)

#plot(table(nmes_old$OFP))

# Descriptive statistics for continous
#Boxplot of OFP
boxplot(nmes_old$OFP,
        main = "Boxplot of Physician Office Visits (OFP)",
        ylab = "Number of Visits",
        col="blue")

#Plot the density of OFP
plot(density(nmes_old$OFP),
     main = "Density Plot of Physician Office Visits (OFP)",
     xlab = "Number of Visits",
     ylab = "Density",
     xaxt = "n")  
axis(side = 1, at = seq(0, max(nmes_old$OFP), by = 5))

#Histogram of OFP
hist(nmes_old$OFP, breaks = 0:max(nmes_old$OFP), 
     main = "Histogram of Physician Office Visits",
     xlab = "Number of Visits",
     ylab = "Frequency")

#How many patients visit physician office more than 35 times
sum(nmes_old$OFP > 35)
#How many patients not visited physician office 
sum(nmes_old$OFP == 0)
length(nmes_old$OFP)
var(nmes_old$OFP)
mean(nmes_old$OFP)
summary(nmes_old$NUMCHRON)
# Boxplot of NUMCHRON

#boxplot(nmes_old$NUMCHRON, main = "Boxplot of Number of Chronic Conditions (NUMCHRON)",xlab = "NUMCHRON (Count)", ylab = "Frequency", col = "blue")


#summary(nmes_old$AGE) 
#boxplot(nmes_old$AGE, xlab = "Age", ylab = "Years", main = "Distribution of Age")
#plot(nmes_old$AGE*10,nmes_old$OFP, xlab = "Age", ylab = "Number of Physician Office Visit", main = "Age vs Physician office visit")
#median(nmes_old$AGE)
#summary(nmes_old$AGE)
##summary(nmes_old$SCHOOL) 
##boxplot(nmes_old$SCHOOL)

##summary(nmes_old$FAMINC) 
##boxplot(nmes_old$FAMINC)

###Descriptive statistics for dummy
#table(nmes_old$EXCLHLTH)
#table(nmes_old$ADLDIFF)
#table(nmes_old$NOREAST)
#table(nmes_old$MIDWEST)
#table(nmes_old$WEST)
#table(nmes_old$BLACK)
#table(nmes_old$MALE)
#table(nmes_old$MARRIED)
#table(nmes_old$EMPLOYED)
#table(nmes_old$PRIVINS)
#table(nmes_old$MEDICAID)

# Specification of Zero-Inflated Poisson Model
zip_model <- zeroinfl(OFP ~ EXCLHLTH + POORHLTH + NUMCHRON + ADLDIFF + NOREAST + MIDWEST + WEST + AGE + BLACK + MALE + MARRIED + SCHOOL + FAMINC + EMPLOYED + PRIVINS + MEDICAID, data = nmes_old, dist = "poisson")
summary(zip_model)
#Using step function to reduce the insignificant predictors from the model
zip_model_reduced <-step(zip_model, direction = "both")
summary(zip_model_reduced)
#Dropping some predictor according to the p-value>0.05
zip_model_reduced_zi <- zeroinfl(OFP ~ EXCLHLTH + POORHLTH + NUMCHRON + ADLDIFF + NOREAST + WEST + AGE + MARRIED + SCHOOL + EMPLOYED + PRIVINS + MEDICAID | NUMCHRON + AGE + MALE + MARRIED + SCHOOL + PRIVINS + MEDICAID, data = nmes_old, dist = "poisson")
summary(zip_model_reduced_zi)
#Using AIC to compare the models
AIC(zip_model)
AIC(zip_model_reduced)
AIC(zip_model_reduced_zi)

logLik(zinb_model)
logLik(zinb_model_reduced)
logLik(zinb_model_reduced_zi)
#Comparing all the models of Poisson
model_comparison <- data.frame(
  Model = c("Full ZIP", "Reduced ZIP", "Reduced ZIP (ZI simplified)"),
  logLik = c(logLik(zip_model), logLik(zip_model_reduced), logLik(zip_model_reduced_zi)),
  df = c(attr(logLik(zinb_model), "df"),
         attr(logLik(zinb_model_reduced), "df"),
         attr(logLik(zinb_model_reduced_zi), "df")),
  AIC = c(AIC(zip_model), AIC(zip_model_reduced), AIC(zip_model_reduced_zi))
)

kable(model_comparison, caption = "Model Comparison: LogLik and AIC Values")
#Using likelihood ratio comparing the models
if (logLik(zip_model) > logLik(zip_model_reduced)) {
  cat("The poisson model with higher likelihood ratio is considered a better fit.\n")
} else {
  cat("The reduced poisson model with higher likelihood is considered a better fit.\n")
}
# Specification of Zero-Inflated Negative Binomial Model
zinb_model <- zeroinfl(OFP ~ EXCLHLTH + POORHLTH + NUMCHRON + ADLDIFF + NOREAST + MIDWEST + WEST + AGE + BLACK + MALE + MARRIED + SCHOOL + FAMINC + EMPLOYED + PRIVINS + MEDICAID, data = nmes_old, dist = "negbin")
summary(zinb_model)
#Using step function to reduce the insignificant predictors from the model
zinb_model_reduced <- step(zinb_model, direction="both")
summary(zinb_model_reduced)
#Dropping some predictor according to the p-value>0.05
zinb_model_reduced_zi <- zeroinfl(OFP ~ EXCLHLTH + POORHLTH + NUMCHRON + ADLDIFF + NOREAST + MIDWEST + WEST + AGE + BLACK + MALE + MARRIED + SCHOOL + FAMINC + EMPLOYED + PRIVINS + MEDICAID | NUMCHRON + AGE + MALE + MARRIED + SCHOOL + PRIVINS + MEDICAID, data = nmes_old, dist = "negbin")
summary(zinb_model_reduced_zi)
#Using AIC to compare the models
#AIC(zinb_model)
#AIC(zinb_model_reduced)
#AIC(zinb_model_reduced_zi)

#logLik(zinb_model)
#logLik(zinb_model_reduced)
#logLik(zinb_model_reduced_zi)

# Comparing the models log-likelihood ratio
if (logLik(zinb_model) > logLik(zinb_model_reduced) && logLik(zinb_model) > logLik(zinb_model_reduced_zi)) {
  cat("The full negative binomial model with higher likelihood ratio is considered a better fit.\n")
} else if (logLik(zinb_model_reduced) > logLik(zinb_model_reduced_zi)) {
  cat("The reduced negative binomial model with higher likelihood ratio is considered a better fit.\n")
} else {
  cat("The zero inflated reduced negative binomial model with higher likelihood is considered a better fit.\n")
}


#comparing the full and reduced negative binomial models
model_comparison <- data.frame(
  Model = c("Full ZINB", "Reduced ZINB", "Reduced ZINB (ZI simplified)"),
  logLik = c(logLik(zinb_model), logLik(zinb_model_reduced), logLik(zinb_model_reduced_zi)),
  df = c(attr(logLik(zinb_model), "df"),
         attr(logLik(zinb_model_reduced), "df"),
         attr(logLik(zinb_model_reduced_zi), "df")),
  AIC = c(AIC(zinb_model), AIC(zinb_model_reduced), AIC(zinb_model_reduced_zi))
)

kable(model_comparison, caption = "Model Comparison: LogLik and AIC Values")

# predict expected mean count
mu_p <- predict(zip_model_reduced, type = "response")
# sum the probabilities of a 0 count for each mean
exp_p <- sum(dpois(0, lambda = mu_p))
# predicted number of 0's
round(exp_p) 

# predict expected mean count
mu_nb <- predict(zinb_model_reduced, type = "response")
# Calculate expected number of zeros from Negative Binomial distribution
exp_nb <- sum(dnbinom(x = 0,size = zinb_model_reduced$theta, mu = mu_nb))
# predicted number of 0's
round(exp_nb) 

# predict expected mean count
mu_nb_zi <- predict(zinb_model_reduced_zi, type = "response")
# Calculate expected number of zeros from Negative Binomial distribution
exp_nb_zi <- sum(dnbinom(x = 0,size = zinb_model_reduced_zi$theta, mu = mu_nb_zi))
# predicted number of 0's
round(exp_nb_zi) 
#Look at the actual number of zeros in the response variable
sum(nmes_old$OFP == 0)


#Let's use an alternative model called hurdle model
hurd_model <- hurdle(OFP ~ EXCLHLTH + POORHLTH + NUMCHRON + ADLDIFF + NOREAST + MIDWEST + WEST + AGE + BLACK + MALE + MARRIED + SCHOOL + FAMINC + EMPLOYED + PRIVINS + MEDICAID, data = nmes_old, dist = "negbin", zero.dist = "binomial")
summary(hurd_model)
#Remove the insignificant predictors and formulating reduced hurdle model
hurd_model_reduced <- step(hurd_model, direction = "both")
summary(hurd_model_reduced)
#Simplify the hurdle model by removing non-significant variables
hurd_model_simplified <- hurdle(
  OFP ~ EXCLHLTH + POORHLTH + NUMCHRON + ADLDIFF + NOREAST + WEST + AGE + BLACK + MALE + MARRIED + SCHOOL + PRIVINS + MEDICAID, 
  zero.formula = ~ EXCLHLTH + NUMCHRON + AGE + BLACK + MALE + MARRIED + SCHOOL + PRIVINS + MEDICAID,
  data = nmes_old,
  dist = "negbin",
  zero.dist = "binomial"
)
summary(hurd_model_simplified)
# Comparing the full hurdle model and reduced hurdle models
model_comparison <- data.frame(
  Model = c("Full Hurdle", "Reduced Hurdle", "Reduced hurdle (simplified)"),
  logLik = c(logLik(hurd_model), logLik(hurd_model_reduced), logLik(hurd_model_simplified)),
  df = c(attr(logLik(hurd_model), "df"),
         attr(logLik(hurd_model_reduced), "df"),
         attr(logLik(hurd_model_simplified), "df")),
  AIC = c(AIC(hurd_model), AIC(hurd_model_reduced), AIC(hurd_model_simplified))
)

kable(model_comparison, caption = "Model Comparison: LogLik and AIC Values")


# using the likelihood
if(logLik(hurd_model)>logLik(hurd_model_reduced)){
  cat("The hurdle model is considered as a better fit than the reduced hurdle model.\n")
}else{
  cat("The reduced hurdle model is considered as a better fit than the hurdle model.\n")
}
#Comparing with respect to AIC
AIC(hurd_model, hurd_model_reduced)




install.packages("tinytex")  # if not installed yet
tinytex::install_tinytex()





























write.csv(nmes_old, "output.csv")  # CSV format
writexl::write_xlsx(nmes_old, "output.xlsx")  # Excel format (requires writexl)