## Load the required libraries
##For winsorizing
install.packages("scipub");
##For winsorizing
install.packages("robustHD");
library('robustHD');

##For VIF
install.packages('car', dependencies=TRUE);
install.packages('abind');
install.packages("car", dependencies = "Imports");

##For VIF
install.packages('Rtools', 'Datacar');
install.packages("caTools");

##For marginal effects in matching analysis
install.packages('marginaleffects');

##For latex output
library(xtable);

##For matchit
library(MatchIt);
library(marginaleffects);

library(readxl)
library(dplyr)
library(readr)
library(car)
library(datawizard)

# Load the data
bond_data <- read_excel("Bond_data.xlsx")

# Count how many different issue dates are in the data
length(unique(bond_data$`Issue Date`))

# Display the structure and head of the original data
str(bond_data)
head(bond_data)

## Data Cleaning
bond_data_clean <- bond_data %>%
  mutate(
    Maturity = as.Date(Maturity, format = "%d/%m/%Y"), # Adjust the date format if needed
    Coupon = as.numeric(Coupon),
    Yield_to_Maturity = as.numeric(`Yield to Maturity`),
    Amount_Outstanding_USD = as.numeric(`Amount Outstanding (USD)`),
    Issue_Date = as.Date(`Issue Date`, format = "%d/%m/%Y"), # Adjust the date format if needed
    Country_of_Issue = as.factor(`Country of Issue`),
    ESG_Bond = ifelse(`ESG Bond` == "Yes", 1, 0),
    Bond_Grade = ifelse(is.na(`Bond Grade`), "Not labeled", `Bond Grade`)
  ) %>%
  select(-`ESG Bond Type`, -`Green Bond`, -Ticker, -`Amount Outstanding (USD)`,
         -`Principal Currency`, -`Bond Grade`, -`Country of Issue`,-`Issue Date`,  -`Amount Issued (USD)`, -`Yield to Maturity`)

colSums(is.na(bond_data_clean))

## Remove rows with NA values in Yield to Maturity and ESG Bond
bond_data_clean <- bond_data_clean %>%
  filter(!is.na(Yield_to_Maturity), !is.na(ESG_Bond), !is.na(Amount_Outstanding_USD), !is.na(Maturity));

#bond_data_clean <- bond_data_clean %>%
#  filter(!is.na(ISIN))
##There are some observations for which amount outstanding is equal to zero, we have to remove them as well cause later we're gonna take it's logarithm
bond_data_clean <- subset(bond_data_clean, bond_data_clean$Amount_Outstanding_USD != 0);


## Create new columns and convert data types
bond_data_clean$Issue_Year <- as.factor(format(bond_data_clean$Issue_Date, "%Y"))
bond_data_clean$Days_to_Maturity <- as.numeric(difftime(bond_data_clean$Maturity, as.Date('2024-04-24', format="%Y-%m-%d"), units = "days"))
bond_data_clean$ln_Amount_Outstanding_USD <- log(bond_data_clean$Amount_Outstanding_USD);
bond_data_clean <- bond_data_clean %>%
  mutate(across(c(Sector, Bond_Grade, Seniority, ESG_Bond), as.factor))

####Regression Analysis
#From bond_data_clean, create a new data frame called bond_data_reg for the regression analysis
bond_data_reg <- bond_data_clean %>%
  select(Coupon, ln_Amount_Outstanding_USD, Yield_to_Maturity, ESG_Bond, Bond_Grade, Days_to_Maturity, Issue_Year, Sector, Country_of_Issue, Seniority)

# Run summary statistics and do winsorization at 5% and 95%
summary(bond_data_reg)
#########to_winsorize <- bond_data_reg$Yield_to_Maturity
#########winsorized_dat <- winsorize(to_winsorize, probs = c(0.05, 0.95))

######### Replace the original values with the winsorized values and check for missing values again before running the regression
#########bond_data_reg$Yield_to_Maturity <- winsorized_dat
#########colSums(is.na(bond_data_reg))
str(bond_data_reg)

## Do the regression analysis
reg <- lm(Yield_to_Maturity ~ ., data = bond_data_reg)
summary(reg)

##Checking for outliers
hats <- as.data.frame(hatvalues(reg));

## Check for multicollinearity, all
vif_values <- vif(reg)
print(vif_values)

####Matching Analysis
#From bond_data_clean, create a new data frame called bond_data_match for the matching analysis
bond_data_match<-bond_data_clean %>%
  select(Issuer, Coupon, ln_Amount_Outstanding_USD, Yield_to_Maturity, ESG_Bond, Bond_Grade, Days_to_Maturity, Issue_Date)
#Treat issuers as factors
bond_data_match$Issuer<-as.factor(bond_data_match$Issuer);
############Winsorizing again
############to_winsorize <- bond_data_match$Yield_to_Maturity
############winsorized_dat <- winsorize(to_winsorize, probs = c(0.05, 0.95))
#
#bond_data_match$Yield_to_Maturity <- winsorized_dat
colSums(is.na(bond_data_match))
str(bond_data_match)
##here comes the matching
matched <- matchit(ESG_Bond ~ Issuer + Coupon + ln_Amount_Outstanding_USD + Bond_Grade + Days_to_Maturity + Issue_Date,
data=bond_data_match, exact=c("Issuer", "Bond_Grade") , method="nearest" , distance='glm');
#
#Warning message:
#Fewer control units than treated units in some `exact` strata; not all treated units will get a match. 
summary(matched);
matched;

jpeg('eqq_coupon.jpg', width=750, height=750);
plot(matched, type="qq", which.xs = ~ Coupon);
dev.off();

jpeg('eqq_lnamount.jpg', width=750, height=750);
plot(matched, type="qq", which.xs = ~ ln_Amount_Outstanding_USD);
dev.off();

jpeg('eqq_mat.jpg', width=750, height=750);
plot(matched, type="qq", which.xs = ~ Days_to_Maturity);
dev.off();

##Analysis with the matched data set
md<-match.data(matched);
mdfit<-lm(Yield_to_Maturity ~ ESG_Bond, data=md, weights=weights);

avg_comparisons(mdfit, variables= "ESG_Bond", vcoc=~subclass, newdata=subset(md, ESG_Bond == 1), wts="weights");

