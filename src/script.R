install.packages('MatchIt');
install.packages('tidyverse');
install.packages('readxl');
install.packages('fastDummies');
library('readxl');
library('dplyr');
library('tidyr');
library('MatchIt');
library('fastDummies');

data<-read_excel('Bond_data.xlsx');
##First things first, convert variables from the chr data type to their appropriate data types...
data$Maturity<-as.Date(data$Maturity, format="%d/%m/%Y");
data$`Issue Date`<-as.Date(data$`Issue Date`, format="%d/%m/%Y");
data$`Amount Issued (USD)`<-as.numeric(data$`Amount Issued (USD)`);
data$`Amount Outstanding (USD)`<-as.numeric(data$`Amount Outstanding (USD)`);
data$`Yield to Maturity`<-as.numeric(data$`Yield to Maturity`);

##Summary Statistics goes here:

##Now after being done with summary statistics, it's time to convert the data into the appropriate format for the analysis
##I like to keep the original data set intact, so i'll create a duplicate of the original data frame...
data_v2<-data;
data_v2$`Issue Date`<-as.numeric(format(data_v2$`Issue Date`, "%Y"));
##Let's see what we have
colSums(is.na(data_v2));
##For 90 observations, we do not have data about whether or not they are ESG, these observations are good as useless...
data_v2<-data_v2[!is.na(data_v2$`ESG Bond`),];
##Same with yield to maturity
data_v2<-data_v2[!is.na(data_v2$`Yield to Maturity`),];
##And amount issued
data_v2<-data_v2[!is.na(data_v2$`Amount Issued (USD)`),];

matchit(eq, data=data_v2, method='nearest', exact=c('Ticker','Issue Date'), );


drop_na(data_v2$`ESG Bond`);




##We shall convert the green bond variable to a boolean data type, or equivalently and integer with 1 for Yes and 0 for No
data$`Green Bond`<-ifelse(data$`Green Bond`=="Yes",1,0);
data$`ESG Bond`<-ifelse(data$`ESG Bond`=="Yes",1,0);
##If bond is of investment grade, 
str(data);
##Since there is really not much that can be done with observations for which yield to maturity and amount issued are missing, we are just gonna omit them...
data<-na.omit(data$`Yield to Maturity`);
max(data$`Yield to Maturity`, na.rm = T);
data_esg<-data[data$`ESG Bond` == 1,];
max(data_esg$`Yield to Maturity`, na.rm = T);
data_esg[data_esg$`Yield to Maturity` == 'max(data_esg$`Yield to Maturity`, na.rm = T)',];
sum(is.na(data_esg$`Yield to Maturity`));
