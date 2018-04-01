#################################################################################################################################################################
##############################################################      BFSI CAPSTONE PROJECT     ############################################################################
#################################################################################################################################################################

#Loading the required set of libraries
library(VIM)
library(ggplot2)
library(dplyr)
library(cowplot)
library(corrplot)

#Loading the input datasets

#Loading the CB Data
CB_Data  <-read.csv('CB_Data.csv',stringsAsFactors = FALSE)
nrow(CB_Data)
str(CB_Data)
length(unique(CB_Data$Application.ID))

##There are 3 duplicate records in the dataframe,removing the duplicate values

CB_Data<-CB_Data[!(duplicated(CB_Data$Application.ID)),]

#Loading the Demo Data

Demo_Data<-read.csv('Demo_Data.csv',stringsAsFactors = FALSE)
nrow(Demo_Data)
str(Demo_Data)
length(unique(Demo_Data$Application.ID))

##There are 3 duplicate records in the dataframe,removing the duplicate values

Demo_Data<-Demo_Data[!(duplicated(Demo_Data$Application.ID)),]

#Combining both the datasets into Final Dataset

Final_Data  <-merge(CB_Data,Demo_Data,by.x ='Application.ID',by.y = 'Application.ID' )
Final_Data$Performance.Tag.y<-NULL

#Changing the Column names

colnames(Final_Data)<-c("App_ID","DPD_90_6","DPD_60_6",
                        "DPD_30_6","DPD_90_12","DPD_60_12",
                        "DPD_30_12","CC_util_12","Trade_6",
                        "Trade_12","Trade_PL_6","Trade_PL_12",
                        "Inq_6","Inq_12","Home_Loan","Out_Bal",
                        "No_of_trades","Auto_Loan","Perf_Tag",
                        "Age","Gender","Marital_Status","Dependents",
                        "Income","Education","Profession",
                        "Residence","Residence_mon","Company_mon")


#Checking the distribution of Target Variables

prop.table(table(Final_Data$Perf_Tag))

#     0          1 
#0.95782046 0.04217954

ggplot(Final_Data,aes(factor(Perf_Tag))) + geom_bar() + labs(x="Performance Tag",y="No of Applicants") +
ggtitle("Dependent Variables Distribution")

#Finding the columns containing the Missing Values

Missing_Val<-as.data.frame(sapply(Final_Data,function(x) sum(length(which(is.na(x))))))

######################   Performing EDA Analysis   ######################

##Categorical Varaibles Box Plot

title <- ggdraw() + draw_label("Categorical Variables Range Plot", fontface='bold')

cat_plot1<-plot_grid(ggplot(Final_Data,aes(x="",y=Age))       + geom_boxplot() + theme_bw() + labs(x="Age",y="No of Applicants"),
                     ggplot(Final_Data,aes(x="",y=Out_Bal))   + geom_boxplot() + theme_bw() + labs(x="Outstanding Balance",y="No of Applicants"),
                     ggplot(Final_Data,aes(x="",y=Income))    + geom_boxplot() + theme_bw() + labs(x="Income",y="No of Applicants"),
                     ggplot(Final_Data,aes(x="",y=Residence_mon))   + geom_boxplot() + theme_bw() + labs(x="Residence Month",y="No of Applicants"),
                     ggplot(Final_Data,aes(x="",y=CC_util_12))      + geom_boxplot() + theme_bw() + labs(x="Credit Card Utilization",y="No of Applicants"),align = 'h')
Cat_title_plot<-plot_grid(title,cat_plot1,ncol=1, rel_heights=c(0.1, 1))
Cat_title_plot

##Categorical Variables Vs Performance Tag Plot

title <- ggdraw() + draw_label("Categorical Variables Vs Performance Tag", fontface='bold')
cat_plot1<-plot_grid(ggplot(Final_Data,aes(x=factor(Perf_Tag),fill=Education))         +geom_bar(position = "dodge") +labs(x="Performance Tag",y="No of Applicants")+theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Perf_Tag),fill=Profession))        +geom_bar(position = "dodge") +labs(x="Performance Tag",y="No of Applicants")+theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Perf_Tag),fill=Marital_Status))    +geom_bar(position = "dodge") +labs(x="Performance Tag",y="No of Applicants")+theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Perf_Tag),fill=Residence))         +geom_bar(position = "dodge") +labs(x="Performance Tag",y="No of Applicants")+theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Perf_Tag),fill=Gender))            +geom_bar(position = "dodge") +labs(x="Performance Tag",y="No of Applicants")+theme_bw(),align = 'h')
Cat_title_plot<-plot_grid(title,cat_plot1,ncol=1, rel_heights=c(0.1, 1))
Cat_title_plot

##Categorical Variables Plot

title    <- ggdraw() + draw_label("Categorical Variables Plot", fontface='bold')
cat_plot1<-plot_grid(ggplot(Final_Data,aes(Profession,fill=Profession)) +geom_bar() + theme_light(),
          ggplot(Final_Data,aes(Marital_Status,fill=Marital_Status)) +geom_bar() + theme_light(),
          ggplot(Final_Data,aes(Gender,fill=Gender)) +geom_bar() + theme_light(),
          ggplot(Final_Data,aes(Education,fill=Education)) +geom_bar() +coord_flip() + theme_light(),
          ggplot(Final_Data,aes(Residence,fill=Residence)) +geom_bar() +coord_flip() + theme_light(),align = 'h')
Cat_title_plot<-plot_grid(title,cat_plot1,ncol=1, rel_heights=c(0.1, 1))
Cat_title_plot

#The highest number of applicants are Professional,masters,bachelors.
#The highest number are that of Salaried Professionals
#High majority of the population is Married
#Most of the applicants are living in a rented house followed by Owned
#Company provided and Living with Parents are low in number
#Majority of the applicants are Male

##Plotting the Age Variable

ggplot(Final_Data,aes(Age,fill=Age))+ geom_histogram(binwidth = 1,col="red",fill= "Turquoise") + 
  labs(x="Age Range",y="No of Applicants") + theme_bw() + ggtitle("Age Plot")

# Higher Number of Applicants have Age ranging 35 to 65

ggplot(Final_Data,aes(CC_util_12,fill=CC_util_12))+ geom_histogram(binwidth = 1,col="red",fill= "Turquoise") + 
labs(x="Credit Card Utilisation Range",y="No of Applicants") + theme_bw() + ggtitle(" CC Utilisation Plot")

#The Credit Card Utilisation Range is left skewed with high number of Applicants having CC below 30
#Another observation is the capping off of the CC_Util value to 113 for values higher than 113 as well

ggplot(Final_Data,aes(factor(Home_Loan),fill=Home_Loan))+ geom_bar(col="red",fill= "Turquoise") +
labs(x="Home Loan",y="No of Applicants") + theme_bw()

#Outstaning Balance Binning and Plotting

breaks<-c(seq(0,5500000,500000))
labels <-c("<5","5-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50","51-55")
Final_Data$Out_bal.cat<-cut(Final_Data$Out_Bal,breaks,labels = labels)

ggplot(Final_Data,aes(Out_bal.cat,fill=Out_bal.cat))+geom_histogram(stat = "count")

##Most of the Applicants have outstaning balance less than 10L

#Income Binning and Plotting

breaks1<-c(seq(0,65,5))
labels1 <-c("<5","5-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50","51-55","56-60","61-65")
Final_Data$income.cat<-cut(as.integer(Final_Data$Income),breaks1,labels = labels1)
ggplot(Final_Data,aes(income.cat,fill=income.cat))+ geom_histogram(stat = "count") + ggtitle("Income Plot")

#Residence Month Binning and Plotting

breaks1<-c(seq(0,130,10))
labels1 <-c("<10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100","101-110","111-120","121-130")

Final_Data$Residence_mon.cat<-cut(Final_Data$Residence_mon,breaks1,labels = labels1)
ggplot(Final_Data,aes(Residence_mon.cat,fill=Residence_mon.cat))+ geom_histogram(stat = "count") + 
ggtitle("Residence Month Plot")

#Majority of the Applicants have residence stay lower than 10 minths and very few have it higher than 120 months

#Company Month Binning and Plotting

breaks1<-c(seq(0,140,10))
labels1 <-c("<10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100","101-110","111-120","121-130","131-140")

Final_Data$Company_mon.cat<-cut(Final_Data$Company_mon,breaks1,labels = labels1)
ggplot(Final_Data,aes(Company_mon.cat,fill=Company_mon.cat))+ geom_histogram(stat = "count") + ggtitle("Company Months Plot")

##There are very few applicants which have company months greater than 75
##These seem to be outliers

ggplot(Final_Data,aes(x=Dependents,fill=Dependents)) + geom_bar() + ggtitle("Dependents Plot")

##The no of Dependents are evenly distributed,with applicants with 3 dependents slightly higher than the others

title <- ggdraw() + draw_label("DPD under 6 Months Defualts", fontface='bold')

cat_plot1<-plot_grid(ggplot(Final_Data,aes(x=factor(DPD_90_6),fill=DPD_90_6)) + geom_bar() + labs(x="DPD_90_6",y="Count") + theme_bw(),
                     ggplot(Final_Data,aes(x=factor(DPD_30_6),fill=DPD_30_6)) + geom_bar() + labs(x="DPD_30_6",y="Count") + theme_bw(),
                     ggplot(Final_Data,aes(x=factor(DPD_60_6),fill=DPD_60_6)) + geom_bar() + labs(x="DPD_60_6",y="Count") + theme_bw(),align='h')

Cat_title_plot<-plot_grid(title,cat_plot1,ncol=1, rel_heights=c(0.1, 1))
Cat_title_plot

##DPD count for the applicants under 6 months seem to be very low for the values >= 3

title <- ggdraw() + draw_label("DPD under 12 Months Defualts", fontface='bold')

cat_plot1<-plot_grid(ggplot(Final_Data,aes(x=factor(DPD_90_12),fill=DPD_90_12)) + geom_bar() + labs(x="DPD_90_12",y="Count") + theme_bw(),
                     ggplot(Final_Data,aes(x=factor(DPD_30_12),fill=DPD_30_12)) + geom_bar() + labs(x="DPD_30_12",y="Count") + theme_bw(),
                     ggplot(Final_Data,aes(x=factor(DPD_60_12),fill=DPD_60_12)) + geom_bar() + labs(x="DPD_60_12",y="Count") + theme_bw(),align='h')

Cat_title_plot<-plot_grid(title,cat_plot1,ncol=1, rel_heights=c(0.1, 1))
Cat_title_plot

##DPD count for the applicants under 12 months seem to be very low for the values >= 4


title <- ggdraw() + draw_label("Trade Plots", fontface='bold')

cat_plot1<-plot_grid(ggplot(Final_Data,aes(x=factor(Trade_6),fill=Trade_6)) + geom_bar() + labs(x="Trade_6",y="Count") + theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Trade_12),fill=Trade_12)) + geom_bar() + labs(x="Trade_12",y="Count") + theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Trade_PL_6),fill=Trade_PL_6)) + geom_bar() + labs(x="Trade_PL_6",y="Count") + theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Trade_PL_12),fill=Trade_PL_12)) + geom_bar() + labs(x="Trade_PL_12",y="Count") + theme_bw(),align='h')

Cat_title_plot<-plot_grid(title,cat_plot1,ncol=1, rel_heights=c(0.1, 1))
Cat_title_plot

##DPD count for the applicants under 12 months seem to be very low for the values >= 4

title <- ggdraw() + draw_label("Inquiry Plots", fontface='bold')

cat_plot1<-plot_grid(ggplot(Final_Data,aes(x=factor(Inq_6),fill=Inq_6))   + geom_bar() + labs(x="Inq_6",y="Count")  + theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Inq_12),fill=Inq_12)) + geom_bar() + labs(x="Inq_12",y="Count") + theme_bw(),align = 'h')
Cat_title_plot<-plot_grid(title,cat_plot1,ncol=1, rel_heights=c(0.1, 1))
Cat_title_plot

##Auto Loan

ggplot(Final_Data,aes(factor(Auto_Loan),fill=Auto_Loan))+ geom_bar(col="red",fill= "Turquoise") + 
labs(x="Auto Loan",y="No of Applicants") + theme_bw()

##Number of Trades

ggplot(Final_Data,aes(x=No_of_trades,fill=factor(No_of_trades)))+
geom_histogram(stat = "count") + labs(x="Trade Range",y="Number of Trades") + ggtitle("Trade Range Plot")


##Bi-Variate Analysis

ggplot(Final_Data,aes(x=Age,fill=Perf_Tag))+ geom_bar(fill= "Turquoise") + 
labs(x="Age Range",y="No of Applicants") + theme_bw()

ggplot(Final_Data, aes(Age, ..count..)) + geom_bar(aes(fill = factor(Perf_Tag)), position = "dodge")

breaks1<-c(seq(0,75,10))
labels1 <-c("<10","11-20","21-30","31-40","41-50","51-60","61-70")
Final_Data$Age.cat<-cut(Final_Data$Age,breaks1,labels = labels1)

##PLotting Performance Tag against the Numeric Values:

##Performance Tag Vs Age Value

ggplot(Final_Data, aes(Age.cat, ..count..)) + geom_bar(aes(fill = factor(Perf_Tag)), position = "dodge")

##Performance Tag Vs Residence Value

ggplot(Final_Data, aes(Residence_mon.cat, ..count..)) + geom_bar(aes(fill = factor(Perf_Tag)), position = "dodge")

##Performance Tag Vs Outbalance Category

ggplot(Final_Data, aes(Out_bal.cat, ..count..)) + geom_bar(aes(fill = factor(Perf_Tag)), position = "dodge")

##Applicants with Outstanding balance less than 15 lcas default the most,Age group 31-40 comes next

##Performance Tag Vs Company_mon.cat

ggplot(Final_Data, aes(Company_mon.cat, ..count..)) + geom_bar(aes(fill = factor(Perf_Tag)), position = "dodge")

##Company Months have defaulters distributed for each of the range from 1-70

##Performance Tag Vs Income.cat

ggplot(Final_Data, aes(income.cat, ..count..)) + geom_bar(aes(fill = factor(Perf_Tag)), position = "dodge")

##Number of Trades

ggplot(Final_Data, aes(No_of_trades, ..count..)) + geom_bar(aes(fill = factor(Perf_Tag)), position = "dodge") +
ggtitle("No of Trades Vs Performance Tag")

##The Performance value count is greater for the applicants with trades less than 10

ggplot(Final_Data, aes(factor(Auto_Loan), ..count..)) + geom_bar(aes(fill = factor(Perf_Tag)), position = "dodge")

##People with no Auto Loan seem to default more than those having it

#Below Columns contain missing values

#Performance.Tag
#Avgas.CC.Utilization.in.last.12.months
#No.of.trades.opened.in.last.6.months
#Presence.of.open.home.loan
#Outstanding.Balance
#No.of.dependents

#Eliminating rows with NA Values from the Performance Tag
#and assigning the rejected column values to Reject Data

Rej_Data<-Final_Data[is.na(Final_Data$Perf_Tag),]

Final_Data<-Final_Data[!is.na(Final_Data$Perf_Tag),]

#Dependants as NA

depend_na<-Final_Data[is.na(Final_Data$Dependents),]

#It can be seen that all the above dependents are Married and 2 of them are Female and 1 is Male
#Finding the average no of dependenats for the combinations as above

depend_avg<-aggregate(cbind(Dependents)~ Marital_Status + Gender,data= Final_Data, mean,na.rm = TRUE)

#The Average number of Dependents for both the cases is 3,hence rounding of the no of dependents to 3

Final_Data[is.na(Final_Data$Dependents),]$Dependents<-3

##Trade_6 has 1 value with NA.

Trade6_NA<-Final_Data[is.na(Final_Data$Trade_6),]

##Replacing it with the Average value of the entire datasets.

mean(Final_Data$Trade_6,na.rm = TRUE)
Final_Data[is.na(Final_Data$Trade_6),]$Trade_6<-2

##Checking the Home_loan column which has 272 missing values

home_loan_na<-Final_Data[is.na(Final_Data$Home_Loan),]

##Outstanding Balance of the Customer

Out_bal_na<-Final_Data[is.na(Final_Data$Out_Bal),]

home_loan_avg<-aggregate(cbind(Home_Loan)~Residence + Profession,data= Final_Data, mean,na.rm = TRUE)

## Using K-NN Imputation to calculate the missing values of Outstanding balance & Credit_Card Utilisation .

Final_Data   <-kNN(Final_Data, variable = c("Out_Bal"), k=6)
Final_Data   <-kNN(Final_Data, variable = c("CC_util_12","Home_Loan"),k=6)

####################   Performing EDA Analysis   ######################

#################### Exploring Numerical Columns ######################

##Finding Numeric Columns which aren't suppose to contain negative values

nums<-sapply(Final_Data,is.numeric)
final_data_num<-Final_Data[,nums]
myNames<-names(final_data_num)[sapply(final_data_num, function(x) min(x))<0]
View(myNames)

#The columns Age and Income have negative values

#Exploring Age Column

ggplot(Final_Data,aes(Age))+geom_histogram()

#there are applicants with age LTE 0
#Extracting the rows for the respective scenario's

Age_Less_18<-Final_Data[Final_Data$Age<18,]

# We have 65 rows with people having age less than 18
# Looking at the entire dataset,the important factors which might help
# determine the correct age is Education,Marital Status
# and 'no of months in a company'.

#There is one row with Age as -3,finding out the correct approximate age by using the factors
#as mentioned above

subset_age<-subset(Final_Data,Final_Data$Education=="Masters" & Final_Data$Marital_Status=="Married" & Final_Data$Company_mon>=35 &Final_Data$Company_mon<=40)

#calculating the Average & Median age from the above dataset

mean(subset_age$Age)
median(subset_age$Age)

#The Mean and Average both seem very close,46 & 47 respectively
#hence the Age for -3 can be replaced with 46

Final_Data[Final_Data$Age==-3,]$Age<-46

#Grouping the Age variable w.r.t Education and Marital Status

a1<-aggregate(cbind(Age)~Education + Marital_Status,data= Final_Data, mean,na.rm = TRUE)

#removing the rest of the rows with Age < 18 from the Main Final Dataset

Final_Data<-Final_Data[!Final_Data$Age<18,]

#Replacing the age of each of the below categories derived based on Education and Marital Status
#derived from a1 dataset values

Age_Less_18[Age_Less_18$Education=="Bachelor" & Age_Less_18$Marital_Status=="Married",]$Age <- 46
Age_Less_18[Age_Less_18$Education=="Phd"      & Age_Less_18$Marital_Status=="Married",]$Age <- 46
Age_Less_18[Age_Less_18$Education=="Masters"  & Age_Less_18$Marital_Status=="Married",]$Age <- 46
Age_Less_18[Age_Less_18$Education=="Bachelor" & Age_Less_18$Marital_Status=="Single",] $Age <- 39
Age_Less_18[Age_Less_18$Education=="Phd"      & Age_Less_18$Marital_Status=="Single",] $Age <- 39
Age_Less_18[Age_Less_18$Education=="Masters"  & Age_Less_18$Marital_Status=="Single",] $Age <- 39
Age_Less_18[Age_Less_18$Education=="Professional" & Age_Less_18$Marital_Status=="Single",] $Age <- 39
Age_Less_18[Age_Less_18$Education=="Professional" & Age_Less_18$Marital_Status=="Married",]$Age <- 46
Age_Less_18[Age_Less_18$Education=="Phd" & Age_Less_18$Marital_Status=="",]$Age<-46

#Combining the Age_less_18 dataframe back with the Final Dataset

Final_Data<-rbind(Final_Data,Age_Less_18)

#Exploring Income Column

ggplot(Final_Data,aes(Income))+geom_histogram()

#There appear to be income less than 3 Lacs,
#which is not desirable for the companies to issue a Credit Card

Income_Less_3<-Final_Data[Final_Data$Income<3,]
a2 <- aggregate(cbind(Income)~Education + Profession ,data= Final_Data, mean,na.rm = TRUE)


#There are 134 rows of people with the Income < 3 Lacs
#There are data quality issues with the Income varaible as many of the applicants
#have income less than 0
#Replacing the age of each of the below categories derived based on Education and Marital Status
#derived from a1 dataset values

Income_Less_3[Income_Less_3$Education=="Bachelor" & Income_Less_3$Profession=="SAL",]$Income <- 27
Income_Less_3[Income_Less_3$Education=="Phd"      & Income_Less_3$Profession=="SAL",]$Income <- 28
Income_Less_3[Income_Less_3$Education=="Masters"  & Income_Less_3$Profession=="SAL",]$Income <- 27
Income_Less_3[Income_Less_3$Education=="Professional" & Income_Less_3$Profession=="SAL",]$Income <- 27 
Income_Less_3[Income_Less_3$Education=="Bachelor" & Income_Less_3$Profession=="SE_PROF",]$Income <- 27
Income_Less_3[Income_Less_3$Education=="Phd"      & Income_Less_3$Profession=="SE_PROF",]$Income <- 26
Income_Less_3[Income_Less_3$Education=="Masters"  & Income_Less_3$Profession=="SE_PROF",]$Income <- 27
Income_Less_3[Income_Less_3$Education=="Professional"  & Income_Less_3$Profession=="SE_PROF",]$Income <- 28
Income_Less_3[Income_Less_3$Education=="Bachelor" & Income_Less_3$Profession=="SE",]$Income <- 27
Income_Less_3[Income_Less_3$Education=="Masters"  & Income_Less_3$Profession=="SE",]$Income <- 27
Income_Less_3[Income_Less_3$Education=="Professional"  & Income_Less_3$Profession=="SE",]$Income <- 27
Income_Less_3[Income_Less_3$Education=="Others"  & Income_Less_3$Profession=="SAL",]$Income <- 25
Income_Less_3[Income_Less_3$Education=="Professional" & Income_Less_3$Profession=="",]$Income<- 15

#removing the rest of the rows with Age < 18 from the Main Final Dataset

Final_Data<-Final_Data[!Final_Data$Income<3,]
Final_Data<-rbind(Final_Data,Income_Less_3)

##Checking for the space vaues in the COlumns

final_space<-sapply(Final_Data,function(x) sum(x==""))
View(as.data.frame(final_space))

##Gender,Education,Marital_Status,Profession & Residence has Empty values in the dataset

space_col<-Final_Data[Final_Data$Marital_Status=="",]
View(space_col)

##Using the Age Vs Marital Status column

ggplot(Final_Data, aes(Age.cat, ..count..)) + geom_bar(aes(fill = factor(Marital_Status)), position = "dodge")

##Most of the people in the age group of 41-50 seem to be married

##Assgining the marital_Status to Married for the space value range:

Final_Data[Final_Data$Marital_Status=="",]$Marital_Status<-"Married"

##Residence

space_col<-Final_Data[Final_Data$Residence=="",]
View(space_col)

space_col<-Final_Data[Final_Data=="",]

##Residents Vs Income

ggplot(Final_Data, aes(income.cat, ..count..)) + geom_bar(aes(fill = factor(Residence)), position = "dodge")
ggplot(Final_Data, aes(Age.cat, ..count..))    + geom_bar(aes(fill = factor(Residence)), position = "dodge")
ggplot(Final_Data, aes(Dependents, ..count..))    + geom_bar(aes(fill = factor(Residence)), position = "dodge")
ggplot(Final_Data, aes(Profession, ..count..))    + geom_bar(aes(fill = factor(Residence)), position = "dodge")
ggplot(Final_Data, aes(factor(Home_Loan), ..count..))    + geom_bar(aes(fill = factor(Residence)), position = "dodge")
ggplot(Final_Data, aes(factor(Auto_Loan), ..count..))    + geom_bar(aes(fill = factor(Residence)), position = "dodge")
ggplot(Final_Data, aes(Profession)) + geom_bar(aes(fill = factor(Education)), position = "dodge")


ggplot(Final_Data, aes(Age.cat, ..count..)) + geom_bar(aes(fill = factor(Education)), position = "dodge")

ggplot(Final_Data, aes(Education)) + geom_bar(aes(fill = factor(Profession)), position = "dodge")
ggplot(Final_Data, aes(Education)) + geom_bar(aes(fill = factor(Profession)), position = "dodge")


### Univariate Analysis ###

##Education Variable

title <- ggdraw() + draw_label("Categorical Variables Plot Vs Performance Tag", fontface='bold')

ggplot(Final_Data,aes(x="",y=Age))   +geom_boxplot() +labs(x="Performance Tag") +theme_bw()

cat_plot1<-plot_grid(ggplot(Final_Data,aes(x="",y=Age))       + geom_boxplot() + theme_bw() + labs(x="Age",y="No of Applicants"),
                     ggplot(Final_Data,aes(x="",y=Out_Bal))   + geom_boxplot() + theme_bw() + labs(x="Outstanding Balance",y="No of Applicants"),
                     ggplot(Final_Data,aes(x="",y=Income))    + geom_boxplot() + theme_bw() + labs(x="Income",y="No of Applicants"),
                     ggplot(Final_Data,aes(x="",y=Residence_mon))   + geom_boxplot() + theme_bw() + labs(x="Residence Month",y="No of Applicants"),
                     ggplot(Final_Data,aes(x="",y=CC_util_12))      + geom_boxplot() + theme_bw() + labs(x="Credit Card Utilization",y="No of Applicants"),align = 'h')
Cat_title_plot<-plot_grid(title,cat_plot1,ncol=1, rel_heights=c(0.1, 1))
Cat_title_plot


ggplot(Final_Data,aes(Education,fill=Education)) +geom_bar() +
scale_y_continuous(name = "No of Applicants", breaks = seq(0,nrow(Final_Data),4000), limits = c(0,nrow(Final_Data)))

#The highest number of applicants are Professional,masters,bachelors,PHD,others

##Profession variable

ggplot(Final_Data,aes(Profession,fill=Profession)) +geom_bar() +
scale_y_continuous(name = "No of Applicants", breaks = seq(0,nrow(Final_Data),4000), limits = c(0,nrow(Final_Data)))

#The highest number are that of Salaried Professionals

##Marital Status

ggplot(Final_Data,aes(Marital_Status,fill=Marital_Status)) +geom_bar() +
scale_y_continuous(name = "No of Applicants", breaks = seq(0,nrow(Final_Data),4000), limits = c(0,nrow(Final_Data)))

#High majority of the population is Married

##Residence

ggplot(Final_Data,aes(Residence,fill=Residence)) +geom_bar() +
scale_y_continuous(name = "No of Applicants", breaks = seq(0,nrow(Final_Data),4000), limits = c(0,nrow(Final_Data)))

#Most of the applicants are living in a rented house followed by Owned
#Company provided and Living with Parents are low in number

##Gender

ggplot(Final_Data,aes(Gender,fill=Gender)) +geom_bar() +
scale_y_continuous(name = "No of Applicants", breaks = seq(0,nrow(Final_Data),4000), limits = c(0,nrow(Final_Data)))

#Majority of the applicants are Male

##Plotting the Age Variable

ggplot(Final_Data,aes(Age,fill=Age))+ geom_histogram(binwidth = 1,col="red",fill= "Turquoise") + 
labs(x="Age Range",y="No of Applicants") + theme_bw()

# Higher Number of Applicants have Age ranging 35 to 65

ggplot(Final_Data,aes(CC_util_12,fill=CC_util_12))+ geom_histogram(binwidth = 1,col="red",fill= "Turquoise") + 
labs(x="Credit Card Utilisation Range",y="No of Applicants") + theme_bw()

ggplot(Final_Data,aes(factor(Home_Loan),fill=Home_Loan))+ geom_bar(col="red",fill= "Turquoise") + 
labs(x="Home Loan",y="No of Applicants") + theme_bw()

ggplot(Final_Data,aes(fill=factor(Home_Loan),x=Perf_Tag)) +
geom_bar(col="red",fill= "Turquoise",position = "dodge")  +
labs(x="Home Loan",y="No of Applicants") + 
theme_bw()


#The distribution of the Credit card utilisation is highly skewed,with the applicants between 0-25 are very high

View(head(Final_Data))

####  Performing Bivariate Analysis Vs Performance Tag #########

##Education Variable

title <- ggdraw() + draw_label("Categorical Variables Plot Vs Performance Tag", fontface='bold')
cat_plot1<-plot_grid(ggplot(Final_Data,aes(x=factor(Perf_Tag),fill=Education))         +geom_bar(position = "dodge") +labs(x="Performance Tag",y="No of Applicants")+theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Perf_Tag),fill=Profession))        +geom_bar(position = "dodge") +labs(x="Performance Tag",y="No of Applicants")+theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Perf_Tag),fill=Marital_Status))    +geom_bar(position = "dodge") +labs(x="Performance Tag",y="No of Applicants")+theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Perf_Tag),fill=Residence))         +geom_bar(position = "dodge") +labs(x="Performance Tag",y="No of Applicants")+theme_bw(),
                     ggplot(Final_Data,aes(x=factor(Perf_Tag),fill=Gender))            +geom_bar(position = "dodge") +labs(x="Performance Tag",y="No of Applicants")+theme_bw(),align = 'h')
Cat_title_plot<-plot_grid(title,cat_plot1,ncol=1, rel_heights=c(0.1, 1))
Cat_title_plot

#Professionals,Masters & Bachelor default the most
#Salaried,SE defualt the most
#Married population are high defaulters
#Rented house applicants default the most
#Male Applicants default rate is high

##Age Vs Performance Tag using Jitter Plot

ggplot(Final_Data,aes(x=factor(Perf_Tag),y=Age)) + geom_jitter()+
labs(x="Performance Tag",y="No of Applicants")+theme_bw() + ggtitle("Age Vs Performance Tag")

##Finding the correlation between the all the variables
View(Final_Data)

cor_mat<-cor(Final_Data[,-c(1,21,22,25,26,27)])
melt_plot<-melt(cor_mat)

Tile_Plot<-ggplot(data = melt_plot, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  scale_x_discrete(name = "")+ 
  scale_y_discrete(name = "") +ggtitle("Correlation Plot B/W Numeric Variables")
Tile_Plot

Final_Data_corr<-Final_Data[,-c(1,21,22,25:27,30:37)]
cor_mat<-cor(Final_Data_corr)
melt_plot<-melt(cor_mat)
View(cor_mat)

Tile_Plot<-ggplot(data = melt_plot, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  scale_x_discrete(name = "")+ 
  scale_y_discrete(name = "") +ggtitle("Correlation Plot B/W Numeric Variables")
Tile_Plot