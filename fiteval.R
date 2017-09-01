setwd('C:/Users/jorgeibarrera/Dropbox (UFL)/Fiteval/FITEVAL_R')
source('blockbootstrap.r')
library(ggplot2)


#input
alpha <- 0.05
classlim <- c(0.65,0.8,0.9)
BiasValue <- 5



# 1) Read CSV file
data <- read.table('file:///C:/Users/jorgeibarrera/Dropbox (UFL)/Fiteval/fiteval.in', sep='\t', header=FALSE)

#Remove NAN and repeated values
#data <- subset(data,(is.numeric(data[,1]))&(is.numeric(data[,1])))

#Remove duplicated data
#data <- data[!duplicated(data),]

l <- nrow(data)
rmse_data <- rmse(data)
nse_data <- nse(data)

# 2) Generate M Bootstrap resamples (Using: *Politis Romano, 1994)
# 3) Calculate Root-mean-square-error (RMSE) and Nash-Sutcliffe-Efficienty (NSE) for each of the M resamples
Yobs <- data[,1]
Ypred <- data[,2]
indmatrix <- stationary_bootstrap(Yobs)
bootstraps_rmse <- apply(indmatrix,2,function(x) rmse(data[x,]))
bootstraps_nse <- apply(indmatrix,2,function(x) nse(data[x,]))


# 4) Construct empirical probabiliy distribution of the NSE
ecdf_nse <- ecdf(bootstraps_nse)
xi <- seq(max(0,min(bootstraps_nse)),1.001,length=1000)
fi <- ecdf_nse(xi)
ggplot()+aes(x=xi,y=fi)+geom_line(colour='blue')

very_good <- 1 - fi[xi>classlim[3]][1]
good <- 1 - fi[xi>classlim[2]][1] - very_good
acceptable <- 1 - fi[xi>classlim[1]][1] - good - very_good
unsatisfactory <- 1- very_good - good - acceptable


# 5) Assess statistical significance of RMSE and NSE, based on the 95% Confidence Interval using (Diccio and Efron, 1996)
ci_rmse <- bca(data,rmse_data,selective_rmse,bootstraps_rmse,alpha)
ci_nse <- bca(data,nse_data,selective_nse,bootstraps_nse,alpha)


# 6) Evaluate CI, Bias and Outlier:
ci_eval <- evaluate_ci(ci_nse,classlim)
bias_eval <- evaluate_bias(Yobs,Ypred,BiasValue)
outlier_eval <- evaluate_outlier(Yobs,Ypred)


# ******* Bibliography *******
# Efron and Tibshirani, 1993
# Politis Romano, 1994
# Diccio and Efron 1996