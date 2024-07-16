# Project exercises in Quantitative Finance course

# Value-at-Risk, VaR

library('readxl')
library('MASS') # Multivariate normal distribution

# Importing the stocks and their prices included in our portfolio
file.name<-'FourAssets.xls'
X<-read_excel(path=file.name, col_names = TRUE) # Str = tibble
X<-as.data.frame(X) # Changing from tibble to data.frame

# Defining the amount to invest in each asset of the 4 assets
alphas<-c(4000000,3000000,1000000,2000000)

# APPROACH 1:
# HISTORICAL SIMULATION APPROACH

# Defining a function to obtain simple returns
SimpleReturn<-function(p) {
  diff(p)/p[-length(p)]
}

# Transforming stockprices to simple return
SR_HS<-apply(X=X,MARGIN=2,FUN='SimpleReturn') # applying the SimpleReturn function on the data frame (row-wise, margin = 2)
m<-dim(SR_HS)[1] # Extracting the rows

ALPHAS_HS <-matrix(rep(x=alphas,times=m),byrow=TRUE,nrow=m) # Creating a data frame to prepare for our investments' returns

M_HS<-ALPHAS_HS*SR_HS # Multiply the alphas by the returns to get the monetary amounts. We get the value of our investments

PL_HS<-apply(M_HS,MARGIN=1,FUN='sum') # Applying column-wise because we want the total value of our investments at each time period
# We get a list of the total value of our investments at each time period


cl<-0.99 # Confidence level 99 %
tail<-1-cl # Left-tail


# Computing VaR
# HS short for Historical Simulation Approach
VaR_HS<-sort(PL_HS)[floor(tail*length(PL_HS))]
VaR_HS # We are "cl" sure that we will not lose more than 'VaR_HS'!

hist(PL_HS,freq=FALSE);abline(v=VaR_HS,col=2)
# plotting a histogram




# MODEL BUILDING APPROACH
# We will be needing simple returns to construct a covaraince-variance matrix
S<-cov(SR_HS)

PLvariance<-alphas%*%S%*%matrix(alphas,ncol=1)
# Matrix-vector calculation

cl<-0.99 # Confidence level 99%
tail<-1-cl # Left-tail

# Computing VaR
# MB short for Model Building Approach
VaR_MB<-qnorm(p=tail)*sqrt(PLvariance)

VaR_MB # We are "cl" sure that we will not lose more than 'VaR_MS'!





# MONTE CARLO SIMULATION APPROACH
set.seed(1)

T<-1000
SR_MC<-mvrnorm(n=T,mu=c(0,0,0,0),Sigma=S) # We replace simple returns by T number of random draws from a multivariate normal distribution

ALPHAS_MC<-matrix(rep(x=alphas,times=T),byrow=TRUE,nrow=T) # preparaing the amounts invested again to equal the lenght of the simulated random draws

M_MC<-ALPHAS_MC*SR_MC # similar to the historical approach, computing the value of our investments after returns

PL_MC<-apply(M_MC,MARGIN=1,FUN='sum') # total value of our investments summed column-wise (PL = portfolio)

VaR_MC<-sort(PL_MC)[floor(tail*length(PL_MC))]
VaR_MC #VaR value for 99 % confidence (tail = 100 - 99 %)
# I am 99 % confident that we will not lose more than VaR_MC

hist(PL_MC,freq=FALSE);abline(v=VaR_MC,col=2)




# Conditional VaR, also known as expected short-fall from all approaches
mean(PL_HS[PL_HS<=VaR_HS]) # Expected short-fall for Historical Approach
-sqrt(PLvariance)*exp(-qnorm(p=tail)^2/2)/(sqrt(2*pi)*tail)# Expected short-fall for Model Building Approach
mean(PL_MC[PL_MC<=VaR_MC]) # Expected short-fall for Monte Carlo Simulation Approach

#This answers the question: assuming that VaR was exceeded, what is the average value below the (to the left) VaR value?
