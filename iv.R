## ---- rcode001
library(tidyverse)
library(fixest)
set.seed(1)

## ---- rcode002
N <- 1000
x1 <- rnorm(N,mean=0,sd=10)
x2 <- rnorm(N,mean=0,sd=10)
x <- x1 + x2

## ---- rcode003
c_rel <- 0.5
z <- c_rel*x1 + rnorm(N,mean=0,sd=1)

## ---- rcode004
c_endo <- 5
e <- c_endo*x2 + rnorm(N,mean=0,sd=1)
y <- 5*x + e
data <- tibble(y=y,x=x,z=z)

## ---- rcode005
reg1 <- feols(y~x,data=data)
reg2 <- feols(y~1|x~z,data=data)
reg_hypothetical <- feols(y ~ x + e, data = data)
etable(reg1,reg2, reg_hypothetical,signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),se.below=TRUE)



##### EXTRA
# --- MODIFICATION IS HERE ---
# Include 'e' when creating the tibble
data <- tibble(y=y, x=x, z=z, e=e)
# --- END MODIFICATION ---


## ---- rcode005 (Regression Analysis - ADDING hypothetical reg)
# Original OLS (Biased)
reg1 <- feols(y~x, data=data)

# Original IV (Corrected)
reg2 <- feols(y~1 | x~z, data=data)

# Hypothetical OLS with 'e' included (Unbiased because 'e' is controlled for)
reg_hypothetical <- feols(y ~ x + e, data = data)

# Compare all three
etable(reg1, reg2, reg_hypothetical,
       signifCode=c("***"=0.01, "**"=0.05, "*"=0.10),
       se.below=TRUE)
