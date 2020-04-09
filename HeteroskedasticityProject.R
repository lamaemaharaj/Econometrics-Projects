# To calculate heteroskedasticity robust standard errors we will use "sandwich" package.
# Install it, if you have not done so before.


library(sandwich)  

data_emp_5_1 <- read_excel("data_emp_5_1.xlsx")

# 1.a)
ols1 = lm(Earnings ~ Height, data = data_emp_5_1)
summary(ols1)
# not statistically significant

# 1.b) calculate a 95% CI for beta_1
robust_se = sqrt(diag(vcovHC(ols1, type = "HC1")))
lower = ols1$coefficients[2] - 1.96*robust_se[2]
upper = ols1$coefficients[2] + 1.96*robust_se[2]
cat(sprintf("95%% CI is [%1.3f, %1.3f]", lower, upper))

# 2.a) for females only repeat
ols_f = lm(Earnings ~ Height, data = data_emp_5_1, 
           subset = (Sex==0))
summary(ols_f)
# not statistically significant

# 2.b) calculate a 95% CI for beta_1
robust_se_f = sqrt(diag(vcovHC(ols_f, type = "HC1")))
lower = ols_f$coefficients[2] - 1.96*robust_se_f[2]
upper = ols_f$coefficients[2] + 1.96*robust_se_f[2]
cat(sprintf("95%% CI is [%1.3f, %1.3f]", lower, upper))

# 3.a) males only repeat
ols_m = lm(Earnings ~ Height, data = data_emp_5_1, 
           subset = (Sex==1))
summary(ols_m)
# not statistically significant

# 3.b) calculate a 95% CI for beta_1
robust_se_m = sqrt(diag(vcovHC(ols_m, type = "HC1")))
lower = ols_m$coefficients[2] - 1.96*robust_se_m[2]
upper = ols_m$coefficients[2] + 1.96*robust_se_m[2]
cat(sprintf("95%% CI is [%1.3f, %1.3f]", lower, upper))

# 4) test for difference in height coefficient for males and females
# denote the difference by w_hat

w_hat = ols_f$coefficients[2] - ols_m$coefficients[2]
se_w_hat = sqrt(robust_se_f[2]^2 + robust_se_m[2]^2)

tstat = (w_hat - 0)/se_w_hat
p_val = 2*pnorm(abs(tstat), lower.tail = FALSE)
cat("t-stat is", tstat, "and its p-values is",  p_val)



