#I.Projection for housing floor area per capita

library(car)
library(carData)
library(readxl)
library(minpack.lm) # 确保已经安装
library(forecast)
# 加载数据
data <- housing_floor_area_per_capita_test
area = data$area
year = data$year
# 数据图形化分析，帮助选择合适的初始参数
plot(year, area, main = "Area vs Year", xlab = "Year", ylab = "Area")
# 使用 Levenberg-Marquardt 算法拟合模型
start_params <- list(a = 1, b = 0.001) # 调整初始参数
model <- nlsLM(area ~ 50 / (1 + (50 / 2 - 1) * exp(a * (1 - exp(b * (year - 1900))))), 
               start = start_params, trace = TRUE)
summary(model)
# 计算拟合值和残差
fitted_values <- predict(model)
residuals <- area - fitted_values
# 将残差转化为时间序列
rate <- ts(residuals, start = min(year), frequency = 1)
# 运行 auto.arima 以获得 ARIMA 参数
set.seed(123) # 设定一个固定的随机数种子以便复现结果
arima1 <- auto.arima(rate, trace = TRUE)
# 定义模拟参数
M <- 5000 # 模拟次数
# 从模型中提取参数 A 和 B
A <- model$par[1]
B <- model$par[2]
# 计算模型拟合的地面面积
fitted_area <- 50 / (1 + (50/2 - 1) * exp(A * (1 - exp(B * (year_sim - 1900)))))
# 定义模拟年份
year_sim <- seq(min(year), 2100) # 修改为您的需求
simulation_years <- length(year_sim) - length(year)
# 初始化模拟矩阵
r.sims <- ts(matrix(NA, nrow = simulation_years, ncol = M), start = min(year_sim), frequency = 1)
# 计算模型拟合的地面面积
fitted_area <- 50 / (1 + (50/2 - 1) * exp(A * (1 - exp(B * (year_sim - 1900)))))
# 循环模拟数据并填充时间序列对象
# 循环模拟数据并填充时间序列对象
for(i in 1:M) {
  sim_resid <- simulate(arima1, nsim = simulation_years)
  fitted_area_rep <- fitted_area[(length(year) + 1):length(year_sim)] # 正确的长度匹配
  r.sims[, i] <- as.numeric(fitted_area_rep + sim_resid) 
}
# 计算中位数和95%置信区间
quantiles <- apply(r.sims, 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE))
median_floor_area <- quantiles[2, ]
lower_bound <- quantiles[1, ]
upper_bound <- quantiles[3, ]
# 确保 selected_years 中的年份都在 year_sim 中
selected_years <- seq(from = 2013, to = 2100, by = 5)
selected_indices <- match(selected_years, year_sim)
# 检查 selected_indices 是否有任何 NA 值或超出 r.sims 行数的值
selected_indices <- selected_indices[!is.na(selected_indices) & selected_indices <= nrow(r.sims)]
# 现在安全地提取 r.sims 中对应的行
floor_u <- r.sims[selected_indices, ]
print(floor_u)


#II.Projection for building lifetime
#’new’ is the newly completed floor area every year; ‘demolish’ is the demolished floor 
#area at every year; ’time’ is time span that each newly completed floor area has survived for 
#from the year being built to one certain year; ‘a’ and ‘b’ are respectively scale and shape 
#parameter of the Weibull distribution function
library(nlstools) # 加载 nlstools 包
set.seed(123)
# 生成模拟数据
time <- rnorm(100, mean = 30, sd = 1)
new <- rnorm(32, mean = 1000, sd = 200)
demolish <- rnorm(100, mean = 500, sd = 100)
# 将 new 向量添加到数据框中
data_weibull <- data.frame(t = time, d = demolish)
# 确保数据没有缺失值或无限值
print(data_weibull)
print(new)
# 假设data_weibull已经包含所需的所有列
# 构建NLS模型的公式
formulaExp<- d~((1-exp(-((t)/a)^b))-(1-exp(-((t-1)/a)^b))*new[1])
library(minpack.lm)
fit <- nlsLM(formulaExp, data = data_weibull, start = list(a = 54.45622357, b = 5))
summary(fit)
# 进行引导抽样
set.seed(1)
nls_boot <- nlsBoot(fit, niter = 5000)
summary(nls_boot)
#perform the bootstrapping and get the 5,000 simulations for building lifetime


#III.Projection for population
#1. Projection for Life Expectancy
library(bayesLife) #load the bayesLife package
sim.dir <- file.path(getwd(), 'e0')
#set the storage path for the MCMC prediction parametersfor Life Expectancy
m <- run.e0.mcmc(sex='F', nr.chains=5, thin=1, iter=7000, output.dir=sim.dir) 
# run theMCMC simulation for Life Expectancy parameters
diag <- e0.diagnose(sim.dir, thin=5, burnin = 2000, express = FALSE, country.sampling.prop= NULL, keep.thin.mcmc=FALSE, verbose = TRUE) 
#run convergence diagnostics of existing LifeExpectancy MCMCs
has.mcmc.converged(diag)
#check if the existing diagnostics converged
pred <- e0.predict(m, burnin=2000, nr.traj=5000, verbose=TRUE) 
# predict the LifeExpectancy
e0.trajectories.plot(pred, country="China", both.sexes=TRUE, pi=c(95,80), main="Life Expectancy of China (Female and Male)")
#plot the Life Expectancy trajectories of China

#2.Projection for Total Fertility Rate (TFR)
library(bayesTFR) #load the bayesTFR package
simulation.dir <- file.path(getwd(), 'mylongrun') 
#set the storage path for the MCMC prediction parameters for TFR
m1 <- run.tfr.mcmc(nr.chains=5, iter=7000, output.dir=simulation.dir) 
#run the MCMC simulation for TFR parameters
diag1 <- tfr.diagnose(simulation.dir, thin=1, burnin=2000)
#run convergence diagnostics of existing TFR MCMCs
has.mcmc.converged(diag1) 
#check if the existing diagnostics converged
pred1 <- tfr.predict(sim.dir=simulation.dir, end.year=2100, burnin=2000, nr.traj=5000,
                     verbose=TRUE)
#predict the Total Fertility Rate
tfr.trajectories.plot(pred2, half.child.variant=FALSE, country='China', pi=c(95, 80),
                      typical.trajectory = TRUE, nr.traj=5000, main="Total Fertility Rate of China (%)") 
#plot the TotalFertility Rate trajectories of China

#3.Projection for Chinese population
library(bayesPop) #load the bayesPop package
pop.dir <- file.path(getwd(), 'pop') 
#set the storage path for the population prediction
pop.pred<-pop.predict(countries=c("China"), output.dir=pop.dir, nr.traj=10000,
                      inputs=list(tfr.sim.dir= simulation.dir, e0F.sim.dir=sim.dir, e0M.sim.dir="joint_")) 
#predict thepopulation of China
pop.trajectories.plot(pop.pred, country="China", pi=c(95,80), typical.trajectory = TRUE,
                      sum.over.ages=TRUE, main="Population Projection of China")
#plot the Population trajectories of China
population<-pop.trajectories(pop.pred,country="China") 
#get the 5,000 simulations of Chinese population
population<-rbind(rep(1362514.26),rep(1369435.67),population)
#add population in 2013 and 2014 to population


#IV.Projection for urbanization rate
#’ urban_rate’ is the historical urbanization rate and ‘year’ is the time series
data_rate <-urban_rate
urban_rate = data_rate$rate
time = data_rate$year
nls1 <- nls(urban_rate ~ 0.8/(1+a*exp(-b*(year-1986))), start = list(a=2,b=0.005), trace = TRUE) 
#define the S-curve for the urbanization rate
A<-summary(nls1)$parameter[1,1] #get the value for a
B<-summary(nls1)$parameter[2,1] #get the value for b
year_sim<-1900:2100 #define the simulation duration
fitted_ur<-0.8/(1+A*exp(-B*(year_sim-1986))) 
#compute the fitted urbanization rate without stochastic errors
rate <- ts(summary(nls1)$residuals, start = 1986, frequency = 1) 
#get the residuals of the Nonlinear Least Squares (nls) and set the residuals as a time series
library(forecast) #load the forecast package
arima1<-auto.arima(rate,trace=T)
#run the auto.arima command to get the parameters ofARIMA
r.sims <- matrix(nrow=86, ncol=5000) 
#define a matrix to store the 5,000 simulations oftime series from 2015-2100
M<-5000 #simulation times
for(i in 1:M)
{
  set.seed(i)
  ts <- simulate(arima1, nsim =86) #simulate the residuals of the urbanization rate
  r.sims[,i]<-as.numeric(ts+ fitted_ur [116:201]) #get the urbanization rate
}
#get the median and 95% CIs of the urbanization rate
median <- apply(r.sims, 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE))[2,]
quantile2.5 <- apply(r.sims, 1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE))[1,]
quantile97.5<-apply(r.sims,1, function(x) quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE))[3,]
urbanization<-rbind(r.sims[1,],r.sims[6,],r.sims[11,],r.sims[16,],r.sims[21,],
                    r.sims[26,],r.sims[31,],r.sims[36,],r.sims[41,],r.sims[46,],r.sims[51,],r.sims[56,],
                    r.sims[61,],r.sims[66,],r.sims[71,],r.sims[76,],r.sims[81,],r.sims[86,])
#get the 5,000 simulations in 2015,2020, 2025, 2030, 2035,.., 2095, 2100
# 首先，我们需要一个包含选定年份的向量
selected_years <- seq(2015, 2100, by = 5)
install.packages("tidyr")
library(tidyr)
# 将 urbanization 数据转换为长格式
urbanization_long <- as.data.frame(t(urbanization))
names(urbanization_long) <- seq_len(ncol(urbanization_long))
urbanization_long <- urbanization_long %>%
  pivot_longer(cols = everything(), names_to = "Simulation", values_to = "Rate")
# 重复 selected_years 向量，以匹配 urbanization_long 的行数
years_repeated <- rep(selected_years, each = ncol(urbanization))
urbanization_long$Year <- years_repeated
library(ggplot2)
# 使用 ggplot2 绘制图形
ggplot(urbanization_long, aes(x = Year, y = Rate, group = Simulation)) +
  geom_line(alpha = 0.1, color = "blue") +
  labs(x = "Year", y = "Urbanization Rate", title = "Urbanization Rate Simulations Over Time")


#V.Uncertainties of material intensity
library("fitdistrplus")
#load the fitdistrplus package
#’data’ contains 146 samples
data_material <- rweibull(100, shape = 2, scale = 1)
fw <- fitdist(data_material, "weibull") #fit the Weibull distribution
fg <- fitdist(data_material, "gamma") #estimate the distribution parameters of Gamma distribution
fln <- fitdist(data_material, "lnorm") #estimate the distribution parameters of Lognormal distribution
fn <-fitdist(data_material,"norm") #estimate the distribution parameters of Normal distribution
gofstat(list(fw,fg,fln,fn)) #compare the distribution functions
#select the best distribution function and estimate parameters for the distribution function
set.seed(1); fw.B <- bootdist(fw, niter = 1000)
set.seed(1); fln.B <- bootdist(fln, niter = 1000)
set.seed(1); fn.B <- bootdist(fn, niter = 1000)
set.seed(1); fg.B<-bootdist(fg, niter = 1000)
#get 5,000 simulations for the material intensity
rweibull(5000,shape=summary(fw.B)$CI[1,1],scale=summary(fw.B)$CI[2,1])
rlnorm(5000,meanlog=summary(fln.B)$CI[1,1],sdlog=summary(fln.B)$CI[2,1])
rnorm(5000,mean= summary(fn.B)$CI[1,1],sd= summary(fn.B)$CI[2,1])
rgamma(5000,shape=summary(fg.B)$CI[1,1],rate=summary(fg.B)$CI[2,1])
# 生成随机样本
samples_weibull <- rweibull(5000, shape=summary(fw.B)$CI[1,1], scale=summary(fw.B)$CI[2,1])
samples_lnorm <- rlnorm(5000, meanlog=summary(fln.B)$CI[1,1], sdlog=summary(fln.B)$CI[2,1])
samples_norm <- rnorm(5000, mean=summary(fn.B)$CI[1,1], sd=summary(fn.B)$CI[2,1])
samples_gamma <- rgamma(5000, shape=summary(fg.B)$CI[1,1], rate=summary(fg.B)$CI[2,1])
# 绘制直方图
par(mfrow=c(2,2))  # 设置图形排列为 2 行 2 列
hist(samples_weibull, main="Weibull Distribution", xlab="Value", col="lightblue", border="black")
hist(samples_lnorm, main="Log-Normal Distribution", xlab="Value", col="lightgreen", border="black")
hist(samples_norm, main="Normal Distribution", xlab="Value", col="lightcoral", border="black")
hist(samples_gamma, main="Gamma Distribution", xlab="Value", col="lightyellow", border="black")


#VII.Uncertainty propagation for the stock, inflow and outflow of floor area
# 设置随机数种子以获得可重复的结果
set.seed(123)
# 模拟城市人口数据
population <- matrix(rnorm(20 * 5000, mean = 100000, sd = 20000), nrow = 20, ncol = 5000)
# 模拟城市化率数据
urbanization <- matrix(runif(20 * 5000, min = 0, max = 1), nrow = 20, ncol = 5000)
# 模拟每个城市住宅的平均楼层数
floor_u <- matrix(rnorm(20 * 5000, mean = 10, sd = 2), nrow = 20, ncol = 5000)
# 计算城市人口
population_u <- matrix(nrow = 20, ncol = 5000)
for(i in 1:5000) {
  population_u[,i] <- population[,i] * 1000 * urbanization[,i]
}
print(population_u)
# 计算城市住房存量
stock_u <- matrix(nrow = 20, ncol = 5000)
for(i in 1:5000) {
  stock_u[,i] <- population_u[,i] * floor_u[,i]
}
# 设置随机数种子以获得可重复的结果
set.seed(123)
# 定义年份向量
year_complete <- 2000:2100
# 模拟每年的完工楼层面积
area_complete <- rnorm(length(year_complete), mean = 10000, sd = 2000)
# 创建数据框
data <- data.frame(V1 = year_complete, V2 = area_complete)
# 查看前几行数据
head(data)
complete.sim<-matrix(nrow=119, ncol=5000) 
#define a matrix to store the newly completefloor area
output.sim<-matrix(nrow=88, ncol=5000) 
#define a matrix to store the outflow of floor area
for(i in 1:3621)
{
  year_complete<-data$V1 #initialize the ‘year_complete’ for every loop
  area_complete<-data$V2 #initialize the ‘area_complete’ for every loop
  A<-nls_boot$coefboot[i,1] #get the scale parameter of building lifetime function
  B<-nls_boot$coefboot[i,2] #get the shape parameter of building lifetime function
  for(j in 2013:2015)
  {
    output<-sum(area_complete*(pweibull(j-year_complete,scale=A,shape=B)-
                                 pweibull(j-1-year_complete,scale = A,shape = B))) #compute the outflow at year j
    input<-stock_u[j-2011,i]-stock_u[j-2012,i]+output # compute the newly completed floor area at year j
    area_complete<-c(area_complete,input) #add the ‘input’ into the ‘area_complete’ vector
    year_complete<-c(year_complete,j) #add the ‘j’ into the ‘year_complete’ vector
    output.sim[j-2012,i]<-output #store the outflow at year j into the ‘output.sim’ matrix
  }
  n_years <- 2100 - 2013 + 1
  complete.sim <- matrix(nrow = n_years, ncol = 3621)
  for(j in 2016:2100) {
    Y <- min(trunc((j-2011)/5) + 4, 20)
    # 现在 Y 的值不会超过 20
    
    # 计算 output 和 input
    output <- sum(area_complete * (pweibull(j - year_complete, scale = A, shape = B) -
                                     pweibull(j - 1 - year_complete, scale = A, shape = B)))
    input <- (stock_u[Y, i] - stock_u[Y - 1, i]) / 5 + output
    
    # 更新 area_complete 和 year_complete
    area_complete <- c(area_complete, input)
    year_complete <- c(year_complete, j)
    
    # 存储 output
    output.sim[j - 2012, i] <- output
  }
  if(length(area_complete) > n_years) {
    area_complete <- area_complete[1:n_years]  # 截断向量
  } else if(length(area_complete) < n_years) {
    area_complete <- c(area_complete, rep(NA, n_years - length(area_complete)))  # 使用 NA 填充
  }
  complete.sim[, i] <- area_complete
  
 
  
}
# 检查维度
print(dim(complete.sim))
print(dim(output.sim))
# 数据摘要
summary(complete.sim)
summary(output.sim)
# 可视化示例（绘制第一个模拟的直方图）
hist(complete.sim[, 1], main = "Histogram of Complete Sim for First Simulation", xlab = "Values")


#VIII. Uncertainty propagation for the stock, inflow and outflow of materials
#obtain 5,000 simulations for the material intensity
set.seed(1); cement_u<-rnorm(5000,mean=151.671,sd=42.656)
set.seed(1); steel_u<-rlnorm(5000,meanlog=2.813,sdlog=0.545)
set.seed(1); wood_u<-rlnorm(5000,meanlog=2.639,sdlog=0.780)
set.seed(1); brick_u<-rweibull(5000,scale=496.964,shape=1.527)
set.seed(1); gravel_u<-rgamma(5000,rate=0.011,shape=5.113)
set.seed(1); sand_u<-rnorm(5000,mean=498.727,sd=162.968)
set.seed(1); asphalt_u<-rlnorm(5000,meanlog=0.422,sd=0.711)
set.seed(1); lime_u<-rweibull(5000,scale=33.041,shape=1.457)
set.seed(1); glass_u<-rgamma(5000,rate=3.733,shape=6.243)
set.seed(1); total_u<-rnorm(5000,mean=1616.980,sd=457.618)
#material stock from 1985-2100
material_stock.sim<- matrix(nrow=48, ncol=5000) 
#define a matrix to store the materialstock
#historical material stock from 1985-2011; ‘stock_u_his’ is a vector that contains the urban housing stock from 1985-2011
# 设置随机数种子以获得可重复的结果
set.seed(1)
# 模拟历史城市住房存量数据 (1985-2011)
stock_u_his <- rnorm(2011 - 1985 + 1, mean = 500000, sd = 100000)
# 模拟预测城市住房存量数据 (2012-2100)
# 假设每年的住房存量呈现逐年增长的趋势
mean_stock <- seq(from = 600000, to = 1000000, length.out = 2100 - 2012 + 1)
stock_u <- matrix(rnorm((2100 - 2012 + 1) * 5000, mean = mean_stock, sd = 120000), nrow = 2100 - 2012 + 1, ncol = 5000)
# 查看结果的一部分
head(stock_u_his)
head(stock_u)
intensity<-10
for(i in 1:27)
{
  material_stock.sim[i,]<-stock_u_his[i]*cement_u/1000
}
#material stock from 2012-2100
for(j in 28:48) {
  material_stock.sim[j,] <- stock_u[j,] * cement_u / 1000
}
# 检查维度和结构
print(dim(material_stock.sim))
str(material_stock.sim)
# 查看数据摘要
summary(material_stock.sim)
# 可视化 - 例如，绘制第一年的材料存量分布
hist(material_stock.sim[1, ], main = "Histogram of Material Stock in First Year", xlab = "Material Stock", ylab = "Frequency")
# 如果有现实数据或理论预测，可以在这里添加代码进行对比分析
#material inflow from 1982-2100
# 调整 material_inflow.sim 的维度
material_inflow.sim <- matrix(nrow = 88, ncol = 3621)
# 调整 cement_u 的长度以匹配 complete.sim
cement_u_adjusted <- cement_u[1:3621]
# 然后进行计算
for(i in 1:88) {
  material_inflow.sim[i,] <- complete.sim[i,] * cement_u_adjusted / 1000
}
hist(material_inflow.sim[1, ], main = "Histogram of Material inflow in First Year", xlab = "Material Inflow", ylab = "Frequency")
#material outflow from 2013-2100
material_outflow.sim<- matrix(nrow = 88, ncol = 5000)
for(i in 1:88)
{
  material_outflow.sim[i,]<-output.sim[i,]*cement_u/1000
}
hist(material_outflow.sim[1, ], main = "Histogram of Material outflow in First Year", xlab = "Material Outflow", ylab = "Frequency")
summary(material_outflow.sim[1, ])
