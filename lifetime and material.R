#’new’ is the newly completed floor area every year; ‘demolish’ is the demolished floor 
#area at every year; ’time’ is time span that each newly completed floor area has survived for 
#from the year being built to one certain year; ‘a’ and ‘b’ are respectively scale and shape 
#parameter of the Weibull distribution function
library(nlstools) #load the nlstools package

# 模拟数据
set.seed(123)  # 设置种子以确保结果可重复
time <- 1:100
demolish <- rnorm(100, mean = 0, sd = 1)  # 这里使用正态分布，你可以根据实际情况选择合适的分布

# 创建数据框
building_lifetime <- data.frame(time = time, demolish = demolish)

# 显示前几行数据
head(building_lifetime)

# 定义模型函数
model_func <- function(params) {
  a <- params[1]
  b <- params[2]
  
  # 计算模型的预测值
  pred <- numeric(length(data$time))
  for (i in 1:31) {
    term <- (exp(-((data$time - i)/a)^b) - exp(-((data$time - (i - 1))/a)^b)) * new[i]
    pred <- pred + term
  }
  pred <- pred + (exp(-((data$time - 32)/a)^b) - exp(-((data$time - 31)/a)^b)) * new[32]
  
  # 计算残差平方和
  rss <- sum((data$demolish - pred)^2)
  
  return(rss)
}

# 使用 optim 进行参数优化
optim_result <- optim(c(3, 2), model_func)

# 输出优化结果
print(optim_result)


#define the empirical data for regression
#define the formula for the Nonlinear Least Squares regression; some lines have been omitted for brevity
# 假设有一个新的变量列表 new
new <- c(1:32)

# 其他参数
a <- 2
b <- 0.5

# 初始化 formula
formulaExp <- as.formula(demolish ~ 0)

# 创建 for 循环
for (i in 1:31) {
  term <- (exp(-((data$time - i)/a)^b) - exp(-((data$time - (i - 1))/a)^b)) * new[i]
  formulaExp <- update(formulaExp, ~. + term)
}

# 添加最后一个 term
formulaExp <- update(formulaExp, ~. + (exp(-((data$time - 32)/a)^b) - exp(-((data$time - 31)/a)^b)) * new[32])

nls1<- nls(formulaExp,start=list(a=3,b=2),data=data) 
#get the scale and shape parameter by the Nonlinear Least Squares regression
set.seed(1); nls_boot<-nlsBoot(model_func,niter=5000) 
#perform the bootstrapping and get the 5,000 simulations for building lifetime

# 设置种子以确保结果可重复
set.seed(42)

# 模拟一个包含 100 个元素的 population 矩阵
population <- matrix(runif(100, min = 1000, max = 5000), nrow = 10, ncol = 10)

# 模拟一个包含 100 个元素的 urbanization 矩阵
urbanization <- matrix(runif(100, min = 0, max = 1), nrow = 10, ncol = 10)

# 定义一个 matrix 来存储 urban population
population_u <- matrix(0, nrow = 10, ncol = 10)

# 运行循环（注意这里改成最大运行 10 次）
for (i in 1:10) {
  population_u[, i] <- population[, i] * 1000 * urbanization[, i]
}

# 显示结果
print(population_u)

stock_u<- matrix(0, nrow = 10, ncol = 10)

# 模拟一个包含 100 个元素的 floor_u 矩阵
floor_u <- matrix(runif(100, min = 10, max = 30), nrow = 10, ncol = 10)

for(i in 1:5000)
{
  stock_u[,i]<-population_u[,i]*floor_u[,i]
}
print(stock_u)


data<-housing_floor_area_per_capita_test
#’data’ contains the newly completed floor area
area_complete<-data$area 
#define the newly completed floor area
year_complete<-data$year 
#define the year vector
stock_u<-rbind(stock_u_2012,stock_u) 
#add the urban housing stock in 2012 into the ‘stock_u’ matrix
complete.sim<-matrix(nrow=119, ncol=5000) 
#define a matrix to store the newly complete floor area
output.sim<-matrix(nrow=88, ncol=5000)
#define a matrix to store the outflow of floor area
for(i in 1:5000)
{
  year_complete<-data$year  #initialize the ‘year_complete’ for every loop
  area_complete<-data$area #initialize the ‘area_complete’ for every loop
  A<-nls_boot$coefboot[i,1] #get the scale parameter of building lifetime function
  B<-nls_boot$coefboot[i,2] #get the shape parameter of building lifetime function
  for(j in 2013:2015)
  {
    output<-sum(area_complete*(pweibull(j-year_complete,scale=A,shape=B)-
                                 pweibull(j-1-year_complete,scale = A,shape = B))) 
    #compute the outflow at year j
    input<-stock_u[j-2011,i]-stock_u[j-2012,i]+output
    # compute the newly completed floor area at year j
    area_complete<-c(area_adjust,input_2013) 
    #add the ‘input’ into the ‘area_complete’ vector
    year_complete<-c(year_complete,j) 
    #add the ‘j’ into the ‘year_complete’ vector
    output.sim[j-2012,i]<-output 
    #store the outflow at year j into the ‘output.sim’ matrix
  }
  for(j in 2016:2100)
  {
    Y<-trunc((j-2011)/5)+4 
    #because the prediction of population can only provide with 5-year interval time series
    
    output<-sum(area_complete*(pweibull(j-year_complete,scale=A,shape=B)-
                                 pweibull(j-1-year_complete,scale = A,shape = B))) 
    #compute the outflow at year j
    input<-(stock_u[Y,i]-stock_u[Y-1,i])/5+output 
    #compute the newly completed floor  area at year j
    area_complete<-c(area_complete,input)
    #add the ‘input’ into the ‘area_complete’ vector
    year_complete<-c(year_complete,j) 
    #add the ‘j’ into the ‘year_complete’ vector
    output.sim[j-2012,i]<-output 
    #store the outflow at year j into the ‘output.sim’ matrix
  }
  complete.sim[,i]<-area_complete 
  #store the inflow from 2013-2100 into the ‘input.sim’matrix
}


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
material_stock.sim<- matrix(nrow=48, ncol=5000) #define a matrix to store the material 
stock
#historical material stock from 1985-2011; ‘stock_u_his’ is a vector that contains the urban housing stock from 1985-2011

for(i in 1:27)
{
  material_stock.sim[i,]<-stock_u_his[i]*intensity/1000
}
#material stock from 2012-2100
for(j in 28:48)
{
  material_stock.sim[j,]<-stock_u[j-27,]*intensity/1000
}

#material inflow from 1982-2100
material_inflow.sim<- matrix(nrow=88, ncol=5000) #define a matrix to store the material 
inflow
for(i in 1:119)
{
  material_inflow.sim[i,]<-complete.sim[i,]*intensity/1000
}

#material outflow from 2013-2100
material_outflow.sim<- matrix(nrow=88, ncol=5000)
for(i in 1:88)
{
  material_outflow.sim[i,]<-output.sim[i,]*intensity/1000
}
