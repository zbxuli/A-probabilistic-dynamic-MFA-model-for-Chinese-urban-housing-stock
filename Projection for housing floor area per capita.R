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



