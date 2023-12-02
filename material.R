#obtain 5,000 simulations for the material intensity
set.seed(1); cement_u<-rnorm(5000,mean=151.671,sd=42.656)


#material stock from 1985-2100
material_stock.sim<- matrix(nrow=48, ncol=5000) 
#define a matrix to store the material stock
#historical material stock from 1985-2011; ‘stock_u_his’ is a vector that contains the urban housing stock from 1985-2011
intensity_cement <- 10
#根据查询,自己定义的强度数值,可更改

for(i in 1:27)
{
  material_stock.sim[i,]<-cement_u[i]*intensity_cement/1000
}
#material stock from 2012-2100
for (j in 28:48) {
  material_stock.sim[j,] <- cement_u[j - 27] * intensity_cement / 1000
}
#material inflow from 1982-2100
#print(material_stock.sim)

material_inflow.sim<- matrix(nrow=88, ncol=5000)
#define a matrix to store the material inflow
for(i in 1:119)
{
  material_inflow.sim[i,]<-material_stock.sim[i,]*intensity_cement/1000
}

 #material outflow from 2013-2100
material_outflow.sim<- matrix(nrow=88, ncol=5000)
for(i in 1:88)
{
  material_outflow.sim[i,]<-material_stock.sim[i,]*intensity_cement/1000
}

plot(cement_u, type = "l", col = "blue", lty = 1, xlab = "Index", ylab = "Values", main = "Line Plot of cement_u")