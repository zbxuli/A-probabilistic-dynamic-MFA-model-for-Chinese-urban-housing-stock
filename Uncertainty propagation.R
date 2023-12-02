population_u <- matrix(nrow=20, ncol=5000) 
#define a matrix to store the urban population

for(i in 1:5000)
{
  population_u[,i]<-population[,i]*1000*urbanization[,i]
}

stock_u<- matrix(nrow=20, ncol=5000) 
#define a matrix to store the urban housing stock
for(i in 1:5000)
{
  stock_u[,i]<-population_u[,i]*floor_u[,i]
}


#’data’ contains the newly completed floor area
area_complete<-data$V2        #define the newly completed floor area
year_complete<-data$V1        #define the year vector


stock_u<-rbind(stock_u_2012,stock_u) 
#add the urban housing stock in 2012 into the ‘stock_u’ matrix

complete.sim<-matrix(nrow=119, ncol=5000) 
#define a matrix to store the newly complete floor area

output.sim<-matrix(nrow=88, ncol=5000) 
#define a matrix to store the outflow of floor area


for(i in 1:5000)
{
  year_complete<-data$V1 #initialize the ‘year_complete’ for every loop
  area_complete<-data$V2 #initialize the ‘area_complete’ for every loop
  A<-nls_boot$coefboot[i,1] #get the scale parameter of building lifetime function
  B<-nls_boot$coefboot[i,2] #get the shape parameter of building lifetime function
  
  for(j in 2013:2015)
  {
    output<-sum(area_complete*(pweibull(j-year_complete,scale=A,shape=B)-
                                 pweibull(j-1-year_complete,scale = A,shape = B))) #compute the outflow at year j
    input<-stock_u[j-2011,i]-stock_u[j-2012,i]+output # compute the newly completed
    floor area at year j
    area_complete<-c(area_adjust,input_2013) #add the ‘input’ into the ‘area_complete’
    vector
    year_complete<-c(year_complete,j) #add the ‘j’ into the ‘year_complete’ vector
    output.sim[j-2012,i]<-output #store the outflow at year j into the ‘output.sim’ matrix
  }
  
  for(j in 2016:2100)
  {
    Y<-trunc((j-2011)/5)+4 
    #because the prediction of population can only provide with 5-year interval time series
    output<-sum(area_complete*(pweibull(j-year_complete,scale=A,shape=B)-
                                 pweibull(j-1-year_complete,scale = A,shape = B)))
    #compute the outflow at year j
    
    input<-(stock_u[Y,i]-stock_u[Y-1,i])/5+output
    #compute the newly completed floor area at year j
    
    area_complete<-c(area_complete,input)
    #add the ‘input’ into the ‘area_complete’ vector
    
    year_complete<-c(year_complete,j)
    #add the ‘j’ into the ‘year_complete’ vector
    output.sim[j-2012,i]<-output
    #store the outflow at year j into the ‘output.sim’ matrix
  }
  
  complete.sim[,i]<-area_complete 
  #store the inflow from 2013-2100 into the ‘input.sim’ matrix
}