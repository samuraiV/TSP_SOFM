#! /usr/bin/env Rscript

# implementation of travelling salesman problem in R
# data in the form of two-dimensional points

loopmax = 100   

library(calibrate)

max = 10^6

data = read.table("tsp.in.txt")

data <- as.matrix(data)            # data matrix

mc <- colMeans(data)            # mean of data

ncities = NROW(data)            # number of cities

W <- matrix(0,ncities,2)       # weight matrix

# weight initialisation

i=1
while(i<ncities+1)
  {
    W <-matrix(c((mc[1]+data[i,1])/2,(mc[2]+data[i,2])/2),i,2)
    i=i+1
  }

# input presentation

loop = 1

while(loop > 0 && loop < loopmax)
  {
    j=1     #index of city
       
    while(j< ncities+1)
      {
        i=1
        dmin = max   # minimum distance from the city of weights
        
        # checking min distance from city no. j
        
        while(i < ncities+1)
          {
            if (dmin > sqrt(sum(abs(W[i,]-data[j,]))^2))
              {
                dmin = sqrt(sum(abs(W[i,]-data[j,]))^2)
                qrow = i
              }
            i = i + 1
          }

        #counting the no of weights not overlapped with the cities
        
   mcount = 0    
        i=1
        while(i<ncities)
          {
            if(sum(abs(data[j,]-W[i,])) != 0)
              mcount = mcount +1
              
            i = i +1
          }
        
       # weight modification  
        i = 1
        
        while(i < ncities+1) 
          {
            if (abs(ncities-(qrow-i)) < abs(qrow-i))
                    W[i,] = W[i,] + (1/(1+abs(ncities-(qrow-1))))*exp(-(abs(ncities-(qrow-i))^2))*(data[j,]-W[i,])
              else 
              W[i,] = W[i,] + (1/(1+abs(qrow-1)))*exp(-(abs(qrow-i)^2))*(data[j,]-W[i,])
                i = i + 1
          }
                j=j+1
        
        # plotting of data and weights
        plot(data, xlim = c(0,100) , ylim = c(0,100) ,  col = "black")
        
        points(W, xlim = c(0,100) , ylim = c(0,100) ,  col = "red")
      }

if(mcount == 0)
  loop = -1

       else  loop = loop + 1
    print(loop)
      
     }

################

print(W)
cities <- vector()
i=1

# calculating the order of the cities to be visited
while(i<ncities+1)
  {
    j=1
    dmin=max
    while(j<ncities+1)
      {
        if(dmin > sqrt(sum(abs(W[i,]-data[j,]))^2))
          {
           dmin = sqrt(sum(abs(W[i,]-data[j,]))^2)
           cities[i] = j
         }
        j=j+1
      }
    i=i+1
  }
print(cities)
textxy(data[,1],data[,2],cities,offset=1,cex=1)      #putting labels on cities

