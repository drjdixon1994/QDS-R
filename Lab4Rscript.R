#Lab4
load("C:/Users/12039/Downloads/kidney.rdata")

# “variance”,  “range”, “minimum”, “max” 
popvar = var(kidney$dage) 
popmean = mean(kidney$dage) 
range(kidney$dage) 
min(kidney$dage) 
max(kidney$dage)

#Run the simulations
sim500 = function(nsize){   
  simdf = matrix(nrow=500, ncol = nsize)   
  for (i in 1:500) {     
    simdf[i,] = sample(kidney$dage, nsize, replace=TRUE)   }     
return(simdf) }

simdf5 = sim500(5)

row_means <- apply(simdf5, 1, mean)   
row_vars = apply(simdf5, 1, var)

Exbar5 = mean(row_means)   
Vxbar5 = var(row_means)

Es2_5 = mean(row_vars) 
Vs2_5 = var(row_vars)

hist(row_means, breaks = 70, main = "Histogram of Sample Means", xlab = "Sample Means", col = "blue", probability = TRUE) 
curve(dnorm(x, Exbar5, sqrt(Vxbar5)), add = TRUE, col = "red")

set.seed(1) 
simdf10 = sim500(10)
row_mean10 <- apply(simdf10, 1, mean)   
row_var10 = apply(simdf10, 1, var)
Exbar10 = mean(row_mean10)   
Vxbar10 = var(row_mean10)

Es2_10 = mean(row_var10) 
Vs2_10 = var(row_var10)

hist(row_mean10, breaks = 70, main = "Histogram of Sample Means (simdf10)", xlab = "Sample Means", col = "blue", probability = TRUE) 
curve(dnorm(x, Exbar10, sqrt(Vxbar10)), add = TRUE, col = "red") 

set.seed(1) 
simdf50 = sim500(50)
row_mean50 <- apply(simdf50, 1, mean)   
row_var50 = apply(simdf50, 1, var)

Exbar50 = mean(row_mean50)   
Vxbar50 = var(row_mean50)

Es2_50 = mean(row_var50) 
Vs2_50 = var(row_var50)

hist(row_mean50, breaks = 70, main = "Histogram of Sample Means (simdf50)", xlab = "Sample Means", col = "blue", probability = TRUE) 
curve(dnorm(x, Exbar50, sqrt(Vxbar50)), add = TRUE, col = "red") 

data.frame(Exbar5, Vxbar5, Es2_5, Vs2_5)
data.frame(Exbar10, Vxbar10, Es2_10, Vs2_10)
data.frame(Exbar50, Vxbar50, Es2_50, Vs2_50)
data.frame(popmean, popvar)

hist(kidney$dage, breaks = 70, main = "Histogram of Donor Age", xlab = "Age", col = "blue", probability = TRUE) 
curve(dnorm(x, Exbar50, sqrt(Vxbar50)), add = TRUE, col = "red") 

hist(kidney$dage)
