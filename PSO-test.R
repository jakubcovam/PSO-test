#------------------------------------
# Michala Jakubcova
# 14. 2. 2016

# Particle swarm optimization search

#-----------------------------------

#---
## PARAMETERS ##
## SURFACE:
o = 50  # shifted center
xOpt = 450  # position of the seached optimum
rangeX = 100  # range of the x and y axis
## PSO:
nPop = 30  # number of individuals in the population
nIter = 10  # number of iteration
nDim = 2  # number of dimension
nFit = nDim+1  # number of dimensions with one value for calculated fitness
K = 0.729  # constriction factor
ck = 2.05  # acceleration coeficient for constriction factor
w1 = 0.9   # initial inertia weight 
w2 = 0.4  # final inertia weight
c = 1.494  # acceleration coeficient inrtia weight
#---

#---
## SURFACE FUNCTION - SPHERE F. ##
Zsurf <- function(x,y)
{
  xx = x - o  
  yy = y - o  
  z = (xx)^2 + (yy)^2 - xOpt
  return(z)
}

# X AND Y COORDINATES:
x = seq(0:(2*rangeX))-(rangeX+1)  
y = seq(0:(2*rangeX))-(rangeX+1)

# NUMBER OF POINTS IN THE SURFACE GRID:
nx = length(x)
ny = length(y)

ZZ = matrix(nrow = nx, ncol = ny)
for(i in 1:nx){
  for(j in 1:ny){
    ZZ[i,j] = Zsurf(x[i],y[j])
  }
}
#---

#---
## POPULATION IN PSO - PARENTS ##
Parents = matrix(nrow = nPop, ncol = nFit)
Parents[,1] = runif(nPop)*(2*rangeX) - (rangeX+1)
Parents[,2] = runif(nPop)*(2*rangeX) - (rangeX+1)

for(i in 1:nPop){
  Parents[i,nFit] = Zsurf(Parents[i,1], Parents[i,2])
}
#---

#---
## DRAW CONTOUR PLOT WITH PARENTS POPULATION ##
contour(x, y, ZZ, xlim=c(min(x),max(x)), ylim=c(min(y),max(y))) 
points(Parents, col = 'red')
#---

#---
## POPULATION IN PSO - OFSPRINGS ##
Offspring = Parents

Xmax = max(x)
Vmax = Xmax

p = matrix(10^8, nrow = nPop, ncol = nFit)  # best position found by the particle
pg = rep(10^8, nFit)  # best position found by any member of the neighborhood
v = matrix(Vmax, nrow = nPop, ncol = nFit)  # velocity of the particle

for (i in 1:nIter) {
  for (j in 1:nPop) { 
    if (Offspring[j,nFit] < p[j,nFit]) {
      p[j,] = Offspring[j,]
    }
    #which.min() finds the min value (i.e. row in 'p' with min value in the last col)   
    if (p[which.min(p[,nFit]),nFit] < pg[nFit]) { 
      pg[] = p[which.min(p[,nFit]),]
    }
    w = (w1-w2)*((nIter-i)/nIter)+w2  
    v[j,] = w * v[j,] + c*runif(1)*(p[j,]-Offspring[j,]) + c*runif(1)*(pg[]-Offspring[j,])
    v[j,] = sign(v[j,]) * min(abs(v[j,]),Vmax)
    Offspring[j,] = Offspring[j,] + v[j,]
    
    Offspring[j,nFit] <- Zsurf(Offspring[j,1],Offspring[j,2])
    
    if(Offspring[j,nFit] > Parents[j,nFit]){
      Offspring[j,] = Parents[j,]
    }
  } 
  p[] = 10^8
}

points(Offspring, col = 'blue')
