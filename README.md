### PCA Example Plotting Example ###

# Bring in required packages
# install.packages("tidyverse")
# install.packages("ggcorrplot")
library(dplyr)
library(tidyverse)
library(ggcorrplot)

# Brings in the desired csv and assigns it to an object
count_final <- read.csv("C:/Users/81hoc/Documents/VIP/count_final_edit_ck.csv", header = T)
# Leave out columns 11 through 29
clim <- na.omit(count_final[,11:29])

## A function is a set of statements organized together to perform a specific task
## A function only runs when it is called
# Function to create plots (adapted from Therese Balkenbush's original code)
pca_summary <- function(x, cor = TRUE, plot.type = 1, dims = c(1,2), biplot = TRUE, ...){
  pca <- princomp(x, cor = cor)
  # The eigenvalue is explained to be a scalar associated with a linear set of equations which, 
  # when multiplied by a nonzero vector, equals to the vector obtained by transformation 
  # operating on the vector
  eigen.val <- pca$sdev^2
  # Eigenvectors are a special set of vectors associated with a linear system of equations
  eigen.vec <- pca$loadings 
  scores <- pca$scores 
  
  ## if...else statements
  # if the "if statement" = True, then the function is executed
  if(plot.type == 1){
    plot(1:length(eigen.val), cumsum(eigen.val/sum(eigen.val)),
         ylab = "Cumulative variance explained", xlab = "Dimension", type = "l",...)
  }
  if(plot.type == 2){
    plot(scores[,dims[1]], scores[,dims[2]], cex = 2.0 ,xlab = "PC1", ylab = "PC2", ...)
    text(scores[,dims[1]], scores[,dims[2]], labels = (count_final$PopNum), cex = 0.7)
    legend("bottomleft", legend = c("BHwy 1","BJct 2","CR 3","FL 4","GJ 5","HC 6","HCrk 7",
                                    "KH 8","Kob 9","LS 10","SM 11","Sta 12"), 
           pch = 1, col = 1:12)
    if(biplot){ require(vegan)
      fit <- envfit(scores, x) 
      plot(fit)
    }
  }
  if(plot.type == 3){
    a <- apply(eigen.vec, 1, function(x) x * eigen.val)
    n <- length(unique(a))
    heatmap(t(a), Rowv = NA, Colv = NA, col = hcl.colors(n, "Zissou 1"), 
            xlab = "", ylab = "Bioclim Variables from Worldclim") 
  }
  # else statements are only executed when "if statement" = False
  if(plot.type == 2) fit <- fit else fit = NA
  invisible(list(eigenval = eigen.val, eigenvec = eigen.vec, 
                 scores = scores, fit = fit)) 
}


# par resets the parameters for the plots
# mar(bottom, left, top, right)
par(mfrow = c(1,1), mar = c(4,4,1,1))

# Cumulative frequency graph using counts and eigenvalues
# (a number that explains how much variance there is in one direction) 
# to get cumulative frequency explained<- pca_summary(clim, plot = 1)
pca_summary(clim, plot = 1)

# Principal Components Analysis (PCA) of 19 Worldclim environmental factors 
# which are shown in table 1 below and our sagebrush populations represented 
# as multi-colored circles.
pca_summary(clim, plot = 2, col = factor(count_final$PopID))

# heatmap of worldclim variables
pca_summary(clim, plot = 3)


# create a correlation plot
correlation_matrix <- round(cor(clim),1)
head(correlation_matrix[, 1:4])
corrp.mat <- cor_pmat(clim)

head(corrp.mat[, 1:4])

ggcorrplot(correlation_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

#sets the maximum amount of lines that will print in the console
options(max.print = 10000)

# create a table with the pca scores
PCA1 <- data.frame(pca$scores)
PCA1

# creates a csv with the pca score values and saves it to the specified location on my computer
write.csv(PCA1, "C:/Users/81hoc/Documents/VIP/pcaVIP.csv", row.names = FALSE)

