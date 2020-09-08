spotify
genre = spotify$genre #save genre before deleting it from data so you can use for external val 
new1 = spotify[,-1]
new2 = new1[,-1]
new3 = new2[,-1]
new4 = scale(new3)


set <- c(5,4,8,10,12) 
colvec <- c("magenta3", "deepskyblue2", "darkorange3") 
cols <- adjustcolor(colvec[genre], 0.5)
spotify[,set]
pairs(spotify[,set], gap = 0, pch = 19, col = cols) 

panel.line <- function(spotify, y){ 
  fit <- lm(y ~ spotify) 
  points(spotify, y, pch = 19, col = cols) 
  abline(fit, col = 1, lwd = 1.5) 
}
# set function for correlation 
panel.cor <- function(spotify, y) { 
  r <- round(cor(spotify, y), 2) 
  txt <- paste0("r = ", r) 
  loc <- c( mean(range(spotify)), mean(range(y)) ) # to place text in the center 
  text(loc[1], loc[2], txt, cex = 1.5) 
}

# plot
pairs(spotify[,set], gap = 0, 
      lower.panel = panel.cor, 
      upper.panel = panel.line, 
      main="Spotify Audio features"
)


# NOW WANT TO FIND K
K <- 12
wss <- bss <- rep(NA, K)
for ( k in 1:K ) { 
  fit <- kmeans(new4, centers = k, nstart = 50)
  wss[k] <- fit$tot.withinss  
  bss[k] <- fit$betweenss 
}
# compute calinski-harabasz index
N <- nrow(new4) 
ch <- ( bss/(1:K - 1) ) / ( wss/(N - 1:K) ) 
ch[1] <- 0
plot(1:K, ch, type = "b", ylab = "CH", xlab = "K", main = "CH Index Values")

# we see highest ch corresponds to 2 clusters and 3 clusters
fit2 <- kmeans(new4, centers = 2, nstart = 50)
fit2
fit3 <- kmeans(new4, centers = 3, nstart = 50)
fit3
symb <- c(15, 16, 17) 
col <- c("darkorange2", "deepskyblue3", "magenta3")
pairs(new4[,c(2,4,6,8,10)], gap = 0, pch = symb[fit2$cluster], 
      col = adjustcolor(col[fit2$cluster], 0.4), main = "Clustering result with K = 2")
pairs(new4[,c(2,4,6,8,10)], gap = 0, pch = symb[fit3$cluster],
      col = adjustcolor(col[fit3$cluster], 0.4), main = "Clustering result with K = 3")


# now look at silhouette
library(cluster)
d <- dist(new4, method = "euclidean")^2
sil2 <- silhouette(fit2$cluster, d) 
sil3 <- silhouette(fit3$cluster, d)
col <- c("darkorange2", "deepskyblue3", "magenta3") 
plot(sil2, col = adjustcolor(col[1:2], 0.3), main = "Spotify data with K = 2") 
plot(sil3, col = adjustcolor(col, 0.3), main = "Spotify data with K = 3")  
# the average sil width for k=2 is higher but it is still quite low.

# EXTERNAL VALIDATION
library(e1071)
# cross tabulation between clustering result and genres
tab2 <- table(fit2$cluster, genre)
tab2
# ClassAgreement is command used to calculate Rand
classAgreement(tab2)






