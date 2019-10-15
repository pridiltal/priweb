data1 <- read.csv(file.choose())
library(mvtsplot)

mvtsplot(data1,levels=8,gcol=2,norm="global")
f <- extract_tsfeatures(data1)
dim(f)
library(lattice)
splom(~f[, 1:7])


nobs = 1500
nts = 1000
tsframe1 <- ts(matrix(ncol=nts, nrow=nobs))
i=1

for(i in 1:nts){
  #tsframe1[,i] <- exp(0.002*(1:nobs)) + rnorm(nobs,0,2) # adding noise
  tsframe1[,i] <- 10 + runif(nobs,0,2) # adding noise
}

f <- extract_tsfeatures(tsframe1)
pc <- get_pc_space(f)


#devtools::install_github("pridiltal/oddstream")
oddstream :: plotpc(pc$pcnorm, colour = "red", pc_boundary =8)

#### concept drift
set.seed(123)
nobs = 1500
nts = 250
tsframe1 <- ts(matrix(ncol=nts, nrow=nobs))
i=1
for(i in 1:nts){
  tsframe1[,i] <- exp(0.002*(1:nobs)) + runif(nobs,0,2) # adding noise
  #tsframe1[,i] <- 10 + rnorm(nobs,0,2) # adding noise
}

set.seed(123)
nobs = 1500
nts = 500
tsframe2 <- ts(matrix(ncol=nts, nrow=nobs))
i=1
for(i in 1:nts){
   tsframe2[,i] <- exp(0.006*(1:nobs)) + runif(nobs,0,2) # adding noise
  #tsframe2[,i] <- 20 + rnorm(nobs,0,2) # adding noise
}
tsframe=cbind(tsframe1,tsframe2)

dim(tsframe)
mvtsplot(tsframe,levels=8,gcol=2,norm="global")

#to make the series an anomalouse time series

##mke anomalous series
tsframe[300:600,40:50]=tsframe[1400:1100,40:50]


mvtsplot(tsframe,levels=8,gcol=2,norm="global")

train_data<- tsframe[1:100,]
test_stream<- tsframe[101:1500,]
library(oddstream)
library(animation)
saveGIF({ find_odd_streams(train_data, test_stream , plot_type = 'pcplot', trials = 100, window_skip = 50, concept_drift = TRUE, pc_boundary = 20) }, 
        interval = 0.2)


find_odd_streams <- function(train_data, test_stream, update_threshold = TRUE, update_threshold_freq,
                             plot_type = c("mvtsplot", "line", "pcplot", "out_location_plot"), window_length = nrow(train_data),
                             window_skip = window_length, concept_drift = FALSE, trials = 500, pc_boundary = 50) {
  
  train_features <- extract_tsfeatures(train_data)
  train_features <- scale(train_features, center = TRUE, scale = TRUE)
  pc <- get_pc_space(train_features)
  t <- set_outlier_threshold(pc$pcnorm, trials = trials )
  start <- seq(1, nrow(test_stream), window_skip)
  end <- seq(window_length, nrow(test_stream), window_skip)
  
  i <- 1
  while (i <= length(end)) {
    window_data <- test_stream[start[i]:end[i], ]
    
    window_features <- extract_tsfeatures(window_data)
    window_features <- scale(window_features, center = TRUE, scale = TRUE)
    pc_test <- scale(window_features, pc$center, pc$scale) %*% pc$rotation
    pctest <- pc_test[, 1:2]
    fhat_test <- ks::kde(x = pc$pcnorm, H = t$H_scv, compute.cont = TRUE, eval.points = pctest)
    outliers <- which(fhat_test$estimate < t$threshold_fnx)
    colnames(pctest) <- c("PC1", "PC2")
    pctest <- tibble::as_tibble(pctest)
    outlier_names <- paste("Series", outliers)
    if (plot_type == "line") {
      window_data_melt <- reshape::melt(window_data)
      window_data_melt <- dplyr::mutate(window_data_melt,
                                        type = ifelse(X2 %in% outlier_names,
                                                      "outlier" ,"normal"))
      line_plot <- ggplot(window_data_melt) +
        geom_line(aes_(x=~X1, y=~value, group = ~X2, color = ~type),
                  alpha=0.9, size = I(0.5))+
        scale_colour_manual(name="Type",
                            values = c("outlier"="red", "normal"="darkgray")) +
        xlab("Time") +
        ggtitle(paste("Data from: ", start[i], " to: ", end[i]))+
        expand_limits(y = c(-pc_boundary, pc_boundary))
      #ylim(-5,50)
      print(line_plot)
    }
    
    if (plot_type == "pcplot") {
      pc_norm <- tibble::as_tibble(pc$pcnorm)
      pc_norm <- dplyr::mutate(pc_norm, Series = 1:nrow(pc_norm))
      p1 <-  ggplot(pc_norm) +
        geom_point(aes(x=PC1, y= PC2),alpha = 0.5, color = "gray")+
        theme(aspect.ratio = 1) +
        expand_limits(y = c(-pc_boundary, pc_boundary),
                      x = c(-pc_boundary,pc_boundary))
      
      
      pc_test <- dplyr::mutate(pctest, Series = 1:nrow(pctest))
      pc_test <- dplyr::mutate(pc_test, type = ifelse(Series %in% outliers,
                                                      "outlier" ,"normal"))
      
      labeled.dat <- pc_test[pc_test$Series %in% outliers ,]
      p2 <-  p1 +
        geom_point(data = pc_test, alpha = 0.5,
                   aes(x=PC1, y= PC2, colour = type)) +
        scale_colour_manual(name="Type",
                            values = c("outlier"="red", "normal"="lightblue")) +
        ggtitle(paste("Data from: ", start[i], " to: ", end[i])) +
        geom_text( data = labeled.dat,aes(x=PC1, y= PC2, label = Series), hjust = 2)
      print(p2)
      
      
    }
    if (plot_type == "out_location_plot") {
      
      pctest <- tibble::as_tibble(pctest)
      pc_test <- dplyr::mutate(pctest, Series = 1:nrow(pctest))
      pc_test <- dplyr::mutate(pc_test, type = ifelse(Series %in% outliers,
                                                      "outlier" ,"normal"))
      
      out_plot <- ggplot(pc_test, aes(x= Series, y= type)) +
        geom_point()+
        ggtitle(paste("Data from: ", start[i], " to: ", end[i]))
      print(out_plot)
    }
    
    if (plot_type == "mvtsplot") {
      par(pty = "m")
      colnames(window_data) <- 1:ncol(window_data)
      mvtsplot::mvtsplot(window_data, levels=8, gcol=2, norm="global")
    }
    
    
    
    if (length(outliers) > 0) {
      cat("Outliers from: ", start[i], " to: ", end[i], ": ", outliers, "\n")
    } else {
      cat("Outliers from: ", start[i], " to: ", end[i], ": ", "NULL", "\n")
    }
    
    if (concept_drift == TRUE)
    {
      if (length(outliers) > 0) {
        t <- set_outlier_threshold(pctest[-outliers,], trials = trials )
        pc$pcnorm <- pctest[-outliers,]
      } else {
        t <- set_outlier_threshold(pctest, trials = trials )
        pc$pcnorm <- pctest
      }
    }
    
    i <- i + 1
    
  }
  # dev.off()
}
 