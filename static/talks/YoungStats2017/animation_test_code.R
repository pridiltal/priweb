# https://yihui.name/animation/example/savegif/
#refer above link for yuhi's comment

# you need to install a package named "installr" prior to installing ImageMagick.
#Simple Steps: 1. Tools->InstallPackages->installr 2. From R command Line write - 
#require(installr) - install.ImageMagick() 
#(This command will by default install the latest version, u can specify the URl of the version needed)

#The package will be installed !!!


###2nd method
#used these commands in RStudio.

#install.packages('installr')
#library(installr)
#install.ImageMagick()
#It installed the latest version of ImageMagick.

library(animation)
saveGIF({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
}, interval = 0.1)

ani.options('convert')

#saveVideo
library(demography)
saveVideo({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
}, interval = 0.1)

ani.options('convert')


#### mvtsplot
library(oddstream)
window_skip = 10
window_length = 100
start <- seq(1, nrow(data1), window_skip)
end <- seq(window_length, nrow(data1), window_skip)
st= length(end)-1

colnames(data1)<- 1:ncol(data1)

saveGIF({
  for (i in 1:st) 
  {window_data <- data1[start[i]:end[i], ]
  mvtsplot(window_data, margin=FALSE,  levels=8, gcol=2, norm="global")}
}, interval = 0.05, ani.width=820)


###############ppt GIF
set.seed(278)
nobs = 1500
nts = 500
tsframe <- ts(apply(matrix(ncol = nts, nrow = nobs), 2, function(nobs){10 + rnorm(nobs, 0, 2)}))
#tsframe1 <- ts(apply(matrix(ncol = nts, nrow = nobs), 2, function(nobs){10 + rnorm(nobs, 0, 2)}))
#tsframe2 <- ts(apply(matrix(ncol = nts, nrow = nobs), 2, function(nobs){20 + rnorm(nobs, 0, 2)}))
#tsframe=cbind(tsframe1,tsframe2)

dim(tsframe)
library(mvtsplot)
colnames(tsframe) <- 1:ncol(tsframe)
#mvtsplot(tsframe,levels=8,gcol=2,norm="global")


#to make the series an anomalouse time series

tsframe[250:350,50:60]=tsframe[250:350,50:60]*2
#tsframe[180:200,80:85]=tsframe[180:200,80:85]*4
mvtsplot(tsframe,levels=8,gcol=2,norm="global")
library(oddstream)

train_data <- tsframe[1:100,]
test_stream <-tsframe[101:1500,]

library(animation)
saveGIF({ find_odd_streams(train_data, test_stream , plot_type = "mvtsplot", trials = 300,  window_skip = 10 ) }, 
        interval = 0.1, ani.width=820)




####example 2
#Generate training dataset
set.seed(700)
nobs = 300
nts = 100
train_data <- ts(apply(matrix(ncol = nts, nrow = nobs), 2, function(nobs){10 + rnorm(nobs, 0, 2.5)}))
# Generate test stream with some outliying series
nobs = 5000
test_stream <- ts(apply(matrix(ncol = nts, nrow = nobs), 2, function(nobs){10 + rnorm(nobs, 0, 2.5)}))
test_stream[360:1060, 20:25] = test_stream[360:1060, 20:25] * 2.5
test_stream[2550:3550, 20:25] =  test_stream[2550:3550, 20:25] * 3.5
library(mvtsplot)
mvtsplot(test_stream,levels=8,gcol=2,norm="global")
library(animation)
saveGIF({ find_odd_streams(train_data, test_stream , plot_type = 'line', trials = 300, window_skip = 50) }, 
        interval = 0.2)

saveGIF({ find_odd_streams(train_data, test_stream , plot_type = 'pcplot', trials = 300, window_skip = 50,  pc_boundary = 20) }, 
        interval = 0.2)

saveGIF({ find_odd_streams(train_data, test_stream , plot_type = 'mvtsplot', trials = 100, window_skip = 50) }, 
        interval = 0.2, ani.width=1000)


####
library(ggplot2)
features <- oddstream::extract_tsfeatures(train_data)
pairs(features, col= 'blue')


pc <- oddstream::get_pc_space(features)
oddstream::plotpc(pc$pcnorm)
