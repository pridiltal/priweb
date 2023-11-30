## ---- load
library(tidyverse)
library(gridExtra)
library(ggpubr)
# install.packages("devtools")
# devtools::install_github("pridiltal/oddstream")
library(oddstream)
library(animation)
library(tourr)
library(stray)
library(HDoutliers)
library(lvplot)

# cover
anomalous_stream <- load("E://pritalks/PhD_mid_candidature_review_2018/data/anomalous_stream_1.rda")
t <- nrow(anomalous_stream_1)
f <- ncol(anomalous_stream_1)
g <- as_tibble(anomalous_stream_1) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p1 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#060135", "#000000", "#000000"),
    values = c(0, 0.1, max(anomalous_stream_1))
  ) +
  ylab(" ") +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  scale_x_continuous(breaks = seq(0, 1400, 200)) +
  xlab(" ") +
  theme(legend.position = "none", axis.title = element_text(size = 15))
print(p1)



## ---- creatGIF
# loading data
# x=read.csv(choose.files())
anomalous_stream <- load("E://pritalks/PhD_mid_candidature_review_2018/data/anomalous_stream_1.rda")

## creating GIF for datastream
window_skip = 10
window_length = 100
start <- seq(1, nrow(anomalous_stream_1), window_skip)
end <- seq(window_length, nrow(anomalous_stream_1), window_skip)
st= length(end)-1
colnames(anomalous_stream_1)<- 1:ncol(anomalous_stream_1)

ggmvts<-function(data, start, end, st)
{
  maxcol = max(data)
  for (i in 1:st)
  {
    window_data <- data[start[i]:end[i], ]
    t <- nrow(window_data)
    f <- ncol(window_data)
    g <- as_tibble(window_data) %>%
      gather() %>%
      mutate(key = rep((1:f), each = t)) %>%
      mutate(Time = rep(1:t, f))
    colnames(g) <- c("Cable", "Value", "Time")
   p<- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
      geom_tile() +
      scale_fill_gradientn(
        colours = c("#F0E442",  "#000000", "#000000")
        ,  values = c(0, 0.3, maxcol)
      ) +
      ylab("") +
      xlab("") +
      theme(legend.position = "none", axis.title = element_text(size = 12) )
    print(p)
  }
}
saveGIF({ ggmvts(data= anomalous_stream_1 , start, end, st)}, interval = 0.05)
#animation::saveVideo({ ggmvts(data= anomalous_stream_1 , start, end, st=4)}, interval = 0.05)

## ---- mvtsplot1
# Figure 1 : Multiple parallel time series plot of a real world data set
# obtained using a fiber optic cable
anomalous_stream <- load("E://pritalks/PhD_mid_candidature_review_2018/data/anomalous_stream_1.rda")
t <- nrow(anomalous_stream_1)
f <- ncol(anomalous_stream_1)
g <- as_tibble(anomalous_stream_1) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p1 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#F0E442", "#000000", "#000000"),
    values = c(0, 0.1, max(anomalous_stream_1))
  ) +
  ylab(" ") +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  scale_x_continuous(breaks = seq(0, 1400, 200)) +
  xlab(" ") +
  theme(legend.position = "none", axis.title = element_text(size = 15))
print(p1)
ggsave(filename = "PhD_mid_candidature_review_2018/fig/2_batch.png", plot = p1,  width = 12, 
        height = 8, units = "cm")


p2<- p1 + ylab(" Cable / Time Series ID") +   xlab("Time ") 

ggsave(filename = "PhD_mid_candidature_review_2018/fig/3_batch.png", plot = p2,  width = 12, 
       height = 8, units = "cm")



## ---- tsfeatures_generate
# Generate Figure 5
t <- nrow(anomalous_stream_1)
f <- ncol(anomalous_stream_1)
ts <- oddstream::extract_tsfeatures(anomalous_stream_1)
ts <- cbind(Cable = 1:f, ts)
ts <- as_tibble(ts)
ts1 <- gather(ts, Feature, Value, -Cable)
ts1$Feature <- factor(ts1$Feature, levels = c(
  "spikiness", "lumpiness", "vchange", "lshift", "maximum",
  "moment3", "variance", "mean", "linearity", "highlowmu",
  "BurstinessFF", "curvature", "rmeaniqmean", "minimum"
))
p1 <- ggplot(ts1, aes(x = Cable, y = Value)) +
  geom_line() +
  facet_grid(Feature ~ ., scales = "free") +
  theme(
    strip.text.y = element_text(angle = 360, size = 10),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 14)
  ) +
  xlab("Cable/ Time Series ID") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2))
ggsave(filename = "PhD_mid_candidature_review_2018/fig/tsfeatures.png", plot = p1,
       width = 15, height = 19.5, units = "cm")



## ---- normalWindow
# Generate Figure for dimension reduction slides
anomalous_stream <- load("E://pritalks/PhD_mid_candidature_review_2018/data/anomalous_stream_1.rda")
anomalous_stream_1 <- anomalous_stream_1[50:150,]
t <- nrow(anomalous_stream_1)
f <- ncol(anomalous_stream_1)
g <- as_tibble(anomalous_stream_1) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p1 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#F0E442", "#000000", "#000000"),
    values = c(0, 0.7, max(anomalous_stream_1))
  ) +
  ylab(" Time Series ID") +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  xlab("Time ") +
  theme(legend.position = "none", axis.title = element_text(size = 13))
print(p1)
ggsave(filename = "PhD_mid_candidature_review_2018/fig/4_typical.png", plot = p1,  width = 12, 
       height = 8, units = "cm")

## high dimensional space
features <- oddstream::extract_tsfeatures(anomalous_stream_1)
saveGIF({ animate(features, grand_tour(d = 2), display = display_xy())}, interval = 0.05)

### 2D feture space
pc <- get_pc_space(features)
p1<-plotpc(pc$pcnorm, colour = "black",  alpha = 1)
ggsave(filename = "PhD_mid_candidature_review_2018/fig/6_typicalfeature.png", plot = p1,  width = 8, 
       height = 8, units = "cm")


## ---- EVDchange
# Figure 3 : Extreme value distributions with respect to m
set.seed(100)
m <- c(1, 10, 100, 1000)
max_m <- list()
for (i in 1:4)
{
  max <- numeric(1000)
  for (j in 1:1000)
  {
    max[j] <- max(rnorm(m[i]))
  }
  max_m[[i]] <- max
}
names(max_m) <- (c("EVD, m = 1", "EVD, m = 10", "EVD, m = 100", "EVD, m = 1000"))
max_m <- as_tibble(max_m)
new_data <- tidyr::gather(max_m, EVD, value)
p1<-ggplot(new_data, aes(x = value)) +
  geom_density(aes(group = EVD, fill = EVD), alpha = 0.8) +
  scale_fill_manual(values = c("yellow3", "grey63", "grey36", "black")) +
  xlab("x") +
  ylab("p(x)") +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22)
  ) +
  theme(legend.title = element_blank())
ggsave(filename = "PhD_mid_candidature_review_2018/fig/7_EVD1.png", plot = p1)


## ---- EVDchange
# Figure 3 : Extreme value distributions with respect to m
## standard normal
set.seed(100)
m <- c(1, 10, 100, 1000)
max_m <- list()
min_m <- list()
for (i in 1:4)
{
  max <- numeric(100000)
  min <- numeric(100000)
  for (j in 1:100000)
  {
    data <- rnorm(m[i])
    max[j] <- max(data)
    min[j] <- min(data)
  }
  max_m[[i]] <- max
  min_m[[i]] <- min
}
names(max_m) <- (c("EVD, m = 1", "EVD, m = 10", "EVD, m = 100", "EVD, m = 1000"))
names(min_m) <- (c("EVD, m = 1", "EVD, m = 10", "EVD, m = 100", "EVD, m = 1000"))
max_m <- as_tibble(max_m)
min_m <- as_tibble(min_m)
new_data1 <- tidyr::gather(max_m, EVD, value)
new_data2 <- tidyr::gather(min_m, EVD, value)
p3a <- ggplot(new_data1, aes(x = value)) +
  geom_density(aes(group = EVD, fill = EVD), alpha = 0.8) +
  scale_fill_manual(values = c("yellow3", "grey63", "grey36", "black")) +
  xlab("x") +
  ylab("p(x)") 

p3a <- p3a + geom_density(data= new_data2, aes(x = value, group = EVD, fill = EVD), alpha = 0.8) +
  scale_fill_manual(values = c("yellow3", "grey63", "grey36", "black")) +
  xlab("x") +
  ylab("p(x)") +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 12)  ) +
  theme(legend.title = element_blank())


## exponential
set.seed(100)
m <- c(1, 10, 100, 1000)
max_m <- list()
for (i in 1:4)
{
  max <- numeric(100000)
  for (j in 1:100000)
  {
    max[j] <- max(rexp(m[i]))
  }
  max_m[[i]] <- max
}
names(max_m) <- (c("EVD, m = 1", "EVD, m = 10", "EVD, m = 100", "EVD, m = 1000"))
max_m <- as_tibble(max_m)
new_data <- tidyr::gather(max_m, EVD, value)
p3b<-ggplot(new_data, aes(x = value)) +
  geom_density(aes(group = EVD, fill = EVD), alpha = 0.8) +
  scale_fill_manual(values = c("yellow3", "grey63", "grey36", "black")) +
  xlab("x") +
  ylab("p(x)") +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 12) ) +
  theme(legend.title = element_blank())


p1<-gridExtra::grid.arrange(p3a, p3b, nrow = 1)
ggsave(filename = "PhD_mid_candidature_review_2018/fig/7_EVD2.png", plot = p1)



## ---- threshold1
# Figure 3 :Distribution of 1000 extrema
# Calculating the density region for typical data
set.seed(123)
anomalous_stream <- load("E://pritalks/PhD_mid_candidature_review_2018/data/anomalous_stream_1.rda")
anomalous_stream_1 <- anomalous_stream_1[50:150,]
trials = 500
features <- extract_tsfeatures(anomalous_stream_1)
pc <- get_pc_space(features)
pc_pcnorm<- pc$pcnorm
H_scv <- ks::Hscv(x = pc_pcnorm)
fhat2 <- ks::kde(x = pc_pcnorm, H = H_scv, compute.cont = TRUE)

# generating data to find the threshold value
fun2 <- function(x) {
  return(MASS::mvrnorm(n = 1, mu = x, Sigma = H_scv))
}
m <- nrow(pc_pcnorm)
xtreme_fx <- numeric(trials)
f_extreme <- function(tempt) {
  s <- sample(1:m, size = m, replace = T)
  fhat <- ks::kde(x = pc_pcnorm, H = H_scv, compute.cont = TRUE, eval.points = t(apply(pc_pcnorm[s, ],
                                                                                       1, fun2)))
  return(tempt <- min(fhat$estimate))
}
xtreme_fx <- sapply(X = xtreme_fx, f_extreme)
xtreme_fx<-as_tibble(xtreme_fx)
p1<-ggplot(xtreme_fx, aes(value)) +
  geom_histogram(bins = 30)+
  xlab( expression(paste(f(x))))+
  ylab(expression(paste(P,'[',f(x),']')))+
  theme(legend.position = "none", axis.title = element_text(size = 13))
ggsave(filename = "PhD_mid_candidature_review_2018/fig/9_threshold1.png", plot = p1, width = 12, 
       height = 8, units = "cm")


## Psi transform

k <- 1/(2 * pi)
psi_trans <- ifelse(xtreme_fx < k, (-2 * log(xtreme_fx) - 2 * log(2 * pi))^0.5, 0)
psi_trans<-as_tibble(psi_trans)
p1<-ggplot(psi_trans, aes(value)) +
  geom_histogram(bins = 30)+
  ylab( expression(paste('f'[e],(psi))))+
  xlab(expression(paste(Psi,'[',f(x),']')))+
  theme(legend.position = "none", axis.title = element_text(size = 14))
ggsave(filename = "PhD_mid_candidature_review_2018/fig/11_psi_trans.png", plot = p1, width = 12, 
       height = 8, units = "cm")


##---- oddstreamdemo
## features for the tyical data set

anomalous_stream <- load("E://pritalks/PhD_mid_candidature_review_2018/data/anomalous_stream_1.rda")
anomalous_stream_1 <- anomalous_stream_1[50:150,]
t <- nrow(anomalous_stream_1)
f <- ncol(anomalous_stream_1)
ts <- oddstream::extract_tsfeatures(anomalous_stream_1)
ts <- cbind(Cable = 1:f, ts)
ts <- as_tibble(ts)
ts1 <- gather(ts, Feature, Value, -Cable)
ts1$Feature <- factor(ts1$Feature, levels = c(
  "spikiness", "lumpiness", "vchange", "lshift", "maximum",
  "moment3", "variance", "mean", "linearity", "highlowmu",
  "BurstinessFF", "curvature", "rmeaniqmean", "minimum"
))
p1 <- ggplot(ts1, aes(x = Cable, y = Value)) +
  geom_line() +
  facet_grid(Feature ~ ., scales = "free") +
  theme(
    strip.text.y = element_text(angle = 360, size = 10),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14)
  ) +
  xlab("Cable/ Time Series ID") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2))
ggsave(filename = "PhD_mid_candidature_review_2018/fig/15_typicalfeatures.png", plot = p1,
       width = 15, height = 19.5, units = "cm")


## ----oddstreamdemo_outlocationgif
### oddstream demo + out location plot

load("E://pritalks/PhD_mid_candidature_review_2018/data/anomalous_stream_1.rda")
train_data <- anomalous_stream_1[1:100,]
test_stream <-anomalous_stream_1[101:1456,]

saveGIF({oddstream::find_odd_streams(train_data, test_stream , 
                                     plot_type = "out_location_plot", 
                                     trials = 100, window_skip = 50, update_threshold = FALSE)},
         interval = 0.2)

 ###oddstream pcplot       

load("E://pritalks/PhD_mid_candidature_review_2018/data/anomalous_stream_1.rda")
train_data <- anomalous_stream_1[1:100,]
test_stream <-anomalous_stream_1[101:1456,]

saveGIF({find_odd_streams(train_data, test_stream , 
                                     plot_type = "pcplot", 
                                     trials = 100, window_skip = 50, update_threshold = FALSE, pc_boundary = 40)},
        interval = 0.2)



###f=data steram starting from  t=100
load("E://pritalks/PhD_mid_candidature_review_2018/data/anomalous_stream_1.rda")
window_skip = 50
window_length = 100
start <- seq(1, nrow(test_stream), window_skip)
end <- seq(window_length, nrow(test_stream), window_skip)
st= length(end)-1
colnames(test_stream)<- 1:ncol(test_stream)

ggmvts<-function(data, start, end, st)
{
  maxcol = max(data)
  for (i in 1:st)
  {
    window_data <- data[start[i]:end[i], ]
    t <- nrow(window_data)
    f <- ncol(window_data)
    g <- as_tibble(window_data) %>%
      gather() %>%
      mutate(key = rep((1:f), each = t)) %>%
      mutate(Time = rep(1:t, f))
    colnames(g) <- c("Cable", "Value", "Time")
    p<- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
      geom_tile() +
      scale_fill_gradientn(
        colours = c("#F0E442",  "#000000", "#000000")
        ,  values = c(0, 0.2, maxcol)
      ) +
      ylab("Time Series ID") +
      xlab("Time") +
      theme(legend.position = "none", axis.title = element_text(size = 16) )
    print(p)
  }
}
saveGIF({ ggmvts(data= test_stream , start, end, st)}, interval = 0.2,  ani.height=800, ani.width = 1500)



## ---- nonstationarity_sudden

## ---- sudden_analysis
# Figure 7
load(file = "E://pritalks/PhD_mid_candidature_review_2018/data/simulate_7.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p1 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000")) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 300, 100)) +
  scale_x_continuous(breaks = seq(0, 700, 100)) +
  xlab("Time") +
  ggtitle("(a) Sudden non-stationarity") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 8)
  ) +
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 0.5))
df <- as_tibble(tsframe)
df <- mutate(df, Time = 1:nrow(df))
df <- gather(df, Series, Value, -Time)
p2 <- ggplot(data = df, aes(x = Time, y = Value, group = Series)) +
  geom_line(alpha = 0.5) +
#  ggtitle("(b)  Multivariate time series plot (side view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 700, 100))




## ---- gradual_analysis
# Figure 8
load(file = "E://pritalks/PhD_mid_candidature_review_2018/data/simulate_10.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p3 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000")) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 300, 100)) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  xlab("Time") +
 ggtitle("(b) Gradual non-stationarity") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 8)
  ) +
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 0.5))
df <- as_tibble(tsframe)
df <- mutate(df, Time = 1:nrow(df))
df <- gather(df, Series, Value, -Time)
p4 <- ggplot(data = df, aes(x = Time, y = Value, group = Series)) +
  geom_line(alpha = 0.5) +
 # ggtitle("(b)  Multivariate time series plot (side view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 1000, 100))


## ---- reoccurring_analysis
# Figure 9
load(file = "E://pritalks/PhD_mid_candidature_review_2018/data/simulate_11.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p5<- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000")) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 300, 100)) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  xlab("Time") +
 ggtitle("(c)  Reoccuring non-stationarity") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 8)
  ) +
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 0.5))
df <- as_tibble(tsframe)
df <- mutate(df, Time = 1:nrow(df))
df <- gather(df, Series, Value, -Time)
p6 <- ggplot(data = df, aes(x = Time, y = Value, group = Series)) +
  geom_line(alpha = 0.5) +
#  ggtitle("(b)  Multivariate time series plot (side view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 1000, 100))

## ---- incremental_analysis
# Figure 10
load(file = "E://pritalks/PhD_mid_candidature_review_2018/data/simulate_14.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p7 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000")) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 300, 100)) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  xlab("Time") +
 ggtitle("(a)  Incremental non-stationarity") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 8)
  ) +
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 0.5))
df <- as_tibble(tsframe)
df <- mutate(df, Time = 1:nrow(df))
df <- gather(df, Series, Value, -Time)
p8 <- ggplot(data = df, aes(x = Time, y = Value, group = Series)) +
  geom_line(alpha = 0.5) +
 # ggtitle("(b)  Multivariate time series plot (side view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 1000, 100))

p <- gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 4)
ggsave(filename = "PhD_mid_candidature_review_2018/fig/19_nonstationaritytypes.png", plot = p, width = 15, 
       height = 16, units = "cm")



## ---- nonstationarity_mechanism_sudden

## ---- sudden_analysis
# Figure 7
load(file = "E://pritalks/PhD_mid_candidature_review_2018/data/simulate_7.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
df <- as_tibble(tsframe)
df <- mutate(df, Time = 1:nrow(df))
df <- gather(df, Series, Value, -Time)
p1 <- ggplot(data = df, aes(x = Time, y = Value, group = Series)) +
  geom_line(alpha = 0.5) +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 700, 100))
ggsave(filename = "PhD_mid_candidature_review_2018/fig/20_sudden plot1.png", plot = p1)


features_1 <- oddstream::extract_tsfeatures(tsframe[1:100, ])
features_2 <- oddstream::extract_tsfeatures(tsframe[200:300, ])
features_3 <- oddstream::extract_tsfeatures(tsframe[201:301, ])

pc1 <- get_pc_space(features_1)
pc_test_2<- scale(features_2, pc1$center, pc1$scale) %*% pc1$rotation
pc2 <- pc_test_2[, 1:2]
pc_test_3<- scale(features_3, pc1$center, pc1$scale) %*% pc1$rotation
pc3 <- pc_test_3[, 1:2]

pc12d<-as_tibble(pc1$pcnorm)
pc22d<-as_tibble(pc2)
pc32d<-as_tibble(pc3)
colnames(pc32d)<-colnames(pc22d) <- c("PC1", "PC2")

pc_boundary = 10
p1<- ggplot(pc12d, aes(PC1, PC2)) +
  geom_point(alpha = 0.3)+
  geom_density_2d(color = "black")+ 
  theme(aspect.ratio = 1) +
  expand_limits(y = c(-5, 5),
                x = c(-5,5))
p2<- p1+ 
  geom_density_2d(data = pc22d,aes(PC1, PC2), color = "forestgreen")+ 
  theme(aspect.ratio = 1) +
  expand_limits(y = c(-5, 5),
                x = c(-5,5))+
  geom_point(data = pc22d,aes(PC1, PC2),alpha = 0.3, color = "forestgreen")
p3<- p1+ 
  geom_density_2d(data = pc32d,aes(PC1, PC2),color = "darkorange")+ 
  theme(aspect.ratio = 1) +
  expand_limits(y = c(-5, 9),
                x = c(-20,42))+
  geom_point(data = pc32d,aes(PC1, PC2),alpha = 0.3, color = "darkorange")
 

ggsave(filename = "PhD_mid_candidature_review_2018/fig/21_noCD1.png", plot = p1,  width = 8, 
       height = 8, units = "cm")

ggsave(filename = "PhD_mid_candidature_review_2018/fig/21_noCD2.png", plot = p2,  width = 8, 
       height = 8, units = "cm")
ggsave(filename = "PhD_mid_candidature_review_2018/fig/21_noCD3.png", plot = p3, width = 8, 
       height = 8, units = "cm")



##---- sudden_output
#### sudden type outpputs

## ---- conceptdrift
# Figure 11
load(file = "E://pritalks/PhD_mid_candidature_review_2018/data/cd_sim7_pval_wonorm_padjust.rda")
r <- length(cd_sim7_wonorm_padjust)
sudden <- tibble(time = (1:r) + 200, pval = cd_sim7_wonorm_padjust)
p1 <- ggplot(sudden, aes(x = time, y = pval)) + geom_line(size = 0.8, linetype = 1, alpha = 1) +
  expand_limits(x = c(0, 700), y = c(0, 1)) +
  ylab("p value") +
  xlab("Time") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 8),
    axis.title = element_text(size = 14), title = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = seq(0, 700, 100)) +
  geom_hline(yintercept = 0.05)
ggsave(filename = "PhD_mid_candidature_review_2018/fig/22_conceptdrift_pval.png", plot = p1, width= 25.3492, height = 5, units = "cm")




## ---- sudden_analysis
# Figure 7
load(file = "E://pritalks/PhD_mid_candidature_review_2018/data/simulate_7.rda")
t <- nrow(tsframe)
f <- ncol(tsframe)
g <- as_tibble(tsframe) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p1 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#F0E442", "#000000")) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 300, 100)) +
  scale_x_continuous(breaks = seq(0, 700, 100)) +
  xlab("Time") +
  ggtitle("(a) Multivariate time series plot (top view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 0.5))
df <- as_tibble(tsframe)
df <- mutate(df, Time = 1:nrow(df))
df <- gather(df, Series, Value, -Time)
p2 <- ggplot(data = df, aes(x = Time, y = Value, group = Series)) +
  geom_line(alpha = 0.5) +
  ggtitle("(b)  Multivariate time series plot (side view)") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 8), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 700, 100))

load(file = "E://pritalks/PhD_mid_candidature_review_2018/data/out_simulate_7.rda")
t <- nrow(out_sim7_wonorm_padjust)
f <- ncol(out_sim7_wonorm_padjust)
g <- as_tibble(out_sim7_wonorm_padjust) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p3 <- ggplot(g, aes(x = Time + 200, y = Cable, fill = factor(Value))) +
  geom_tile() +
  scale_fill_manual(values = c("lightgray", "black"), labels = c("normal", "outlier"), name = "Output") +
  scale_x_continuous(breaks = seq(0, 700, 100)) +
  ylab("Time Series ID") +
  xlab("Time") +
  expand_limits(x = c(0, 700)) +
  ggtitle("(c) Resulting Output") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 7), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))

load(file = "E://pritalks/PhD_mid_candidature_review_2018/data/true_sim7.rda")
true_sim7 <- true_sim7[201:700, ]
c <- ncol(true_sim7)
r <- nrow(true_sim7)
false_positive_7 <- rep(0, r)
false_negative_7 <- rep(0, r)
accuracy_7 <- rep(0, r)
for (i in 1:r)
{
  false_positive_7[i] <- sum(out_sim7_wonorm_padjust[i, ] == 1 & true_sim7[i, ] == 0) / c
  false_negative_7[i] <- sum(out_sim7_wonorm_padjust[i, ] == 0 & true_sim7[i, ] == 1) / c
  accuracy_7[i] <- sum(out_sim7_wonorm_padjust[i, ] == true_sim7[i, ]) / c
}
fp <- tibble(time = (1:r) + 200, fptv = false_positive_7)
power <- tibble(time = (1:r) + 200, fntv = false_negative_7)
accuracy <- tibble(time = (1:r) + 200, correct = accuracy_7)
p4 <- ggplot(fp, aes(x = time, y = fptv)) + geom_line(aes(color = "False positive"), size = 1, linetype = 1, alpha = 1) +
  geom_line(data = accuracy, aes(x = time, y = correct, color = "Accuracy"), size = 1, linetype = 1) +
  geom_line(data = power, aes(x = time, y = fntv, color = "False negative"), size = 1, linetype = 1, alpha = 1) +
  labs(color = "") +
  expand_limits(x = c(0, 700), y = c(0, 1)) +
  ylab("Performance") +
  theme(
    legend.title = element_text(size = 8), legend.text = element_text(size = 12), axis.text = element_text(size = 6),
    axis.title = element_text(size = 8), title = element_text(size = 12)
  ) +
  ggtitle("(d) Performance of the algorithm 1-3") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("#000000", "#D55E00", "#0072B2")) +
  scale_y_continuous(breaks = seq(0, 500, 0.2)) +
  scale_x_continuous(breaks = seq(0, 700, 100))
p <- gridExtra::grid.arrange(p1,p2,p3,p4)
ggsave(filename = "PhD_mid_candidature_review_2018/fig/23_sudden_out.png", plot = p)

#########################################

##project 2

##--- clustering step
set.seed(1234)
data<- cbind(x= c(rnorm(1000), rnorm(5, mean = 10, sd = .2),-2.6),y= c(rnorm(1000), rnorm(5, mean = 10, sd = .2),10) )
data<-as.data.frame(data)

out_woclust<-HDoutliers(data)

p1 <- ggplot(data, aes(x=x, y=y)) +
  geom_point() +
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot1.png", plot = p1, width = 8, 
       height = 8, units = "cm")

out_withclust<-HDoutliers(data, maxrows = 100)

unitize <- function(z) {
  zrange <- range(z)
  if (!(dif <- diff(zrange))) 
    return(rep(0, length(z)))
  (z - zrange[1])/dif
}
udata <- apply(as.matrix(data), 2, unitize)
members <- getHDmembers(udata, radius = NULL, maxrows = 100)
index<- unlist(lapply(members, `[[`, 1))
r= .1/(log(nrow(data))^(1/ncol(data)))
p2<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data=data, aes(x=x, y=y), colour = "black")+
  geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.5)+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 
 
ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot2.png", plot = p2, width = 8, 
       height = 8, units = "cm")

p3<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data=data, aes(x=x, y=y), colour = "dark gray", fill = "dark gray")+
  geom_point(data = data[index, ], aes(x, y), colour= "black")+
  geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.5)+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot3.png", plot = p3, width = 8, 
       height = 8, units = "cm")


p4<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data = data, aes(x, y), colour= "black")+
 #geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.2)+
  geom_point(data=data[out_withclust,], aes(x=x, y=y), colour = "red", shape=23, fill = "red", size = 1.5) +
  #ggtitle("(c) Case: n > maxrows ")+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 


ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot4.png", plot = p4, width = 8, 
       height = 8, units = "cm")



### adding nearest neightbours
set.seed(1234)
data<- cbind(x= c(rnorm(1000), rnorm(5, mean = 10, sd = .5),-2.6),y= c(rnorm(1000), rnorm(5, mean = 10, sd = .3),10) )
data<-as.data.frame(data)
p5 <- ggplot(data, aes(x=x, y=y)) +
  geom_point() +
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 
ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot5.png", plot = p5, width = 8, 
       height = 8, units = "cm")
out_withclust<-HDoutliers(data, maxrows = 100)

unitize <- function(z) {
  zrange <- range(z)
  if (!(dif <- diff(zrange))) 
    return(rep(0, length(z)))
  (z - zrange[1])/dif
}
udata <- apply(as.matrix(data), 2, unitize)
members <- getHDmembers(udata, radius = NULL, maxrows = 100)
index<- unlist(lapply(members, `[[`, 1))
r= .1/(log(nrow(data))^(1/ncol(data)))
p6<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data=data, aes(x=x, y=y), colour = "black")+
  geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.5)+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot6.png", plot = p6, width = 8, 
       height = 8, units = "cm")


p7<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data=data, aes(x=x, y=y), colour = "dark gray", fill = "dark gray")+
  geom_point(data = data[index, ], aes(x, y), colour= "black")+
  geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.5)+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot7.png", plot = p7, width = 8, 
       height = 8, units = "cm")




p8<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data = data, aes(x, y), colour= "black")+
  #geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.2)+
  geom_point(data=data[out_withclust,], aes(x=x, y=y), colour = "red", shape=23, fill = "red", size = 1.5) +
  #ggtitle("(c) Case: n > maxrows ")+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 


ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot8.png", plot = p8, width = 8, 
       height = 8, units = "cm")

out_withclust<-stray::find_HDoutliers(data, maxrows = 100)

p10<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data = data, aes(x, y), colour= "black")+
  #geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.2)+
  geom_point(data=data[out_withclust,], aes(x=x, y=y), colour = "red", shape=23, fill = "red", size = 1.5) +
  #ggtitle("(c) Case: n > maxrows ")+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot10.png", plot = p10, width = 8, 
       height = 8, units = "cm")



## dense cluster problem
library(stray)
## example 2

set.seed(123)
data<-cbind(x= c(rnorm(1000, mean =10, sd=1), 18, c(rnorm(1000, mean =28, sd=0.15))),y= c(rnorm(1000,mean =28, sd=1.5), 18,  c(rnorm(1000, mean =10, sd=0.15)) ) )
data <- as.data.frame(data)

out_woclust<-HDoutliers(data)

p11 <- ggplot(data, aes(x=x, y=y)) +
  geom_point() +
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot11.png", plot = p11, width = 8, 
       height = 8, units = "cm")

out_withclust<-HDoutliers(data, maxrows = 100)

unitize <- function(z) {
  zrange <- range(z)
  if (!(dif <- diff(zrange))) 
    return(rep(0, length(z)))
  (z - zrange[1])/dif
}
udata <- apply(as.matrix(data), 2, unitize)
members <- getHDmembers(udata, radius = NULL, maxrows = 100)
index<- unlist(lapply(members, `[[`, 1))
r= .1/(log(nrow(data))^(1/ncol(data)))
p12<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data=data, aes(x=x, y=y), colour = "black")+
  geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.5)+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot12.png", plot = p12, width = 8, 
       height = 8, units = "cm")


p13<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data=data, aes(x=x, y=y), colour = "dark gray", fill = "dark gray")+
  geom_point(data = data[index, ], aes(x, y), colour= "black")+
  geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.5)+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot13.png", plot = p13, width = 8, 
       height = 8, units = "cm")


p14<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data = data, aes(x, y), colour= "black")+
  #geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.2)+
  geom_point(data=data[out_withclust,], aes(x=x, y=y), colour = "red", shape=23, fill = "red", size = 1.5) +
  #ggtitle("(c) Case: n > maxrows ")+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 


ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot14.png", plot = p14, width = 8, 
       height = 8, units = "cm")



break_list<-function(x){
  max <- floor(nrow(data) / 20)
  seq <- seq_along(x)
  split(x, ceiling(seq/max))
}

members<- lapply(members, break_list)
members<- purrr::flatten(members)
exemplars <- sapply(members, function(x) x[[1]])
names(members) <- exemplars
index<- unlist(lapply(members, `[[`, 1))
p15<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data=data, aes(x=x, y=y), colour = "dark gray", fill = "dark gray")+
  geom_point(data = data[index, ], aes(x, y), colour= "black")+
  geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.5)+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot15.png", plot = p15, width = 8, 
       height = 8, units = "cm")


out_withclust<-stray::find_HDoutliers(data, maxrows = 100)
p16<-ggplot(data, aes(x=x, y=y))+ 
  geom_point(data = data, aes(x, y), colour= "black")+
  #geomnet::geom_circle( data = data[index, ],radius=r, alpha=0.2)+
  geom_point(data=data[out_withclust,], aes(x=x, y=y), colour = "red", shape=23, fill = "red", size = 1.5) +
  #ggtitle("(c) Case: n > maxrows ")+
  theme(aspect.ratio = 1, text = element_text(size=14), plot.margin = unit(c(1,1,0,1), "lines"), axis.text.x = element_blank(),
        axis.text.y = element_blank()) 


ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot16.png", plot = p16, width = 8, 
       height = 8, units = "cm")


## ---- spacings
# Figure 1 : Spacing theorem 
set.seed(123)
nobs <- 20000
nsamples <- 1000
trials <- matrix(rnorm(nobs*nsamples,0,1),nrow=nsamples)
trials.sorted <- t(apply(trials, 1, function(x) sort(x, decreasing=TRUE)))
trials.means <- apply(trials.sorted, 2, mean)
mean.spacings <- -diff(trials.means)
x <- 1/(1:(nobs-1))
a <- coef(lm(mean.spacings ~ 0 + x))
pred.spacings <- a*x
trial_mean <- tibble::tibble(Rank = 1:10 , order_mean = trials.means[1:10] )

p1 <- trial_mean %>% ggplot(aes(x=Rank, y = order_mean )) +
  geom_line() +
  xlab("Rank (i)")+
  ylab("Order Statistics")

ts_matrix <- trials.sorted[,1:10]
colnames(ts_matrix) <- 1:10
trial_sorted <- as.tibble(ts_matrix) %>%
  gather(Rank, "order_stat") %>% mutate(Rank = as.numeric(Rank))

p2<- p1 +
  geom_point(data = trial_sorted, aes(x = Rank, y= order_stat ), 
             colour = "dark gray", shape = 3)+
  geom_point(data= trial_mean,aes(x=Rank, y = order_mean ),       colour = "black", size = 2 )+
  scale_x_discrete(limits=0:10)+
  theme(aspect.ratio = 1)+
  ggtitle("(a)")


pred_spacings <- tibble(Rank = 1:10 , ps = pred.spacings[1:10] )
p3 <- pred_spacings %>% ggplot(aes(x=Rank, y = ps)) +
  geom_line() +
  xlab("Rank (i)")+
  ylab("Spacings") +
  scale_x_discrete(limits=0:10) 

mean_spacing <- tibble(Rank = 1:10, ms = mean.spacings[1:10] )
p4 <- p3 +
  geom_point(data= mean_spacing, aes(x=Rank, y =ms ),             colour = "black", size = 2, shape = 3 )+
  theme(aspect.ratio = 1)+
  ggtitle("(b)")

ts_matrix_diff<- t(apply(ts_matrix, 1, (diff) )) * -1
ts_matrix_stdiff<- t(apply(ts_matrix_diff, 1, function(x) x*(1:9)))
data<- reshape2::melt(ts_matrix_stdiff)
p5 <- ggplot(data,aes(x=factor(Var2), y=value)) + 
  geom_lv(fill = "grey80", colour = "black")+
  ggtitle("(c)")+
  ylab(expression(paste("Standardized Spacings (",i*D[i],")")))+
  xlab("Rank (i)")+
  stat_summary(fun.y=mean, geom="point", shape=3, size=2)+
  theme(    text = element_text(size=12))   

p <- ggarrange(ggarrange(p2, p4, ncol = 2), p5, 
          # Second row with box and dot plots
          nrow = 2, heights = c(2, 1.5)  ) 


ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot17.png", plot = p)


########## Examples HDoutliers vs stray
#Example 1
set.seed(1234)
data<-cbind(x= c(rnorm(1000), rnorm(3, mean = 10, sd = .5), rnorm(1000, mean= 20)),y= c(rnorm(1000), rnorm(3, mean = 10, sd = .5), rnorm(1000) ) )
data <- as.data.frame(data)

out_HD<-HDoutliers(data, maxrows = 100)
out_stray <- stray::find_HDoutliers(data, maxrows = 100, alpha = 0.05)

p1<- stray::display_HDoutliers(data, out_HD ) +   theme(aspect.ratio = 1, legend.position = 'none', axis.title = element_text(size = 8), axis.text = element_text(size = 6)) 
p2<- stray::display_HDoutliers(data, out_stray )  + theme(axis.text = element_text(size = 6),aspect.ratio = 1, legend.position = 'none', axis.title = element_text(size = 8))

#Example 2-  wheel
set.seed(123)
n <- 1e3
rho <- sqrt(runif(n))+0.9
theta <- runif(n, 0, 2*pi)
x <- rho * cos(theta)
y <- rho * sin(theta)
data <- rbind(cbind(x,y),c(0,0), c(0.2,0.2))
data <- as.data.frame(data)

out_HD<-HDoutliers(data, maxrows = 100)
out_stray <- stray::find_HDoutliers(data, maxrows = 100, alpha = 0.05)

p3<- stray::display_HDoutliers(data, out_HD ) + theme(axis.text = element_text(size = 6),aspect.ratio = 1, legend.position = 'none', axis.title = element_text(size = 8)) 
p4<- stray::display_HDoutliers(data, out_stray )  + theme(axis.text = element_text(size = 6),aspect.ratio = 1, legend.position = 'none', axis.title = element_text(size = 8))

#Example 3
set.seed(123)
data<-cbind(x= c(rnorm(1000, mean =10, sd=1), 18, c(rnorm(1000, mean =28, sd=0.15))),y= c(rnorm(1000,mean =28, sd=1.5), 18,  c(rnorm(1000, mean =10, sd=0.15)) ) )
data <- as.data.frame(data)

out_HD<-HDoutliers(data, maxrows = 100)
out_stray <- stray::find_HDoutliers(data, maxrows = 100, alpha = 0.05)

p5<- stray::display_HDoutliers(data, out_HD ) + theme(axis.text = element_text(size = 6),aspect.ratio = 1, legend.position = 'none', axis.title = element_text(size = 8)) 
p6<- stray::display_HDoutliers(data, out_stray )  + theme(axis.text = element_text(size = 6),aspect.ratio = 1, legend.position = 'none', axis.title = element_text(size = 8))



## example 4
set.seed(123)
data<-cbind(x= c(rnorm(2000, mean =10, sd=.25), 18),y= c(rnorm(2000,mean =28, sd=.25), 18) )
data <- as.data.frame(data)


out_HD<-HDoutliers(data, maxrows = 100)
out_stray <- stray::find_HDoutliers(data, maxrows = 100, alpha = 0.05)

p7<- stray::display_HDoutliers(data, out_HD ) + theme(axis.text = element_text(size = 6),aspect.ratio = 1, legend.position = 'none', axis.title = element_text(size = 8)) 
p8<- stray::display_HDoutliers(data, out_stray )  + theme(axis.text = element_text(size = 6),aspect.ratio = 1, legend.position = 'none', axis.title = element_text(size = 8))




## example 5

set.seed(123)
data<-cbind(x= c(rnorm(1000, mean =10, sd=6), 28),y= c(rnorm(1000,mean =10, sd=3),28))
data <- as.data.frame(data)

out_HD<-HDoutliers(data, maxrows = 100)
out_stray <- stray::find_HDoutliers(data, maxrows = 100, alpha = 0.05)

p9<- stray::display_HDoutliers(data, out_HD ) + theme(axis.text = element_text(size = 6),aspect.ratio = 1, legend.position = 'none', axis.title = element_text(size = 8)) 
p10<- stray::display_HDoutliers(data, out_stray )  + theme(axis.text = element_text(size = 6),aspect.ratio = 1, legend.position = 'none', axis.title = element_text(size = 8))


p <- gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, p9,p10, ncol=2)


ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot18.png", plot = p, width = 8, 
       height = 16, units = "cm")

######
##stray for stream

## ---- creatGIF

window_data <- dat5

    f <- ncol(window_data)
    t <- nrow(window_data)
    g <- as_tibble(window_data) %>%
      gather() %>%
      mutate(key = rep((1:f), each = t)) %>%
      mutate(Time = rep(1:t, f))
    colnames(g) <- c("Cable", "Value", "Time")
    p<- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
      geom_tile() +
      scale_fill_gradientn(
        colours = c("#F0E442",  "#000000", "#000000")
        ,  values = c(0, 0.3, max(dat5))
      ) +
      ylab("") +
      xlab("") +
      theme(legend.position = "none", axis.title = element_text(size = 12) )


ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot22.png", plot = p, width = 12, 
           height = 8, units = "cm")



features<-oddstream::extract_tsfeatures(dat5)


saveGIF({ stray::display_HDoutliers(features, out=c(49,65,78))}, interval = 0.05)

###############

## ---- sensortypes
# Figure 4 : Feature space for different sensor types
# Figure 4(b)
load("E://pritalks/PhD_mid_candidature_review_2018/data/anomalous_stream_1.rda")
features1 <- extract_tsfeatures(anomalous_stream_1)
pc1 <- get_pc_space(features1)
g <- as_tibble(pc1$pcnorm) %>%
  mutate(row_n = 1:ncol(anomalous_stream_1)) %>%
  mutate(type = ifelse(1:ncol(anomalous_stream_1) %in%
                         350:560, "out", "normal")) %>%
  mutate(Value = features1[, 1])
g <- g[-(c(449:450, 520, 613:616)), ]
p1 <- g %>%
  ggplot(aes(x = PC1, y = PC2, label1 = row_n)) +
  geom_point(aes(colour = Value)) +
  scale_color_gradientn(
    colours = c("#F0E442", "#000000", "#000000"),
    values = c(0, .1, max(anomalous_stream_1))
  ) +
  geom_text(
    aes(label = ifelse(row_n %in% c(seq(350, 550, 25), 448, 451),
                       as.character(row_n), ""
    )), hjust = 1, vjust = 1,
    col = "black", size = 3
  ) +
  theme(aspect.ratio = 1) +
  expand_limits(
    y = c(-8, 4),
    x = c(-4, 7)
  ) +
  ggtitle("(b) Feature space") +
  theme(legend.position = "none") +
  stat_density_2d(alpha = 0.4, bins = 4)

# Figure 4(a)
anomalous_stream_2 <- anomalous_stream_1
j <- c(290:399, 401:449, 451:590)
set.seed(123)
for (i in 1:length(j)) {
  anomalous_stream_2[, j[i]] <- (anomalous_stream_2[, sample(1:300, 1)] +
                                   rnorm(nrow(anomalous_stream_2), 0, 0.5))
}

anomalous_stream_2[, 1:290] <- anomalous_stream_2[, sample(1:290)] +
  rnorm(nrow(anomalous_stream_2), 0, 0.5)
anomalous_stream_2[, 200] <- anomalous_stream_2[, 400] * 4
anomalous_stream_2[, 400] <- anomalous_stream_2[, 300]
features2 <- extract_tsfeatures(anomalous_stream_2)
pc2 <- get_pc_space(features2)
g2 <- as_tibble(pc2$pcnorm) %>%
  mutate(row_n = 1:ncol(anomalous_stream_2)) %>%
  mutate(type = ifelse(1:ncol(anomalous_stream_2) %in%
                         290, "out", "normal")) %>%
  mutate(Value = features2[, 1])
g2 <- g2[-(613:616), ]
p2 <- g2 %>%
  ggplot(aes(x = PC1, y = PC2, label1 = row_n)) +
  geom_point(aes(colour = Value)) +
  scale_color_gradientn(
    colours = c("#F0E442", "#000000", "#000000"),
    values = c(0, .1, max(anomalous_stream_2))
  ) +
  geom_text(
    aes(label = ifelse(row_n %in% c(200, 450), as.character(row_n), "")),
    hjust = 1, vjust = 1, col = "black", size = 3
  ) +
  theme(aspect.ratio = 1) +
  expand_limits(y = c(-5, 20), x = c(-25, 7)) +
  ggtitle("(a) Feature space") +
  theme(legend.position = "none") +
  stat_density_2d(alpha = 0.4, bins = 3)

# Figure 4(d)
fence_climb1 <- anomalous_stream_1[, -(c(449:450, 520, 613:616))]
t <- nrow(fence_climb1)
f <- ncol(fence_climb1)
g <- as_tibble(fence_climb1) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p3 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#F0E442", "#000000", "#000000"),
    values = c(0, .1, max(fence_climb1))
  ) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  scale_x_continuous(breaks = seq(0, 1400, 200)) +
  xlab("Time") +
  theme(legend.position = "none") +
  ggtitle("(d) Multivariate time series plot")

# Figure 4(c)
fence_climb2 <- anomalous_stream_1
j <- c(290:399, 401:449, 451:590)
set.seed(123)
for (i in 1:length(j)) {
  fence_climb2[, j[i]] <- (fence_climb2[, sample(1:300, 1)] +
                             rnorm(nrow(fence_climb2), 0, 0.5))
}
fence_climb2[, 1:290] <- fence_climb2[, sample(1:290)] +
  rnorm(nrow(fence_climb2), 0, 0.5)
fence_climb2[, 200] <- fence_climb2[, 400] * 4
fence_climb2[, 400] <- fence_climb2[, 300]
t <- nrow(fence_climb2)
f <- ncol(fence_climb2)
g <- as_tibble(fence_climb2) %>%
  gather() %>%
  mutate(key = rep((1:f), each = t)) %>%
  mutate(Time = rep(1:t, f))
colnames(g) <- c("Cable", "Value", "Time")
p4 <- ggplot(g, aes(x = Time, y = Cable, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#F0E442", "#000000", "#000000"),
    values = c(0, .1, max(fence_climb2))
  ) +
  ylab("Time Series ID") +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  scale_x_continuous(breaks = seq(0, 1400, 200)) +
  xlab("Time") +
  theme(legend.position = "none") +
  ggtitle("(c) Multivariate time series plot")
p <- ggpubr::ggarrange(
  ggarrange(p2, p1, ncol = 2), ggarrange(p4, p3, ncol = 2),
  nrow = 2, heights = c(5, 3)
)

ggsave(filename = "PhD_mid_candidature_review_2018/fig/P2_plot21.png", plot = p)
