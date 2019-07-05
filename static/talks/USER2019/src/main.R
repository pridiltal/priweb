
## ---- outtype
# Figure 2: Different types of anomalies in temporal data
set.seed(1234)
ts1 <- as.ts(rnorm(500, 10, 3))
ts1[300] <- 35
ts1[150] <- 30
ts1 <- as_tibble(ts1) %>% mutate(Time = 1:500)
colnames(ts1) <- c("Value", "Time")
point <- tibble(Time = c(150, 300), Value = c(30, 35))
p1 <- ggplot(ts1) +
  geom_line(aes(x = Time, y = Value), size = 0.5, alpha = 0.7) +
  geom_point(data = point, aes(x = Time, y = Value), col = "red", size = 2, alpha = 1) +
  ggtitle("(a) Contextual anomalies within a  given series") +
  expand_limits(y = c(-0, 40))+
  theme(plot.title = element_text(size = 12))

ts2 <- as.ts(rnorm(500, 10, 3))
ts2[250:325] <- rnorm(325:250, 40, 4)
ts2 <- as_tibble(ts2) %>% mutate(Time = 1:500)
colnames(ts2) <- c("Value", "Time")
point <- ts2[249:326, ]
p2 <- ggplot(ts2) +
  geom_line(aes(x = Time, y = Value), size = 0.5, alpha = 0.7) +
  geom_line(data = point, aes(x = Time, y = Value), col = "red", size = .5, alpha = 1) +
  ggtitle("(b) Collective anomalies: Anomalous sub-sequences within a given series")+
  theme(plot.title = element_text(size = 12))
# Data Generation
nobs <- 500
nts <- 20
tsframe <- ts(matrix(ncol = nts, nrow = nobs))
for (i in 1:nts) {
  tsframe[, i] <- 10 + rnorm(nobs, 0, .5) # adding noise
}
tsframe[200:500, 15] <- 5 + 1 * (200:500)^(1 / 3) + rnorm(301, 4, 1)
tsframe[1:200, 15] <- tsframe[1:200, 15] + rnorm(200, 2, 1)
data_melt <- reshape::melt(as.matrix(tsframe))
data_melt <- dplyr::mutate(data_melt, type = ifelse(X2 %in% "Series 15", "Outlier", "Typical"))
p3 <- ggplot(data_melt) +
  geom_line(
    aes_(x = ~X1, y = ~value, group = ~X2, color = ~type),
    alpha = 0.9, size = 0.5
  ) +
  scale_colour_manual(
    name = "Type",
    values = c("Typical" = "black", "Outlier" = "red")
  ) +
  xlab("Time") +
  ylab("Value") +
  ggtitle("(c) Anomalous series within a space of a collection of series") + 
  theme(legend.position = "",   plot.title = element_text(size = 12))
pa <- grid.arrange(p1, p2, p3, ncol = 1)



set.seed(12456)
x<- c(rnorm(2000,1,2) , rnorm(2000,13,2),  35, rnorm(5,-8, .5), -6, 14, -8)
y<- c(rnorm(2000,1,2),  rnorm(2000,13,2),  35, rnorm(5, 25, 0.5), 12, -6 , -8)
type <- c(rep("Typical", 4000), rep("Outlier", 9))
data <- data.frame(x,y, type)
label_data <- data[c(4001, 4002, 4008),] %>% mutate(outtype = c("Global Anomaly", "Micro clusters", "Local Anomaly"))
pb <- ggplot(data, aes(x,y, color = type))+
  geom_point()+
  scale_colour_manual(
    name = "Type",
    values = c("Typical" = "black", "Outlier" = "red")
  ) +
  geom_label_repel(data = label_data, aes(x = x, y = y,label = outtype),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'black') +
  theme(aspect.ratio = 1, legend.position="none")
  
 pb 
 
 p <- grid.arrange(pa, pb, ncol = 2)
 ggsave(
   filename = "figure/outtype.png", plot = p
 )
 
 
## ---- oddwater_analysis
 library(oddwater)
 library(tidyverse)
 library(ggplot2)
 
 data("data_sandy_anom")
 ## Change the format and the class of the existing variable
 data_sandy_anom$Timestamp <- lubridate::dmy_hm(data_sandy_anom$Timestamp)
 # Turbidity
 data_sandy_anom$out_type_Tur <- ifelse(data_sandy_anom$type_Tur %in% c("B", "C", "E", "H", "L"), "0", as.character(data_sandy_anom$type_Tur))
 # Conductivity
 data_sandy_anom$out_type_Cond <- ifelse(data_sandy_anom$type_Cond %in% c("B", "C", "E", "H", "L"), "0", as.character(data_sandy_anom$type_Cond))
 # Level
 data_sandy_anom$out_type_Level <- ifelse(data_sandy_anom$type_Level %in% c("B", "C", "E", "H", "L"), "0", as.character(data_sandy_anom$type_Level))
 # creat a new dataset including only outliers
 data_sandy_out <- data_sandy_anom[, c("Timestamp", "Level", "Cond", "Tur", "out_type_Tur", "out_type_Cond", "out_type_Level")]
 data_sandy_out$out_label_Tur <- ifelse((data_sandy_out$out_type_Tur == "0"), 0, 1)
 data_sandy_out$out_label_Cond <- ifelse((data_sandy_out$out_type_Cond == "0"), 0, 1)
 data_sandy_out$out_label_Level <- ifelse((data_sandy_out$out_type_Level == "0"), 0, 1)
 
 data_sandy_out[, c(8:10)] <- lapply(
   data_sandy_out[, c(8:10)],
   function(x) {
     x <- as.factor(x)
     levels(x) <- c("typical", "outlier")
     x
   }
 )
 
 out1 <- data_sandy_out[which(data_sandy_out$out_label_Tur == "outlier"), 1]
 out2 <- data_sandy_out[which(data_sandy_out$out_label_Cond == "outlier"), 1]
 out3 <- data_sandy_out[which(data_sandy_out$out_label_Level == "outlier"), 1]
 out <- unique(c(out1, out2, out3))
 
 index <- which(data_sandy_out$Timestamp %in% out)
 
 # neighbours
 n1 <- which(data_sandy_out$Timestamp %in% out1)-1
 n1b <- which(data_sandy_out$Timestamp %in% out1)+1
 n2 <- which(data_sandy_out$Timestamp %in% out2)-1
 n2b <- which(data_sandy_out$Timestamp %in% out2)+1
 n3 <- which(data_sandy_out$Timestamp %in% out3)-1
 n3b <- which(data_sandy_out$Timestamp %in% out3)+1
 
 
 data_sandy_out$out_label_Tur <- as.character(data_sandy_out$out_label_Tur)
 data_sandy_out$out_label_Tur[c(n1, n1b)] <- "neighbour"
 data_sandy_out$out_label_Tur <- as.factor(data_sandy_out$out_label_Tur)
 
 
 data_sandy_out$out_label_Cond  <- as.character(data_sandy_out$out_label_Cond )
 data_sandy_out$out_label_Cond[c(n2, n2b)] <- "neighbour"
 data_sandy_out$out_label_Cond  <- as.factor(data_sandy_out$out_label_Cond)
 
 
 data_sandy_out$out_label_Level <- as.character(data_sandy_out$out_label_Level)
 data_sandy_out$out_label_Level[c(n3, n3b)] <- "neighbour"
 data_sandy_out$out_label_Level <- as.factor(data_sandy_out$out_label_Level)
 
 
 data_sandy_out$label_all <- ifelse((data_sandy_out$out_label_Tur == "typical" &
                                       data_sandy_out$out_label_Cond == "typical" &
                                       data_sandy_out$out_label_Level == "typical"),
                                    "typical", "outlier"
 )
 
 data_sandy_out$label_all[unique(c(n1,n1b, n2,n2b,n3, n3b))] <- "neighbour"
 
 colnames(data_sandy_out)[2:4] <- c("Level", "Conductivity", "Turbidity")
 
 data_sandy_out$flag_TC <- ifelse((data_sandy_out$Cond <= 0 |
                                     data_sandy_out$Tur <= 0),
                                  "outlier", "typical"
 )
 
 data_sandy_out$flag_TCL <- ifelse((data_sandy_out$Cond <= 0 |
                                      data_sandy_out$Tur <= 0 |
                                      data_sandy_out$Level < 0), "outlier", "typical")
 data_sandy_out <- drop_na(data_sandy_out)
 sandy_trans <- oddwater::transform_data(data_sandy_out[, 1:4])
 sandy_full <- left_join(sandy_trans, data_sandy_out[, -c(2:4)], by = "Timestamp")
 
 p1 <- plot_original(
   data = sandy_full, y = "Turbidity",
   colour = "out_label_Tur", y_label = "Turb"
 ) +
   theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
   scale_colour_manual(values = c("typical" = "black", "outlier" = "red", "neighbour" = "lightsalmon3"))
 p2 <- plot_original(
   data = sandy_full, y = "Conductivity",
   colour = "out_label_Cond", y_label = "Cond"
 ) +
   theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
   scale_colour_manual(values = c("typical" = "black", "outlier" = "red", "neighbour" = "lightsalmon3"))
 p3 <- plot_original(
   data = sandy_full, y = "Level",
   colour = "out_label_Level", y_label = "Level"
 ) +
   theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
   scale_colour_manual(values = c("typical" = "black", "outlier" = "red", "neighbour" = "lightsalmon3"))
 
data <- sandy_full[, c(
   "Timestamp", "neg_der_log_bound_Turbidity",
   "pos_der_log_bound_Conductivity", "neg_der_log_bound_Level"
 )] %>% drop_na()

outliers <- stray::find_HDoutliers(data[, 2:3], method = "knn_sum", knnsearchtype = "FNN_brute")
outliers$outliers

plotdata<- mutate(data, Score = (as.vector(outliers$out_scores)), Type = outliers$type)
p4 <- ggplot(plotdata, aes(x=Timestamp, y= Score, color = Type))+
  geom_point()+
  scale_colour_manual(values = c("typical" = "black", "outlier" = "red", "neighbour" = "lightsalmon3"))+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) 
p4


p <- ggpubr::ggarrange(p1, p2, p4, nrow = 3, common.legend = TRUE, legend = "none")
p



  
