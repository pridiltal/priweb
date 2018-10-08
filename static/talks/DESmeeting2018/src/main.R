## ---- load
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(plotly)
library(lubridate)
library(GGally)
library(zoo)
library(oddwater)
library(HDoutliers)
library(DDoutlier)
library(kableExtra)
## Load user-defined functions
source("src/functions.R")
data("data_sandy_anom")
## Change the format and the class of the existing variable
data_sandy_anom$Timestamp <- lubridate::dmy_hm(data_sandy_anom$Timestamp)



## ---- VisualiseAnomaly

p1 <- ggplot(data_sandy_anom, aes(x = Timestamp, y = Tur)) +
  geom_line() +
  geom_point(data = data_sandy_anom, aes(colour = type_Tur, shape= type_Tur)) +
  scale_colour_manual(values = c("0" = "black", "A" = "red", 
                                 "E" = "green4", "D" = "lightsalmon3",
                                 "K" = "blue", "F" = "brown"))+
  scale_shape_manual(values = c("0" = 19, "A" = 17, 
                                 "E" = 15, "D" = 16,
                                 "K" =20 , "F" = 18))+
  ylab("a) Turbidity") +
  theme(legend.position = "none")



p2 <- ggplot(data_sandy_anom, aes(x = Timestamp, y = Cond)) +
  geom_line() +
  geom_point(data = data_sandy_anom, aes(colour = type_Cond, shape= type_Cond)) +
  ylab("b) Conductivity") +
  theme(legend.position = "none")+
  scale_colour_manual(values = c("0" = "black", "A" = "red", 
                                 "E" = "green", "D" = "lightsalmon3",
                                 "K" = "blue", "F" = "brown"))+
  scale_shape_manual(values = c("0" = 19, "A" = 17, 
                                 "E" = 15, "D" = 16,
                                 "K" =20 , "F" = 18))



p3 <- ggplot(data_sandy_anom, aes(x = Timestamp, y = Level)) +
  geom_line() +
  geom_point(data = data_sandy_anom, aes(colour = type_Level, shape= type_Level)) +
  ylab("c) Level") +
  theme(legend.position = "none")+
  scale_colour_manual(values = c("0" = "black", "A" = "red", 
                                 "E" = "green", "D" = "lightsalmon3",
                                 "K" = "blue", "F" = "brown"))+
  scale_shape_manual(values = c("0" = 19, "A" = 17, 
                                 "E" = 15, "D" = 16,
                                 "K" =20 , "F" = 18))


p <- ggpubr::ggarrange(p1, p2, p3, nrow = 3, common.legend = TRUE)
ggsave("./fig/Visualise_anomaly.png")



## ---- filter_outliers_from_anormalies
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
n2 <- which(data_sandy_out$Timestamp %in% out2)-1
n3 <- which(data_sandy_out$Timestamp %in% out3)-1


data_sandy_out$out_label_Tur <- as.character(data_sandy_out$out_label_Tur)
data_sandy_out$out_label_Tur[n1] <- "neighbour"
data_sandy_out$out_label_Tur <- as.factor(data_sandy_out$out_label_Tur)


data_sandy_out$out_label_Cond  <- as.character(data_sandy_out$out_label_Cond )
data_sandy_out$out_label_Cond[n2] <- "neighbour"
data_sandy_out$out_label_Cond  <- as.factor(data_sandy_out$out_label_Cond)


data_sandy_out$out_label_Level <- as.character(data_sandy_out$out_label_Level)
data_sandy_out$out_label_Level[n3] <- "neighbour"
data_sandy_out$out_label_Level <- as.factor(data_sandy_out$out_label_Level)



## ---- VisualiseOutlier

p1 <- plot_original(
  data = data_sandy_out, y = "Tur", colour = "out_label_Tur",
  y_label = "(a) Turbidity"
)
p2 <- plot_original(
  data = data_sandy_out, y = "Cond", colour = "out_label_Cond",
  y_label = "(b) Conductivity"
)
p3 <- plot_original(
  data = data_sandy_out, y = "Level", colour = "out_label_Level",
  y_label = "(c) Level"
)
p <- ggpubr::ggarrange(p1, p2, p3, nrow = 3)
ggsave("./fig/Visualise_outlier.png")

## ---- Visualise_outlier_pairs_original_data
data_sandy_out$label_all <- ifelse((data_sandy_out$out_label_Tur == "typical" &
                                      data_sandy_out$out_label_Cond == "typical" &
                                      data_sandy_out$out_label_Level == "typical"),
                                   "typical", "outlier"
)

data_sandy_out$label_all[unique(n1,n2,n3)] <- "neighbour"

colnames(data_sandy_out)[2:4] <- c("Level", "Conductivity", "Turbidity")
p <- GGally::ggpairs(data_sandy_out,
                     columns = c(4, 3, 2),
                     ggplot2::aes(colour = label_all, shape = label_all, alpha = label_all)
)

for (i in 1:p$nrow) {
  for (j in 1:p$ncol) {
    p[i, j] <- p[i, j] +
      scale_fill_manual(values = c(  "lightsalmon3","red", "black")) +
      scale_color_manual(values = c(  "lightsalmon3", "red","black")) +
      scale_shape_manual(values = c(15, 17, 19)) +
      scale_alpha_manual(values = c(1,1, 0.5)) +
      scale_size_manual(values = c(4, 4, 1))
  }
}

ggsave("./fig/Visualise_outlier_pairs_original_data.png", plot = p)


## ---- transform_sandy
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

## ---- transformType
p1 <- plot_trans(x = "Turbidity", y = "Conductivity") + ggtitle("(a)")
p2 <- plot_trans(x = "log_Turbidity", y = "log_Conductivity") + ggtitle("(b)")
p3 <- plot_trans(x = "diff_log_Turbidity", y = "diff_log_Conductivity") + ggtitle("(c)")
p4 <- plot_trans(x = "der_log_bound_Turbidity", y = "der_log_bound_Conductivity") + ggtitle("(d)")
p5 <- plot_trans(x = "neg_der_log_bound_Turbidity", y = "pos_der_log_bound_Conductivity") + ggtitle("(f)")
p6 <- plot_trans(x = "rc_Turbidity", y = "rc_Conductivity") + ggtitle("(e)")

# p<-gridExtra::grid.arrange(p1, p2, p3, p4, p5,p6, nrow= 2)
p <- ggpubr::ggarrange(p1, p2, p3, p4, p6, p5)
ggsave("./fig/transform_type.png")

## ---- trans_demo_TCL
p1 <- plot_original(
  data = sandy_full, y = "neg_der_log_bound_Turbidity",
  colour = "out_label_Tur", y_label = "(a) Turbidity"
)
p2 <- plot_original(
  data = sandy_full, y = "pos_der_log_bound_Conductivity",
  colour = "out_label_Cond", y_label = "(b) Conductivity"
)
p3 <- plot_original(
  data = sandy_full, y = "neg_der_log_bound_Level",
  colour = "out_label_Level", y_label = "(c) Level"
)

p <- ggpubr::ggarrange(p1, p2, p3, nrow = 3)
ggsave("./fig/trans_demo_TCL.png")


## ---- timegapSandy

p <- ggplot(sandy_full, aes(Timestamp, time)) +
  geom_point() +
  geom_line() +
  xlab("Time gap (in minutes)")

ggsave("./fig/timegapSandy.png", height = 3, width = 12)


## ---- Visualise_outlier_pairs_trans_data
data <- sandy_full[, c(
  "Timestamp", "neg_der_log_bound_Turbidity",
  "pos_der_log_bound_Conductivity",
  "neg_der_log_bound_Level", "label_all"
)]

colnames(data)[2:4] <- c("Trans_Tur", "Trans_Cond", "Trans_level")
p <- GGally::ggpairs(data,
                     columns = 2:4,
                     ggplot2::aes(colour = label_all, shape = label_all, alpha = label_all, label = Timestamp)
)
for (i in 1:p$nrow) {
  for (j in 1:p$ncol) {
    p[i, j] <- p[i, j] +
      scale_fill_manual(values = c(  "lightsalmon3","red", "black")) +
      scale_color_manual(values = c(  "lightsalmon3", "red","black")) +
      scale_shape_manual(values = c(15, 17, 19)) +
      scale_alpha_manual(values = c(1,1, 0.5)) +
      scale_size_manual(values = c(4, 4, 1))
  }
}

ggsave("./fig/Visualise_outlier_pairs_trans_data.png", plot = p)


## ---- plot_score_function

sandy_full$label_all <- ifelse((data_sandy_out$out_label_Tur == "typical" &
                                  data_sandy_out$out_label_Cond == "typical" &
                                  data_sandy_out$out_label_Level == "typical"),
                               "typical", "outlier"
)

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
p11 <- plot_original(
  data = sandy_full, y = "Level",
  colour = "out_label_Level", y_label = "Level"
) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_colour_manual(values = c("typical" = "black", "outlier" = "red", "neighbour" = "lightsalmon3"))

test_data <- sandy_full[, c(
  "Timestamp", "neg_der_log_bound_Turbidity",
  "pos_der_log_bound_Conductivity", "neg_der_log_bound_Level"
)] %>% drop_na()

p3 <- plot_score(sandy_full, test_data,
                       method = "HDoutliers",
                       flag = "flag_TC", title = "HDo", variable = "out_score"
)
p4 <- plot_score(sandy_full, test_data,
                       method = "KNN-AGG", flag = "flag_TCL",
                       title = "KNN-AGG", variable = "out_score"
)
p5 <- plot_score(sandy_full, test_data,
                       method = "KNN-SUM", flag = "flag_TCL",
                       title = "KNN-SUM", variable = "out_score"
)
p6 <- plot_score(sandy_full, test_data,
                       method = "LOF", flag = "flag_TCL",
                       variable = "out_score"
)
p7 <- plot_score(sandy_full, test_data,
                       method = "COF", flag = "flag_TCL",
                       variable = "out_score"
)
p8 <- plot_score(sandy_full, test_data,
                       method = "INFLO", flag = "flag_TCL",
                       variable = "out_score"
)
p9 <- plot_score(sandy_full, test_data,
                       method = "LDOF", flag = "flag_TCL",
                       variable = "out_score"
) +
  # p10<- performance_plot(sandy_full, test_data, method = "RKOF" , flag = "flag_TCL",
  #                      variable = "out_score") +
  theme(legend.position = "none", legend.title = element_blank())

pA <- ggpubr::ggarrange(p1, p2, p11, p3, p4, p5, p6, p7, p8, p9, nrow = 10)
ggsave("./fig/outlier_Score.png", height = 13, width = 12)


pB <- ggpubr::ggarrange(p1, p2, p11,  p4,  nrow = 4, heights = c(5,5,5,15))
ggsave("./fig/outlier_Score_demo.png", height = 13, width = 12)

## ---- one_sided_derivative_TCL_sandy

sandy_full$label_all <- ifelse((data_sandy_out$out_label_Tur == "typical" &
                                  data_sandy_out$out_label_Cond == "typical" &
                                  data_sandy_out$out_label_Level == "typical"),
                               "typical", "outlier"
)

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
p11 <- plot_original(
  data = sandy_full, y = "Level",
  colour = "out_label_Level", y_label = "Level"
) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  scale_colour_manual(values = c("typical" = "black", "outlier" = "red", "neighbour" = "lightsalmon3"))

test_data <- sandy_full[, c(
  "Timestamp", "neg_der_log_bound_Turbidity",
  "pos_der_log_bound_Conductivity", "neg_der_log_bound_Level"
)] %>% drop_na()

p3 <- performance_plot(sandy_full, test_data,
                       method = "HDoutliers",
                       flag = "flag_TC", title = "HDo", variable = "out_score"
)
p4 <- performance_plot(sandy_full, test_data,
                       method = "KNN-AGG", flag = "flag_TCL",
                       title = "KNN-AGG", variable = "out_score"
)
p5 <- performance_plot(sandy_full, test_data,
                       method = "KNN-SUM", flag = "flag_TCL",
                       title = "KNN-SUM", variable = "out_score"
)
p6 <- performance_plot(sandy_full, test_data,
                       method = "LOF", flag = "flag_TCL",
                       variable = "out_score"
)
p7 <- performance_plot(sandy_full, test_data,
                       method = "COF", flag = "flag_TCL",
                       variable = "out_score"
)
p8 <- performance_plot(sandy_full, test_data,
                       method = "INFLO", flag = "flag_TCL",
                       variable = "out_score"
)
p9 <- performance_plot(sandy_full, test_data,
                       method = "LDOF", flag = "flag_TCL",
                       variable = "out_score"
) +
  # p10<- performance_plot(sandy_full, test_data, method = "RKOF" , flag = "flag_TCL",
  #                      variable = "out_score") +
  theme(legend.position = "bottom", legend.title = element_blank())

pA <- ggpubr::ggarrange(p1, p2, p11, p3, p4, p5, p6, p7, p8, p9, nrow = 10)
ggsave("./fig/one_sided_derivative_TCL_sandy.png", height = 13, width = 12)
