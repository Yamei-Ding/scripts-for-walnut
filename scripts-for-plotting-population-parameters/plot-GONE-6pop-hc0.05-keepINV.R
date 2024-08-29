library(ggplot2)
library(MASS)
library(scales)
library(RColorBrewer)
setwd("/Users/yameiding/Desktop/unkonw/chencj/20240804-sentieon/GONE/hc0.05/keep-INV/")
path <- "./"
fileNames <- dir(path,pattern = "*.txt")
tmpname <- strsplit(fileNames,split = "\\.")
filePath <- sapply(fileNames, function(x){paste(path,x,sep = '/')})
tmp<-paste(tmpname[[1]][1])
data<-data.frame(label=tmp,read.table(filePath[1],header = TRUE))
for(i in 2:length(filePath)){
  tmp<-paste(tmpname[[i]][1])
  newdata <- data.frame(label=tmp,read.table(filePath[i],header = TRUE))
  data<-rbind(data,newdata)
}
data1 <- data.frame(Generation = c(data$Generation),
                    Geometric_mean = c(data$Ne_genomatric),
                    label = c(as.character(data$label))
                    # Ne_2.5 = c(data$Ne_2.5),
                    # Ne_97.5 = c(data$Ne_97.5)
)

q <- ggplot(data1) +
  geom_step(aes(x = Generation, y = Geometric_mean, group = label, colour = label), size = 1.5) + # 加粗线条
  scale_fill_manual(values = c('#80C7EF', '#ECB740', '#D99BBD', '#7B5892', '#40B696', '#E08640')) +
  scale_y_log10(limits = c(10, NA), breaks = trans_breaks("log10", function(y) 10^y, n = 3)) +
  annotation_logticks(base = 10, sides = "l") +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) +
  coord_cartesian(xlim = c(1, 200), ylim = c(10, 100000)) +
  scale_color_manual(values = c('#80C7EF', '#ECB740', '#D99BBD', '#7B5892', '#40B696', '#E08640'))
gone <- q + theme_bw() +
  labs(title = "GONE hc=0.05, keep inversion region") +
  xlab("Generation") +
  ylab("Effective population size Ne") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12),  # 增大标题字体
    axis.title = element_text(size = 12),  # 增大坐标轴标签字体
    #axis.text = element_text(size = 12),  # 增大坐标轴刻度字体
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.title = element_text(size = 12),  # 增大图例标题字体
    legend.text = element_text(size = 12)  # 增大图例字体
  ) +
  labs(fill = "", color = "")

pdf("recombiantion2.63-hc0.05-keepINV-200generation.pdf", width = 8, height = 6)
gone
dev.off()

