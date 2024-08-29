library(ggplot2)
library(MASS)
library(scales)
library(RColorBrewer)
setwd("/Users/yameiding/Desktop/unkonw/chencj/20240804-sentieon/GONE/hc0.05/remove-INV//")
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

gone<-q+theme_bw()+labs(title="GONE hc=0.05, remove inversion region")+xlab("Generation")+ylab("effective population size Ne")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  labs(fill="",color="")
pdf("recombiantion2.63-hc0.01-300generation.pdf",width = 8, height = 6)
#pdf("recombiantion2.63-hc0.01-300generation.pdf",width = 8, height = 6)
gone
dev.off()


