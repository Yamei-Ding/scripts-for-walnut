# 安装和加载必要的包
#install.packages("ggplot2")
#install.packages("ggsignif")
library(ggplot2)
library(ggsignif)
#library(showtext)
setwd("/Users/yameiding/Desktop/unkonw/chencj/20240804-sentieon/private_snps/")
# 读取数据
data <- read.table("bak.txt", header=TRUE)
head(data)
populations <- unique(data$popname)
# 指定分组的颜色
group_colors <- c("North-China" = '#D99BBD', "Europe" = '#80C7EF', "IRAN" = '#ECB740', "South-Asia" = '#7B5892', "Tibet" = '#40B696', "Xinjiang" = '#E08640')
# 参考分组的名称
reference_group <- "South-Asia"
# 将参考分组放置在最后
data$popname <- factor(data$popname, levels = c(setdiff(unique(data$popname), reference_group), reference_group))
populations <- c(setdiff(populations, reference_group), reference_group)
# 将种群列转换为因子，并指定顺序
data$popname <- factor(data$popname, levels=populations)
# 绘制小提琴图，并在其内部添加箱线图
p <- ggplot(data, aes(x=popname, y=private_snps, fill=popname)) +
  # 绘制小提琴图
  geom_violin(alpha=0.5, color="black") +
  # 绘制箱线图
  geom_boxplot(width=0.2, color="black",outlier.shape=NA, outlier.colour=NA) +
  #geom_boxplot(width=0.2, color="black", outlier.colour="red", outlier.shape=16) +
  # 自定义填充颜色
  scale_fill_manual(values=group_colors) +
  # 设置主题和标签
  theme_minimal() +
  labs(x="", y="Private SNPs Rate") +
  theme(panel.grid = element_blank(),  # 去掉网格线
        #text = element_text(family = "Arial"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_text(size = 12,color = "black"),  # 调整横坐标字体大小
        axis.text.y = element_text(size = 12, color = "black"),  # 调整纵坐标字体大小
        legend.position = "none")  # 隐藏图例标签


# 对其他种群与参考种群进行显著性检验，并添加星号
other_groups <- populations[populations != reference_group]
y_positions <- seq(from=max(data$private_snps) + 0.01, by=0.01, length.out=length(other_groups))

for (i in 1:length(other_groups)) {
  group <- other_groups[i]
  group_data <- subset(data, popname %in% c(group, reference_group))
  
  # 使用 wilcox.test 进行显著性检验
  test_result <- wilcox.test(private_snps ~ popname, data=group_data)
  
  # 根据 p 值设置注释
  annotation <- ifelse(test_result$p.value < 0.001, "***",
                       ifelse(test_result$p.value < 0.01, "**",
                              ifelse(test_result$p.value < 0.05, "*", "ns")))
  
  # 在图中添加显著性标记
  p <- p + geom_signif(comparisons = list(c(group, reference_group)), 
                       annotations = annotation, 
                       y_position = y_positions[i], 
                       tip_length = 0.03, 
                       textsize = 4)
}


# 显示图形
pdf("private-snps-violin+boxplot.pdf",width=6,height = 6)
p
dev.off()


