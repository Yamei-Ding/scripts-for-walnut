# 安装和加载必要的包
#install.packages("ggplot2")
#install.packages("ggsignif")
library(ggplot2)
library(ggsignif)
setwd("/Users/yameiding/Desktop/unkonw/chencj/20240804-sentieon/PI")
# 读取数据
data <- read.table("pi.20k.window", header=TRUE)
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
p <- ggplot(data, aes(x = popname, y = PI, fill = popname)) +
  # 绘制小提琴图
  geom_violin(alpha = 0.5, color = "black") +
  # 绘制箱线图
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA, outlier.colour = NA) +
  # 自定义填充颜色
  scale_fill_manual(values = group_colors) +
  # 设置主题和标签
  theme_minimal() +
  labs(x = "Population", y = "PI") +
  theme(
    panel.grid = element_blank(),  # 去掉网格线
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.title = element_text(size = 12, color = "black"),  # 修改坐标轴标题的字体大小
    axis.text = element_text(size = 12, color = "black"),  # 修改坐标轴文本的字体大小
    legend.position = "none"  # 去掉图例
  )

# 对其他种群与参考种群进行显著性检验，并添加星号
other_groups <- populations[populations != reference_group]
y_positions <- seq(from=max(data$PI) + 0.0001, by=0.001, length.out=length(other_groups))

for (i in 1:length(other_groups)) {
  group <- other_groups[i]
  group_data <- subset(data, popname %in% c(group, reference_group))
  
  # 使用 wilcox.test 进行显著性检验
  test_result <- wilcox.test(PI ~ popname, data=group_data)
  
  p <- p + geom_signif(comparisons=list(c(group, reference_group)), 
                       annotations=ifelse(test_result$p.value < 0.001, "***",
                                          ifelse(test_result$p.value < 0.01, "**",
                                                 ifelse(test_result$p.value < 0.05, "*", "ns"))),
                       y_position=y_positions[i], 
                       tip_length=0.03, 
                       textsize=4)
}

# 显示图形
pdf("pi-from-total-vcf-violin+boxplot.pdf",width = 6,height = 6)
#print(p)
p
dev.off()
