# 加载必要的库
library(ggplot2)
library(dplyr)

# 设置工作目录
setwd("/Users/yameiding/Desktop/unkonw/chencj/20240804-sentieon/het")

# 读取输入数据
data <- read.table("het.txt", header = TRUE)

# 检查数据
head(data)

# 指定分组的颜色
group_colors <- c("North-China" = '#D99BBD', "Europe" = '#80C7EF', "IRAN" = '#ECB740', "South-Asia" = '#7B5892', "Tibet" = '#40B696', "Xinjiang" = '#E08640')

# 参考分组的名称
reference_group <- "South-Asia"

# 将参考分组放置在最后
data$popname <- factor(data$popname, levels = c(setdiff(unique(data$popname), reference_group), reference_group))

# 绘制箱线图
p <- ggplot(data, aes(x = popname, y = Het_rate, color = popname)) +
  geom_boxplot(fill = NA, size = 1.2) +  # 箱子透明，轮廓有颜色
  geom_jitter(width = 0.2, alpha = 0.6) +  # 显示取值点
  scale_color_manual(values = group_colors) +  # 指定分组颜色
  theme_minimal() +  # 使用简洁的主题
  labs(x = "", y = "Heterozygosity Rate") +
  theme(legend.position = "none",  # 隐藏图例
        panel.grid = element_blank(),  # 去除网格线
        panel.border = element_rect(color = "black", fill = NA, size = 1),        
        axis.text.x = element_text(size = 12,color = "black"),  # 调整横坐标字体大小
        axis.text.y = element_text(size = 12, color = "black"))  # 调整纵坐标字体大小

# 进行 Wilcoxon 检验，并添加显著性标记
#数据框列名为 'popname' 和 'Het_rate'
for (group in levels(data$popname)) {
  if (group != reference_group) {
    # 获取参考组和当前组的非缺失数据
    ref_values <- data$Het_rate[data$popname == reference_group]
    group_values <- data$Het_rate[data$popname == group]
    
    # 确保两个组都有数据
    if (length(ref_values[!is.na(ref_values)]) > 0 && length(group_values[!is.na(group_values)]) > 0) {
      test <- wilcox.test(ref_values, group_values)
      p_value <- test$p.value
      significance <- ""
      
      # 根据 p 值确定显著性标记
      if (p_value < 0.001) {
        significance <- "***"
      } else if (p_value < 0.01) {
        significance <- "**"
      } else if (p_value < 0.05) {
        significance <- "*"
      }
      
      # 添加显著性标记到图中
      p <- p + annotate("text", x = which(levels(data$popname) == group), 
                        y = max(data$Het_rate, na.rm = TRUE) + 0.001, 
                        label = significance, 
                        color = "black")
    }
  }
}

# 保存图形为 PDF
pdf("plot-Heterozygosity-Rate.pdf",width = 6, height = 6)
p
dev.off()
