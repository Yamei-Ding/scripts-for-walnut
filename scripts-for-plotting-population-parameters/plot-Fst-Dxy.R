library(ggplot2)
library(reshape2)
library(cowplot)

# 设置工作目录
setwd("/Users/yameiding/Desktop/unkonw/chencj/20240804-sentieon/Fst_Dxy")

# 读取数据文件
fst_lower <- read.table("Fst.matrix", header = FALSE)
fst_upper <- read.table("Dxy.matrix", header = FALSE)

# 提取种群名称
pop_names <- fst_lower[, 1]  # 使用 fst_lower 的种群名称，因为两个矩阵应相同

# 创建下三角矩阵
lower_matrix <- as.matrix(fst_lower[, -1])
rownames(lower_matrix) <- pop_names
colnames(lower_matrix) <- pop_names

# 创建上三角矩阵
upper_matrix <- as.matrix(fst_upper[, -1])
rownames(upper_matrix) <- pop_names
colnames(upper_matrix) <- pop_names

# 创建完整的矩阵
full_matrix <- matrix(NA, nrow = length(pop_names), ncol = length(pop_names),
                      dimnames = list(pop_names, pop_names))

# 填充下三角矩阵
full_matrix[lower.tri(full_matrix, diag = FALSE)] <- lower_matrix[lower.tri(lower_matrix, diag = FALSE)]

# 填充上三角矩阵
full_matrix[upper.tri(full_matrix, diag = FALSE)] <- upper_matrix[upper.tri(upper_matrix, diag = FALSE)]

# 将矩阵转换为长格式
melted_matrix <- melt(full_matrix, na.rm = TRUE)
melted_matrix$value <- as.numeric(melted_matrix$value)

# 将Var1和Var2转换为字符类型
melted_matrix$Var1 <- as.character(melted_matrix$Var1)
melted_matrix$Var2 <- as.character(melted_matrix$Var2)

# 创建对角线标签数据
diagonal_labels <- data.frame(Var1 = pop_names, Var2 = pop_names, value = NA)

# 创建上三角区域数据
upper_tri_data <- melted_matrix[melted_matrix$Var1 != melted_matrix$Var2 & melted_matrix$Var2 > melted_matrix$Var1, ]

# 创建下三角区域数据
lower_tri_data <- melted_matrix[melted_matrix$Var1 != melted_matrix$Var2 & melted_matrix$Var2 < melted_matrix$Var1, ]
# 绘制上三角矩阵热图（Dxy）
p_upper <- ggplot() +
  geom_tile(data = upper_tri_data, aes(x = Var1, y = Var2, fill = value), color = "white") +
  geom_text(data = upper_tri_data, aes(x = Var1, y = Var2, label = sprintf("%.3f", value)), color = "black", size = 3) +
  geom_text(data = diagonal_labels, aes(x = Var1, y = Var2, label = Var1), color = "black", size = 3.5) +
  scale_fill_gradient(low = "#fc9272", high = "#4DBBD5B2", name = "Dxy") +  # 颜色渐变
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "", y = "") +
  coord_fixed()

# 绘制下三角矩阵热图（Fst）
p_lower <- ggplot() +
  geom_tile(data = lower_tri_data, aes(x = Var1, y = Var2, fill = value), color = "white") +
  geom_text(data = lower_tri_data, aes(x = Var1, y = Var2, label = sprintf("%.3f", value)), color = "black", size = 3) +
  geom_text(data = diagonal_labels, aes(x = Var1, y = Var2, label = Var1), color = "black", size = 3.5) +
  scale_fill_gradient(low = "#fc9272", high = "#4DBBD5B2", name = "Fst") +  # 颜色渐变
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "", y = "") +
  coord_fixed()

# 提取图例并设置为渐变色
legend_upper <- get_legend(
  ggplot(data = upper_tri_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "#fc9272", high = "#4DBBD5B2", name = "Dxy") +
    theme(legend.position = "bottom", legend.title = element_text(size = 10), 
          legend.text = element_text(size = 8), legend.key.width = unit(0.5, "cm"))
)

legend_lower <- get_legend(
  ggplot(data = lower_tri_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "#fc9272", high = "#4DBBD5B2", name = "Fst") +
    theme(legend.position = "right", legend.title = element_text(size = 10), 
          legend.text = element_text(size = 8), legend.key.width = unit(0.5, "cm"))
)

# 创建带图例的热图
combined_plot <- ggdraw() +
  draw_plot(p_upper, 0, 0, 1, 1) +
  draw_plot(p_lower, 0, 0, 1, 1) +
  draw_plot(legend_upper, 0.5, 0, 0.5, 0.1, hjust = 0.5) +
  draw_plot(legend_lower, 0, 0.5, 0.1, 0.5, vjust = 0.5)

# 保存和显示图形
print(combined_plot)
pdf("Fst_Dxy.pdf", width = 6, height = 6)
combined_plot
dev.off()
