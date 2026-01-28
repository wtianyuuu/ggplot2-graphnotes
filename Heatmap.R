library(ggplot2) #先要启动
set.seed(1)

n_gene <- 30
n_samp <- 12

genes <- paste0("G", 1:n_gene)
samples <- paste0("S", 1:n_samp)

# gene x sample 的表达矩阵
expr <- matrix(rnorm(n_gene * n_samp), nrow = n_gene,
               dimnames = list(genes, samples))

# 模拟：让后 6 个样本整体偏高一点（像 treatment）
expr[, 7:12] <- expr[, 7:12] + 1

df <- as.data.frame(as.table(expr))
colnames(df) <- c("gene", "sample", "value")
head(df)
p <- ggplot(df, aes(x = sample, y = gene, fill = value)) +
  geom_tile() +
  labs(title = "Heatmap (raw values)", x = "Sample", y = "Gene") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

print(p)
可以看见热图的版本了
#z score标准化
expr_z <- t(scale(t(expr)))   # 对每一行（gene）标准化
dfz <- as.data.frame(as.table(expr_z))
colnames(dfz) <- c("gene", "sample", "value")
#画图
p_z <- ggplot(dfz, aes(x = sample, y = gene, fill = value)) +
  geom_tile() +
  labs(title = "Heatmap (row z-score)", x = "Sample", y = "Gene") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

print(p_z)
