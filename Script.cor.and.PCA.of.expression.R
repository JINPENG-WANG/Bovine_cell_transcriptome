setwd("e:/01.研究生培养/罗忠泽/02.转录组/牛成纤维细胞转录组报告/F23A040004742_BOSuzdtT_xreport/Core_table/")
library(corrplot)
library(ggplot2)
library(ggpubr)

#画相关性图
td <- read.table("expression.data.",header=T)
class(td)
cor(td,method="pearson")
tdc <- cor(td,method="spearman")
corrplot(tdc)

#画PCA图
exp <- read.table("expression.data.with.genename.txt",header=T,row.names=1)
mode(exp)

#将基因表达值矩阵作个转置，使行为样本，列为基因
exp_trans <-apply(exp,2,as.numeric) 

#进行PCA分析
exp_tras_pca <- prcomp(t(exp_trans[,1:20]))
#为分组添加名称
exp_pcs <- data.frame(exp_tras_pca$x,feature=rep(c("42_0H","42_H2","42_H4","42_H8"),c(5,5,5,5)))

#画图
pdf("expression.pdf")
ggplot(exp_pcs,aes(x=PC1, y=PC2, color=feature))+geom_point()
dev.off()

