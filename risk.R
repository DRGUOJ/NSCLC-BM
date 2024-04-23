setwd("E:/文章/4 NSCLC BM/数据整理/fig 3 高低风险图")

# 加载R包 --------------------------------------------------------------------
library(ggrisk)
library(survival)
library(ggsci)
library(ggplot2)

# 载入示例数据 ------------------------------------------------------------------
mydata <- read.csv("test.csv",row.names = 1,header = T,sep = ",")

# 查看重要变量
summary(mydata$time)# 确认时间的单位，天/月/年
table(mydata$status)# 查看结局事件的数量

# 将时间转换成 年，易于理解
mydata$time <- mydata$time/12

# 模型拟合 --------------------------------------------------------------------
multi_cox_genes <- colnames(mydata)[3:9]# 这里选择除了生存结局和生存时间之外所有的变量，根据你自己的数据自行修改
# 拟合回归公式
formula <- as.formula(paste("Surv(time, status) ~", paste(multi_cox_genes, collapse = " + ")))
formula
multi_cox_model <- coxph(formula, data = mydata)
# 查看模型概况
summary(multi_cox_model)

# 可视化 ---------------------------------------------------------------------
# Cox回归的风险评分图
ggrisk(multi_cox_model)
# Cox回归的双散点图
two_scatter(multi_cox_model)

#颜色
mycolor<-pal_npg("nrc", alpha = 0.5)(8) #提取8种颜色，透明度80%
mycolor

# ggrisk增加可调节的参数，固定cut-off值
ggrisk(multi_cox_model, # 绘制生存分析图，multi_cox_model是Cox比例风险模型
       heatmap.genes = multi_cox_genes, # 绘制热图的基因
       cutoff.value=1.39, # 将风险分数的截断值直接设置为1.39
       code.0 = 'BM(-)', # 代码0代表仍然存活
       code.1 = 'BM(+)', # 代码1代表已经死亡
       code.highrisk = 'High Risk', # 高风险组用"High Risk"表示
       code.lowrisk = 'Low Risk', # 低风险组用"Low Risk"表示
       title.A.ylab='Risk Score', # y轴标签表示风险分数
       title.B.ylab='Survival Time (year)', # y轴标签表示生存时间（年）
       title.A.legend='Risk Group', # 风险组的图例标题
       title.B.legend='Status', # 生存状态的图例标题
       title.C.legend='Score', # 基因表达的图例标题
       size.ABC=1.5, # 整个图的大小
       size.ylab.title=14, # y轴标签和标题的字体大小
       size.Atext=11, # 风险组标签的字体大小
       size.Btext=11, # 生存状态标签的字体大小
       size.Ctext=11, # 基因表达标签的字体大小
       size.yticks=0.5, # y轴刻度线的大小
       size.yline=0.5, # y轴的线条粗细
       size.points=2, # 散点的大小
       size.dashline=1, # 虚线的粗细
       size.cutoff=5, # 截断线的粗细
       size.legendtitle=13, # 图例标题的字体大小
       size.legendtext=12, # 图例文字的字体大小
       color.A=c(low='#4DBBD57F',high='#DC00007F'), # 风险组的颜色范围
       color.B=c(code.0='#4DBBD57F',code.1='#DC00007F'), # 生存状态的颜色
       color.C=c(low='#4DBBD57F',median='white',high='#DC00007F'), # 基因表达的颜色
       vjust.A.ylab=1, # y轴标签的位置
       vjust.B.ylab=2, # y轴标签的位置
       family='serif', # 字体类型，"sans"表示Arial，"serif"表示Times New Roman
       expand.x = 3, # x轴的扩展系数
       relative_heights = c(0.1,0.1,0.01,0.15)) # 图形的相对高度


# ggrisk增加可调节的参数，不固定cut-off值
ggrisk(multi_cox_model, # 绘制生存分析图，multi_cox_model是Cox比例风险模型
       heatmap.genes = multi_cox_genes, # 绘制热图的基因
       cutoff.value='cutoff', # 风险分数的中位数作为截断值，#可选median、roc、cutoff'
       cutoff.x = 200, # 按照x轴145的位置截断
       cutoff.y = -0.8, # 按照y轴-0.8的位置截断
       code.0 = 'BM(-)', # 代码0代表仍然存活
       code.1 = 'BM(+)', # 代码1代表已经死亡
       code.highrisk = 'High Risk', # 高风险组用"High Risk"表示
       code.lowrisk = 'Low Risk', # 低风险组用"Low Risk"表示
       title.A.ylab='Risk Score', # y轴标签表示风险分数
       title.B.ylab='Survival Time (year)', # y轴标签表示生存时间（年）
       title.A.legend='Risk Group', # 风险组的图例标题
       title.B.legend='Status', # 生存状态的图例标题
       title.C.legend='Score', # 基因表达的图例标题
       size.ABC=1.5, # 整个图的大小
       size.ylab.title=14, # y轴标签和标题的字体大小
       size.Atext=11, # 风险组标签的字体大小
       size.Btext=11, # 生存状态标签的字体大小
       size.Ctext=11, # 基因表达标签的字体大小
       size.yticks=0.5, # y轴刻度线的大小
       size.yline=0.5, # y轴的线条粗细
       size.points=2, # 散点的大小
       size.dashline=1, # 虚线的粗细
       size.cutoff=5, # 截断线的粗细
       size.legendtitle=13, # 图例标题的字体大小
       size.legendtext=12, # 图例文字的字体大小
       color.A=c(low='#4DBBD57F',high='#DC00007F'), # 风险组的颜色范围
       color.B=c(code.0='#4DBBD57F',code.1='#DC00007F'), # 生存状态的颜色
       color.C=c(low='#4DBBD57F',median='white',high='#DC00007F'), # 基因表达的颜色
       vjust.A.ylab=1, # y轴标签的位置
       vjust.B.ylab=2, # y轴标签的位置
       family='serif', # 字体类型，"sans"表示Arial，"serif"表示Times New Roman
       expand.x = 3, # x轴的扩展系数
       relative_heights = c(0.1,0.1,0.01,0.15)) # 图形的相对高度


# two_scatter增加可调节的参数
two_scatter(multi_cox_model, # 绘制双散点图
            cutoff.value = 'median', # 风险分数的中位数作为截断值，#可选median、roc、cutoff'
            cutoff.x = 142, # 按照x轴142的位置截断
            cutoff.y = -0.5, # 按照y轴-0.5的位置截断
            code.0 = 'Still Alive', # 代码0代表仍然存活
            code.1 = 'Dead', # 代码1代表死亡
            code.highrisk = 'High Group', # 高风险组用"High Group"表示
            code.lowrisk = 'Low Group', # 低风险组用"Low Group"表示
            title.A.legend = 'Riskscore', # 风险分数的图例标题
            title.B.legend = 'Event Status', # 事件状态的图例标题
            title.A.ylab = 'Riskscore', # y轴标签表示风险分数
            title.B.ylab = 'Survival Time (year)', # y轴标签表示生存时间（年）
            title.xlab = 'Rank', # x轴标签
            vjust.A.ylab = 1, # y轴标签的位置
            vjust.B.ylab = 2, # y轴标签的位置
            size.AB = 2, # 散点图中散点的大小
            size.ylab.title = 16, # y轴标签和标题的字体大小
            size.xlab.title = 16, # x轴标签和标题的字体大小
            size.Atext = 12, # 散点图中数字的字体大小
            size.Btext = 12, # 散点图中数字的字体大小
            size.xtext = 12, # x轴刻度标签的字体大小
            size.xyticks = 0.5, # x和y轴刻度线的大小
            size.xyline = 0.5, # 散点图中x和y轴的线条粗细
            size.dashline = 1.5, # 散点图中虚线的粗细
            size.points = 2, # 散点图中散点的大小
            size.cutoff = 5, # 截断线的粗细
            size.legendtitle = 14, # 图例标题的字体大小
            size.legendtext = 13, # 图例文字的字体大小
            color.A = c(low='#4B70E4',high='#E12543'), # 风险分数的颜色范围
            color.B = c(code.0='#4B70E4',code.1='#E12543'), # 事件状态的颜色
            family = 'sans', # 字体类型，"sans"表示Arial，"serif"表示Times New Roman
            expand.x = 10) # x轴的扩展系数

