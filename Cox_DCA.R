# 准备一个彩色板
#mycolors <- c('#D4E2A7','#88D7A4','#A136A1','#BAE8BC','#C757AF',
              '#DF9FCE','#D5E1F1','#305691','#B6C2E7','#E8EFF7',
              '#9FDFDF','#EEE0F5','#267336','#98CEDD','#CDE2EE',
              '#DAD490','#372E8A','#4C862D','#81D5B0','#BAE8C9',
              '#A7DCE2','#AFDE9C')

mycolors <- c('#E64B357F','#4DBBD57F','#00A0877F','#3C54887F','#F39B7F7F',
              '#8491B47F','#91D1C27F','#DC00007F')


# 加载所需要的包 -----------------------------------------------------------------
library(rms)#拟合模型
library(survival)#生存分析
library(VIM)#缺失值可视化
library(ggDCA)#绘制DCA曲线
library(ggsci)
library(ggplot2)
# 载入示例数据 ------------------------------------------------------------------
#train
mydata <- read.csv(file = "E:/文章/4 NSCLC BM/数据整理/fig 4 校准和DCA/test.csv",header = TRUE)
#时间变量的处理
summary(mydata$TIME)#检查时间的单位
#mydata$time <- mydata$time/365#将时间数值设置为年
units(mydata$TIME) <- "month"#将时间单位设置为月
#生存变量的处理
table(mydata$Ending)#检查生存状态

# 开始构建Cox 回归模型 ------------------------------------------------------------
#数据打包 
dd <- datadist(mydata)
options(datadist = "dd")

surv_object <- with(mydata, Surv(TIME, Ending==1))#构建回归分析因变量
colnames(mydata)#查看行名，把你要的变量放到下面拟合的公式中
model <-  cph(surv_object ~ DL, x = TRUE ,y = TRUE, surv = TRUE, data = mydata)
model#查看模型具体情况
HR <- exp(model$coefficients)#根据回归系数求HR值
HR
#cph()函数是 rms 包中的一个函数，用于拟合 Cox 模型并计算基于似然比检验的 p 值和其他与风险相关的度量。
#coef是公式中的回归系数b(beta值)，exp(coef)是风险比(HR-hazard ratio)
#p值是基于似然比检验的p值，即检验回归系数是否显著不为0


# 函数科普 --------------------------------------------------------------------
# R语言中的cph函数和coxph函数都用于拟合 Cox 比例风险回归模型,它们的区别和联系如下:
# ① cph来自rms包,coxph来自survival包。
# ②coxph更加常用,是拟合Cox模型的标准函数。cph使用较少。
# ③cph基于rms包的模型框架,继承了更多 rms 模型对象的功能。coxph生成的模型对象方法更少。
# ④cph可以更方便地进行生存预测等后续分析。coxph需要组合其他函数使用。
# ⑤两者模型拟合部分使用的优化算法是一致的,得到的结果是一样的。
# ⑥cph支持更多高级功能,如设定交互项、处理时间变化 covariates 等。coxph更基础。
# ⑦coxph是学习和使用Cox模型的标准入口,上手更简单。cph功能更全面。
# cph是基于rms的Cox模型实现,功能更强大完整。coxph更标准简洁,学习入门更易。


# DCA曲线的纵坐标Net Benefit,是在指定阈值下,根据模型的预测结果,计算出的总体医疗效益指标。
# 不是人群基线发病率。
# 计算公式为: Net Benefit = (真阳性数/实际阳性数) x 阳性准确预测的收益 - (假阳性数/实际阴性数) x 阴性错误预测的损失
# 即准确预测阳性病例带来的收益,减去错误预测阴性病例的损失。
# Net Benefit综合考虑了真阳性率、假阳性率及预测后果,反映总体效益。
# 通过比较不同阈值下的Net Benefit,可以选取使净收益最大化的最佳预测阈值。

# 绘制模型的临床决策曲线 -------------------------------------------------------------
# 拟合多个模型
fit_1 <- coxph(Surv(TIME, Ending) ~ DL, data = mydata)
fit_2 <- coxph(Surv(TIME, Ending) ~ Postoperative.treatment+Clinical.T.category+Clinical.N.category+CEA.ng.mL.+Pleural.attachment+Spiculation, data = mydata)
fit_3 <- coxph(Surv(TIME, Ending) ~ Postoperative.treatment+DL+Clinical.T.category+Clinical.N.category+CEA.ng.mL.+Pleural.attachment+Spiculation, data = mydata)


#颜色
mycolor<-pal_npg("nrc", alpha = 0.5)(8) #提取8种颜色，透明度80%
mycolor

# ①绘制单个模型的DCA曲线，不设置时间，默认取中位生存时间
dca_1 <- dca(fit_1, # 模型1
             model.names = "DL")
ggplot(dca_1)# 基础版本绘图

# check一下
head(dca_1)
# thresholds:预测阳性的阈值概率,从小到大排列。
# TPR:真阳性率,在该阈值下模型预测阳性中的真阳性例数比例。
# FPR:假阳性率,在该阈值下模型预测阳性中的假阳性例数比例。
# NB:净增益,使用该阈值进行预测的净收益。
# time:生存时间的单位,这里是天。
# model:模型名称。
# 随着阈值概率的增大,TPR趋于不变,FPR降低,NB先上升后下降。
# 我们可以找到NB最大的阈值,作为模型预测的最佳cutoff值。
# 绘制DCA曲线更直观,不同阈值下比较预测效果。
# 所以这些指标可以用来评估不同阈值下模型的预测效能,选择最佳阈值。曲线则可以直观比较不同模型。
summary(mydata$TIME)# 检查一下中位生存时间

# 图形美化
ggplot(dca_1, # 模型1
       linetype = F,# 线型
       color = c('#E64B357F','#4DBBD57F','gray'))+ # 线段颜色,如果线段增多，就增加颜色参数
       labs(title = 'Decision Curve Analysis',# 标题
       subtitle = 'DL',# 副标题
       x = 'Threshold probability',# x轴标题
       y = 'Net benefit')+# y轴标题
       theme(plot.title = element_text(size = 20,face = 'bold',hjust = 0.5),# 标题字体大小、粗细、位置
       plot.subtitle = element_text(size = 15,face = 'italic',hjust = 0.5),# 副标题字体大小、粗细、位置
       axis.title = element_text(size = 15,face = 'bold'),# xy轴标题字体大小、粗细
       axis.text = element_text(size = 12,face = 'bold'),# xy轴刻度字体大小、粗细
       legend.title = element_text(size = 15,face = 'bold'),# 图例标题字体大小、粗细
       legend.text = element_text(size = 12,face = 'bold'),# 图例刻度字体大小、粗细
       legend.position = 'bottom')# 图例位置

# ②绘制多个模型的DCA曲线，不设置时间，默认取中位生存时间
dca_2 <- dca(fit_1,# 模型1
             fit_2,# 模型2
             fit_3,# 模型3
             model.names = c("DL signature", "Clinical", "DLCS"))

ggplot(dca_2,
       linetype = F,# 线型
       )

# ③绘制单个模型的DCA曲线，设置具体时间
time_1 <- 12
time_2 <- 12*2
time_3 <- 12*3

dca_3 <- dca(fit_1, # 模型1
             model.names = "DL", # 模型名称
             times = c(time_1,time_2,time_3)# 时间
             )

ggplot(dca_3,
       linetype = F,# 线型
       )

# ④绘制多个模型的DCA曲线，设置多个时间
time_1 <- 12
time_2 <- 12*2
time_3 <- 12*3
dca_4 <- dca(fit_1,# 模型1
             fit_2,# 模型2
             fit_3,# 模型3
             model.names = c("DL signature", "Clinical", "DLCS"), # 模型名称
             times = c(time_1,time_2,time_3)# 时间
             )

ggplot(dca_4,
       linetype = F# 线型
       )+ 
  theme_bw()+ # 白色背景
  facet_wrap(~time)# 按照时间分面

