# 加载所需要的包 -----------------------------------------------------------------
library(rms)#拟合模型
library(survival)#生存分析
library(foreign)
library(VIM)#缺失值可视化
library(pROC)#ROC曲线
library(timeROC)#时间依赖ROC

# test
mydata2 <- read.csv(file = "E:/文章/4 NSCLC BM/数据整理/table2 c-index和时t-roc/test.csv",header = TRUE)
#时间变量的处理
summary(mydata2$TIME)#检查时间的单位
#mydata$time <- mydata$time/365#将时间数值设置为年
units(mydata2$TIME) <- "month"#将时间单位设置为月
#生存变量的处理
table(mydata2$Ending)#检查生存状态

# 开始构建Cox 回归模型 ------------------------------------------------------------
#数据打包 
dd <- datadist(mydata2)
options(datadist = "dd")

surv_object2 <- with(mydata2, Surv(TIME, Ending==1))#构建回归分析因变量
colnames(mydata2)#查看行名，把你要的变量放到下面拟合的公式中
model2 <-  cph(surv_object2 ~ DL+Clinical.T.category + Clinical.N.category + CEA.ng.mL. + Pleural.attachment + Spiculation, x = TRUE ,y = TRUE, surv = TRUE, data = mydata2)
model2#查看模型具体情况
HR <- exp(model2$coefficients)#根据回归系数求HR值
HR

# 设置不同节点的生存函数 -------------------------------------------------------------
surv <- Survival(model2)#拟合生存函数

surv_1y <- function(x)surv(12,lp=x)#一年生存函数
surv_2y <- function(x)surv(12*2,lp=x)#2年生存函数
surv_3y <- function(x)surv(12*3,lp=x)#3年生存函数


# 模型评估与验证 -----------------------------------------------------------------
# ROC曲线 -------------------------------------------------------------------
pred <- predict(model2,mydata2,type="lp")# 使用模型预测数据
#write.csv(pred,"E:/博士课题/第二部分/1survival/nomo/predict.csv")#预测值导出
colnames(mydata2)
ROC_table <- data.frame(time = mydata2[,"TIME"],Ending = mydata2[,"Ending"],score = pred)
# 计算不同时间点的时间ROC曲线
time_roc_res <- timeROC(T = ROC_table$time,
                        delta = ROC_table$Ending,
                        marker = ROC_table$score,
                        cause = 1,
                        weighting="marginal",
                        times = c(12, 2*12, 3*12),
                        ROC = TRUE,
                        iid = TRUE
)
#计算置信区间
confint(time_roc_res,level = 0.95)$CI_AUC
# 创建一个数据框，包含1年、2年和3年生存的真阳性和假阳性率
time_ROC_df <- data.frame(TP_1year = time_roc_res$TP[, 1],
                          FP_1year = time_roc_res$FP[, 1],
                          TP_2year = time_roc_res$TP[, 2],
                          FP_2year = time_roc_res$FP[, 2],
                          TP_3year = time_roc_res$TP[, 3],
                          FP_3year = time_roc_res$FP[, 3]
)
#绘制3年的ROC曲线
ggplot(data = time_ROC_df) +
  geom_line(aes(x = FP_3year, y = TP_3year), size = 1, color = "#BC1328") +
  geom_abline(slope = 1, intercept = 0, color = "grey", size = 1, linetype = 2) +
  theme_bw() +
  annotate("text",x = 0.70, y = 0.25, size = 5.5,label = paste0("AUC of 3-year survival = ", sprintf("%.3f", time_roc_res$AUC[[3]])), color = "#BC1328") +
  labs(x = "1-specificity", y = "Sensitivity") +
  theme(axis.text = element_text(face = "bold", size = 11, color = "black"),
        axis.title.x = element_text(face = "bold", size = 14, color = "black", margin = margin(c(15, 0, 0, 0))),
        axis.title.y = element_text(face = "bold", size = 14, color = "black", margin = margin(c(0, 15, 0, 0))))
#绘制1年、3年、5年的ROC曲线
ggplot(data = time_ROC_df) +
  geom_line(aes(x = FP_1year, y = TP_1year), size = 1, color = "#0067B5") +
  geom_line(aes(x = FP_2year, y = TP_2year), size = 1, color = "#09891D") +
  geom_line(aes(x = FP_3year, y = TP_3year), size = 1, color = "#BC1328") +
  geom_abline(slope = 1, intercept = 0, color = "grey", size = 1, linetype = 2) +
  theme_bw() +
  annotate("text",x = 0.75, y = 0.20, size = 4.5,label = paste0("AUC of 1-year survival = ", sprintf("%.3f", time_roc_res$AUC[[1]])), color = "#0067B5") +
  annotate("text",x = 0.75, y = 0.15, size = 4.5,label = paste0("AUC of 2-year survival = ", sprintf("%.3f", time_roc_res$AUC[[2]])), color = "#09891D") +
  annotate("text",x = 0.75, y = 0.10, size = 4.5,label = paste0("AUC of 3-year survival = ", sprintf("%.3f", time_roc_res$AUC[[3]])), color = "#BC1328") +
  labs(x = "1-specificity", y = "Sensitivity") +
  theme(axis.text = element_text(face = "bold", size = 11, color = "black"),
        axis.title.x = element_text(face = "bold", size = 14, color = "black", margin = margin(c(15, 0, 0, 0))),
        axis.title.y = element_text(face = "bold", size = 14, color = "black", margin = margin(c(0, 15, 0, 0))))


# external
mydata2 <- read.csv(file = "E:/文章/4 NSCLC BM/数据整理/table2 c-index和时t-roc/external.csv",header = TRUE)
#时间变量的处理
summary(mydata2$TIME)#检查时间的单位
#mydata$time <- mydata$time/365#将时间数值设置为年
units(mydata2$TIME) <- "month"#将时间单位设置为月
#生存变量的处理
table(mydata2$Ending)#检查生存状态

# 开始构建Cox 回归模型 ------------------------------------------------------------
#数据打包 
dd <- datadist(mydata2)
options(datadist = "dd")

surv_object2 <- with(mydata2, Surv(TIME, Ending==1))#构建回归分析因变量
colnames(mydata2)#查看行名，把你要的变量放到下面拟合的公式中
model2 <-  cph(surv_object2 ~ DL+ Clinical.T.category + Clinical.N.category + CEA.ng.mL. + Spiculation+ Pleural.attachment , x = TRUE ,y = TRUE, surv = TRUE, data = mydata2)
model2#查看模型具体情况
HR <- exp(model2$coefficients)#根据回归系数求HR值
HR

# 设置不同节点的生存函数 -------------------------------------------------------------
surv <- Survival(model2)#拟合生存函数

surv_1y <- function(x)surv(12,lp=x)#一年生存函数
surv_2y <- function(x)surv(12*2,lp=x)#2年生存函数
surv_3y <- function(x)surv(12*3,lp=x)#3年生存函数

# 模型评估与验证 -----------------------------------------------------------------
# ROC曲线 -------------------------------------------------------------------
pred <- predict(model2,mydata2,type="lp")# 使用模型预测数据
#write.csv(pred,"E:/博士课题/第二部分/1survival/nomo/predict.csv")#预测值导出
colnames(mydata2)
ROC_table <- data.frame(time = mydata2[,"TIME"],Ending = mydata2[,"Ending"],score = pred)
# 计算不同时间点的时间ROC曲线
time_roc_res <- timeROC(T = ROC_table$time,
                        delta = ROC_table$Ending,
                        marker = ROC_table$score,
                        cause = 1,
                        weighting="marginal",
                        times = c(12, 2*12, 3*12),
                        ROC = TRUE,
                        iid = TRUE
)
#计算置信区间
confint(time_roc_res,level = 0.95)$CI_AUC
# 创建一个数据框，包含1年、2年和3年生存的真阳性和假阳性率
time_ROC_df <- data.frame(TP_1year = time_roc_res$TP[, 1],
                          FP_1year = time_roc_res$FP[, 1],
                          TP_2year = time_roc_res$TP[, 2],
                          FP_2year = time_roc_res$FP[, 2],
                          TP_3year = time_roc_res$TP[, 3],
                          FP_3year = time_roc_res$FP[, 3]
)
#绘制3年的ROC曲线
ggplot(data = time_ROC_df) +
  geom_line(aes(x = FP_3year, y = TP_3year), size = 1, color = "#BC1328") +
  geom_abline(slope = 1, intercept = 0, color = "grey", size = 1, linetype = 2) +
  theme_bw() +
  annotate("text",x = 0.70, y = 0.25, size = 5.5,label = paste0("AUC of 3-year survival = ", sprintf("%.3f", time_roc_res$AUC[[3]])), color = "#BC1328") +
  labs(x = "1-specificity", y = "Sensitivity") +
  theme(axis.text = element_text(face = "bold", size = 11, color = "black"),
        axis.title.x = element_text(face = "bold", size = 14, color = "black", margin = margin(c(15, 0, 0, 0))),
        axis.title.y = element_text(face = "bold", size = 14, color = "black", margin = margin(c(0, 15, 0, 0))))
#绘制1年、3年、5年的ROC曲线
ggplot(data = time_ROC_df) +
  geom_line(aes(x = FP_1year, y = TP_1year), size = 1, color = "#0067B5") +
  geom_line(aes(x = FP_2year, y = TP_2year), size = 1, color = "#09891D") +
  geom_line(aes(x = FP_3year, y = TP_3year), size = 1, color = "#BC1328") +
  geom_abline(slope = 1, intercept = 0, color = "grey", size = 1, linetype = 2) +
  theme_bw() +
  annotate("text",x = 0.75, y = 0.20, size = 4.5,label = paste0("AUC of 1-year survival = ", sprintf("%.3f", time_roc_res$AUC[[1]])), color = "#0067B5") +
  annotate("text",x = 0.75, y = 0.15, size = 4.5,label = paste0("AUC of 2-year survival = ", sprintf("%.3f", time_roc_res$AUC[[2]])), color = "#09891D") +
  annotate("text",x = 0.75, y = 0.10, size = 4.5,label = paste0("AUC of 3-year survival = ", sprintf("%.3f", time_roc_res$AUC[[3]])), color = "#BC1328") +
  labs(x = "1-specificity", y = "Sensitivity") +
  theme(axis.text = element_text(face = "bold", size = 11, color = "black"),
        axis.title.x = element_text(face = "bold", size = 14, color = "black", margin = margin(c(15, 0, 0, 0))),
        axis.title.y = element_text(face = "bold", size = 14, color = "black", margin = margin(c(0, 15, 0, 0))))


