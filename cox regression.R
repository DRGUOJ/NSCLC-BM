# 加载所需要的包 -----------------------------------------------------------------
library(survival)# cox回归模型需要
library(rms)
library(survminer)
library(ggplot2)#绘制森林图3

#数据读取
#训练集
mydata <- read.csv(file = "E:/文章/4 NSCLC BM/数据整理/table3 临床单因素多因素cox模型/train1.csv",header = TRUE)
# 单因素cox
#先分析单个因素
fit1<-coxph(Surv(TIME,Ending==1)~Gender,data=mydata)
summary(fit1)

#提取有效信息
data_use1<-summary(fit1)

HR<-round(data_use1$coefficients[,2],2)#提取HR值
CI<-paste0(round(data_use1$conf.int[,3:4],2),collapse = '-')#HR的95%置信区间
p_value<-round(data_use1$coefficients[,5],3)#p值
#输出结果数据框
result1<-data.frame("character"="sex",
                    "Hazard Ratio"=HR,
                    "95%CI"=CI,
                    "p value"=p_value)

# for循环提取每一个变量，进行单因素cox回归分析 -----------------------------------------------
surv_object <- with(mydata,Surv(TIME,Ending))
#生成一个空白的数据框，用来记录循环提取获得的结果
result <- data.frame("model" = character(),
                     "Hazard Ratio" = numeric(),
                     "95%CI" = character(),
                     "P value" = numeric())

for (i in 4:length(colnames(mydata))) {
  print(i)
  model <- colnames(mydata)[i]
  Model <- coxph(surv_object ~ mydata[[model]], data = mydata)
  data_use <- summary(Model)
  HR <- round(data_use$coefficients[,2],2)#提取HR值
  CI <- paste0(round(data_use$conf.int[,3:4],2),collapse = '-')#HR的95%置信区间
  P_value <- round(data_use$coefficients[,5],3)#P值
  temp_result <- data.frame("model" = colnames(mydata)[i],
                            "Hazard Ratio" = HR,
                            "95%CI" = CI,
                            "P value" = P_value)
  result <- rbind(result, temp_result)
}
#write.csv(result, file = "单cox results.csv")

# 筛选单因素cox回归分析中P＜0.05阈值的因素 -----------------------------------------
result_use <- result
result_use$significant <- ifelse(result_use$P.value < 0.05,"significant","Not significant")
table(result_use$significant)

# 多因素cox回归分析 --------------------------------------------------------------
multi_cox_character <- result_use[result_use$significant == "significant",]$model#提取显著的基因
formula <- as.formula(paste("Surv(TIME, Ending) ~", paste(multi_cox_character, collapse = " + ")))
formula#确认公式中的自变量是否正确
multi_cox_model <- coxph(formula, data = mydata)

summary(multi_cox_model)



#提取多因素cox回归中的结果及数据可视化
data_use <- summary(multi_cox_model)
multi_cox_HR <- round(data_use$coefficients[,2],2)#提取HR值
multi_cox_CI2.5 <- round(data_use$conf.int[,3],2)
multi_cox_CI97.5 <- mul_CI95<-round(data_use$conf.int[,4],2)
multi_cox_CI <- paste0('(',multi_cox_CI2.5,'-',multi_cox_CI97.5,')')#HR的95%置信区间
multi_cox_P_value <- round(data_use$coefficients[,5],3)#P值
Variable <- row.names(data.frame(data_use$coefficients))
multi_cox_result<- data.frame(Variable,multi_cox_HR,multi_cox_CI2.5,multi_cox_CI97.5,multi_cox_CI,multi_cox_P_value)
multi_cox_result

#write.csv(multi_cox_result, file = "multi_cox_result.csv")

model<-coxph(Surv(TIME,Ending==1)~ Postoperative.treatment+Clinical.T.category + Clinical.N.category + CEA.ng.mL. + Pleural.attachment + Spiculation,data=mydata)
summary(model)

#可视化--森林图3--基于ggplot包
colnames(multi_cox_result)#查看数据变量名称

#提取多因素cox回归中的结果及数据可视化
data_use <- summary(model) #如果想要显示全部特征，包括＞0.05的，就multi_cox_model
multi_cox_HR <- round(data_use$coefficients[,2],2)#提取HR值
multi_cox_CI2.5 <- round(data_use$conf.int[,3],2)
multi_cox_CI97.5 <- mul_CI95<-round(data_use$conf.int[,4],2)
multi_cox_CI <- paste0('(',multi_cox_CI2.5,'-',multi_cox_CI97.5,')')#HR的95%置信区间
P_value <- round(data_use$coefficients[,5],3)#P值
Variable <- row.names(data.frame(data_use$coefficients))
multi_cox_result<- data.frame(Variable,multi_cox_HR,multi_cox_CI2.5,multi_cox_CI97.5,multi_cox_CI,multi_cox_P_value)
multi_cox_result

ggplot(multi_cox_result, aes(multi_cox_HR, Variable)) + 
  geom_vline(xintercept = 1,
             linetype = "dashed",
             size = 1) +
  geom_errorbar(aes(xmin = multi_cox_CI2.5, xmax = multi_cox_CI97.5),width = 0.1) +
  geom_point(aes(color = P_value),size = 5, shape = 18) +
  scale_color_continuous(low = 'skyblue', high = 'red') +
  labs(x = 'Hazard ratio', title = 'Clinical Model') +
  theme_pubr() +
  theme(legend.position = 'right')
