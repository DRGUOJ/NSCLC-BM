# ��������Ҫ�İ� -----------------------------------------------------------------
library(survival)# cox�ع�ģ����Ҫ
library(rms)
library(survminer)
library(ggplot2)#����ɭ��ͼ3

#���ݶ�ȡ
#ѵ����
mydata <- read.csv(file = "E:/����/4 NSCLC BM/��������/table3 �ٴ������ض�����coxģ��/train1.csv",header = TRUE)
# ������cox
#�ȷ�����������
fit1<-coxph(Surv(TIME,Ending==1)~Gender,data=mydata)
summary(fit1)

#��ȡ��Ч��Ϣ
data_use1<-summary(fit1)

HR<-round(data_use1$coefficients[,2],2)#��ȡHRֵ
CI<-paste0(round(data_use1$conf.int[,3:4],2),collapse = '-')#HR��95%��������
p_value<-round(data_use1$coefficients[,5],3)#pֵ
#���������ݿ�
result1<-data.frame("character"="sex",
                    "Hazard Ratio"=HR,
                    "95%CI"=CI,
                    "p value"=p_value)

# forѭ����ȡÿһ�����������е�����cox�ع���� -----------------------------------------------
surv_object <- with(mydata,Surv(TIME,Ending))
#����һ���հ׵����ݿ�������¼ѭ����ȡ��õĽ��
result <- data.frame("model" = character(),
                     "Hazard Ratio" = numeric(),
                     "95%CI" = character(),
                     "P value" = numeric())

for (i in 4:length(colnames(mydata))) {
  print(i)
  model <- colnames(mydata)[i]
  Model <- coxph(surv_object ~ mydata[[model]], data = mydata)
  data_use <- summary(Model)
  HR <- round(data_use$coefficients[,2],2)#��ȡHRֵ
  CI <- paste0(round(data_use$conf.int[,3:4],2),collapse = '-')#HR��95%��������
  P_value <- round(data_use$coefficients[,5],3)#Pֵ
  temp_result <- data.frame("model" = colnames(mydata)[i],
                            "Hazard Ratio" = HR,
                            "95%CI" = CI,
                            "P value" = P_value)
  result <- rbind(result, temp_result)
}
#write.csv(result, file = "��cox results.csv")

# ɸѡ������cox�ع������P��0.05��ֵ������ -----------------------------------------
result_use <- result
result_use$significant <- ifelse(result_use$P.value < 0.05,"significant","Not significant")
table(result_use$significant)

# ������cox�ع���� --------------------------------------------------------------
multi_cox_character <- result_use[result_use$significant == "significant",]$model#��ȡ�����Ļ���
formula <- as.formula(paste("Surv(TIME, Ending) ~", paste(multi_cox_character, collapse = " + ")))
formula#ȷ�Ϲ�ʽ�е��Ա����Ƿ���ȷ
multi_cox_model <- coxph(formula, data = mydata)

summary(multi_cox_model)



#��ȡ������cox�ع��еĽ�������ݿ��ӻ�
data_use <- summary(multi_cox_model)
multi_cox_HR <- round(data_use$coefficients[,2],2)#��ȡHRֵ
multi_cox_CI2.5 <- round(data_use$conf.int[,3],2)
multi_cox_CI97.5 <- mul_CI95<-round(data_use$conf.int[,4],2)
multi_cox_CI <- paste0('(',multi_cox_CI2.5,'-',multi_cox_CI97.5,')')#HR��95%��������
multi_cox_P_value <- round(data_use$coefficients[,5],3)#Pֵ
Variable <- row.names(data.frame(data_use$coefficients))
multi_cox_result<- data.frame(Variable,multi_cox_HR,multi_cox_CI2.5,multi_cox_CI97.5,multi_cox_CI,multi_cox_P_value)
multi_cox_result

#write.csv(multi_cox_result, file = "multi_cox_result.csv")

model<-coxph(Surv(TIME,Ending==1)~ Postoperative.treatment+Clinical.T.category + Clinical.N.category + CEA.ng.mL. + Pleural.attachment + Spiculation,data=mydata)
summary(model)

#���ӻ�--ɭ��ͼ3--����ggplot��
colnames(multi_cox_result)#�鿴���ݱ�������

#��ȡ������cox�ع��еĽ�������ݿ��ӻ�
data_use <- summary(model) #�����Ҫ��ʾȫ��������������0.05�ģ���multi_cox_model
multi_cox_HR <- round(data_use$coefficients[,2],2)#��ȡHRֵ
multi_cox_CI2.5 <- round(data_use$conf.int[,3],2)
multi_cox_CI97.5 <- mul_CI95<-round(data_use$conf.int[,4],2)
multi_cox_CI <- paste0('(',multi_cox_CI2.5,'-',multi_cox_CI97.5,')')#HR��95%��������
P_value <- round(data_use$coefficients[,5],3)#Pֵ
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