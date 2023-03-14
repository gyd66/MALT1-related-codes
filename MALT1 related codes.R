library(reshape2)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(survival)
library(survminer)

#################################---1-expression analysis---------#################
###
svdata=read.csv("data.csv")
####
ggplot(data = svdata,aes(x = Subtype, y = svdata$RUFY1, fill = Subtype))+
  scale_fill_manual(values = c( "blue","red","yellow")) + ##"blue", "purple","green","red", "blue","purple"
  geom_violin(alpha=0.4, position = position_dodge(width = .75),
              size=0.8, color="black") + # 边框线黑色
  geom_boxplot(notch = TRUE, outlier.size = -1, 
               color="black", lwd=0.8, alpha = 0.7)+ # 背景色透明化
  geom_point(shape = 21, size=2, 
             position = position_jitterdodge(), 
             color="black", alpha=1)+ # 边框线黑色
  theme_classic() +

  theme(axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.2,"cm"),
        legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  # 如果不要组间比较就注释掉下面这行
  #stat_compare_means(comparisons = my_comparisons) +
  stat_compare_means(method = "t.test", label.y = max(svdata$RUFY1))  ###  kruskal.test
ggsave(filename = "RUFY1.pdf", width = 4, height = 5)


#################################---2-survival analysis---------#################
svdata=read.csv("data2.csv")
res.cut <- surv_cutpoint(svdata, time = "OS.time", 
                         event = "OS",
                         variables = names(svdata)[4], 
                         minprop = 0.0) 
res.cut$RUFY1#
res.cat=svdata
res.cat$group <- cut(res.cat[,4],breaks=c(-Inf,res.cut$RUFY1[["estimate"]], Inf),
                     labels=c("low","high"))

table(res.cat$group)

##
fit <- survfit(Surv(OS.time, OS) ~ group, data = res.cat)
##basic plot
ggsurvplot(fit, data = res.cat,pval=T)

##
##Uber customized survival curves
pdf("OS.pdf")
ggsurv <- ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  data =  res.cat,             # data used to fit survival curves.
  risk.table = TRUE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  #conf.int = TRUE,         # show confidence intervals for 
  # point estimates of survival curves.
  palette = c("#2E9FDF","#e74500" ),
  xlim = c(0,100),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "Time in months",   # customize X axis label.
  break.time.by = 24,     # break X axis in time intervals by 5年时间.
  #ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.height = 0.25, # the height of the risk table
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  #ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  ncensor.plot.height = 0.25,
  conf.int.style = "step",  # customize style of confidence intervals
  #surv.median.line = "hv",  # add the median survival pointer.
  legend.labs =
    c( "Low risk","High risk")    # change legend labels.
)
print(ggsurv, newpage = FALSE)
dev.off()



