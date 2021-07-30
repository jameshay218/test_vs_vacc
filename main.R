library(tidyverse)
library(patchwork)

calc_npv <- function(sens, prev, spec){
  (spec * (1-prev))/((1-sens)*prev+spec*(1-prev))
}

calc_p_I_given_U <- function(prev, vacc_coverage, vacc_effectiveness){
  prev/((1-vacc_effectiveness)*vacc_coverage + 1-vacc_coverage)
}
calc_p_I_given_V <- function(prev, vacc_coverage, vacc_effectiveness){
  (1-vacc_effectiveness)*calc_p_I_given_U(prev, vacc_coverage, vacc_effectiveness)
}
(1-calc_npv(0.7,0.01,0.99))
calc_p_I_given_U(0.01,0.6,0.75)
calc_p_I_given_V(0.01,0.6,0.75)


to_test <- expand.grid(prev=seq(0.001,0.02,by=0.001),coverage=seq(0.2,0.8,by=0.2),vacc_effective=0.75,spec=c(0.995,0.99,0.95),sens=c(0.5,0.6,0.7,0.8))
to_test <- to_test %>% mutate(test=1-calc_npv(sens,prev,spec),vacc=calc_p_I_given_V(prev, coverage,vacc_effective))
to_test <- to_test %>% mutate(coverage=as.factor(coverage),spec=as.factor(spec),sens=as.factor(sens)) %>%
  rename(`Test sensitivity`=sens,`Vaccine coverage`=coverage,`Test specificity`=spec) %>%
  mutate(`Test specificity`=paste0("Specificity=",`Test specificity`))
to_test <- to_test %>% mutate(relative=vacc/test)


theme_main <- theme_classic() + theme(plot.title=element_text(size=10),
                                      axis.text=element_text(size=7),
                                      legend.text=element_text(size=7),
                                      legend.title=element_text(size=8),
                                      axis.title=element_text(size=8))

p1 <- ggplot(to_test %>% rename()) + geom_line(aes(x=prev,y=vacc,col=`Vaccine coverage`)) + 
  scale_y_continuous(limits=c(0,0.02)) + 
  theme_main +
  theme(legend.position="bottom") +
  scale_color_viridis_d() +
  xlab("Prevalence")+
  ggtitle("Assuming vaccine effectiveness=0.75")+
  ylab("Probability of being infected,\n given vaccinated")
p2 <- ggplot(to_test %>% filter(`Test specificity`=="Specificity=0.99")) + 
  geom_line(aes(x=prev,y=test,linetype=`Test sensitivity`)) + 
  #facet_wrap(~`Test specificity`)+ 
  scale_y_continuous(limits=c(0,0.02))+ 
  theme_main +
  theme(legend.position="bottom") +
  xlab("Prevalence")+
  ggtitle("Assuming specificity=0.99")+
  ylab("Probability of being infected,\n given test negative and unvaccinated")
p3 <- ggplot(to_test %>% mutate(`Test sensitivity`=paste0("Sensitivity=",`Test sensitivity`))) +
  geom_line(aes(x=prev,y=relative,col=`Vaccine coverage`)) + facet_grid(`Test sensitivity`~`Test specificity`) +
  scale_color_viridis_d()+
  geom_hline(yintercept=1,linetype="dashed") +
  theme_main+
  xlab("Prevalence") +
  ylab("Ratio (log scale)") +
  scale_y_log10(breaks=c(0,0.25,0.5,0.75,1,1.25,1.5,2,2.5,3)) +
  ggtitle("Probability of being infected given vaccinated/\nProbability of being infected given test negative and unvaccinated")
                
p_main <- p1 + p2 + plot_layout(widths=c(1,1))
ggsave("~/Google Drive/nCoV/vacc_test_compare1.png",p_main,dpi=300,height=3,width=7,units="in")
ggsave("~/Google Drive/nCoV/vacc_test_compare2.png",p3,dpi=300,height=5,width=8)
