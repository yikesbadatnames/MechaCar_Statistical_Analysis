# D1 Linear Regression ---------------------------------------------------
lr_table <- read.csv(file='MechaCar_mpg.csv', check.names = F, stringsAsFactors = F) 

summary(lm(mpg ~ vehicle_weight + vehicle_length + spoiler_angle + ground_clearance + AWD, lr_table))
# D2 Summary of Data ------------------------------------------------------
sum_table <- read.csv(file='Suspension_Coil.csv', check.names = F, stringsAsFactors = F) 

summarize_df <- sum_table %>% summarize(Mean_PSI=mean(PSI),
                                        Median_PSI=median(PSI),
                                        Var_PSI=var(PSI),
                                        Std_Dev_PSI=sd(PSI),
                                        Num_Coil=n(), .groups = 'keep') 
lot_df <- sum_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),
                                                             Median_PSI=median(PSI),
                                                             Var_PSI=var(PSI),
                                                             Std_Dev_PSI=sd(PSI),
                                                             Num_Coil=n(), .groups = 'keep')
#D3 T Tests ----------------------------------------------------------------------
t.test(sum_table$PSI, mu=15000)

lot1 <- subset(sum_table, Manufacturing_Lot=="Lot1")
lot2 <- subset(sum_table,Manufacturing_Lot=="Lot2")
lot3 <- subset(sum_table,Manufacturing_Lot=="Lot3")

t.test(lot1$PSI,mu=1500)
t.test(lot2$PSI,mu=1500)
t.test(lot3$PSI,mu=1500)

