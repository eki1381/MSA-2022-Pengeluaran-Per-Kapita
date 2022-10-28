#Import libraries we need
library(emdi)
library(writexl)

####################################################
#Modelling Fay-Herriot Model without Transformation#
#This model is for Global Model                    #
####################################################

#Import indicator and auxiliary variables that already selected by Recursive Feature Elimination
indikator <- read.csv('C:\\Users\\user\\Documents\\Hobby\\Small Area Estimation MSA 2022\\3. Pengeluaran per Kapita\\dataset\\indikator\\DE_INDIKATOR3.csv', sep = ';')
auxiliary <- read.csv('C:\\Users\\user\\Documents\\Hobby\\Small Area Estimation MSA 2022\\3. Pengeluaran per Kapita\\dataset\\auxiliary\\Pendataan PODES 2021\\auxiliary_linear_transformed.csv')
auxiliary <- cbind(indikator$Kecamatan, auxiliary)
dataset <- combine_data(pop_data = auxiliary, pop_domains = "indikator$Kecamatan", smp_data = indikator, smp_domains = "Kecamatan" )

#Define formula
col <- names(dataset)[-c(1:10)]
lf1 <- formula(paste("Rata2.Kapita"," ~ ", paste(col, collapse=" + ")))

#Run the modelling, then export the result
fh <- fh(fixed = lf1, vardir = "VAR", combined_data = dataset, domains = "Kecamatan", method = "ml", MSE = TRUE)
result <- estimators(fh, CV = TRUE)
write_xlsx(result$ind, 'C:\\Users\\user\\Documents\\Hobby\\Small Area Estimation MSA 2022\\3. Pengeluaran per Kapita\\result\\FH_notrans_podes.csv')

#Compare plot
#compare_plot(fh, CV = TRUE, MSE = TRUE, label = 'orig')

####################################################
#Modelling Fay-Herriot Model without Transformation#
#This model is for Local Sumsel Model              #
####################################################

#Import indicator and auxiliary variables that already selected by Recursive Feature Elimination. In this case,
#feature selection only applied for Sumsel domain
indikator <- read.csv('C:\\Users\\user\\Documents\\Hobby\\Small Area Estimation MSA 2022\\3. Pengeluaran per Kapita\\dataset\\indikator\\DE_INDIKATOR3.csv', sep = ';')
indikator_sumsel <- indikator[indikator$Provinsi == 16,]
auxiliary <- read.csv('C:\\Users\\user\\Documents\\Hobby\\Small Area Estimation MSA 2022\\3. Pengeluaran per Kapita\\dataset\\auxiliary\\Pendataan PODES 2021\\auxiliary_sumsel_linear_transformed.csv')
auxiliary <- cbind(indikator_sumsel$Kecamatan, auxiliary)
dataset <- combine_data(pop_data = auxiliary, pop_domains = "indikator_sumsel$Kecamatan", smp_data = indikator_sumsel, smp_domains = "Kecamatan" )

#Define formula
col <- names(dataset)[-c(1:10)]
lf1 <- formula(paste("Rata2.Kapita"," ~ ", paste(col, collapse=" + ")))

#Run the modelling, then export the result
fh <- fh(fixed = lf1, vardir = "VAR", combined_data = dataset, domains = "Kecamatan", method = "ml", MSE = TRUE)
result <- estimators(fh, CV = TRUE)
write_xlsx(result$ind, 'C:\\Users\\user\\Documents\\Hobby\\Small Area Estimation MSA 2022\\3. Pengeluaran per Kapita\\result\\FH_notrans_podes_sumsel.csv')

#Compare plot
#compare_plot(fh, CV = TRUE, MSE = TRUE, label = 'orig')

####################################################
#Modelling Fay-Herriot Model without Transformation#
#This model is for Local Kalsel Model              #
####################################################

#Import indicator and auxiliary variables that already selected by Recursive Feature Elimination. In this case,
#feature selection only applied for Kalsel domain
indikator <- read.csv('C:\\Users\\user\\Documents\\Hobby\\Small Area Estimation MSA 2022\\3. Pengeluaran per Kapita\\dataset\\indikator\\DE_INDIKATOR3.csv', sep = ';')
indikator_kalsel <- indikator[indikator$Provinsi == 63,]
auxiliary <- read.csv('C:\\Users\\user\\Documents\\Hobby\\Small Area Estimation MSA 2022\\3. Pengeluaran per Kapita\\dataset\\auxiliary\\Pendataan PODES 2021\\auxiliary_kalsel_linear_transformed.csv')
auxiliary <- cbind(indikator_kalsel$Kecamatan, auxiliary)
dataset <- combine_data(pop_data = auxiliary, pop_domains = "indikator_kalsel$Kecamatan", smp_data = indikator_kalsel, smp_domains = "Kecamatan" )

#Define formula
col <- names(dataset)[-c(1:10)]
lf1 <- formula(paste("Rata2.Kapita"," ~ ", paste(col, collapse=" + ")))

#Run the modelling, then export the result
fh <- fh(fixed = lf1, vardir = "VAR", combined_data = dataset, domains = "Kecamatan", method = "ml", MSE = TRUE)
result <- estimators(fh, CV = TRUE)
write_xlsx(result$ind, 'C:\\Users\\user\\Documents\\Hobby\\Small Area Estimation MSA 2022\\3. Pengeluaran per Kapita\\result\\FH_notrans_podes_kalsel.csv')

#Compare plot
#compare_plot(fh, CV = TRUE, MSE = TRUE, label = 'orig')