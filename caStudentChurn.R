#############################
###Correspondence Analysis###
#############################


library("FactoMineR")
library("factoextra")

dat1 <- read.csv("studentChurn.csv", header = TRUE) %>% clean_names()

attach(dat1)

print(dat1)


dat1 <- as.data.frame(dat1)

row.names(dat1) <- c("gender_f",
                     "gender_m",
                     "age_under_21",
                     "age_over_21",
                     "family_closeby",
                     "admission_before_term_over_90_days",
                     "traveltime_miles_greater_20",
                     "studytime_greater_4",
                     "sat_scores_greater_1100"
)
dat1$x <- NULL
dat1$active <- NULL

dt2 <- as.table(as.matrix(dat1))


dt2 <- sqrt(dt2)

dt2 <- as.table(dt2)


#Plot Correlations
correlations <- cor(dt2)
correlations

corrplot(correlations)

#Compute chi-squared test to determine if relationships are statistically significant 
chisq <- chisq.test(dt2)
chisq

chisq_plot <- as.data.frame(chisq)

#Compute correspondence analysis
CA(dt2, ncp = 5, graph = TRUE)

res_ca <- CA(dt2, graph = FALSE)
res_ca

#Plot Biplot 
fviz_ca_biplot(res_ca, repel = TRUE)

#extract eigenvalues
get_eigenvalue(res_ca)
fviz_eig(res_ca)

#Extract results of row values
row_values <- get_ca_row(res_ca)
print(row_values)

#Examine quality of the representation of the rows in data
plot(row_values$contrib)
row_values$cos2
plot(row_values$cos2)

#plot dimensions
corrplot(res_ca$row$coord, is.corr = FALSE)

#Plot contribution by row
fviz_contrib(res_ca, choice = 'row', top = 10)
fviz_contrib(res_ca, choice = 'row', top = 10, axes = 2)

#asymmetric biplot
fviz_ca_biplot(res_ca, 
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)

#Row importance
fviz_ca_row(res_ca, col.row = "contrib", gradient.cols = c("gray", "orange", "red"), repel = TRUE)

#Column importance
fviz_ca_col(res_ca, col.col = "cos2", gradient.cols = c("gray", "orange", "red"), repel = TRUE)




dat_pca <- PCA(dat1)

dat_table <- as.matrix(dat1)

dat_table <- as.table(dat1)

fviz_pca_ind(dt2,
             label = "none", # hide individual labels
             habillage = dat_table$retention_stay, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)
