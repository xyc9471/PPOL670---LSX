## vis1- average income by gender and education level
# step 1 - arranging the data
vis1 <- (FinalData)
means <- aggregate(indinc_cpi ~ educ_degree + GENDER, vis2, mean)
a=round(means, digits = 2)
# step 2 - make plot
a$GENDER = as.character(a$GENDER)
a$educ_degree = as.character(a$educ_degree)
ggplot(data = a, mapping = aes(x = educ_degree, y = indinc_cpi, fill = GENDER)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  theme(plot.margin = unit(c(7, 1, 4, 1), "cm")) + 
  scale_fill_manual(values = c("mistyrose4", "navajowhite")) +
  geom_text(aes(label = indinc_cpi)) + theme(axis.title.y = element_blank(), panel.background = element_blank())

## vis2-average income by gender and area and 
vis2 <- (FinalData)
means2 <- aggregate(indinc_cpi ~ area + GENDER, vis2, mean)
a2=round(means2, digits = 2)

#vis3- labor force participation by gender and marriage 
vis3 <- (FinalData)
partA = data.frame(table(vis3$GENDER, vis2$mar_status))
names(partA)[names(partA) == "Var1"] <- "GENDER"
names(partA)[names(partA) == "Var2"] <- "marriage status"
names(partA)[names(partA) == "Freq"] <- "pop"
vis2 = group_by(FinalData, area) %>% mutate(percent = wpop/sum(wpop))
partB$pct = partB$percent * 100