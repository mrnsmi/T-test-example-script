##Example script for creating two groups of simulated data, running a t-test, and visualizing
#Create two groups of data with different means
set.seed(1234)
group1 <- rnorm(1000, mean=2, sd=10)

set.seed(5678)
group2 <- rnorm(1000, mean=10, sd=10)


#Confirm normal distribution by Shapiro-Wilk test
shapiro.test(group1)
shapiro.test(group2)


#Confirm equal variances by F-test
var.test(group1, group2)


#Use an unpaired t-test to determined whether groups are significantly different
t.test(group1, group2, var.equal = TRUE)


#Write a function to calculate standard error
se <- function(x) {
  sd(x)/sqrt(length(x))
}


#Use standard error as an estimate of variability
error_group1 <- sd(group1)
error_group2 <- sd(group2)

#Combine into a data frame for plotting
data <- data.frame(
  group = c("Group1", "Group2"),
  mean = c(mean(group1), mean(group2)),
  std_error = c(se(group1), se(group2))
)

#Plot using basic R plotting
pdf(file="MarenSmith_Basic_Barplot.pdf", height = 11, width = 8.5) #Save to a pdf file

basic_plot <- barplot(data$mean ~ group, ylim=c(0,12), xlab="Group", ylab="Mean")

#Add error brs
arrows(x0 = basic_plot,y0 = data$mean + data$std_error, 
       y1 = data$mean - data$std_error, angle = 90, code = 3)

#Add significance bar and text
segments(x0 = 0.5, x1 = 2, y0 = 11, y1 = 11,)
text(1,11,expression(paste("***")))

dev.off()

#Make a more elegant version using ggplot2
library("ggplot2")

pdf(file="MarenSmith_ggplot_Barplot.pdf", height = 11, width = 8.5)

ggplot(data, aes(group, mean, fill = group)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-std_error, ymax=mean+std_error), width=.2) + #Add error bars
  geom_segment(aes(x = 1, y = 11, xend = 2, yend = 11)) + #Add significance bar
  annotate("text", x=1.5, y=11.5, label= "***") #Add asterisks to signify statistical significance

  
  dev.off()


###Written by Maren L Smith, 06/13/2023###
