#Maxwell Greene
#Programming Competency Exam
#Data Science
#9/21/18

#Question 1
getwd()

#Question 2
setwd("C:/Users/Max/Documents/GitHub/DataScience")
     #Sets my working directory to my DataScience local GitHub repository 

#Question 3
my_matrix <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),byrow = FALSE, nrow = 3)

#Question 4
my_matrix2 <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),byrow = TRUE, nrow = 4)

#Question 5
colnames <- c("one","two","three")
my_dataframe <- as.data.frame(my_matrix2,row.names = colnames)

#Question 6a
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))

#Question 6b
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = depth),binwidth = .5)

