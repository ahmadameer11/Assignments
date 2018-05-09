install.packages("pwr")
library(pwr)
Effect_size <- cohen.ES((test = "t"), size = c("large")) #calculating effect size
Effect_size
power <- pwr.t.test(d = .8, sig.level = .05, power = .70, alternative = 'greater')
power
plot(power)

data <- read.csv('C:/Users/Owner/Downloads/carbon.csv', header = FALSE, stringsAsFactors = FALSE)[-c(1, 2, 3, 4),]
colnames <- c("Year", "Agricultureandfishing", "Rubberandplasticproduct") # renaming the columns
colnames(data) <- colnames
str(data)
data$Year <- as.numeric(as.character(data$Year))
data$Agricultureandfishing <- as.numeric(as.character(data$Agricultureandfishing))
data$Rubberandplasticproduct <- as.numeric(as.character(data$Rubberandplasticproduct))
row.names(data) <- NULL #getting rid of row names.
str(data)
data
finding <- t.test(data$Rubberandplasticproduct, data$Agricultureandfishing, alternative = "greater")
finding