#Frank Dye, Anthony Rodriguez
#12/3/2013

#Question 1
print("Question 1")

#define our array of mean values
meanArrayOne = array(dim=c(225,1),0)

for(i in 1:225)
{
	#generate our random numbers
	a = runif(22500, -1.0, 1.0)
	innerCount <- 0
	for (j in 1:22500)
	{
		innerCount = innerCount + a[j]
	}
	#Calculating individual means of our experiment
	meanArrayOne[i] = innerCount / 22500
}

#Count the number of means that are between 0.2 and 0.0 
count = 0
sumOfMeans = 0

for (k in 1:225)
{
	if (meanArrayOne[k] >= 0.0 && meanArrayOne[k] <= 0.2)
	{
		count = count + 1
	}
	sumOfMeans = sumOfMeans + meanArrayOne[k]
}

#Question 1A, Calculating probability 
questionOneProb = count / 225
#Question 1B, Calculating mean of means
questionOneMean = sumOfMeans / 225

sumOfVariances = 0
for (l in 1:225)
{
	sumOfVariances = sumOfVariances + ((meanArrayOne[l] - questionOneMean)^2)
}


#Question 1C, Calculating standard deviation
questionOneStdDev = sqrt((1/224) * sumOfVariances)


#Print propability that meanArray is between 0.0 and 0.2 or P(0.0 <= X <= 0.2)
print("(Question 1A) Find the simulated probability that the mean is between 0 and 0.2 inclusive: ")
print(questionOneProb)

#Print mean of the mean's
print("(Question 1B) Mean of sample mean's: ")
print(questionOneMean)

#print standard deviation of mean's
print("(Question 1C) Standard Deviation of mean's: ")
print(questionOneStdDev)

dev.new()
# Question 1D Draw the histogram of the mean's
hist(meanArrayOne, col = "red", main = "Question 1D, Histogram mean sampling -1:1")


#Question 2
print("Question 2")

meanArrayTwo = array(dim=c(225,1),0)

for(i in 1:225)
{
	#generate our random numbers
	b = sample(1:8,22500,replace=T)
	innerCount = 0
	for (j in 1:22500)
	{
		innerCount = innerCount + b[j]
	}
	meanArrayTwo[i] = innerCount / 22500
}

#Count the number of means that are between 0.2 and 0.0 
count = 0
sumOfMeans = 0

for (k in 1:225)
{
	if (meanArrayTwo[k] >= 3.8 && meanArrayTwo[k] <= 4.0)
	{
		count = count + 1
	}
	sumOfMeans = sumOfMeans + meanArrayTwo[k]
}

#Question 2A, Finding simulated propability
questionTwoProb = count / 225

#Question 2B, Finding mean of means
questionTwoMean = sumOfMeans / 225


sumOfVariances = 0
for (l in 1:225)
{
	sumOfVariances = sumOfVariances + ((meanArrayTwo[l] - questionTwoMean)^2)
}

#Question 2C, Finding standard deviation
questionTwoStdDev = sqrt((1/224) * sumOfVariances)


#Print propability that meanArray is between 3.8 and 4.0 or P(3.8 <= X <= 4.0)
print("(Question 2A) Find the simulated probability that the mean is between 3.8 and 4.0 inclusive: ")
print(questionTwoProb)

#Print mean of the mean's
print("(Question 2B) Mean of sample mean's: ")
print(questionTwoMean)


#print standard deviation of mean's
print("(Question 2C) Standard Deviation of mean's: ")
print(questionTwoStdDev)



#Open new screen
dev.new()

#Question 2D, Draw the histogram of the mean's
hist(meanArrayTwo, col = "red", main = "Question 2D, Histogram mean sampling {1,2,...,8}")


