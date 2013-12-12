#Frank Dye
#12/3/2013

#Question 1

EXPERIMENT_RUNS = 10000


NCount = 0
MCount = 0

i = 0
#We want 10,000 experiments of N
while (i < EXPERIMENT_RUNS)
{
	sum = 0
	#Sentinel value to stop this run
	gameEnds = FALSE
	while (gameEnds == FALSE)
	{
		a = runif(1, 0.0, 1.0)
		sum = sum + a
		
		MCount = MCount + 1
		if (sum > 1)
		{
			gameEnds = TRUE
		}
	}
	i = i + 1
}

expectedM = MCount / EXPERIMENT_RUNS

print("Question 1A) M = min(n: x1+x2,...,xn  > 1)")
print("Question 1A) E(M): ")
print(expectedM)


#Question 1B)



i = 0
NCount = 0
#We want 10,000 series of N
while (i < EXPERIMENT_RUNS)
{
	N = 1
	num1 = runif(1, 0.0, 1.0)
	num2 = runif(1, 0.0, 1.0)
	gameEnds = FALSE
	while (gameEnds == FALSE)
	{	
		N = N + 1
		if (num1 > num2)
		{
			gameEnds = TRUE
		}
		num1 = num2
		num2 = runif(1, 0.0, 1.0)
	}
	N = N + 1
	NCount = NCount + min(N)
	i = i + 1
}

expectedN = NCount / EXPERIMENT_RUNS


print("Question 1B) N = min(n+1: x1>x2)")
print("Question 1B) E(N): ")
print(expectedN)

#Question 2

#initialize some counter arrays
winArray = array(dim=c(EXPERIMENT_RUNS,1),0)
tossArray = array(dim=c(EXPERIMENT_RUNS,1),0)

for(i in 1:EXPERIMENT_RUNS)
{
	playingGame = TRUE
	
	#initialize an array of possible sum of dice, when one is rolled we set
	#the entry from 0 to 1 to indicate we have come across this sum before
	
	sumMemory = array(dim=c(12,1),0)
	#0 can't be sum so it is a sentinel indicator that this is our first toss
	
	while (playingGame == TRUE)
	{
		#Toss our dice
		die1 = sample(1:6,1,replace=T)
		die2 = sample(1:6,1,replace=T)
		
		tossArray[i] = tossArray[i] + 1
		
		sum = die1+die2

		if (die1 == die2)
		{
			#We lost so just exit
			playingGame = FALSE
		}
		else if (sumMemory[sum] == 1)
		{
			#We won so increment the counter and exit
			winArray[i] = 1
			playingGame = FALSE
		}
		else if (sumMemory[sum] == 0)
		{
			#First time running this sum
			sumMemory[sum] = 1
		}
	}
				
}

#Calculating sum of total number of wins and total tosses in our experiment
#Question 2A and 2B

winCount = 0
tossCount = 0
for (i in 1:EXPERIMENT_RUNS)
{
	winCount = winCount + winArray[i]
}

for (i in 1:EXPERIMENT_RUNS)
{
	tossCount = tossCount + tossArray[i]
}

propWinning = winCount / EXPERIMENT_RUNS
avgToss = tossCount / EXPERIMENT_RUNS

print("Question 2A) Propability of winning: ")
print(propWinning)

print("Question 2B) Average number of tosses per game: ")
print(avgToss)


