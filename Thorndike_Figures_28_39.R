## Make figures replicating Thorndike pp. 28 - 39

## CLEAR WORKSPACE ####
rm(list=ls())

## LOAD PACKAGES ####
library(tidyverse)

## READ IN DATA ####
# read data from your own computer or directly from Hoffman's GitHub
Table.2.1 <- read_csv(file = "Table.2.1_clean.csv") 

## Make Frequency Distribution (p. 29) ####
# Display the frequency of math scores on Console
table(Table.2.1$Math) 

# Produce the Frequency Distribution as an object
Table.2.2 <- cbind(Freq=table(Table.2.1$Math)) 

# Display Table.2.2 on Console vertically (Note: 31 levels)
Table.2.2 

## One way to fix the lack of "zeros" that are shown on p. 29
# Define 42 levels between 19 & 60
Table.2.2 <- factor(Table.2.1$Math, levels = c(60:19)) 

# Make Table.2.2 a vertically shown object
Table.2.2 <- cbind(Freq=table(Table.2.2)) 

# Make Table.2.2 a data frame (was a vector)
Table.2.2 <- data.frame(Table.2.2)

# Add a second column using the row names (60 - 19)
Table.2.2 <- Table.2.2 %>%
  mutate(Table.2.2, ScoreX = row.names(Table.2.2), .before = 1) 
Table.2.2 # Display on Console

## Grouped Frequency Distribution (p. 31)

# create a vector called "bins", counting by threes
bins <- seq(17, 62, by=3) 
# NOTE: Starting at 17 lines up with Thorndike

# Then create a vector called "Interval"
Interval <- cut(Table.2.1$Math, bins)
# The "cut()" command divides the range of Table.2.1$Math into intervals and codes the values in x according to which interval they fall.

table(Interval) # Produces a horizontal table

# transform() makes a vertical table like Table.2.3
Table.2.3 <- transform(table(Interval))

# Display on console
arrange(Table.2.3, desc(Interval)) 
# Note that arrange() and desc() reorder the table from high to low

## Cumulative Frequency Distribution (p. 35)
# Create data frame of Math scores
# Note: There are much more elegant ways to do this.
Table.2.4 <- data.frame(Table.2.3) %>% 
  mutate(Cumulative_Frequency = cumsum(Freq)) %>% 
  mutate(Cumulative_Percent = round(100*cumsum(Freq)/52))

## Display on console
arrange(Table.2.4, desc(Interval)) 

## Histograms
# Histogram of Math
math.hist = ggplot(Table.2.1, aes(x = Math, y = ..count..)) +
  geom_histogram(binwidth = 3, color = "black", fill = "grey") + 
  theme_classic() +
  labs(x = "Mathematics test scores",
       y = "Frequency") +
  scale_x_continuous(breaks = 20 + c(0:14)*3) +
  scale_y_continuous(breaks = 0 + c(0:2)*6) + 
  ggtitle("Figure 2-1\nHistogram of 52 mathematics scores")

math.hist # display the figure

## Cumulative Frequency Curve

# Again, this is one way of approaching it. Not super elegant though
MathByThrees <- data.frame(Table.2.4) %>%
  mutate(Threes = 20 + c(0:14)*3) 

# Figure 2-3
ggplot(MathByThrees, aes(x=Threes, y=Cumulative_Frequency)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(x = "Math Score",
       y = "Cumulative frequency") +
  ggtitle("Figure 2-3\nCumulative frequency curve") +
  scale_x_continuous(breaks = 18 + c(0:14)*3)

## Figure 2-4
ggplot(MathByThrees, aes(x=Threes, y=Cumulative_Percent)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(x = "Math Score",
       y = "Frequency") +
  scale_x_continuous(breaks = 20 + c(0:14)*3) +
  scale_y_continuous(breaks = 0 + c(0:5)*20) + 
  ggtitle("Figure 2-4\nCumulative frequency curve\nProduced by EXCEL")


## Plotting an empirical cumulative distribution function for Math
#Because really, why put them in bins when you want to look at how it all comes together? 
ggplot(Table.2.1) +
  stat_ecdf(aes(x=Math)) + 
  #Produce empirical cumulative density function
  scale_y_continuous(labels = scales::percent) + 
  #change from proportion to percentage
  theme_classic() +
  labs(x = "Math Score",
       y = "Cumulative percentage") +
  ggtitle("Cumulative frequency (step curve)") 

###########
# Supplemental Code beyond page 39 of Thorndike

## MAKE FIGURES ####

# Frequency Plot of Reading - bins of 5
read.hist <- hist(Table.2.1$Reading, # histogram
                  border="black",
                  prob = FALSE, # show frequencies; not densities
                  xlab = "Reading Score",
                  main = "Reading")

# Frequency Plot of Reading - bins of 1
read.hist <- hist(Table.2.1$Reading, # histogram
                  border="black",
                  prob = FALSE, # show frequencies; not densities
                  xlab = "Reading Score",
                  main = "Reading",
                  breaks = 25)

# Density Plot of Reading
read.hist <- hist(Table.2.1$Reading, # histogram
                  breaks = 20,
                  col="peachpuff", # column color
                  border="black",
                  prob = TRUE, # show densities instead of frequencies
                  xlab = "Reading Score",
                  main = "Reading")
lines(density(Table.2.1$Reading, bw=1.5), 
      # bw refers bandwidth of kernel smooth
      lwd = 2, # thickness of kernel smooth line
      col = "black") # color of line

### Using ggplot2
# Frequency Plot
read.hist2 <- ggplot(Table.2.1, aes(x = Reading)) + 
  # Use geom_histogram to make the figure a histogram
  geom_histogram(binwidth=1, color=1, fill="light blue") +
  # Add a title
  labs(title =  "Reading Scores") +
  labs(subtitle = "Frequency of Scores for 2 classes")

read.hist2 # display the figure for yourself

# Histogram with kernel density
read.hist3 <-ggplot(Table.2.1, aes(x = Reading)) + 
  geom_histogram(aes(y = ..density..), # plot histogram
                 binwidth = 1, color = "black", fill = "white") +
  geom_density(lwd = .5, color = "black", # add kernel smooth
               adjust = .7 , # adjust bandwidth (.7 means 70% of default kernel)
               fill = "blue", # 4 = light blue
               alpha = .4) + # how shaded (from zero to 1)
  labs(title = "Reading Scores for Two Classes") +
  labs(subtitle = "and overlayed density plot")

read.hist3 # display the figure

# Histogram with kernel density with counts
read.hist4 <-ggplot(Table.2.1, aes(x = Reading)) + 
  geom_histogram(aes(y = ..count..), # plot histogram with count
                 binwidth = 1, color = "black", fill = "white") +
  geom_density(aes(y = ..count..),
               lwd = .5, color = "black", # add kernel smooth
               adjust = .5 , # adjust bandwidth (.5 means 50% of default kernel)
               fill = "blue", # 4 = light blue
               alpha = .4) + # how shaded (from zero to 1)
  labs(title = "Reading Scores for Two Classes") +
  labs(subtitle = "and overlayed density plot")

read.hist4 # display the figure

# Frequency Plot of Math
math.freq = ggplot(Table.2.1, aes(x = Math)) +
  geom_histogram(binwidth = 1, color="black", fill="pink") +
  labs(title =  "Math Scores") +
  labs(subtitle = "Frequency of Scores for 2 classes")

math.freq # display the figure

# Histogram of Math
math.hist = ggplot(Table.2.1, aes(x = Math, y = ..count..)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  geom_density(lwd = .5, 
               color = "black", adjust =.7, fill = "pink",
               alpha = .5) + # how shaded (from zero to 1)
  labs(title = "Math Scores for Two Classes") +
  labs(subtitle = "and overlayed density plot")

math.hist # display the figure

# Frequency Plot of Spelling
spell.freq<-ggplot(Table.2.1, aes(x=Spelling)) +
  geom_histogram(binwidth=1, color="black", fill="green") +
  labs(title = "Frequency of Spelling Scores for 2 Classes")

spell.freq # display the figure

# Histogram of Spelling
spell.hist = ggplot(Table.2.1, aes(x = Spelling, y = ..count..)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  geom_density(lwd = .5, color = "black", adjust = .7, 
               fill = "green", alpha = .5) +
  labs(title = "Spelling Scores for Two Classes") +
  labs(subtitle = "and overlayed density plot") +
  theme_classic() 

spell.hist # display the figure


# Barplots
# A quick way to look at the number of boys and girls in each class
barplot(table(Table.2.1$Class, Table.2.1$Gender))


# Box and whiskers plots
# by Gender, if that makes sense in 2022
# Note, I chose to set the boxplots horizontally, with x axis similar to how I set up the histograms
ggplot(Table.2.1, aes(x = Reading, y = Gender)) + geom_boxplot()
ggplot(Table.2.1, aes(x = Math, y = Gender)) + geom_boxplot() # note the outlier
ggplot(Table.2.1, aes(x = Spelling, y = Gender)) + geom_boxplot() # perhaps the variability is larger for females?

# Density plot of Reading, by Gender

ggplot(Table.2.1, aes(x = Reading, color = Gender)) +
  geom_density()

# More details
read.density.gender = ggplot(Table.2.1, 
                             aes(x = Reading, color = Gender)) +
  geom_density(lwd = 1, adjust = .7) +
  labs(title = "Reading Score Density Plot") +
  labs(subtitle = "by Gender: 26 females; 26 males") +
  ylab(label=NULL) +  # remove the y-axis title 
  theme_classic() + # makes the background white
  theme(axis.text.y = element_blank()) + # remove the density scale markers
  theme(axis.ticks.y = element_blank()) + # remove the tick marks
  theme(axis.line.y = element_blank()) # remove the y

read.density.gender # display the figure

# Create a Spelling plot to highlight the variability by gender
# Note that we used a looser kernel estimator to show that the females have a slightly larger spread of scores. Males have a higher peak and shorter tail.
spell.density.gender = ggplot(Table.2.1, 
                              aes(x = Spelling, color = Gender)) +
  geom_density(lwd = 1, adjust = 1.5) + # larger adjust (150%)
  labs(title = "Spelling Score Density Plot") +
  labs(subtitle = "by Gender: 26 females; 26 males") +
  ylab(label=NULL) +  # remove the y-axis title 
  theme_classic() + # makes the background white
  theme(axis.text.y = element_blank()) + # remove the density scale markers
  theme(axis.ticks.y = element_blank()) + # remove the tick marks
  theme(axis.line.y = element_blank()) # remove the y

spell.density.gender # display the figure

