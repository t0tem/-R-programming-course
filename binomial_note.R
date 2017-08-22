
# from here https://fr.mathworks.com/help/stats/binopdf.html
# A Quality Assurance inspector tests 200 circuit boards a day. 
# If 2% of the boards have defects, what is the probability 
# that the inspector will find no defective boards on any given day?

dbinom(0,200,0.02) #0.0176

# What is the most likely number of defective boards the inspector will find?

x <- 0:200
d <- dbinom(x, 200, 0.02)
max <- max(dbinom(x, 200, 0.02))
x[which(d == max)]

plot(dbinom(x,200,0.02))