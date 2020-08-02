library(rmda)
library(rms)
data(dcaData)
base.model <- lrm(Cancer~Age + Female + Smokes,data = dcaData)

d <-dca(base.model)

# as default, lines were classified by linetype and color
ggplot(data = d)

# depress color
# classified by linetype
ggplot(data = d,color = FALSE)

# depress linetype
# classified by color
ggplot(data = d,linetype = FALSE)

# all line color is blue
# classified by linetype
ggplot(data = d,color='blue')

# colors of all and none lines are black and gray
# classified by linetype
ggplot(data = d,color=c('blue','black','gray'))

# all linetype is 2
# classified by color
ggplot(data = d,linetype = 2)


# all and none linetypes are 1, model linetype is 2
# classified by color
ggplot(data = d,linetype = c(2,1,1))

# all and none linetypes are 1, model linetype is 2
# all and none line colors are black and gray, model color is red
ggplot(data = d,
       color = c('red','black','gray'),
       linetype = c(2,1,1))




