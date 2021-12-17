# 3) A)
# Unique ID : E7321008
library(tidyverse)
mpg
?mpg
data=mpg
#Displaying the information about the dataset
str(data)

#scatterplot

# 3) B)
ggplot(mpg,aes(x=hwy,y=cyl,col=manufacturer,shape=manufacturer))+
  geom_point()

# 3) C)
ggplot(mpg, aes(x = class, y = drv)) +
  geom_count()

# 3) D)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()+
  facet_grid(drv~cty)

ggplot(mpg) +
  geom_point(mapping=aes(x=displ,y=hwy),colour="red",size=3,shape=21)+
  facet_wrap(~manufacturer,nrow=2)

# 3) E) Regression line 
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(color="green") +
  geom_smooth(method="lm",se=TRUE,color="yellow") +
  labs(title="Regression line",y="hwy", x = "displ")


# 3) F1)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = FALSE)

# 3) F2)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(mapping = aes(group = drv), se = FALSE)

# 3) F3)
ggplot(mpg, aes(x = displ, y = hwy, colour = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

# 3) F4)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour=drv)) +
  geom_smooth(se = FALSE)

# 3) F5)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour=drv)) +
  geom_smooth(aes(linetype=drv),se = FALSE)

# 3) F6)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(colour='white',size=3) +
  geom_point(aes(colour=drv))
