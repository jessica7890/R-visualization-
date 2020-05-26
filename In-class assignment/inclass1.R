help(Orange)
str(Orange)
# tree:Ordinal(ordered vector)
# age:numerical
# circumference:numerical
ggplot() +
  geom_point(aes(x=age,y=circumference),data=Orange)+
  labs(x="age", y="circumference",title="relationship between age and circumference")
  
