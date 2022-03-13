library(ggplot2)
ggplot(cars)+
  aes(speed,dist)+
  geom_point()+
  labs(title="Speed and Stopping Distances of Cars",
       x="Speed (MPH)", 
       y="Stopping Distance (ft)",
       subtitle = "Your informative subtitle text here",
       caption="Dataset: 'cars'") +
  geom_smooth(method="lm", se=FALSE) +
  theme_bw()

url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)

nrow(genes)
colnames(genes)
ncol(genes)
table(genes$State)
round(table(genes$State)/nrow(genes)*100,2)
ggplot(genes)+
  aes(x=Condition1, y=Condition2)+
  geom_point()


p <- ggplot(genes) + 
  aes(x=Condition1, y=Condition2, col=State) +
  geom_point()
p

p + scale_colour_manual( values=c("blue","gray","red") )

p + scale_colour_manual(values=c("blue","gray","red")) +
  labs(title="Gene Expresion Changes Upon Drug Treatment",
       x="Control (no drug) ",
       y="Drug Treatment")


