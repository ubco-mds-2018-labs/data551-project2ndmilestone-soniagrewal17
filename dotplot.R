# The Cleveland dotplot originally featured in William S. Clevelandâs book Elements of Graphing Data (1985). 
# It is similar to a bar chart but uses position rather than length to display values. 

library(stats)
#install.packages("stringi",type="win.binary"); install.packages("readr"); install.packages("Rcpp", dependencies=TRUE)
#install.packages("bindrcpp"); install.packages("crayon"); install.packages("DBI", dependencies=TRUE); install.packages("tidyr")
#install.packages("ggplot2"); install.packages("RCurl"); install.packages("lazyeval"); install.packages("tidyverse", dependencies=TRUE)
#install.packages("dplyr", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
#install.packages("reshape2")
                 
library(stringi);library(DBI);library(tidyverse);library(bindrcpp);library(Rcpp);library(dplyr);
library(ggplot2);library(tidyr);library(readr); library(crayon); library(reshape2)

crimes <- read.csv("crimedata.csv", header = TRUE, stringsAsFactors = FALSE)
attach(crimes)

cc<- count(crimes, category, sort = TRUE) %>%
  mutate(percent = round(n/sum(n)*100, 1))
cc

ccc<- ggplot(cc, aes(x = reorder(category, n), y = percent)) + 
  geom_bar(position="dodge",stat="identity", fill = "DarkBlue") +
  coord_flip() 
  ggtitle("Sorting crime categories")
topcat<-print(ccc + labs(y="percentage", x = "crime_categories"))
ggsave("ccc.png", topcat)

tc<- count(crimes, town, sort = TRUE)%>% mutate(percent = round(n/sum(n)*100, 1))
tc= tc[1:9,]

tcc<- ggplot(tc, aes(x = reorder(town, n), y = percent)) + 
  geom_bar(position="dodge",stat="identity", fill = "DarkBlue") +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) + 
  opts(panel.background = theme_blank()) + 
  opts(axis.title.x = theme_blank(), axis.title.y = theme_blank()) 
  coord_flip() 
ggtitle("Administrative units by crime count ")
toptown<-print(tcc + labs(y="percentage", x = "administrative units"))



# Frequency of Burglary by town (descending order)
bur <- crimes %>% 
  filter(category == "Burglary") %>% 
  group_by(town) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

head(bur)

b<- ggplot(bur, aes(x = reorder(town, n), y = n)) + 
  geom_point(size = 12, stat = "identity", color = "black") + 
  geom_text(aes(label = n, fontface = "bold"), color = "white", size = 2.5) + 
  coord_flip() + 
  theme_minimal(base_size = 11) + 
  xlab("") + ylab("") + 
  ggtitle("Burglary offences") +
  scale_y_continuous(limits=c(0,max(bur$n)))

b
# Frequency of Criminal damage and arson by town (descending order)
rob <- crimes %>% 
  filter(category == "Criminal damage and arson") %>% 
  group_by(town) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

head(rob)
r<- ggplot(rob, aes(x = reorder(town, n), y = n)) + 
  geom_point(size = 12, stat = "identity", color = "black") + 
  geom_text(aes(label = n, fontface = "bold"), color = "white", size = 2.5) + 
  coord_flip() + 
  theme_minimal(base_size = 11) + 
  xlab("") + ylab("") + 
  ggtitle("Criminal damage and arson") +
  scale_y_continuous(limits=c(0,max(rob$n)))
r
# Frequency of Violence and sexual offences by town (descending order)
vso <- crimes %>% 
  filter(category == "Violence and sexual offences") %>% 
  group_by(town) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

vso
v<-ggplot(vso, aes(x = reorder(town, n), y = n)) + 
  geom_point(size = 12, stat = "identity", color = "black") + 
  geom_text(aes(label = n, fontface = "bold"), color = "white", size = 2.5) + 
  coord_flip() + 
  theme_minimal(base_size = 11) + 
  xlab("") + ylab("") + 
  ggtitle("Violence and sexual offences") +
  scale_y_continuous(limits=c(0,max(vso$n)))

v
# Frequency of Public order by town (descending order)
po <- crimes %>% 
  filter(category == "Public order") %>% 
  group_by(town) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))

head(po)
d<- ggplot(po, aes(x = reorder(town, n), y = n)) + 
  geom_point(size = 12, stat = "identity", color = "black") + 
  geom_text(aes(label = n, fontface = "bold"), color = "white", size = 2.5) + 
  coord_flip() + 
  theme_minimal(base_size = 11) + 
  xlab("") + ylab("") + 
  ggtitle("Public order offences") +
  scale_y_continuous(limits=c(0,max(po$n)))
d


#install.packages("cowplot")
library(cowplot)
dotplot<- plot_grid(b,r,v,d)
dotplot
save_plot("offences.png", dotplot,
          base_aspect_ratio = 1.3 
)


