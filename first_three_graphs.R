library(here)
library(ggplot2)
library(dplyr)
library(ggmosaic)
options(scipen=10000)

############################################ Graph #1

yr <- read.csv("data_viz_static/sqf_year_race_8_total.csv")
yr<- transform(yr, race_w_hisp.ord  = factor( 
yr$race_w_hisp, levels=c('B', 'H', 'W', 'O', 'A'), ordered =TRUE))
yr <- yr[order(-xtfrm(yr$race_w_hisp.ord)), ]
yr$pct <- yr$pct / 1000

p <- ggplot(yr, aes(x = year, y=pct, , fill = race_w_hisp.ord))
p + geom_area() + 
  scale_fill_discrete(guide=FALSE) + 
  xlab("Year") + ylab("Number of Stops (in Thousands") + scale_x_continuous(breaks=seq(2009,2017,1), expand = c(0.04,0.01)) +
  scale_y_continuous(position="right", expand=c(0,0)) +
  ggtitle(label = "Stop, Question, and Frisk in NYC", subtitle = "NYPD Stops by Race, 2009 - 2016") +
  labs(caption="Data Source: NYPD Stop, Question, and Frisk Database") +
  geom_vline(xintercept=2010.416, color="dark gray", linetype="solid") +
  geom_vline(xintercept=2012.5, colour="dark gray", linetype="solid") +
annotate("text", x = 2010.5, y = 500, label = "Black non-Hispanic", fontface=2, color="white") +
  annotate("text", x = 2010.5, y = 235, label = "Hispanic", color="white",fontface=2) +
  annotate("text", x = 2010.5, y = 70, label = "White non-Hispanic", color="white", fontface=2) +
  annotate("text", x = 2010.5, y = 40, label = "Other", color="white", fontface=2) +
  annotate("text", x = 2010.5, y = 15, label = "Asian", color="white", fontface=2) +
  geom_hline(yintercept=seq(0, 600, by=200), colour="white", linetype="dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust=0.5)) +
  geom_text(aes(x= 2015, y = 525, label="March against \n Stop-And-Frisk \n (June 2012)"), colour="black") +
  geom_text(aes(x=2014, y = 340, label="Village Voice publishes \n misconduct reports \n (May 2010)"), color="black") +
  annotate("segment", x = 2014, xend = 2012.5, y = 525, yend=525, size=1, alpha=0.1, color = "black",  arrow=arrow()) + 
  annotate("segment", x = 2012.8, xend = 2010.416, y = 350, yend=350, size=1, alpha=0.1, color = "black",  arrow=arrow())
  
############################################ Graph #2

wf <- read.csv('wf.csv')

ww <- ggplot(wf, aes(fill = forcats::fct_rev(weapon_found), y=num_stops, x=year)) 
ww + geom_bar(stat="identity") + xlab("Year") + ylab("Number of Stops (in Thousands)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(2009,2017,1), expand = c(0,0)) +
  
  scale_y_continuous(expand = c(0,0)) + theme_classic() +
  scale_fill_manual(values=c("#d60a29", "#c6c2c3"),
    name="Was a weapon found?",
                      breaks=c("Y", "N"),
                      labels=c("Yes", "No")) +
  labs(title="Weapons Found through NYC Stop & Frisk Stops", 
       subtitle="Knives, Pistols, Assault Weapons, Rifles, and Machine Guns\nfound during NYPD stops from 2009 - 2016") +
  labs(caption="Data Source: NYPD Stop, Question, and Frisk Database") 
  
############################################ Graph #3

frisked <- read.csv("Documents/Winter2019/data_viz/data/frisked.csv")

f <- ggplot(data = frisked)
f +
  geom_mosaic(aes(product(race_w_hisp), fill=frisked), na.rm=TRUE) +
  theme( legend.position="none",  plot.title = element_text(hjust = 0.5),
         plot.subtitle=element_text(hjust=0.5),
         axis.text.x = element_text(angle = 10,  face='bold'),
  panel.background = element_rect(fill = "white"), axis.ticks = element_line(color = "white")
) + 
  scale_fill_manual(values=c("slategray2", "navyblue"))+ ylab(' ') +
  xlab('') +
  labs(title="Is everyone who is stopped frisked?", 
       subtitle="All NYPD Stops 2009 - 2016 by Race/Ethnicity") +
  annotate("text", x = .26, y = .7, label = "58%", color="white", fontface=1.5) +
  annotate("text", x = .68, y = .7, label = "59%", color="white", fontface=1.5) +
  annotate("text", x = .92, y = .7, label = "47%", color="white", fontface=1.5) +
  labs(caption="Data Source: NYPD Stop, Question, and Frisk Database") 
