library(ggplot2)
library(plyr)
library(lubridate)
library(scales)
library(ggthemes)
library(ggrepel)

setwd("~/Projects/Metacritic")

### Data Prep ###

metacritic <- read.csv('reddit_metacritic.csv', na.strings=c("","NA"))

metacritic$User.Score <- as.numeric(as.character(metacritic$User.Score))
#metacritic$Release.Date <- as.Date(as.character(metacritic$Release.Date), format="%b %d, %Y")
metacritic$Release.Date <- as.Date(metacritic$Release.Date)
metacritic$Allscore <- (metacritic$Meta.Score + 10*metacritic$User.Score)/2
metacritic$Diff <- metacritic$Meta.Score - 10*metacritic$User.Score
metacritic$year <- format(metacritic$Release.Date, "%Y")

# Pastel
base <- "#42647f"
accent <- "#f0645b"

# Very light base, heavy accent
base <- "#bcd2d0"
accent <- "#f96161"

# Discord
base <- '#2c2f33'
accent <- '#7289da'

# Light grey and base accent
base <- '#848484'
accent <- '#336699'

### End Data Prep ###

### Meta Score Distribution ###

c <- count(metacritic$Meta.Score)
c$highlight <- ifelse(c$x == round(mean(metacritic$Meta.Score, na.rm = T)), c$freq, 0)

ggplot(c, aes(x = c$x, y = c$freq)) +
  ggtitle('Meta Score Distribution') + 
  geom_bar(stat = 'identity', width = 0.8, fill = base) +
  geom_bar(stat = 'identity', mapping = aes(y = highlight), fill = accent) +
  scale_x_continuous(limits = c(0, 100)) +
  theme_fivethirtyeight()
### End Meta Score Distribution ###

### User Score Distribution ###

c <- count(metacritic$User.Score*10)
c$highlight <- ifelse(c$x == round(mean(metacritic$User.Score*10, na.rm = T)), c$freq, 0)

# grey <- "#565656"
# accent <- "#ef5349"

ggplot(c, aes(x = c$x, y = c$freq)) +
  ggtitle('User Score Distribution') + 
  geom_bar(stat = 'identity', width = 0.8, fill = base) +
  geom_bar(stat = 'identity', mapping = aes(y = highlight), fill = accent) +
  scale_x_continuous(limits = c(0, 100)) +
  theme_fivethirtyeight()

### End User Score Distribution ###

### Begin User Meta Comparison ###

c <- subset(subset(metacritic, Release.Date > as.Date("2011-01-01")),
            (Meta.Score > 89 | User.Score*10 > 88))

ggplot(c, aes(x = User.Score*10, y = Meta.Score)) +
  labs(title = 'Modern games by user and critic scores',
       subtitle = 'Modern: post 2010   |   X-axis: User Score   |   Y-axis: Meta Score') +
  geom_point(color = accent) +
  geom_text_repel(aes(label=Title), size=3, color = base) +
  scale_x_continuous(limits = c(70, 95)) +
  scale_y_continuous(limits = c(70, 95)) +
  theme_fivethirtyeight()

### End User Meta Comparison ###

### Begin Release Date ###

# ggplot(subset(metacritic, !is.na(year)), aes(x = year)) +
#   ggtitle('Yearly amount of games reviewed') +
#   geom_bar(fill = '#42647f') +
#   theme_fivethirtyeight()

c <- subset(count(metacritic, vars = 'year'), !is.na(year))
# colors <- rep('#42647f', nrow(c))
# colors[length(colors)] <- '#ef5349'

ggplot(c, aes(x = year, y = freq, label = freq)) +
  ggtitle('Yearly amount of games reviewed') +
  geom_bar(stat = 'identity', fill = base) +
  geom_text(size = 3, color = accent, aes(y = 10),
            fontface = "bold") +
  theme_fivethirtyeight()


### End Release Date ###


### Begin Top Games ###

top_2010 <- subset(metacritic, 
                  Release.Date > as.Date("2011-01-01")
                  & Allscore > 87)

ggplot(top_2010, aes(x = reorder(Title, Allscore), y = Allscore, label = Allscore)) +
  labs(title = 'Top 20 modern games by weighted score',
       subtitle = 'Modern: post 2010   |   Weighted: average of Critic and User score') +
  geom_segment( aes(x=reorder(Title, Allscore), 
                    xend=reorder(Title, Allscore), 
                    y=75, 
                    yend=Allscore), 
                color = base,
                size = 2) +
  geom_point(color = accent, size=4) +
  #geom_bar(stat = 'identity', fill = base) +
  coord_flip() +
  #geom_text(size = 3, color = accent,
  #          hjust = 'left', aes(y = 3), fontface = "bold") +
  theme_fivethirtyeight()

### End Top Games

### Begin Controversial Games ###

# controversial_big <- subset(metacritic, AmountCritics > mean(metacritic$AmountCritics) &
#                           AmountUsers > mean(metacritic$AmountUsers, na.rm = T) &
#                           Diff > 30)
# 
# ggplot(controversial_big, aes(x = reorder(Title, Diff), y = Diff)) +
#   labs(title = 'Difference between good critic and bad user score',
#        subtitle = 'On a scale of 100') +
#   geom_bar(stat = 'identity', fill = '#42647f') + 
#   coord_flip() +
#   theme_fivethirtyeight()

ggplot(head(arrange(metacritic, -Diff), 20), aes(x = reorder(Title, Diff), y = Diff, label = Diff)) +
  labs(title = 'Difference between good critic and bad user score',
       subtitle = 'On a scale of 100') +
  #geom_bar(stat = 'identity', fill = base) +
  geom_segment( aes(x=reorder(Title, Diff), 
                    xend=reorder(Title, Diff), 
                    y=0, 
                    yend=Diff), 
                color = base,
                size = 2) +
  geom_point(color = accent, size=4) +
  coord_flip() +
  # geom_text(size = 3, color = accent,
  #           hjust = 'left', aes(y = 3), fontface = "bold") +
  theme_fivethirtyeight()

ggplot(head(arrange(metacritic, Diff), 20), aes(x = reorder(Title, Diff), y = Diff, label = Diff)) +
  labs(title = 'Difference between bad critic and good user score',
       subtitle = 'On a scale of 100') +
  #geom_bar(stat = 'identity', fill = base) + 
  coord_flip() +
  geom_segment( aes(x=reorder(Title, Diff), 
                    xend=reorder(Title, Diff), 
                    y=0, 
                    yend=Diff), 
                color = base,
                size = 2) +
  geom_point(color = accent, size=4) +
  # geom_text(size = 3, color = accent,
  #           hjust = 'left', aes(y = -3), fontface = "bold") +
  theme_fivethirtyeight()

### End Controversial Games ###

### Begin Developers ###

metacritic$Developer <- gsub(',.*', '', metacritic$Developer)
metacritic$Developer <- gsub('EA.*', 'EA', metacritic$Developer)
metacritic$Developer <- gsub('1C.*', '1C', metacritic$Developer)
metacritic$Developer <- gsub('Electronic Arts', 'EA', metacritic$Developer)
metacritic$Developer <- gsub('Ubisoft.*', 'Ubisoft', metacritic$Developer)
metacritic$Developer <- gsub('Rockstar.*', 'Rockstar', metacritic$Developer)

ss <- metacritic[ metacritic$Developer %in%  names(table(metacritic$Developer))[table(metacritic$Developer) > 5] , ]
ss <- ss[!is.na(ss$Allscore),]
a <- arrange(aggregate(ss[c('Allscore')], list(ss$Developer), mean, na.action = na.pass), -Allscore)

ggplot(head(a, 20), aes(x = reorder(Group.1, Allscore), y = Allscore, label = round(Allscore,1))) +
  labs(title = 'Sizable developers by weighted score',
       subtitle = 'Sizeable: more than 5 games   |   Weighted: average of Critic and User score') +
  #geom_bar(stat = 'identity', fill = base) + 
  geom_segment( aes(x=reorder(Group.1, Allscore), 
                    xend=reorder(Group.1, Allscore), 
                    y=75, 
                    yend=Allscore), 
                color = base,
                size = 2) +
  geom_point(color = accent, size=4) +
  coord_flip() +
  # geom_text(size = 3, color = accent,
  #           hjust = 'left', aes(y = 3), fontface = 'bold') +
  theme_fivethirtyeight()

ss <- metacritic[ metacritic$Developer %in%  names(table(metacritic$Developer))[table(metacritic$Developer) > 10] , ]
ss <- ss[!is.na(ss$Allscore),]
contdev <- aggregate(ss[c('Diff')], list(ss$Developer), mean, na.action = na.pass)
contdev$colour <- ifelse(contdev$Diff < 0, "negative","positive")

ggplot(contdev, aes(x = reorder(Group.1, Diff), y = Diff)) +
  labs(title = 'Developers (10+ games) by difference in critic and user score',
       subtitle = 'Positive: Critic > User   |   Negative: Critic < User   |   On scale 100') +
  geom_bar(stat = 'identity', aes(fill = colour)) +
  scale_fill_manual(values=c(positive=base,negative=accent)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(legend.position = 'none')


# Correllate amount of games made with average score?

### End Developers ###
