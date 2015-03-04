#Load scaped Hearthpwn data
Hearthpwn <- read.csv("hearthpwn.csv")
#Reformat hero, card, and multiplicity data
Hearthpwn$hero <- unlist(strsplit(as.character(Hearthpwn$hero)," Cards"))
Hearthpwn$card <- strsplit(as.character(Hearthpwn$card),"\n\n× ")
Hearthpwn$mult <- as.numeric(unlist(Hearthpwn$card)[seq(2,2*length(Hearthpwn$card),2)])
Hearthpwn$card <- unlist(Hearthpwn$card)[seq(1,2*length(Hearthpwn$card)-1,2)]
#Create counts of card usage unweighted and weighted by deck ratings
Count <- rep(0,length(unique(Hearthpwn$card)))
names(Count) <- unique(Hearthpwn$card)
Weight <- Count
for (i in 1:length(Hearthpwn$card))
{
	Count[Hearthpwn$card[i]] = Count[Hearthpwn$card[i]] + Hearthpwn$mult[i]
	Weight[Hearthpwn$card[i]] = Weight[Hearthpwn$card[i]] + Hearthpwn$mult[i] * log(Hearthpwn$rating[i])
}
Score <- Weight/Count
#Sort card usages from greatest to least
Count <- Count[order(-Count)]
Weight <- Weight[order(-Weight)]
Score <- Score[order(-Score)]
