require(xlsx)
require(tidyverse)
s1 = read.xlsx("Connaissance.xlsx",sheetIndex=1)

conn = s1 %>% gather(discipline,niveau,1:15) %>% group_by(discipline,niveau) %>% summarize(comptage = n()) %>% na.omit %>% filter(niveau !=0)
conn$niveau = as.factor(conn$niveau)
conn$discipline = as.factor(conn$discipline)
spconn = conn %>% spread(niveau,comptage)
colnames(spconn) = c("discipline","bon","vague","inconnu")
conn = spconn %>% arrange(inconnu) %>%  ungroup %>% mutate(discipline = factor(discipline,discipline)) %>%    gather(niveau,comptage,2:4)
conn = conn %>% mutate(niveau = factor(niveau,c("bon","vague","inconnu")))

levels(conn$niveau)


ggplot(conn) + geom_bar(stat = "identity",aes(x=discipline, y=comptage,fill=niveau)) +  theme_minimal() + theme(axis.text.x = element_text(angle=45,hjust=1)) + scale_fill_brewer(name="Niveau de connaissance") + ggtitle("Autoevalution du niveau de connaissance sur les disciplines")
ggsave("Connaissance.png",dpi=300,height=10,width=20,units="cm")


s2 = read.xlsx("utilisation.xlsx",sheetIndex=1)

ut = s2 %>% gather(discipline,utilisation,1:15) %>% group_by(discipline,utilisation) %>% summarize(comptage = n()) %>% na.omit %>% filter(utilisation !=0)

ut$utilisation = as.factor(ut$utilisation)
ut$discipline = as.factor(ut$discipline)
sput = ut %>% spread(utilisation,comptage)
colnames(sput) = c("discipline","actuel/passé","futur","actuel/passé/futur","non")
ut = sput %>% arrange(non) %>%  ungroup %>% mutate(discipline = factor(discipline,discipline)) %>%    gather(utilisation,comptage,2:5)
ut = ut %>% mutate(utilisation = factor(utilisation, c("discipline","actuel/passé","futur","actuel/passé/futur","non")))

levels(ut$utilisation)


ggplot(ut %>% filter(utilisation !="non")) + geom_bar(stat = "identity",aes(x=discipline, y=comptage,fill=utilisation)) +  theme_minimal() + theme(axis.text.x = element_text(angle=45,hjust=1)) + scale_fill_brewer(type="qual",name="") + ggtitle("Utilisation des approches dans des projets ")
ggsave("Utilisation.png",dpi=300,height=10,width=20,units="cm")


comb = conn %>% left_join(ut,by="discipline")     



