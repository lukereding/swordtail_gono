library(tidyverse)
source("~/Documents/random_scripts/plotting_functions.R")

setwd("~/Desktop/swordtail_gono/")


###### to make the master dataframe
columns <- c("genus", "species", paste0("character_", 1:86))

df <- read_delim("matrix_redone", delim = " ", col_names = columns)

require(visdat)

vis_dat(df) + scale_fill_blues() +theme_mod() + rotate_labels() # lots of missing data


# make tidy:
df %<>%
  gather(character_1:character_86, key = "character", value = "value")


## get sane character names

col <- read_csv("character_names.csv")

col %<>%
  unite(character, character, number)


df <- full_join(df, col)

df %<>% unite(species, genus, species, sep = " ")

df %>% write_csv("df.csv")

## though this is the nicest form in some ways, we actually want the variables to be columns

df %>%
  select(-character, -type) %>%
  spread(key = name, value = value) %>%
  write_csv("df_wide.csv")

#################

# create some plots

## what are some of the behaviors measured?
# [1] *"arching_display"                       *"circling"                             
# [3] "close transverse presenting behavior"  "*dancing_display"                      
# [5] *"figure_eight"                          "gonopodial thrusting"                 
# [7] "nipping"                               *"open_display"                         
# [9] "*side_change_over_head"                 "slow_approach"                        
# [11] "sneaking"                              "*stiff_frontal_presenting"             
# [13] "*Well-developed precopulatory behavior"

## gono traits
# 1                                Distal serrae on ray 4p of the gonopodium.    28
# 2 Granular tissue on the dorsal part of the hook on ray 3 of the gonopodium    28
# 3          Size of segments of the distal ramus of ray 4a of the gonopodium    28
# 4                               Subdistal spines on ray 3 of the gonopodium    28
# 5                               ubdistal serrae on ray 4p of the gonopodium    28
# 6                                     Well-formedhookonray5aofthegonopodium    28
# 
### get a sense of correlations among gono traits:
df <- read_csv("df.csv")
gono <- df %>%
  filter(type == "gonopodium") %>%
  select(name) %>%
  unlist %>%
  unique
require(corrplot)
df <- read_csv("df_wide.csv")
df %>%
  select(one_of(gono)) %>%
  cor %>%
  corrplot(method = "ellipse", order = "hclust")
# overall, not super correlated


df <- read_csv("df_wide.csv")

require(ggrepel)

# chisq.test(matrix(c(2,12,3,1), ncol = 2)); p = 0.07
df %>% 
  group_by(`Well-developed precopulatory behavior`, `Distal serrae on ray 4p of the gonopodium.`) %>% 
  count %>%
  ggplot(aes(factor(`Well-developed precopulatory behavior`), n, fill = factor(`Distal serrae on ray 4p of the gonopodium.`))) +
  geom_bar(stat = "identity",position = "fill") +
  scale_fill_blues() +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_polar()
  
df %>% 
  group_by(`Well-developed precopulatory behavior`, `Well-formedhookonray5aofthegonopodium`) %>% 
  count %>%
  filter(`Well-developed precopulatory behavior` != "NA") %>%
  ggplot(aes(factor(`Well-developed precopulatory behavior`), n, fill = factor(`Well-formedhookonray5aofthegonopodium`))) +
  geom_bar(stat = "identity",position = "fill") +
  scale_fill_blues() +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_polar()

# not significant matrix(c(3, 12, 2, 1), ncol = 2)
df %>% 
  group_by(`Well-developed precopulatory behavior`, `Subdistal spines on ray 3 of the gonopodium`) %>% 
  count %>%
  filter(`Well-developed precopulatory behavior` != "NA") %>%
  ggplot(aes(factor(`Well-developed precopulatory behavior`), n, fill = factor(`Subdistal spines on ray 3 of the gonopodium`))) +
  geom_bar(stat = "identity",position = "fill") +
  scale_fill_blues() +
  scale_y_continuous(labels = scales::percent_format()) 


# not significant: chisq.test(matrix(c(5,10,0,3), ncol = 2))
df %>% 
  group_by(`Well-developed precopulatory behavior`, `Size of segments of the distal ramus of ray 4a of the gonopodium`) %>% 
  count %>%
  filter(`Well-developed precopulatory behavior` != "NA") %>%
  ggplot(aes(factor(`Well-developed precopulatory behavior`), n, fill = factor(`Size of segments of the distal ramus of ray 4a of the gonopodium`))) +
  geom_bar(stat = "identity",position = "fill") +
  scale_fill_blues(guide = F) +
  scale_y_continuous(labels = scales::percent_format()) 

# not significant
df %>% 
  group_by(sword, `Size of segments of the distal ramus of ray 4a of the gonopodium`) %>% 
  count %>%
  filter(sword != "NA") %>%
  ggplot(aes(factor(sword), n, fill = factor(`Size of segments of the distal ramus of ray 4a of the gonopodium`))) +
  geom_bar(stat = "identity",position = "fill") +
  scale_fill_blues() +
  scale_y_continuous(labels = scales::percent_format()) 


####################


df %>% 
  group_by(open_display, `Distal serrae on ray 4p of the gonopodium.`) %>% 
  count %>%
  ggplot(aes(factor(open_display), n, fill = factor(`Distal serrae on ray 4p of the gonopodium.`))) +
  geom_bar(stat = "identity",position = "fill") +
  scale_fill_blues() +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_polar()

# p = 0.1 matrix(c(9, 0, 6, 4), ncol = 2)
df %>% 
  group_by(open_display, `Well-formedhookonray5aofthegonopodium`) %>% 
  count %>%
  filter(open_display != "NA") %>%
  ggplot(aes(factor(open_display), n, fill = factor(`Well-formedhookonray5aofthegonopodium`))) +
  geom_bar(stat = "identity",position = "fill") +
  scale_fill_blues()



#############




df %>% 
  group_by(`Vertical bars`, `Size of segments of the distal ramus of ray 4a of the gonopodium`) %>% 
  count %>%
  filter(`Vertical bars` != "NA") %>%
  ggplot(aes(factor(`Vertical bars`), n, fill = factor(`Size of segments of the distal ramus of ray 4a of the gonopodium`))) +
  geom_bar(stat = "identity",position = "fill") +
  scale_fill_blues() +
  scale_y_continuous(labels = scales::percent_format()) 


df %>% 
  group_by(`Vertical bars`, `Size of segments of the distal ramus of ray 4a of the gonopodium`) %>% 
  count %>%
  filter(`Vertical bars` != "NA") %>%
  ggplot(aes(factor(`Vertical bars`), n, fill = factor(`Size of segments of the distal ramus of ray 4a of the gonopodium`))) +
  geom_bar(stat = "identity",position = "fill") +
  scale_fill_blues() +
  scale_y_continuous(labels = scales::percent_format()) 










# 
# 
# con <- df %>%
#   group_by(name, value) %>% 
#   count
# con %>%
#   ggplot(aes(x = name, y = n, fill = factor(value))) +
#   geom_bar(stat = "identity",position = "fill") +
#   scale_fill_blues() +
#   rotate_labels()


# 
# tab <- tibble::frame_data(~character_name, ~character,
#                           'Character 58',	'Claw presence vs absence',


# 'Character 59', 'Claw size described in relation to distal serrae of ray 4',
# 'Character 60', 'Hook shape, crescent versus sickle shap',
# 'Character 61', 'Ramus shape around the blad',
# 'Character 62', 'Shape of ray 4a, four categories: from totally straight to curved in shap',
# 'Character 63', 'Spine angle of ray ',
# 'Character 4', 'Distal serrae on ray 4',
# 'Character 5', 'Well-formed hook on ray 5',
# 'Character 6', 'Granular tissue on the dorsal part of the hook on ray ',
# 'Character 7', 'Subdistal spine on ray' ,
# 'Character 8', 'Size of segments of the distal ramus of ray 4',
# 'Character 9', 'Subdistal serrae on ray 4',
# 'Character 39', 'Black or darkly pigmented gonopodiu',
# 'Character 1', 'Sword',
# 'Character 2', 'Sword consisting exclusively of unbranched ray',
# 'Character 3', 'Upturned swor',
# 'Character 10', 'Head bum',
# 'Character 13', 'Elongated ventral caudal fin ray',
# 'Character 15', 'Growth rat',
# 'Character 16', 'Allometric growth of swor',
# 'Character 18', 'Dusky band continuous with dorsal pigment of swor',
# 'Character 19', 'Proximal dorsal pigmentation of the swor',
# 'Character 20', 'Distal dorsal sword pigmen',
# 'Character 21', 'Grave spo',
# 'Character 22', 'Ventral margin of caudal fin and sword densely edged by melanophore',
# 'Character 23', 'Yellow and orange carotenoid sword pigmentatio',
# 'Character 25', 'Drosopteri',
# 'Character 26', 'Sex-linked red and yellow patter',
# 'Character 30', 'Two or more rows of red lateral mark',
# 'Character 31', 'Multiple lateral stripe',
# 'Character 32', 'Solid mid-lateral stripe at birt',
# 'Character 33', 'Vertical bar',
# 'Character 34', 'Body bicolore',
# 'Character 35', 'Dark subdermal dashes of pigmen',
# 'Character 36', 'Two or more oblique lines behind pectoral bas',
# 'Character 37', 'Mid-dorsal spot',
# 'Character 38', 'Dorsal fin with dark marginal pigment and a sub-basal row of dark spots on the inter-radial membran',
# 'Character 40', 'Caudal blotc',
# 'Character 41', 'Spotted cauda',
# 'Character 42', 'Carbomaculatu',
# 'Character 43', 'Alleles at the tailspot locus')
