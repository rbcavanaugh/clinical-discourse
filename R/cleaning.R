# 
# gdc_picnic <- read.csv(here('data', 'gdc_picnic_allwords.csv'))[,1:8] %>%
#   mutate_all(tolower) %>%
#   pivot_longer(2:8, names_to = 'age', values_to = 'lexeme') %>%
#   mutate(age = gsub('X','', age)) %>%
#   arrange(story, age)
# 
# write.csv(gdc_picnic, here('data', 'gdc_picnic_cleaned.csv'))
# 
# 
# gdc_picnic_pos <- read.csv(here('data', 'gdc_picnic.csv'))[,1:6] %>%
#   pivot_longer(cols = Nouns:Adverbs, names_to = 'pos', values_to = 'lexeme') %>%
#   arrange(story, age, pos, lexeme)
# 
# write.csv(gdc_picnic_pos, here('data', 'gdc_picnic_pos_cleaned.csv'))

