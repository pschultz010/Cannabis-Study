library('XML')
library('rvest')
library(stringr)

master_links <- c()
site <- "https://www.leafly.com/strains/"
sess <- html_session(site)
is.session(sess)

# get number of pages
page_amt <- html_nodes(sess, xpath='//*[@id="__next"]/div[2]/div[4]/span') %>% 
  html_text() %>% str_extract("\\d*$") %>% as.numeric()

# get links
for (i in 1:page_amt) {
  
  links <- html_nodes(sess, xpath = '//*[@id="__next"]//a[@class="strain-tile justify-start relative"]') %>%
    html_attr("href") %>% str_extract("[\\w-]*$")
  master_links <- c(master_links, links)

  sess <- sess %>% follow_link("Next")
}

# rid duplicates and backup
master_links <- unique(master_links)
#save(master_links, file="master_link.RData")
#master_links <- read(file="master_link.RData")

# get all feelings, helps with, and negatives for each strain
feelings <- c()
helps_with <- c()
negatives <- c()
for (i in 1:length(master_links)) {

  # load strain page
  pagehtml <- read_html(paste0(site,master_links[i]))

  for (j in 1:5) {
    # feeling
    feel <- pagehtml %>% html_node(xpath=sprintf('//*[@id="__next"]//div[@class="react-tabs__tab-panel-container mt-md"]/div[1]/div[%d]/div[1]', j)) %>%
      html_text() %>% str_extract("[^\\d%]{1,}") %>% trimws("right")
    feelings <- unique(c(feelings, feel))
    # helps with
    helps <- pagehtml %>% html_node(xpath=sprintf('//*[@id="__next"]//div[@class="react-tabs__tab-panel-container mt-md"]/div[2]/div[%d]/div[1]', j)) %>%
      html_text() %>% str_extract("[^\\d%]{1,}") %>% trimws("right")
    helps_with <- unique(c(helps_with, helps))
    # negatives
    neg <- pagehtml %>% html_node(xpath=sprintf('//*[@id="__next"]//div[@class="react-tabs__tab-panel-container mt-md"]/div[3]/div[%d]/div[1]', j)) %>%
      html_text() %>% str_extract("[^\\d%]{1,}") %>% trimws("right")
    negatives <- unique(c(negatives, neg))
  }
  print(i)
}

# automation 
strains <- data.frame()
for (i in 1:length(master_links)) {
  print(i)
  # load strain page
  pagehtml <- read_html(paste0(site,master_links[i]))
  # name
  name <- pagehtml %>% html_node("h1") %>% html_text()
  #type
  type <- pagehtml %>% html_node(xpath='//*[@id="__next"]//a[contains(@href, "strains/lists/category")]') %>% 
    html_text()
  # rating
  rating <- pagehtml %>% html_node(xpath='//*[@id="__next"]//p[contains(@class,"font-bold")]/span') %>% 
    html_text() %>% str_trim()
  # reviews
  reviews <- pagehtml %>% html_node(xpath='//*[@id="__next"]//a[contains(@href, "reviews") and @class="text-deep-green"]') %>%
    html_text() %>% str_extract("\\d*")
  # thc
  thc <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[@class="font-body"]') %>% 
    html_text() %>% str_extract("\\d*")
  # growth difficulty
  growth_difficulty <- pagehtml %>% 
    html_node(xpath = '//*[@id="__next"]//div[text()="Difficulty"]//following::div[1]/div[contains(@class, "deep-green")]') %>%
    html_text()
  # plant height
  plant_height <- pagehtml %>% 
    html_node(xpath = '//*[@id="__next"]//div[text()="Height (inches)"]//following::div[1]/div[contains(@class, "deep-green")]') %>%
    html_text()
  # yield (oz/ft)^2
  yield_oz_per_ft_sqrd <- pagehtml %>% 
    html_node(xpath = '//*[@id="__next"]//div[contains(text(), "Yield (oz/ft)")]//following::div[1]/div[contains(@class, "deep-green")]') %>%
    html_text()
  # flowering (weeks)
  flowering_weeks <- pagehtml %>% 
    html_node(xpath = '//*[@id="__next"]//div[contains(text(), "Flowering (weeks)")]//following::div[1]/div[contains(@class, "deep-green")]') %>%
    html_text()
  # energy percentage
  energy <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[contains(@class, "calm-energize")]') %>% 
    html_attr('style') %>% str_extract("\\d+\\.\\d+") %>% as.numeric() %>% round(2)
  # parent 1
  parent1 <- pagehtml %>% 
    html_node(xpath='(//*[@id="__next"]//div[text()="parent"])[1]//preceding::div[1]') %>% html_text()
  # parent 2
  parent2 <- pagehtml %>% 
    html_node(xpath='(//*[@id="__next"]//div[text()="parent"])[2]//preceding::div[1]') %>% html_text()
  
  # FEELINGS
  # happy
  happy <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Happy"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # euphoric
  euphoric <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Euphoric"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # relaxed
  relaxed <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Relaxed"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # uplifted
  uplifted <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Uplifted"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # creative
  creative <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Creative"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # energetic
  energetic <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Energetic"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # hungry
  hungry <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Hungry"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # sleepy
  sleepy <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Sleepy"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # focused
  focused <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Focused"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # tingly
  tingly <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Tingly"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # giggly
  giggly <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Giggly"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # aroused
  aroused <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Aroused"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # talkative
  talkative <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Talkative"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  
  # HELPS WITH
  # stress
  stress <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Stress"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # anxiety
  anxiety <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Anxiety"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # depression
  depression <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Depression"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # pain
  pain <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Pain"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # insomnia
  insomnia <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Insomnia"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # fatigue
  fatigue <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Fatigue"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # lack of appetite
  lack_of_appetite <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Lack of appetite"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # headaches
  headaches <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Headaches"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # arthritis
  arthritis <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Arthritis"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # inflammation
  inflammation <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Inflammation"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # PTSD
  PTSD <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="PTSD"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # ADD/ADHD
  ADD_ADHD <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="ADD/ADHD"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # bipolar disorder
  bipolar_disorder <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Bipolar disorder"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # nausea
  nausea <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Nausea"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  #asthma
  asthma <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Asthma"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # muscle spasms
  muscle_spasms <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Muscle spasms"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # migraines
  migraines <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Migraines"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # cramps
  cramps <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Cramps"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # eye pressure
  eye_pressure <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Eye pressure"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # crohn's disease
  crohns_disease <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Crohn\'s disease"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # gastrointestinal disorder
  gastrointestinal_disorder <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Gastrointestinal disorder"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # cancer
  cancer <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Cancer"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # fibromyalgia
  fibromyalgia <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Fibromyalgia"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # epilepsy
  epilepsy <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Epilepsy"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # anorexia
  anorexia <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Anorexia"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # spasticity
  spasticity <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Spasticity"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  
  # NEGATIVES
  # dry mouth
  dry_mouth <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Dry mouth"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # dry eyes
  dry_eyes <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Dry eyes"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # paranoid
  paranoid <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Paranoid"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # dizzy
  dizzy <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Dizzy"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # anxious
  anxious <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Anxious"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  # headache
  headache <- pagehtml %>% html_node(xpath='//*[@id="__next"]//div[text()="Headache"]/span') %>% 
    html_text() %>% str_extract("\\d*")
  
  # build record
  strain_row <- cbind(name, type, rating, reviews, thc, growth_difficulty, plant_height, yield_oz_per_ft_sqrd,
                      flowering_weeks, energy, parent1, parent2, happy, euphoric, relaxed, uplifted,
                      creative, energetic, hungry, sleepy, focused, tingly, giggly, aroused, talkative, stress, anxiety,
                      depression, pain, insomnia, fatigue, lack_of_appetite, headaches, arthritis, inflammation, PTSD, ADD_ADHD,
                      bipolar_disorder, nausea, asthma, muscle_spasms, migraines, cramps, eye_pressure, crohns_disease, gastrointestinal_disorder,
                      cancer, fibromyalgia, epilepsy, anorexia, spasticity, dry_mouth, dry_eyes, paranoid, dizzy, anxious, headache)
  # add to data frame
  strains <- rbind(strains, strain_row)
}

strains$name <- as.character(strains$name)
strains$type <- as.character(strains$type)
strains$rating <- as.numeric(as.character(strains$rating))
strains$reviews <- as.numeric(as.character(strains$reviews))
strains$thc <- as.numeric(as.character(strains$thc))
strains$growth_difficulty <- as.character(strains$growth_difficulty)
strains$plant_height <- as.character(strains$plant_height)
strains$yield_oz_per_ft_sqrd <- as.character(strains$yield_oz_per_ft_sqrd)
strains$flowering_weeks <- as.character(strains$flowering_weeks)
strains$energy <- as.numeric(as.character(strains$energy))
strains$parent1 <- as.character(strains$parent1)
strains$parent2 <- as.character(strains$parent2)
strains$happy <- as.numeric(as.character(strains$happy))
strains$euphoric <- as.numeric(as.character(strains$euphoric))
strains$relaxed <- as.numeric(as.character(strains$relaxed))
strains$uplifted <- as.numeric(as.character(strains$uplifted))
strains$creative <- as.numeric(as.character(strains$creative))
strains$energetic <- as.numeric(as.character(strains$energetic))
strains$hungry <- as.numeric(as.character(strains$hungry))
strains$sleepy <- as.numeric(as.character(strains$sleepy))
strains$focused <- as.numeric(as.character(strains$focused))
strains$tingly <- as.numeric(as.character(strains$tingly))
strains$giggly <- as.numeric(as.character(strains$giggly))
strains$aroused <- as.numeric(as.character(strains$aroused))
strains$talkative <- as.numeric(as.character(strains$talkative))
strains$stress <- as.numeric(as.character(strains$stress))
strains$anxiety <- as.numeric(as.character(strains$anxiety))
strains$depression <- as.numeric(as.character(strains$depression))
strains$pain <- as.numeric(as.character(strains$pain))
strains$insomnia <- as.numeric(as.character(strains$insomnia))
strains$fatigue <- as.numeric(as.character(strains$fatigue))
strains$lack_of_appetite <- as.numeric(as.character(strains$lack_of_appetite))
strains$headaches <- as.numeric(as.character(strains$headaches))
strains$arthritis <- as.numeric(as.character(strains$arthritis))
strains$inflammation <- as.numeric(as.character(strains$inflammation))
strains$PTSD <- as.numeric(as.character(strains$PTSD))
strains$ADD_ADHD <- as.numeric(as.character(strains$ADD_ADHD))
strains$bipolar_disorder <- as.numeric(as.character(strains$bipolar_disorder))
strains$nausea <- as.numeric(as.character(strains$nausea))
strains$asthma <- as.numeric(as.character(strains$asthma))
strains$muscle_spasms <- as.numeric(as.character(strains$muscle_spasms))
strains$migraines <- as.numeric(as.character(strains$migraines))
strains$cramps <- as.numeric(as.character(strains$cramps))
strains$eye_pressure <- as.numeric(as.character(strains$eye_pressure))
strains$crohns_disease <- as.numeric(as.character(strains$crohns_disease))
strains$gastrointestinal_disorder <- as.numeric(as.character(strains$gastrointestinal_disorder))
strains$cancer <- as.numeric(as.character(strains$cancer))
strains$fibromyalgia <- as.numeric(as.character(strains$fibromyalgia))
strains$epilepsy <- as.numeric(as.character(strains$epilepsy))
strains$anorexia <- as.numeric(as.character(strains$anorexia))
strains$spasticity <- as.numeric(as.character(strains$spasticity))
strains$dry_mouth <- as.numeric(as.character(strains$dry_mouth))
strains$dry_eyes <- as.numeric(as.character(strains$dry_eyes))
strains$paranoid <- as.numeric(as.character(strains$paranoid))
strains$dizzy <- as.numeric(as.character(strains$dizzy))
strains$anxious <- as.numeric(as.character(strains$anxious))
strains$headache <- as.numeric(as.character(strains$headache))

write.csv(strains, "strains.csv")
