#load packages----------------------------------
library(NLP)
library(gmodels)
library(tm)
library(plyr)
library(wordcloud)
library(SnowballC)
library(XML)
library(rvest)
library(data.table)
library(stringr)
library(pbapply)

#potential word bank for later bot
word_bank = c(
  "pay",
  "Pay",
  "requirements",
  "Requirements",
  "bonus",
  "fees",
  "fares",
  "money",
  "salary",
  "start",
  "plans",
  "New",
  "promotion",
  "Promotion",
  "lyft",
  "Lyft",
  "gett"
)



#prep fuel---------------------------------------------------------------

#create parent page loop fuel
page_numbers = 1:4
parent_pages = paste0("page=", page_numbers[-1])

#insert into loop to create big bank of threads
forum_bank = rbindlist(
  pblapply(parent_pages, function(x) {
    forum_bank = setDT(data.frame(paste0("https://uberpeople.net/search/47509842/?",x,"&q=promotion&o=relevance&c[node]=51")))
  }
  )
)

#change title and add original first which has no "page=1"
names(forum_bank) = c("links")
forum_bank = rbind(data.frame(links = "https://uberpeople.net/search/47509842/?q=promotion&o=relevance&c[node]=51"),
                   forum_bank)
forum_bank[,links:=as.character(links)]
forum_bank = as.data.frame(forum_bank)


#loop through each page in the forum_bank to create the threads list to search on
threads = NA
for (i in forum_bank[,1]) {
  print(i)
  parent_thread = read_html(i)
  links = html_nodes(parent_thread, ".title a")
  little_threads = data.frame(deals = html_text(links),
                              correpsonding_link = html_attr(links, "href"))
  
  
  type = html_text(html_nodes(parent_thread, ".contentType"))
  
  little_threads = cbind(little_threads, type)
  threads = setDT(rbind(threads, little_threads))
}
threads = as.data.frame(na.omit(threads)[type == "Thread"])


#initiate collection------------------------------------------------------------------
master_forum = NA
threads$correpsonding_link = as.character(threads$correpsonding_link)
for (i in threads[, 2]) {
  topic = i
  site = paste0("https://uberpeople.net/", i)
  print(site)
  
  #sweep through page
  library(XML)
  library(rvest)
  page_numbers = 1:4
  urls = paste(site,
               page_numbers,
               sep = "page-")
  
  uber_forum = NULL
  
  for (i in page_numbers) {
    site = urls[i]
    print(site)
    uber = paste(site)
    uber_call = read_html(uber)
    uber_name = html_nodes(uber_call, ".userText .username")
    uber_name = gsub("[^[:alnum:] ]", "", html_text(uber_name))
    uber_name = as.data.frame(uber_name)
    
    #call in text
    uber_write = html_nodes(uber_call, ".SelectQuoteContainer")
    uber_write = gsub("[^[:alnum:] ]", "", html_text(uber_write))
    uber_write = as.character(uber_write)
    
    #call in date
    uber_date = html_text(html_nodes(uber_call, "#messageList .DateTime"))
    #uber_date = gsub("[^[:alnum:] ]", "", html_text(uber_date))
    #uber_date = str_sub(uber_date, -7,-18)
    
    #mesh
    #print(uber_name)
    #print(uber_date)
    uber_master = cbind(uber_name, uber_write
                        , uber_date[1:length(uber_name)]
    )
    uber_master$page_num = paste(i)
    uber_master$threads = substr(site, 32, (nchar(site) - 14))
    uber_master$links = site
    #print(head(uber_master))
    uber_forum = rbind(uber_forum, uber_master)
    #uber_forum$threads = site
    #print(names(uber_forum))
  }
  
  #print(head(uber_forum))
  
  master_forum = setDT(rbind(master_forum, uber_forum))
  #print(head(master_forum))
  
}



#PREP CORPUS AND CLEAN------------------------------------------------------------------------
#n = nrow(master_forum)

#eliminate all duplicate values
names(master_forum) = c("uber_name","uber_write","uber_date", "page_num","threads","links")
master_forum = master_forum[!duplicated(paste(uber_name, uber_write)),]
master_forum[,tot_pop:= .N, by = .(uber_name)]
master_forum[, pop_per_thread:= .N, by = .(uber_name, threads)]
master_forum[,pop_per_thread:=sum(pop_per_thread), by = .(threads)]

#lets turn all the letters in the character vectors into lowercase
master_forum[,uber_write:= as.character(uber_write)]

#remove any names referenced in comments
uber_names = master_forum$uber_name
master_forum[, uber_write_filtered := gsub(
  pattern = paste0("(", paste(uber_names, collapse = "|"), ")"),
  replacement = "", uber_write)]

#master_forum$uber_write2 = gsub("[^[:alnum:] ]", "", master_forum$uber_write)
write.csv(master_forum, "uber_forum.csv")

#master_forum$uber_write = grepl('"\n\t\t\t\t\t\n\n\t\t\t\t\t\n\"',"", master_forum$uber_write)
str(master_forum)

#subset just description
master_forum = na.omit(master_forum)
uber_n = master_forum[, c("threads", "uber_write_filtered")]

#check table for accuracy
table(uber_n$threads)
table(is.na(uber_n$threads))


#parameters (aka. classifiers)
candidates = uber_n[, c("threads")]

#function built for
#cleaning via tm library the character data
trim = function (x)
  gsub("^\\s+|\\s+$", "", x)
cleanCorpus = function(corpus) {
  corpus.tmp = tm_map(corpus, removePunctuation)
  corpus.tmp = tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp = tm_map(corpus.tmp, content_transformer(tolower))
  corpus.tmp = tm_map(corpus.tmp, stemDocument)
  #corpus.tmp = tm_map(corpus.tmp, trim)
  #corpus.tmp = tm_map(corpus.tmp, PlainTextDocument)
  corpus.tmp = tm_map(corpus.tmp, removeWords, stopwords("english"))
  return(corpus.tmp)
}


#extract corpus from data frame
#corpus fancy word for one huge string of writing
#open_n_c = as.data.frame(open_n[,2])
corp = Corpus(DataframeSource(uber_n))
writeLines(as.character(corp[[12]])) #look at one example


#clean corpus
corp = cleanCorpus(corp)

#turn to docment term matrix---------------------------------------------------------------------------------------
#each row will be one document with the classifier it originally had
#aka the complaint type
dtm = DocumentTermMatrix(corp)
#we can also specify how we create document matrix using only certain words
# dtmr =DocumentTermMatrix(corp, control=list(wordLengths=c(4, 20),
#                                              bounds = list(global = c(3,27))))


#######################################################
#dtmr =DocumentTermMatrix(corp, control=list(wordLengths=c(4, 20),
#                                           bounds = list(global = c(3,20))))

dtmr = dtm

freqr = colSums(as.matrix(dtmr))
#length should be total number of terms
length(freqr)

#create sort order (asc)
ordr = order(freqr, decreasing = TRUE)
#inspect most frequently occurring terms
freqr[head(ordr)]


#inspect least frequently occurring terms
freqr[tail(ordr)]

findFreqTerms(dtmr, lowfreq = 20)
findAssocs(dtmr, "debt", 0.6)

#word cloud limit words by specifying min frequency
wordcloud(names(freqr), freqr, min.freq = 20)
###############################################################

#remove sparse terms
#note that the value on te right will denote the strenght of the word removal
#this can change depending on your sample size
#may be worth writing a function to adapt to sample size
dtm = removeSparseTerms(dtm, .99)
inspect(dtm[1:10, ])
findFreqTerms(dtm, 5)

#most common words
freq = colSums(as.matrix(dtm))
ord = order(freq, decreasing = T)
freq[head(ord)]
freq[tail(ord)]

#cumulative sum of words across
freqr = colSums(as.matrix(dtm))
length(freqr)
#create sort order (asc)
ordr = order(freqr, decreasing = TRUE)
#inspect most frequently occurring terms
freqr[head(ordr)]
#inspect least frequently occurring terms
freqr[tail(ordr)]
#find frequent terms
findFreqTerms(dtm, lowfreq = 80)
#test association with a word
findAssocs(dtm, "uber", 0.7)


#----------------------------------------------------------WORD ASSOCIATIONS AND VISUALIZATIONS
#*********create a loop to tabulate associatios with the most
#frequent terms in this case those that appear at leas 80 times
z = findFreqTerms(dtm, lowfreq = 80)
for (i in z) {
  x = findAssocs(dtm, paste(i), .3)
  print(as.data.frame(x))
}
#transform into data frame
dtm_df = as.data.frame(as.matrix(dtm))
#View(dtm_df)

#table each category with associated words
dtm_df_c = cbind(dtm_df, candidates)

for (i in colnames(dtm_df_c)) {
  print(i)
  print(table(dtm_df_c$candidates, dtm_df_c[, i]))
}

#create a word cloud for each category
#in order to understand which words are most common (4o times)
word_mat = data.frame(t(dtm_df_c))
wordcloud(word_mat, min.freq = 40, random.order = FALSE)















#income extract-------------------------------------------------------



income_forum = master_forum[master_forum$threads == "estimated-weekly-income",]
income_list = as.numeric(gsub("([0-9]+).*$", "\\1", income_forum$uber_write))
income_list = income_list[which(income_list > 200 & income_list < 5000)]
mean(income_list) * 52

