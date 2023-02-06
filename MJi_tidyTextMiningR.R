install.packages("tidytext")
install.packages("tidyr")
install.packages("tm")
install.packages("tidyverse")

######################################################  LIBRARY LOAD################################################################
library (tm)
library (tidyr)
library (readr)
library (stringi)
#library (RWeka)
library (ggplot2)
library (wordcloud)
library (SnowballC)
library (gridExtra)
library (textmineR)
library (tidytext)
library (SentimentAnalysis)
library (dplyr)
library (stringr)
library (tidytext)
#library (qdap)
library (textdata)
library (igraph)
library (ggraph)

#####################################################   SENTIMENT DATA HANDLING   #####################################################
#
install.packages("textdata")
install.packages("SentimentAnalysis")
#
#loading sentiment data for inspection...
nrc <- get_sentiments("nrc")
#
#creating sentiment variables to filter sentiments by category 'negative'...
nrc_sent_neg <- get_sentiments("nrc") %>% filter(sentiment == "negative")
nrc_sent_pos <- get_sentiments("nrc") %>% filter(sentiment == "positive")
bing_sent <- get_sentiments("bing") %>% filter(sentiment == "negative")
nafinn_sent <- get_sentiments("nafinn") %>% filter(sentiment == "negative")
#
#
####################################################    LOAD DATA   ###############################################################

#creating Volotile corpus of  txt files located in Working Directory...
RAcorpus <- VCorpus(DirSource(directory = getwd(), 
                              pattern = "*.txt"))
#creating tidy corpus...
RAtidy <- tidy(RAcorpus) 


###################################################     TidyText Functions################################################################
#
tokenize_it <- function(file_corpus) {
 #this function requires a tidy file corpus as input 
  
  tidy_corp <- file_corpus %>% 
    group_by(id) %>% 
    summarize(text) %>% 
    mutate(id = factor(id)) %>% #make sure to mutate the "id" column^
    ungroup() #formats corpus for tidy analysis
  
  tidy_text <- tidy_corp %>% 
    unnest_tokens(word, text) #creates tidy text tibble
  
  file_corpus <- tidy_text
  
  rm(tidy_corp)
  
  return(file_corpus)
  
}
#
bigram_it <- function(file_corpus, tidyIDs = FALSE) {
  #this function requires a tidy file corpus as input 
  
  if (tidyIDs == TRUE)
  {
    cnt <- 1 #tidy up the id names
    repeat{
      file_corpus[["id"]][[cnt]] <- substr(file_corpus[["id"]][[cnt]], 1, 12) #substracts all parts of the char string and leaves 1-10
      cnt <- cnt+1
      if(cnt > length(file_corpus)){
        break
      }
    }
  }
  
  tidy_corp <- file_corpus %>% 
    group_by(id) %>% 
    summarize(text) %>% 
    mutate(id = factor(id)) %>% #make sure to mutate the "id" column^
    ungroup() #formats corpus for tidy analysis
  
  tidy_text <- tidy_corp %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2) #creates tidy text tibble
  
  bigrams_seperated <- tidy_text %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  bigrams_filtered <- bigrams_seperated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
  mystopwords <- tibble(word = c("name 1", "name 2", "uhm", "um", "uh", "00", "etc", "let's", "gotta", "adrian", "bn")) 
  
  bigrams_filtered <- bigrams_filtered %>%
    filter(!word1 %in% mystopwords$word) %>%
    filter(!word2 %in% mystopwords$word)
  
  bigram_counts <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE)
  file_corpus <- tidy_text
  
  
  return(bigram_counts)
  
}
#
rem_stopwords <- function(x, customRemove, mystopwords) {
  
  if (customRemove == TRUE)
  {
    data("stop_words") #calls stop_word data set into Global Environment
    tidy_text <- x %>% 
      anti_join(stop_words) #removes stop_words found in tidy_text
    tidy_text <- x %>% 
      anti_join(mystopwords)
    return(tidy_text)
    
  }
    #mystopwords <- tibble(word = c("pop", "forget", "library", "skip", "spelling", "technology", "clerical", "unintelligible", "inaudible", "fully", "pay", "money", "swim", "splitting", "mother", "illegal", "stealing", "feeling", "police", "words", "question", "name1", "name2", "uhm", "um", "uh", "00", "etc", "let's", "gotta", "adrian", "bn")) 
    #mystopwords <- tibble(word = c("bug", "bugs" ,"pop", "forget", "library", "skip", "spelling", "technology", "clerical", "unintelligible", "inaudible", "fully", "pay", "money", "swim", "splitting", "mother", "illegal", "stealing", "feeling", "police", "words", "question", "name1", "name2", "uhm", "um", "uh", "00", "etc", "let's", "gotta", "adrian", "bn")) 
  else
  {
    data("stop_words") #calls stop_word data set into Global Environment
    tidy_text <- x %>% 
      anti_join(stop_words) #removes stop_words found in tidy_text
    return(tidy_text)
  }
  
}
#
tf_analysis <- function(file_corpus, sent_input, plotTF, plotTFidf){
  
  get_Total <- file_corpus %>% mutate(id = factor(id)) %>%
    unnest_tokens(word, text)
  get_Total <- rem_stopwords(get_Total, FALSE)
  words <- get_Total %>% count (id, word, sort = TRUE) %>% group_by(id) %>% summarise(wordcount = sum(n))
  get_Total <- get_Total %>%
    inner_join(get_sentiments(sent_input), by = c(word = "word"))
  get_Total <- get_Total %>% count (id, word, sort = TRUE) 
  
  
  total_words <- get_Total %>%
    group_by(id) %>%
    summarize(total = sum(n))
  
  doc_words <- left_join(get_Total, total_words)
  
  freq_by_rank <- doc_words %>%
    group_by(id) %>%
    mutate(rank = row_number(), termfrequency = n/total) %>%
    ungroup()
  
  doc_tf_idf <- doc_words %>%
    bind_tf_idf(word, id, n)
  
  doc_tf_idf %>%
    select(-total) %>%
    arrange(desc(tf_idf))
  
  if (plotTF == TRUE)
  {
  ggplot(doc_words, aes(n/total, fill = id)) +
    geom_histogram(show.legend = FALSE) +
    xlim(NA, 0.0009) +
    facet_wrap(~id, ncol = 2, scales = "free_y")
  }

  
  Sent_tf <- doc_tf_idf %>%
    inner_join(get_sentiments(sent_input), by = c(word = "word"))
  
  possent <- Sent_tf %>% filter(sentiment=="positive")
  negsent <- Sent_tf %>% filter(sentiment=="negative")
  Sent_tf <- bind_rows(possent, negsent)
  #Sent_tf <- Sent_tf %>% semi_join(sent)
  
  #return(Sent_tf)
  
  if (plotTFidf == TRUE)
  {
    Sent_tf %>%
      group_by(id) %>%
      slice_max(tf_idf, n= 10) %>%
      ungroup() %>%
      ggplot(aes(tf_idf, reorder(word, tf_idf), fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~id, ncol = 4, scales = "free", shrink = FALSE) +
      labs(x = "tf-idf", y = NULL)
  }
  else
  {
     addcounts <- create_tf_ratios(Sent_tf)
     addcounts <- left_join(addcounts, words, by = "id")
     return(addcounts)
  }
}
#
visualize_sentiment <- function(tidy_text, sent_input, numwords){
  
  tidy_text_plot <- tidy_text %>% 
    group_by(id) %>% 
    count(word, sort = TRUE) #creates a new tibble for plotting
  
  IMP_all <- tidy_text_plot %>%
    inner_join(get_sentiments(sent_input), by = c(word = "word")) 
  
  plot <- IMP_all %>% filter(as.integer(id)==1) %>%
    count(sentiment, word, wt = n) %>%
    ungroup() %>%
    filter(n >= numwords) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word, fill = sentiment)) +
    geom_col() +
    labs(x = "Contribution to sentiment", y = NULL)
  
  sent_count <- IMP_all %>% filter(as.integer(id)==1) %>%
    count(sentiment, word, wt = n) %>%
    ungroup()
    
  print(plot)
  print(sent_count)
  
}
#
sentiment_counts <- function(tidy_text, sent_input, sent){
  tidy_text_plot <- tidy_text %>% 
    group_by(id) %>% 
    count(word, sort = TRUE) #creates a new tibble for plotting
  
  IMP_all <- tidy_text_plot %>%
    inner_join(get_sentiments(sent_input), by = c(word = "word")) 
  
  #plot <- IMP_all %>% filter(as.integer(id)==1) %>%
    #count(sentiment, word, wt = n) %>%
    #ungroup()
  IMP_all <- IMP_all %>% filter(sentiment==sent) %>% summarize(Psum = sum(n)) 
  
  return(IMP_all)
  
}
#
create_ratios <- function(tidy_text, sent_input) {
  
  tidy_text_plot <- tidy_text %>% 
    group_by(id) %>% 
    count(word, sort = TRUE) #creates a new tibble for plotting
  
  count <- tidy_text_plot %>% group_by(id) %>% summarize(wordcount = sum(n))
  
  IMP_all <- tidy_text_plot %>%
    inner_join(get_sentiments(sent_input), by = c(word = "word")) 

  possent <- IMP_all %>% filter(sentiment=="positive") %>% summarize(Psum = sum(n)) 
  negsent <- IMP_all %>% filter(sentiment=="negative") %>% summarize(Nsum = sum(n)) 
  sent <- left_join(possent, negsent, by = "id")
  counts <- full_join(sent, count, by = "id")
  final <- counts %>% mutate(Pratio = Psum/wordcount)
  final2 <- final %>% mutate(Nratio = Nsum/wordcount)
  final3 <- final2 %>% mutate(PNratio = (Psum-Nsum)/(Psum+Nsum))
  return(final3)
}
#
create_tf_ratios <- function(IMP_all, sent_input) {
  

  possent <- IMP_all %>% group_by(id) %>% filter(sentiment=="positive") %>% summarize(Psum = sum(n)) 
  negsent <- IMP_all %>% group_by(id) %>% filter(sentiment=="negative") %>% summarize(Nsum = sum(n)) 
  posidtdf <- IMP_all %>% group_by(id) %>% filter(sentiment=="positive") %>% summarize(Pitf = mean(tf_idf))
  negidtdf <- IMP_all %>% group_by(id) %>% filter(sentiment=="negative") %>% summarize(Nitf = mean(tf_idf))
  
  negidtdf
  sent <- left_join(possent, negsent, by = "id")
  sent <- left_join(sent, posidtdf, by = "id")
  sent <- left_join(sent, negidtdf, by = "id")
  
  
  sent <- sent %>% mutate(total = Psum+Nsum)
  
  final <- sent %>% mutate(Pratio = Psum/total)
  final2 <- final %>% mutate(Nratio = Nsum/total)
  final3 <- final2 %>% mutate(PNratio = (Psum-Nsum)/total)
  return(final3)
}
#
export <- function(tidy, name){
  
  tidy <- data.frame(tidy)
  write.csv(tidy, "C:\\Users\\jgronemeyer\\Documents\\R\\INS Abstract\\Exports\\" + name + ".csv")
  
}
#
bigram_editing <- function(bigrams_separated){
  not_words <- bigrams_separated %>%
    filter(word1 == "feeling") %>%
    inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
    count(word2, value, sort = TRUE)
  
  plot <- not_words %>%
    mutate(contribution = n * value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(n * value, word2, fill = n * value > 0)) +
    geom_col(show.legend = FALSE) +
    labs(x = "Sentiment value * number of occurrences",
         y = "Words preceded by \"not\"")
  print(plot)
}
#
bigram_network <- function (bigram_counts){
  #requires library("igraph") and ggraph
  
  bigram_graph <- bigram_counts %>%
    filter(n > 1) %>%
    graph_from_data_frame()
  

  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
}

##################################################      Data Wranglin' ################################################################

#calculating term-document frequency...
RAtdf <- tokenize_it(RAtidy)

#removing stop words...
RAtdf <- rem_stopwords(RAtdf, FALSE)

#term frequency analysis with Bing sentiment dictionary
  #with or without visualized plots...
tf_analysis(RAtidy, "bing", FALSE, TRUE)
#retrieve values...
tf_analysis(RAtidy, "bing", TRUE, FALSE)


#storing tf analysis as new data object...
RA_tf <- tf_analysis(RAtidy, "bing", FALSE, FALSE)


RAcounts_tdf <- create_tf_ratios(RAtdf, "bing")
RAcounts <- create_ratios(RAtdf, "bing")


visualize_sentiment(RAtdf, "bing", 2)

#creating bigrammable data structure...
RAbigrams <- bigram_it(RAtidy)
#visualizing bigrams...
bigram_network(RAbigrams)

#################################################       Get SUMS      ################################################################

tidy <- data.frame(SentimentSums)
write.csv(tidy, "...")
SumstatsNT <- IMPNT_SentimentSums %>% group_by(id) %>% summarise(mean = mean(Psum), sd)
boxplot(IMPBL_SentimentSums$Psum)
boxplot(IMPNT_SentimentSums$Psum)


################################################        Experimenting################################################################



library("ggpubr")

ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"),
          ylab = "Weight", xlab = "Groups")


vda(BLcounts$ratio, FUcounts$ratio)

2 paired samples t.test (1 in experimental, expect difference; control expect no difference)

vda <- function(formula=NULL, data=NULL, x=NULL, y=NULL, 
           ci=FALSE, conf=0.95, type="perc", R=1000, histogram=FALSE, 
           reportIncomplete=FALSE, brute=FALSE, verbose=FALSE, digits=3,
           ...){
    
    if(!is.null(formula)){
      x  = eval(parse(text=paste0("data","$",all.vars(formula[[2]])[1])))
      g  = factor(eval(parse(text=paste0("data","$",all.vars(formula[[3]])[1]))))
      A  = x[g==levels(g)[1]]
      B  = x[g==levels(g)[2]]
    }
    
    if(is.null(formula)){
      A = x
      B = y
      x = c(A, B)
      g = factor(c(rep("A", length(A)), rep("B", length(B))))
    }
    
    if(brute==FALSE){
      n1  = as.numeric(length(A))
      n2  = as.numeric(length(B))
      U   = suppressWarnings(wilcox.test(x=A, y=B, ...))$statistic
      VDA = signif(U / (n1 * n2), digits=digits)
    }
    
    if(brute==TRUE){
      Matrix = outer(A,B,FUN="-")
      Diff   = ifelse(Matrix==0, 0.5, Matrix>0)
      VDA    = signif(mean(Diff), digits=digits)
    }
    
    if(verbose){
      Matrix = outer(A,B,FUN="-")
      Out = data.frame(
        Statistic = c("Proportion Ya > Yb","Proportion Ya < Yb",
                      "Proportion ties"),
        Value     = c(signif(mean(Matrix>0), digits=3),
                      signif(mean(Matrix<0), digits=3),
                      signif(mean(Matrix==0), digits=3))
      )
      cat("\n")
      print(Out)
      cat("\n")
    } 
    
    if(ci==TRUE){
      Data = data.frame(x,g)
      Function = function(input, index){
        Input = input[index,]
        if(length(unique(droplevels(Input$g)))==1){
          FLAG=1
          return(c(NA,FLAG))}
        
        if(length(unique(droplevels(Input$g)))>1){
          
          if(brute==FALSE){
            U = suppressWarnings(wilcox.test(x ~ g, 
                                             data=Input, ...))$statistic
            n1  = length(Input$x[Input$g==levels(Input$g)[1]])
            n2  = length(Input$x[Input$g==levels(Input$g)[2]])
            p   = U / (n1 * n2)
            FLAG=0}
          
          if(brute==TRUE){
            Matrix = outer(Input$x[Input$g==levels(Input$g)[1]],
                           Input$x[Input$g==levels(Input$g)[2]],
                           FUN="-")
            Diff   = ifelse(Matrix==0, 0.5, Matrix>0)
            p      = mean(Diff)
            FLAG=0}
          
          return(c(p, FLAG))}}
      
      Boot = boot(Data, Function, R=R)
      BCI  = boot.ci(Boot, conf=conf, type=type)
      if(type=="norm") {CI1=BCI$normal[2];  CI2=BCI$normal[3];}
      if(type=="basic"){CI1=BCI$basic[4];   CI2=BCI$basic[5];}
      if(type=="perc") {CI1=BCI$percent[4]; CI2=BCI$percent[5];}
      if(type=="bca") {CI1=BCI$bca[4];      CI2=BCI$bca[5];}  
      
      if(sum(Boot$t[,2])>0 & reportIncomplete==FALSE) {CI1=NA; CI2=NA} 
      
      CI1=signif(CI1, digits=digits)
      CI2=signif(CI2, digits=digits)
      
      if(histogram==TRUE){hist(Boot$t[,1], col = "darkgray",
                               main="", xlab="VDA")}
    }
    
    if(ci==FALSE){names(VDA)="VDA"; return(VDA)}
    if(ci==TRUE){names(VDA) = ""
    return(data.frame(VDA=VDA, lower.ci=CI1, upper.ci=CI2))}
}



cnt <- 1 #tidy up the id names
repeat{
  file_corpus[["id"]][[cnt]] <- substr(file_corpus[["id"]][[cnt]], 1, 12) #substracts all parts of the char string and leaves 1-10
  cnt <- cnt+1
  if(cnt > 24){
    break
  }
}