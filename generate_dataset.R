################################################################################################
#
# 01. Creation of benchmark data sets
#
################################################################################################

# 01a. Get text from Coursera quizzes
################################################################################################

quizzes <- readLines('data/quizzes.txt', encoding = 'UTF-8')


# 01b. Get text from randomly selected tweets
################################################################################################
install.packages(c('twitteR', 'xml', 'textcat'))
library('twitteR')
library('XML')
library('textcat')

# To use the Twitter API, you'll need to create an application via the following link:
# https://apps.twitter.com/app/new
# Once the app was created, paste the consumer and access details below.
setup_twitter_oauth(consumer_key = '...',
                    consumer_secret = '...',
                    access_token = '...',
                    access_secret = '...')

# extract 1000 tweets containing the word 'the'
tweets.orig <- searchTwitter('the', 1000)

# extract the relevant information into a data.table
tweets.dt <- data.table(
    text = sapply(tweets.orig, function(x) {
        stri_replace_all_regex(xpathSApply(htmlParse(x$text, 
                                                     asText=T, 
                                                     encoding = 'UTF-8'), 
                                           '//body/p/text()',
                                           xmlValue)
                               , '[\r\n]', ' ')
    }),
    user = sapply(tweets.orig, function(x){x$screenName}),
    rt.count = sapply(tweets.orig, function(x){x$retweetCount}),
    is.truncated = sapply(tweets.orig, function(x){x$truncated})    
)

# determine the language of each tweet (this is not very precise, but all we 
# want to do here is filter out the obvious non-English tweets)
tweets.dt[,lang:=textcat(text, ECIMCI_profiles)]

# retain only the unique tweets fulfilling the following criteria:
# 1. they contain the string 'the' (the search somehow also returns tweets not 
#    containing this)
# 2. they were recognized as language 'en'
# 3. the tweet is not flagged as truncated
tweets <- unique(tweets.dt[stri_detect_fixed(text, 'the') & 
                               lang=='en' & 
                               !is.truncated]$text)

# put everything into a file
writeLines(tweets, 'data/tweets.txt', useBytes = T)
# make sure we can read it back in
tweets2<-readLines('data/tweets.txt', encoding = 'UTF-8')
identical(tweets, tweets2)
#  TRUE

rm(tweets2)


# 01c. Get text from randomly selected blog descriptions
################################################################################################

# Note that we're not really crawling blog content here, but rather just descriptions of blogs.

# full list of RSS feeds taken from http://www.blog-search.com/feeds/
urls <- c('http://www.blog-search.com/categories/autos/recent.rss',
          'http://www.blog-search.com/categories/autos/popular.rss',
          'http://www.blog-search.com/categories/business/recent.rss',
          'http://www.blog-search.com/categories/business/popular.rss',
          'http://www.blog-search.com/categories/computers/recent.rss',
          'http://www.blog-search.com/categories/computers/popular.rss',
          'http://www.blog-search.com/categories/education/recent.rss',
          'http://www.blog-search.com/categories/education/popular.rss',
          'http://www.blog-search.com/categories/entertainment/recent.rss',
          'http://www.blog-search.com/categories/entertainment/popular.rss',
          'http://www.blog-search.com/categories/environment/recent.rss',
          'http://www.blog-search.com/categories/environment/popular.rss',
          'http://www.blog-search.com/categories/family/recent.rss',
          'http://www.blog-search.com/categories/family/popular.rss',
          'http://www.blog-search.com/categories/finance/recent.rss',
          'http://www.blog-search.com/categories/finance/popular.rss',
          'http://www.blog-search.com/categories/fitness/recent.rss',
          'http://www.blog-search.com/categories/fitness/popular.rss',
          'http://www.blog-search.com/categories/food/recent.rss',
          'http://www.blog-search.com/categories/food/popular.rss',
          'http://www.blog-search.com/categories/gardening/recent.rss',
          'http://www.blog-search.com/categories/gardening/popular.rss',
          'http://www.blog-search.com/categories/health/recent.rss',
          'http://www.blog-search.com/categories/health/popular.rss',
          'http://www.blog-search.com/categories/hobbies/recent.rss',
          'http://www.blog-search.com/categories/hobbies/popular.rss',
          'http://www.blog-search.com/categories/home-repair/recent.rss',
          'http://www.blog-search.com/categories/home-repair/popular.rss',
          'http://www.blog-search.com/categories/humor/recent.rss',
          'http://www.blog-search.com/categories/humor/popular.rss',
          'http://www.blog-search.com/categories/internet/recent.rss',
          'http://www.blog-search.com/categories/internet/popular.rss',
          'http://www.blog-search.com/categories/law/recent.rss',
          'http://www.blog-search.com/categories/law/popular.rss',
          'http://www.blog-search.com/categories/marketing/recent.rss',
          'http://www.blog-search.com/categories/marketing/popular.rss',
          'http://www.blog-search.com/categories/multimedia/recent.rss',
          'http://www.blog-search.com/categories/multimedia/popular.rss',
          'http://www.blog-search.com/categories/personal/recent.rss',
          'http://www.blog-search.com/categories/personal/popular.rss',
          'http://www.blog-search.com/categories/pets/recent.rss',
          'http://www.blog-search.com/categories/pets/popular.rss',
          'http://www.blog-search.com/categories/politics/recent.rss',
          'http://www.blog-search.com/categories/politics/popular.rss',
          'http://www.blog-search.com/categories/religion/recent.rss',
          'http://www.blog-search.com/categories/religion/popular.rss',
          'http://www.blog-search.com/categories/science/recent.rss',
          'http://www.blog-search.com/categories/science/popular.rss',
          'http://www.blog-search.com/categories/self-help/recent.rss',
          'http://www.blog-search.com/categories/self-help/popular.rss',
          'http://www.blog-search.com/categories/social-issues/recent.rss',
          'http://www.blog-search.com/categories/social-issues/popular.rss',
          'http://www.blog-search.com/categories/sports/recent.rss',
          'http://www.blog-search.com/categories/sports/popular.rss',
          'http://www.blog-search.com/categories/technology/recent.rss',
          'http://www.blog-search.com/categories/technology/popular.rss',
          'http://www.blog-search.com/categories/travel/recent.rss',
          'http://www.blog-search.com/categories/travel/popular.rss',
          'http://www.blog-search.com/categories/webmasters/recent.rss',
          'http://www.blog-search.com/categories/webmasters/popular.rss')

# extract the blog descriptions from all RSS feeds
blogs <- unique(unlist(
    lapply(urls,
           function(url) {
               xpathSApply(xmlParse(url, isURL = T), 
                           '//description',
                           function(cont) {
                               xpathSApply(htmlParse(xmlValue(cont), asText = T, encoding = 'UTF-8'),
                                           '//body/p/text()',
                                           xmlValue)
                           })
           })
))

# put everything into a file
writeLines(blogs, 'data/blogs.txt', useBytes = T)

# make sure we can read it back in
blogs2<-readLines('data/blogs.txt', encoding = 'UTF-8')
identical(blogs, blogs2)
# TRUE

rm(blogs2)