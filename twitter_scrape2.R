#=============================================================================
# This is R code for automatically retrieving EEB_POC papers from their 
# twitter handle using rtweet. 
#
# Twitter examples here:
# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
# https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html
#
# HTML retrieval: 
# https://statistics.berkeley.edu/computing/r-reading-webpages
#=============================================================================
#=============================================================================
# If this is the first time running this then it is necessary to authenticate 
# this on Twitter first. 
# You need to have a twitter account. If you do not have one go to 
# http://twitter.com/signup and set one up. Also you need to have a modile number 
# as part of this account.
#
# After have created a twitter account you need to go to https://apps.twitter.com 
# and sign on with your twitter account.
#
# Once you click on the “Create New App” button you will go to the Create an 
# Application screen. There are three fields, a click box and a button you need 
# to click on this page. The three fields are Name, Description and Website. 
# The name of the application must be unique so this may take a few tries. 
# The description needs to be at least 10 character long, and put in a website. 
# If you do not have one you can use https://bigcomputing.blogspot.com.
# Now click the “Yes, I agree” box for the license agreement and click the
# “Create your Twitter application”.
#
# Go here for more detailed instructions: 
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
#=============================================================================
# library(rtweet)

# appname = "eeb_poc_database_harvester"
# consumer_key = "glEFLB8aY0kSWsVRnqIZS9V3F"
# secret_key = "USMIJnSJWVZtLDNDFe7XMNdMSeLxGWLSHGGn6A4s9ZoKTmmZUs"
# access_token = "" 
# access_secret = ""

# token = create_token(
#   app = appname,
#   consumer_key = consumer_key,
#   consumer_secret = secret_key)

#=============================================================================
#Load libraries
#=============================================================================
library(rtweet)
library(ggplot2)
library(tidyverse)
library(RCurl)
library(rcrossref)

decode.short.url <- function(u) {
  x <- try( getURL(u, header = TRUE, nobody = TRUE, followlocation = FALSE) )
  if(class(x) == 'try-error') {
    return(u)
  } else {
    x <- sub("\\r.*","", strsplit(x, c("ocation: "))[[1]][2])
    return(strsplit(x, "\r")[[1]][1])
  }
}

#=============================================================================
# Once a token has been generated, it can just be accessed like this: 
#=============================================================================
get_token()

#=============================================================================
# Load up some data from Twitter for EEB_POC! 
#=============================================================================
eb1 = get_timeline("EEB_POC",n=10)

#Search tweets for HTMLs that could lead to papers: 
nt = dim(eb1)[1]
for (n in 1:nt) {

	html_y = grep("https:", eb1$text[n]) #Look for a web link
	if(length(html_y)>0) { 
		t1 = grep("https:", eb1$text[n],value = TRUE) #Get the tweet
		#Extract address. This is done in 2 steps due to prevalance of tiny
		#esque URLs. 
		page1 = paste("https:", sub(".*https:","",t1),sep="") 
		page1_u= decode.short.url(page1)
		thepage_tmp = getURL(page1_u) 
	
		#Throw in a test to see if this is another redirect link: 
		test1 = grepl("oved", thepage_tmp) | grepl("edirect", thepage_tmp)
		if(test1 == TRUE) {
			thepage = paste("https:",sub("\\\".*","",sub(".*https:","",thepage_tmp)),sep="") 
		}else { 
			thepage = thepage_tmp
		}

		#Download the page HTML 
		thepage = getURL(thepage)
	
		#Look for a DOI: 
		test2 = grepl("doi",thepage)
		if(test2 == TRUE){ 
			#Get the citation: 
			cr_cn(doi,format="citeproc-json")
		}
	}


}

