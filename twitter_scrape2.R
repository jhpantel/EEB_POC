#=============================================================================
# This is R code for automatically retrieving EEB_POC papers from their 
# twitter handle using rtweet. 
#
# This has been adapted from examples here:
# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
# https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html
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
library(twitteR)

consumer_key <- consumer_key_nt
consumer_secret <- consumer_secret_nt
access_token <- access_token_nt
access_secret <- access_secret_nt

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#=============================================================================
#Load libraries
#=============================================================================
library(tidyverse)
library(twitteR)