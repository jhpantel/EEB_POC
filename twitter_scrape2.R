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

decode.short.url = function(u) {
  x = try( getURL(u, header = TRUE, nobody = TRUE, followlocation = FALSE)
  		, silent=TRUE )
  if(class(x) == 'try-error') {
    return(u)
  } else {
    x = sub("\\r.*","", strsplit(x, c("ocation: "))[[1]][2])
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
# This will retrieve tweets, look for an URL references, then follow these 
# URLs to look for DOIs. 
#
# If DOIs are found then cr_cn() from "rcrossref" is called to retrieve 
# the citation in a list-JSON format. 
# 
# The citations are stored recursively in "new_papers" with headers that match
# the current table values in the EEB_POC database. Currently, these include: 
#	Authors
#	Title
#	Publication
#	Volume
#	Number
#	Pages
#	Year
#	Keywords
#	ORCID (lead author)
#	DOI
#
#=============================================================================
eb1 = get_timeline("EEB_POC",n=100)

#Search tweets for HTMLs that could lead to papers: 
nt = dim(eb1)[1]
#Store the papers here: 
new_papers = NULL

n_index = 1
for (n in 1:nt) {

	html_y = grep("https:", eb1$text[n]) #Look for a web link
	
	if(length(html_y)>0) { 

		cit1 = NULL #Citation variable
		t1 = grep("https:", eb1$text[n],value = TRUE) #Get the tweet
		#Extract address. This is done in 2 steps due to prevalance of tiny
		#esque URLs. 
		#page1 = paste("https:", sub(".*https:","",t1),sep="") 
		page1 = sub("\n.*","",sub(".*https:","",t1))
		page1_u= decode.short.url(page1)
		thepage_tmp = getURL(page1_u) 
	
		#Throw in a test to see if this is another redirect link: 
		test1 = grepl("oved", thepage_tmp) | grepl("edirect", thepage_tmp)
		if(test1 == TRUE) {
			page1_u=paste("https:",sub("\\\".*","",sub(".*https:","",thepage_tmp)),sep="") 
			thepage = getURL(page1_u) 
		
		}else { 
			thepage = thepage_tmp
		}

		#Download the page HTML 
		page_test = try(getURL(page1_u),silent=TRUE)
		#Check to make sure it is of a type that will work: 
	 	if(class(thepage) != 'try-error'){ 
			#Look for a DOI: 
			test2 = grepl("doi",thepage)
			if(test2 == TRUE){ 
				doi_tmp = gsub(".*doi.org/*|*[\"|<].*", '',thepage)
				#Get the citation: 
				cit1= cr_cn(doi_tmp,format="citeproc-json")

				#Get the authors:
				numb_a = dim(cit1$author)[1]
				a_s = NULL
				for(a in 1:numb_a){
					a_s = c(a_s, paste(cit1$author[a,4],", ",cit1$author[a,3], sep="") )
				}
				a_s = paste(a_s, collapse = '')

				#Get the title:
				ta = cit1$title

				#Get the Publication
				pa =  cit1$"container-title"

				#Get the Volume
				va = cit1$volume

				#Get the Number
				num = cit1$issue

				#Get the Pages 
				pgs = cit1$page

				#Get the Year
				ya = cit1$indexed[[1]][1]
				
				#Get the Keywords
				ka = "0"

				#Get the ORCID
				oa = sub(".*orcid.org/",'', x=cit1$author$ORCID[[1]])
				
				#Get the DOI
				da = cit1$DOI

				#Put these all in the list: 
				new_papers[[n_index]] = c( Authors=list(a_s), Title = list(ta), 
					Publication = list(pa), Volume = list(va), Number = list(num),
					Pages = list(pgs), Year = list(ya), Keywords=list(ka), 
					"ORCID (lead author)" = oa, DOI = da  )

				n_index = n_index+1
			}



		}


	}


}

#=============================================================================
# All of this information is then stored externally to a (possibly already
# existing) databse in the form of a tab-delimited CSV file. 
#=============================================================================

file_name = "./database/EEB_POC_database.csv"
nrows = length(new_papers)
new_csv_df =data.frame(matrix(unlist(new_papers), nrow=nrows, byrow=T),stringsAsFactors=FALSE)
csv_names = c("Authors", "Title", "Publcation", "Volume", "Number", "Pages", "Year",
	"Keywords", "ORCID (lead author)", "DOI")
colnames(new_csv_df) = csv_names
write.table(new_csv_df, paste(file_name), sep = "\t", col.names = !file.exists(paste(file_name)), append = T)

