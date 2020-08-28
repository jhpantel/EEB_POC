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
library(rvest)
library(ggplot2)
library(tidyverse)
library(RCurl)
library(httr)
library(rcrossref)

#Define this useful function
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
eb1 = get_timeline("EEB_POC")

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
		#Extract address. This is done in 2 steps due to prevalance of tiny-
		#esque URLs. 

		#Old regex attempts:
		# page1 = paste("https:", sub(".*https:","",t1),sep="") 
		# page1 = paste("https:", sub("\n|@| |)|. .*","",sub(".*https:","",t1)),sep="" )
		# page1 = paste("https:", sub("[\n|@| |)].*","",sub(".*https:","",t1)),sep="" )
		
		#Currently the best regex filter: 
		page1 =paste("https:", gsub(".*https:*|*\n|@| |)| .*", '',t1),sep="" ) 
		page1_u= decode.short.url(page1)
		
		#Old attempts at getting the page:
		#thepage_tmp = getURL(page1_u) 
		#thepage = GET(page1_u,timeout(20)) %>% content("text")

		#Currently the best attempt: 
		#Think about using nodes with html_nodes("span") to make this easier? 
		test_url1= try(read_html(page1_u), silent=TRUE)
		if(class(test_url1) != 'try-error' ){ 
			thepage = html_text(read_html(page1_u ) )
		} else{
			#Basically, read_html fails sometimes for unknown reasons. 
			#Try a different fuction -- in this case GET -- to retrieve
			#the HTML. Then if that fails, give up. 
			test_url2= try(GET(page1_u,timeout(20)), silent=TRUE)
			
			if(class(test_url2) != 'try-error' ){ 
				thepage = GET(page1_u,timeout(20)) %>% content("text")
			}else{    
				thepage = ("Failure to get page")
			}
		}
	

		#Download the page HTML 
		page_test = try(getURL(page1_u),silent=TRUE)
		#Check to make sure it is of a type that will work: 
	 	if(class(page_test) != 'try-error'){ 
			#Look for a DOI: 
			test2 = grepl("doi",thepage,ignore.case = TRUE)
			
			if(test2 == TRUE){ 

				#This DOI will only be valid with a certain format, so skip
				#it if it doesn't conform (i.e. this could be reference within
				#another kind of webpage). Also try a few different ways of 
				#filtering it from the html data. 
				
				#Filter 1: Covers most things
				doi_test = 0 
				doi_tmp = gsub(".*doi.org/*|*\"|</|\\n|\\r| |  |Key.*|)|Cit.*|Pub.*| .*", '',thepage)
				if( nchar(doi_tmp)>6 & nchar(doi_tmp)< 40) { doi_test = 1}

				#Filter 2:
				if ( doi_test != 1){   
					doi_tmp = gsub(".*doi: *|*</|\\n|\n|\n\n|PMCID:| .*", '',thepage)
					if( nchar(doi_tmp)>6 & nchar(doi_tmp)< 40) { doi_test = 1}
				}
				
				#Filter 3:
				if ( doi_test != 1){   
					doi_tmp = gsub(".*DOI\">*|*</a>.*",'',thepage)
					if( nchar(doi_tmp)>6 & nchar(doi_tmp)< 40) { doi_test = 1}
				}

				#Filter 4:
				if ( doi_test != 1){   
					doi_tmp = gsub(".*DOI: *|*</|\\n|\n|\n\n.*", '',thepage)
					if( nchar(doi_tmp)>6 & nchar(doi_tmp)< 40) { doi_test = 1}
				}

				#Filter 5:
				if ( doi_test != 1){   
					doi_tmp = gsub(".*doi.org/*|* |</a.*", '',thepage)
					if( nchar(doi_tmp)>6 & nchar(doi_tmp)< 40) { doi_test = 1}
				}

				#A final round of tests to make sure this is a feasible DOI
				if(doi_test == 1 ) { test3=TRUE} else {test3=FALSE}
				#Could be other random garbage: 
				if(nchar(doi_tmp)<4 | grepl("/",doi_tmp) ==FALSE ) { test3 = FALSE}
				#Try running the link, is it garbage? 
				if(is.null(try(cr_cn(doi_tmp,format="citeproc-json"),silent=TRUE)) |
					class(try(
						cr_cn(doi_tmp,format="citeproc-json"),silent=TRUE))== 
					'try-error'){ test3= FALSE}

				if(test3 == TRUE){ 
					#doi_tmp = gsub(".*doi.org/*|*[\"|<].*", '',thepage)
					#Get the citation: 
					cit1= cr_cn(doi_tmp,format="citeproc-json")

					#Get the authors:
					numb_a = dim(cit1$author)[1]
					a_s = NULL
					for(a in 1:numb_a){
						a_s = c(a_s, paste(cit1$author$family[a],", ",cit1$author$given[a], sep=" ") )
					}
					a_s = paste(a_s, collapse = '')

					#Get the title:
					ta = cit1$title

					#Get the Publication
					if(exists("container-title", where = cit1)) {
						if(length(cit1$"container-title")>0){ 
						pa =  cit1$"container-title"}else{ 
						pa="NA"} } else {
						pa="NA"
					}

					#Get the Volume
					if(exists("volume", where = cit1) ){
						if(length(cit1$"volume")>0){ 
						va = cit1$volume}else{ 
						va="NA"}}else{ 
						va ="NA"
					}

					#Get the Number
					if(exists("issue", where = cit1) ){
						if(length(cit1$"issue")>0){ 
						num = cit1$issue}else{ 
						num="NA"}}else{
						num="NA"
					}

					#Get the Pages
					if(exists("pages", where = cit1) ){ 
						if(length(cit1$"pages")>0){ 
						pgs = cit1$page}else{ 
						pgs="NA"} }else{
						pgs="NA"
					}

					#Get the Year
					if(exists("indexed", where = cit1) ){
						if(length(cit1$"indexed")>0){ 
						ya = cit1$indexed[[1]][1]}else{ 
						ya="NA"}}else{
						ya="NA"
					}
					
					#Get the Keywords
					ka = "0"

					#Get the ORCID
					if(exists("ORCID", where = cit1$author) ){
						if(is.na(cit1$author$ORCID[[1]]) ){  
							oa = "NA"
						}else { 
							oa = sub(".*orcid.org/",'', x=cit1$author$ORCID[[1]])
						}
					}else{ 
						oa = "NA"
					}
					#Get the DOI
					da = cit1$DOI
					print(paste(n,da,sep =" ") )
					#Put these all in the list: 
					new_papers[[n_index]] = c( Authors=list(a_s), Title = list(ta), 
						Publication = list(pa), Volume = list(va), Number = list(num),
						Pages = list(pgs), Year = list(ya), Keywords=list(ka), 
						"ORCID (lead author)" = oa, DOI = da  )

					#Replace NULL with 0 to preserve table spacing when converting to csv.
					new_papers[[n_index]][vapply(new_papers[[n_index]], is.null, NA)] = "NA"

					n_index = n_index+1
				}

			}

		}


	}


}
#=============================================================================
#Known issues: 
#=============================================================================
#Fom 1 to 20: 
#Gets: 3,6,8,12,16,17,18,19,20
#Misses:
#2: Interactive pdf
#7: Wrong tiny url in tweet?
#14: Grabs dryad doi?? 
#15: Link to another tweet? 
#=============================================================================
# All of this information is then stored externally to a (possibly already
# existing) databse in the form of a tab-delimited CSV file. 
#=============================================================================

file_name = "./database/EEB_POC_database.csv"
nrows = length(new_papers)
new_csv_df =data.frame(matrix(unlist(new_papers), nrow=nrows, byrow=T),stringsAsFactors=FALSE)
csv_names = c("Authors", "Title", "Publication", "Volume", "Number", "Pages", "Year",
	"Keywords", "ORCID (lead author)", "DOI")
colnames(new_csv_df) = csv_names
write.table(new_csv_df, paste(file_name), sep = "\t", col.names = !file.exists(paste(file_name)), 
	append = T,row.names = FALSE)

