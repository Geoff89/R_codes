
mydata <- read.csv("http://www.mysite.com/mydata.csv")
mytext <- readLines("http://www.mysite.com/myfile.txt")
download.file("http://www.mysite.com/myfile.zip", "myfile.zip")

library(RCurl)
packt_page <- ("https://www.packtpub.com/")
str(packt_page, nchar.max=200)

library(httr)
packt_page <- GET("https://www.packtpub.com")
str(packt_page, max.level = 1)
List of 10
$ url        : chr "https://www.packtpub.com/"
$ status_code: int 200
$ headers    :List of 11
..- attr(*, "class")= chr [1:2] "insensitive" "list"
$ all_headers:List of 1
$ cookies    :'data.frame':	0 obs. of  7 variables:
  $ content    : raw [1:163167] 3c 21 44 4f ...
$ date       : POSIXct[1:1], format: "2018-06-18 12:39:16"
$ times      : Named num [1:6] 0 0.766 1.094 2.109 3.187 ...
..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
$ request    :List of 7
..- attr(*, "class")= chr "request"
$ handle     :Class 'curl_handle' <externalptr>
  - attr(*, "class")= chr "response"
str(content(packt_page, type="text"), nchar.max=200)
chr "<!DOCTYPE html>\n<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">\n\t<head>\n\t\t<title>Packt Publishing | Technology Books, eBooks & Videos</title>\n\t\t<scr"| __truncated__

library(rvest)
rvest::
packt_page <- read_html("https://www.packtpub.com")
html_node(packt_page, "title")#scrappig the title

html_node(packt_page, "title") %>% html_text()#filtering to normal text minus html tags
[1] "Packt Publishing | Technology Books, eBooks & Videos"
#scrapping the contents of cra machine learning packages
#start with html function since we dont know how the web page looks like
cran_ml <-read_html("https://cran.r-project.org/web/views/MachineLearning.html")
cran_ml
ml_packages <- html_nodes(cran_ml, "a")

#parsing jsons from web apis
library(httr)
map_search <-
  GET("https://maps.googleapis.com/maps/api/geocode/json",
      query = list(address = "Eiffel Tower"))

map_search
Response [https://maps.googleapis.com/maps/api/geocode/json?address=Eiffel%20Tower]
Date: 2018-06-18 13:03
Status: 200
Content-Type: application/json; charset=UTF-8
Size: 2.18 kB
{
  "results" : [
    {
      "address_components" : [
        {
          "long_name" : "Champ de Mars",
          "short_name" : "Champ de Mars",
          "types" : [ "establishment", "point_of_interest" ]
        },
        {
          ...
          > content(map_search)
          $results
          $results[[1]]
          $results[[1]]$address_components
          $results[[1]]$address_components[[1]]
          $results[[1]]$address_components[[1]]$long_name
          [1] "Champ de Mars"

          $results[[1]]$address_components[[1]]$short_name
          [1] "Champ de Mars"

          $results[[1]]$address_components[[1]]$types
          $results[[1]]$address_components[[1]]$types[[1]]
          [1] "establishment"

          $results[[1]]$address_components[[1]]$types[[2]]
          [1] "point_of_interest"



          $results[[1]]$address_components[[2]]
          $results[[1]]$address_components[[2]]$long_name
          [1] "5"

          $results[[1]]$address_components[[2]]$short_name
          [1] "5"

          $results[[1]]$address_components[[2]]$types
          $results[[1]]$address_components[[2]]$types[[1]]
          [1] "street_number"



          $results[[1]]$address_components[[3]]
          $results[[1]]$address_components[[3]]$long_name
          [1] "Avenue Anatole France"

          $results[[1]]$address_components[[3]]$short_name
          [1] "Avenue Anatole France"

          $results[[1]]$address_components[[3]]$types
          $results[[1]]$address_components[[3]]$types[[1]]
          [1] "route"



          $results[[1]]$address_components[[4]]
          $results[[1]]$address_components[[4]]$long_name
          [1] "Paris"

          $results[[1]]$address_components[[4]]$short_name
          [1] "Paris"

          $results[[1]]$address_components[[4]]$types
          $results[[1]]$address_components[[4]]$types[[1]]
          [1] "locality"

          $results[[1]]$address_components[[4]]$types[[2]]
          [1] "political"



          $results[[1]]$address_components[[5]]
          $results[[1]]$address_components[[5]]$long_name
          [1] "Paris"

          $results[[1]]$address_components[[5]]$short_name
          [1] "Paris"

          $results[[1]]$address_components[[5]]$types
          $results[[1]]$address_components[[5]]$types[[1]]
          [1] "administrative_area_level_2"

          $results[[1]]$address_components[[5]]$types[[2]]
          [1] "political"



          $results[[1]]$address_components[[6]]
          $results[[1]]$address_components[[6]]$long_name
          [1] "?le-de-France"

          $results[[1]]$address_components[[6]]$short_name
          [1] "?le-de-France"

          $results[[1]]$address_components[[6]]$types
          $results[[1]]$address_components[[6]]$types[[1]]
          [1] "administrative_area_level_1"

          $results[[1]]$address_components[[6]]$types[[2]]
          [1] "political"



          $results[[1]]$address_components[[7]]
          $results[[1]]$address_components[[7]]$long_name
          [1] "France"

          $results[[1]]$address_components[[7]]$short_name
          [1] "FR"

          $results[[1]]$address_components[[7]]$types
          $results[[1]]$address_components[[7]]$types[[1]]
          [1] "country"

          $results[[1]]$address_components[[7]]$types[[2]]
          [1] "political"



          $results[[1]]$address_components[[8]]
          $results[[1]]$address_components[[8]]$long_name
          [1] "75007"

          $results[[1]]$address_components[[8]]$short_name
          [1] "75007"

          $results[[1]]$address_components[[8]]$types
          $results[[1]]$address_components[[8]]$types[[1]]
          [1] "postal_code"




          $results[[1]]$formatted_address
          [1] "Champ de Mars, 5 Avenue Anatole France, 75007 Paris, France"

          $results[[1]]$geometry
          $results[[1]]$geometry$location
          $results[[1]]$geometry$location$lat
          [1] 48.85837

          $results[[1]]$geometry$location$lng
          [1] 2.294481


          $results[[1]]$geometry$location_type
          [1] "ROOFTOP"

          $results[[1]]$geometry$viewport
          $results[[1]]$geometry$viewport$northeast
          $results[[1]]$geometry$viewport$northeast$lat
          [1] 48.85972

          $results[[1]]$geometry$viewport$northeast$lng
          [1] 2.29583


          $results[[1]]$geometry$viewport$southwest
          $results[[1]]$geometry$viewport$southwest$lat
          [1] 48.85702

          $results[[1]]$geometry$viewport$southwest$lng
          [1] 2.293132




          $results[[1]]$place_id
          [1] "ChIJLU7jZClu5kcR4PcOOO6p3I0"

          $results[[1]]$types
          $results[[1]]$types[[1]]
          [1] "establishment"

          $results[[1]]$types[[2]]
          [1] "point_of_interest"

          $results[[1]]$types[[3]]
          [1] "premise"




          $status
          [1] "OK"

content(map_search)
content(map_search)$results[[1]]$formatted_address
content(map_search)$results[[1]]$geometry$location$lat#48.8583
content(map_search)$results[[1]]$geometry$location$lng#2.294481

library(rjson)
ml_book <- list(book_title = "Machine Learning with R",
                  author = "Brett Lantz")
toJSON(ml_book)

ml_book_json <- "{
\"title\": \"Machine Learning with R\",
\"author\": \"Brett Lantz\",
\"publisher\": {
\"name\": \"Packt Publishing\",
\"url\": \"https://www.packtpub.com\"
},
\"topics\": [\"R\", \"machine learning\", \"data mining\"],
\"MSRP\": 54.99
}"

ml_book_r <- fromJSON(ml_book_json)
str(ml_book_r)
#add_header
GET("http://httpbin.org/headers")
# Add arbitrary headers
GET("http://httpbin.org/headers",
    add_headers(version = version$version.string))
# Override default headers with empty strings
GET("http://httpbin.org/headers", add_headers(Accept = ""))

GET("http://httpbin.org/basic-auth/user/passwd")
GET("http://httpbin.org/basic-auth/user/passwd",
    authenticate("user", "passwd"))

BROWSE("http://google.com")
BROWSE("http://had.co.nz")

# * you can add directly to a request
HEAD("https://www.google.com", verbose())
# * you can wrap with with_config()
with_config(verbose(), HEAD("https://www.google.com"))
# * you can set global with set_config()
old <- set_config(verbose())
HEAD("https://www.google.com")
# and re-establish the previous settings with
set_config(old, override = TRUE)
HEAD("https://www.google.com")
# or
reset_config()
HEAD("https://www.google.com")
# If available, you should use a friendly httr wrapper over RCurl
# options. But you can pass Curl options (as listed in httr_options())
# in config
HEAD("https://www.google.com/", config(verbose = TRUE))

# Expires after 5 seconds
## Not run:
r4 <- GET("http://httpbin.org/cache/5")
cache_info(r4)
r4$date
rerequest(r4)$date
Sys.sleep(5)
cache_info(r4)
rerequest(r4)$date
## End(Not run)

#content
GET("http://httpbin.org/headers")
GET("http://httpbin.org/headers", accept_json())
GET("http://httpbin.org/headers", accept("text/csv"))
GET("http://httpbin.org/headers", accept(".doc"))
GET("http://httpbin.org/headers", content_type_xml())
GET("http://httpbin.org/headers", content_type("text/csv"))
GET("http://httpbin.org/headers", content_type(".xml"))

facebook <- oauth_endpoint(
  authorize = "https://www.facebook.com/dialog/oauth",
  access = "https://graph.facebook.com/oauth/access_token")

facebook

library(rvest)
# A file with bad encoding included in the package
path <- system.file("html-ex", "bad-encoding.html", package = "rvest")
x <- read_html(path)
x %>% html_nodes("p") %>% html_text()
guess_encoding(x)
# Two valid encodings, only one of which is correct
read_html(path, encoding = "ISO-8859-1") %>% html_nodes("p") %>% html_text()
read_html(path, encoding = "ISO-8859-2") %>% html_nodes("p") %>% html_text()

# From a url:
google <- read_html("http://google.com", encoding = "ISO-8859-1")
google %>% xml_structure()
google %>% html_nodes("div")
# From a string: (minimal html 5 document)
# http://www.brucelawson.co.uk/2010/a-minimal-html5-document/
minimal <- read_html("<!doctype html>
                     <meta charset=utf-8>
                     <title>blah</title>
                     <p>I'm the content")
minimal
minimal %>% xml_structure()
# From an httr request
google2 <- read_html(httr::GET("http://google.com"))

#html_nodes
# CSS selectors ----------------------------------------------
ateam <- read_html("http://www.boxofficemojo.com/movies/?id=ateam.htm")
html_nodes(ateam, "center")
html_nodes(ateam, "center font")
html_nodes(ateam, "center font b")
# But html_node is best used in conjunction with %>% from magrittr
# You can chain subsetting:
ateam %>% html_nodes("center") %>% html_nodes("td")
ateam %>% html_nodes("center") %>% html_nodes("font")
td <- ateam %>% html_nodes("center") %>% html_nodes("td")
td
# When applied to a list of nodes, html_nodes() returns all nodes,
# collapsing results into a new nodelist.
td %>% html_nodes("font")
# html_node() returns the first matching node. If there are no matching
# nodes, it returns a "missing" node
if (utils::packageVersion("xml2") > "0.1.2") {
  td %>% html_node("font")
}
# To pick out an element at specified position, use magrittr::extract2
# which is an alias for [[
library(magrittr)
ateam %>% html_nodes("table") %>% extract2(1) %>% html_nodes("img")
ateam %>% html_nodes("table") %>% `[[`(1) %>% html_nodes("img")
# Find all images contained in the first two tables
ateam %>% html_nodes("table") %>% `[`(1:2) %>% html_nodes("img")
ateam %>% html_nodes("table") %>% extract(1:2) %>% html_nodes("img")
# XPath selectors ---------------------------------------------
# chaining with XPath is a little trickier - you may need to vary
# the prefix you're using - // always selects from the root noot
# regardless of where you currently are in the doc
ateam %>% html_nodes(xpath ="//center//font//b") %>% html_nodes(xpath = "//b")

library(httr)

yelp_endpoint <- oauth_endpoint(
  authorize = NULL,
  access    = "https://api.yelp.com/oauth2/token")

# 2. Register an application at https://www.yelp.com/developers/v3/manage_app
#    Replace key and secret below.
yelp_app <- oauth_app(
  appname = "infoapp",
  key = "bvmjj2EOBvOknQ",
  secret = "n8ueSvTNdlE0BDDJpLljvmgUGUw")

# 3. Get OAuth credentials using client credential grant
#    Yelp do not use basic auth. Use `use_basic_auth = T` otherwise
yelp_token <- oauth2.0_token(
  endpoint = yelp_endpoint,
  app = yelp_app,
  client_credentials = T
)

# 4. Use API
url <- modify_url(
  url = "https://api.yelp.com",
  path = c("v3", "businesses", "search"),
  query = list(
    term = "coffee",
    location = "Vancouver, BC",
    limit = 3
  )
)

req <- GET(url, config(token = token))
stop_for_status(req)
content(req)

Client ID
xy8g2CWmhScQ946xSyRUAw

API Key
AdPRCrzAD4-vcyIuvxQXw1CQNQh-iKof4cjET2_N3rNtaxbIFG1ZRryWsy7QZB4KIs6jZriOKP25dE9GMjBc2tT9omS5TwLXdaxf0JX7oK2n1JHRuDemXlO7ZJBYW3Yx

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at 
#    https://github.com/settings/developers. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("github",
                   key = "56b637a5baffac62cad9",
                   secret = "8e107541ae1791259e9987d544ca568633da2ebf")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)

# OR:
req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
stop_for_status(req)
content(req)