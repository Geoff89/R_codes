library(httr)

Client ID
d9ed2894659275c60b0c
Client Secret
e11a96d1270c39cfc7e6e8962f2048dc550b4c3b



#finding github OAuth settings
oauth_endpoints("github")
#authorize: https://github.com/login/oauth/authorize
#access:    https://github.com/login/oauth/access_token
oauth_endpoints("twitter")
#request:   https://api.twitter.com/oauth/request_token
#authorize: https://api.twitter.com/oauth/authenticate
#access:    https://api.twitter.com/oauth/access_token
oauth_endpoints("facebook")
#authorize: https://www.facebook.com/dialog/oauth
#access:    https://graph.facebook.com/oauth/access_token
 
oauth_endpoints("google")
#authorize: https://accounts.google.com/o/oauth2/auth
#access:    https://accounts.google.com/o/oauth2/token
#validate:  https://www.googleapis.com/oauth2/v1/tokeninfo
#revoke:    https://accounts.google.com/o/oauth2/revoke
oauth_endpoints("vimeo")
#request:   https://vimeo.com/oauth/request_token
#authorize: https://vimeo.com/oauth/authorize
#access:    https://vimeo.com/oauth/access_token
oauth_endpoints("linkedin")
#authorize: https://www.linkedin.com/uas/oauth2/authorization
#access:    https://www.linkedin.com/uas/oauth2/accessToken


#regsiter your own application
myapp <- oauth_app("ducknet",
                   key = "d9ed2894659275c60b0c",
                   secret = "e11a96d1270c39cfc7e6e8962f2048dc550b4c3b"
                   )

#GEt oauth credentials
github_tokens <- oauth2.0_token(oauth_endpoints("github"), myapp)

#use the api
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)

#facebook api
app id: 197412854451559
secret key: 1670187bdc84e6f20523ae207d84800d

oauth_endpoints("facebook")

#registering my app
myapp <- oauth_app("jeffapp", "197412854451559", "1670187bdc84e6f20523ae207d84800d")


#get oauth credentials
facebook_token <- oauth2.0_token(oauth_endpoints("facebook"),
                                 myapp)

#use api to get the data
req <- GET("https://graph.facebook.com/me", config(token = facebook_token))
stop_for_status(req)
str(content(req))

req1 <- GET("https://graph.facebook.com/photos", config(token = facebook_token))
stop_for_status(req)
content(req1)
