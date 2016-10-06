# examples from https://github.com/hadley/httr/blob/master/demo/oauth2-github.r


library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")


# 2. To make your own application, register at at
#    https://github.com/settings/applications. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("github", key = "a1eea5b193cb664a44e1", 
                   secret = "e7152ca7135515f0f248678a16420b67c1b8d6d6")


# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
content(req)

# OR:
req <- with_config(gtoken, GET("https://api.github.com/users/jtleek/repos"))
stop_for_status(req)
content(req)

json1 <- content(req)

library(jsonlite)

json2 <- jsonlite::fromJSON(toJSON(json1)) 


#this is what worked
json4 <- fromJSON("https://api.github.com/users/jtleek/repos")

with(json4, subset(created_at, name == "datasharing" ))





