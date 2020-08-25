#https://github.com/JimboMahoney/rIG

#################################
### Loading Required Packages
#################################

if (!require("rlist")) {
  install.packages("rlist", dependencies = TRUE)
  library(rlist)
}

if (!require("assertthat")) {
  install.packages("assertthat", dependencies = TRUE)
  library(assertthat)
}

if (!require("glue")) {
  install.packages("glue", dependencies = TRUE)
  library(glue)
}

if (!require("httr")) {
  install.packages("httr", dependencies = TRUE)
  library(httr)
}

if (!require("purrr")) {
  install.packages("purrr", dependencies = TRUE)
  library(purrr)
}

if (!require("jsonlite")) {
  install.packages("jsonlite", dependencies = TRUE)
  library(jsonlite)
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to authenticate ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ig_login <- function(body.input, api.key = ""){
  
  r <- httr::POST(url = "https://api.ig.com/gateway/deal/session",
                  httr::add_headers(`VERSION` = "2",
                                    `X-IG-API-KEY` = api.key),
                  httr::content_type_json(),
                  body = body.input,
                  encode = "json")
  
  if(r$status_code == 200){print("Login is good")} else {(print("BAD LOGIN - PLEASE CHECK LOGIN DETAILS"))}
  
  httr::stop_for_status(r)
  httr::content(r, "parsed", "application/json", encoding="UTF-8")
  
  # save access tokens
  cst     <<- r$headers$cst
  token   <<- r$headers$`x-security-token`
  api_key <<- api.key
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Function to logout ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ig_logout <- function(){
  
  r <- httr::DELETE("https://api.ig.com/gateway/deal/session",
                    httr::add_headers(`VERSION` = "1",
                                      `X-IG-API-KEY` = api_key,
                                      `CST` = cst,
                                      `X-SECURITY-TOKEN` = token),
                    httr::content_type_json())
  
  if(r$status_code == 204){print("Logout successful")} else {(print("BAD LOGOUT - PLEASE CHECK LOGIN DETAILS"))}
  
  httr::stop_for_status(r)
  httr::content(r, "parsed", "application/json", encoding="UTF-8")
  
  if(r$status_code == 204){
    # clear tokens
    cst     <<- 0
    token   <<- 0
    api_key <<- 0}
  
}

#####################
### Login/Logout
#####################

b   <- '{"identifier":"your_username", "password": "your_password"}'
api <- "your_API_key"

ig_login(b, api) 


##############################
### Data Request Function
##############################

make_ig_request <- function(path, api_version, query = NULL) {
  request_url <- httr::modify_url(
    url = glue::glue("https://api.ig.com"),
    path = file.path("gateway", "deal", path),
    query = query
  )
  
  response <- httr::GET(
    url = request_url,
    config = httr::add_headers(
      VERSION = api_version,
      `X-IG-API-KEY` = api_key,
      CST = cst,
      `X-SECURITY-TOKEN` = token
    )
  )
  
  assertthat::assert_that(response$status_code == 200,
                          msg = glue::glue("Response code: {httr::content(response)[[1]]}")
  )
  
  response
}

##############################
### Get Positions Function
##############################

get_current_positions <- function() {
  pos <- make_ig_request(path = "positions", api_version = 2) %>% httr::content()
  
  size <- pos$positions %>% purrr::map_dbl(list("position", "size"))
  
  opening_prices <- pos$positions %>% purrr::map_dbl(list("position", "level"))
  
  direction <- pos$positions %>%
    purrr::map_chr(list("position", "direction")) %>%
    purrr::modify(~ dplyr::if_else(. == "BUY", 1, -1)) %>%
    as.numeric()
  
  positions <- size * direction
  
  epics <- pos$positions %>% purrr::map_chr(list("market", "epic"))
  
  tibble::tibble(epics, positions, opening_prices)
}

#################################
### Get Positions
#################################

IGPositions <- get_current_positions()

#####################################
### Get prices / index / epic details
#####################################

# Returns a list of details from each market in the positions

for (i in IGPositions$epics){
  temp <- make_ig_request(path = paste0("markets/", i), api_version = 1) %>% httr::content()
  assign(i, temp)
}

# Get relevant details from each of these markets
Expiry <- NULL
IndexName <- NULL
IndexPrice <- NULL
for (i in IGPositions$epics){
  Expiry <- c(Expiry, get(i)$instrument$expiry)
  IndexName <- c(IndexName, get(i)$instrument$name)
  IndexPrice <- c(IndexPrice, get(i)$snapshot$bid)
  
}

# Create Profit/Loss
ProfitLoss <- (IndexPrice - IGPositions$opening_prices) * IGPositions$positions

# Create new table like IG
IGSummary <- as.data.frame(cbind(IndexName, Expiry, IGPositions$positions, IGPositions$opening_prices, IndexPrice, round(ProfitLoss, 2)))
# Rename columns
colnames(IGSummary) <- c("Market", "Expiry", "Size", "Opening", "Latest", "Profit/Loss")

########################
### Get balance
########################

Account_Details <- make_ig_request(path = "accounts", api_version = 1) %>% httr::content()

# Account #1 here is CFD
# Account #2 is Spread Bet

# Account Balance / Deposit / PnL / Available
Account_Details$accounts[[2]]$balance

# IG Margin
Account_Details$accounts[[2]]$balance$deposit
# IG Cash
Account_Details$accounts[[2]]$balance$available


########################
### Logout
########################

ig_logout() # this will end your session


