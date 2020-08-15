# rIG
Basic access to get IG Account Balance etc.

### Description
I wanted to create a method to get the basic details like cash balance and open positions without needing to login to IG.

### Usage
Use the code snippets attached.
<br>
<br>
Edit the Login/Logout Section with your username, password and API key:
```
#####################
### Login/Logout
#####################

b   <- '{"identifier":"your_username", "password": "your_password"}'
api <- "your_API_key"
```

### Limitations
This is a work in progress and it's unlikely I will add any functionality other than what I need.

Check IG's API guide [here](https://labs.ig.com/rest-trading-api-reference) for commands that you can probably use to modify the code to your needs.
<br><br>
They also have some examples in other programs / languages [here](https://labs.ig.com/sample-apps). The Excel is probably the easiest to get started with.

### Credits
Almost all the code is copied from other packages, namely:

[rIG](https://github.com/JoeFernando/rIG/blob/master/R/functions.R)
<br>
<br>
[OptionsAnalytics](https://github.com/zumthor86/OptionsAnalytics)
