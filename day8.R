rm(list=ls())

stopifnot(require("XML"))
# e.g. Here's code from a program to extract data from the NFL
# website. Note that
#   readLines(con = stdin())
# reads lines from a connection (a file, URL, or some other things),
# returning a character vector.
#   writeLines(text, con = stdout())
# is the inverse.

# From http://www.nfl.com/teams, get a list of 32 teams from lines like
#   ...<option value="/teams/greenbaypackers/statistics?team=GB">Statistics</option>
teams.page = readLines("http://www.nfl.com/teams") # 2151 lines of HTML
g = grep(pattern = "Statistics", x = teams.page)   # 32 indices
team.lines = teams.page[g]                         # 32 lines
# next: 32 abbreviations
team.abbreviations = sub(pattern = ".*team=(\\w*).*", replacement = "\\1", x=team.lines)

# loop through 32 NFL team statistics pages
n = length(team.abbreviations)
best.rushing = integer(n)   # vector of 32 zeros
best.receiving = integer(n) # vector of 32 zeros
i = 1
for (team in team.abbreviations[19]) {
  print(team)
  # make a link like "http://www.nfl.com/teams/statistics?team=GB"
  url = paste(sep="", "http://www.nfl.com/teams/statistics?team=", team)
  #print(url)
  tables = readHTMLTable(url)
  best.rushing[i]   = as.integer(as.character(tables[[3]][2, 3]))
  best.receiving[i] = as.integer(as.character(tables[[4]][2, 3]))
  i = i + 1
}

plot(x=best.rushing, y=best.receiving)