rm(list=ls())
require("XML")
# <a href="/music/The+Beatles/Sgt.+Pepper%27s+Lonely+Hearts+Club+Band" 
beatles = "http://www.last.fm/music/The+Beatles/"

beatles.albums.page = paste(sep="", beatles, "+albums")
lines = readLines(beatles.albums.page)
album.lines = grep(pattern="href.*link-reference", lines, value=TRUE)
album.names = sub(pattern=".*<h3>(.*)</h3>.*", replacement="\\1", x=album.lines)
album.names = gsub(pattern=" ", replacement="+", x=album.names)
album.names = gsub(pattern="'", replacement="%27", x=album.names)

for (album in album.names[1]) {
  print(album)
  album.link = paste(sep="", beatles, album)
  print(album.link)
  tables = readHTMLTable(album.link,header=FALSE)
  
}