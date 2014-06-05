require("XML")
url="http://simple.wikipedia.org/wiki/List_of_U.S._states_by_area"
wiki_page=readLines(url)
length(wiki_page)
tables=readHTMLTable(url)