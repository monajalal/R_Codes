v = readLines(con="jalalQ1.txt")

v0=grep(pattern="^[A-C]",x=v)
#grep(pattern="^[aeiou]$",x=v)
v1=sapply(v, function(x){
  gsub("\\s[0-9].*", "", grep("[aeiou]\\s", x, value = TRUE, invert = TRUE))
})

sapply(v, function(x){ gsub(".*[0-9]\\s", "", grep("[aeiou]{2}", x, value = TRUE, invert = FALSE)) })


> zz <- sapply(v, function(x) gsub(".*[0-9]\\s", "", x))
> length(unlist(sapply(zz, function(f) grep("[aeiou]{2}", f))))
[1] 8
#r'\b([^aeiou]*)
r'\b[^aeoui]*'
yy=sub(r'\b[aeiou][^aeiou]*',r'\b[^aeiou][aeiou]*',v)
length(unlist(sapply(yy, function(y) grep("[aeiou]{2}", y))))

yy=sub("\\b([­aeiou])([^­aeious])",­"\\2\\1","abmm­")

rep=sub("\\b([aeiou](^aeious))","\\2\\1","abmm")
buf <- sapply(v, function(f) gsub(".*[0-9]\\s", "", f))
reversed=sub("([aeiou])([^aeiou])","\\2\\1",buf)
length(unlist(sapply(reversed, function(rev) grep("[aeiou]{2}", rev))))
