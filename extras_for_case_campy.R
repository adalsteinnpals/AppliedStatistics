## Lead-in for case campy
##
## Looking at some useful commands ... 

a<-sample(10,20,replace=TRUE)
b<-3:5

## Finding matches between vectors (of any type):
match(a,b) ## NA returned when no match.
match(b,a) ## Matches first occurence!

## If wanting logical indexes 
a %in% b ## TRUE when the corresponding element in a is in b
b %in% a ## Reverse :-)

a
unique(a) ## Vector of unique values in order of appearance
duplicated(a) ## TRUE when a previous element has the same value
!duplicated(a) ## FALSE when a previous element has the same value
a[!duplicated(a)] ## Same as unique ...

## Working with strings:
c<-"These are words"
substr(c,1,9)
paste(substr(c,1,9),"some",substr(c,11,16),sep=" - ")

grep() ## Is also useful for finding patterns ...

## Dates can be coded in many ways ...
as.Date("12/98/27",format="%m/%y/%d") ## Even silly ones ;-)
as.Date("129827",format="%m%y%d") ## Even silly ones ;-)
as.Date("12199827",format="%m%Y%d") ## Even silly ones ;-)
as.numeric(as.Date("12/98/27",format="%m/%y/%d"))

## Note that week one of a year is the week with the first Thursday!

strptime() ## may also be a good source of information ...

