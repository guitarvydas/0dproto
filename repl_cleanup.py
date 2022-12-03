import re
import sys
blankline = r'^$'
blanklinereplacement = ''
spuriouscomma = r', \)'
spuriouscommareplacement = ')'

for line in sys.stdin:
    newstr1 = re.sub (blankline, blanklinereplacement, line)
    newstr2 = re.sub (spuriouscomma, spuriouscommareplacement, newstr1)
    print (newstr2,end="")
