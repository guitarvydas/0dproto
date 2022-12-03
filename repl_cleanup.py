import re
import sys
blankline = r'^$'
blanklinereplacement = ''

for line in sys.stdin:
    newstr = re.sub (blankline, blanklinereplacement, line)
    print (newstr,end="")
