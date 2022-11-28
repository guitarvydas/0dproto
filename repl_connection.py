import re
import sys
pattern = r'⎨([^⇉]*) +⇉ +([^⎬]*)⎬'
replacement = r'⟨Connection ⟨xSender \1⟩ ⟨xReceiver \2⟩⟩'

for line in sys.stdin:
    newstr = re.sub (pattern, replacement, line)
    print (newstr,end="")
