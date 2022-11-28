import re
import sys
str = '''                           ⎨✕in ⇉ ❲leaf 1❳ ✕in⎬
    ⎨✕in ⇉ ❲leaf 4❳ ✕in⎬'''
# print (str)
pattern = r'⎨([^ ⇉]*) +⇉ +([^⎬]*)⎬'
replacement = r'⟨Connection ⟨xSender \1⟩ ⟨xReceiver \2⟩⟩'

for line in sys.stdin:
    newstr = re.sub (pattern, replacement, line)
    print (newstr,end="")
