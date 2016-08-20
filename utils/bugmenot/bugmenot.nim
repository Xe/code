import q
import xmltree

const
  html = staticRead "./bugmenot.html"

var
  doc = html.q
  ctr: int = 0

for elem in doc.select "kbd":
  ctr += 1

  if $elem == "<kbd>BugmenotBooru</kbd>" and ctr mod 2 == 1:
    ctr -= 1

  if ctr mod 2 == 1:
    for item in elem.items:
      echo item.text
