(format nil "~a.~a" <mark>owner's name</mark> name)

(format nil "~a.~a" <mark>eh.name</mark> name)

(format nil "~a.~a" (slot-value eh 'name) name)

Level0 {
  stuff = char+
  char = 
    | macro
	| any

  macro = "<mark>" (~"</mark>" any)+ "</mark>"
}

Level5 {
  stuff = char+
  char = 
    | macro
	| any

  macro = "<mark>" (~"." any)+ "." (~"</mark>" any)+ "</mark>"
}
