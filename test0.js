pattern matching error<br><br>Line 106, col 23:
  105 |   List = DtypeList | TagList | GenericList
> 106 |   DtypeList = "[" "⟨" DtypeName "⟩" expr* "]"
                              ^~~~~~~~~
  107 |   TagList = "[" "«" TagName "»" expr* "]"
Errors:
- Line 106, col 23:
  105 |   List = DtypeList | TagList | GenericList
> 106 |   DtypeList = "[" "⟨" DtypeName "⟩" expr* "]"
                              ^~~~~~~~~
  107 |   TagList = "[" "«" TagName "»" expr* "]"
Rule DtypeName is not declared in grammar PseudoCode
- Line 107, col 21:
  106 |   DtypeList = "[" "⟨" DtypeName "⟩" expr* "]"
> 107 |   TagList = "[" "«" TagName "»" expr* "]"
                            ^~~~~~~
  108 |   GenericList = "[" "*" expr* "]"
Rule TagName is not declared in grammar PseudoCode
