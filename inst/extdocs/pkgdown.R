require(desc)
require(devtools)

dd <- description$new()

me <- person(given = 'Przemyslaw', 'Stempor', email = 'ps562@cam.ac.uk', role = c("aut", "cph", "cre"))
dd$set_authors(me)

dd$normalize()
dd$reformat_fields()
dd$reorder_fields()
dd$print()

dd$write()

library(pkgdown)
build_site()
