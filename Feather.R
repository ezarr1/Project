
###------------------------------------------###
#---          write feather files        -----
###------------------------------------------###

write_feather(Loans, "Feather_Files/Loans.feather")
write_feather(Link_loans_counterparties, "Feather_Files/Link_loans_counterparties.feather")
write_feather(Counterparties, "Feather_Files/Counterparties.feather")
write_feather(Link_counterparties_entities, "Feather_Files/Link_counterparties_entities.feather")
write_feather(Entities, "Feather_Files/Entities.feather")

###------------------------------------------###
#---         read feather files        -----
###------------------------------------------###

Loans_feather <- read_feather("Feather_Files/Loans.feather")
Link_loans_counterparties_feather <- read_feather("Feather_Files/Link_loans_counterparties.feather")
Counterparties_feather <- read_feather("Feather_Files/Counterparties.feather")
Link_counterparties_entities_feather <- read_feather("Feather_Files/Link_counterparties_entities.feather")
Entities_feather <- read_feather("Feather_Files/Entities.feather")

