

library(tameDP)
library(tidyverse)


dp<-tame_dp("COP20Approval\\Data Pack_South Africa_20200223205229_v43 Final 1130pm.xlsx")


write_tsv(dp,"COP20Approval\\psnuxim_v43final1130pm.txt",na="")
