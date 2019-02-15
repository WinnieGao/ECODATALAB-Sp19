veh = read.csv("vehpub.csv")
hh = read.csv("hhpub.csv")

for(i in 1:nrow(hh)){
  hh$BESTMILE[i]=sum(veh[veh$HOUSEID==hh[i,1],"BESTMILE"])
} ##we put "BESTMILE" in "hh"

write.csv(hh,"recs.csv")