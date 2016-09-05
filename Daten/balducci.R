balducci2000 <- function(adl, iadl, charlson){
    if (is.na(adl) | is.na(iadl) | is.na(charlson))
        return(NA)
    if (adl == 100)
        if (iadl == 8){
            if (charlson == 0)
                return("go_go")
            if (charlson < 3)
                return("slow_go")
        }
    if (adl == 100)
        if (charlson < 3)
            return("slow_go")
    return("no_go")
}

geon$balducci <- factor(NA, levels = c("go_go", "slow_go", "no_go"))

for (i in (1:nrow(geon))){
    geon$balducci[i] <- balducci2000(geon$barthel[i], geon$iadl[i],
                                     geon$charlson[i])
}
