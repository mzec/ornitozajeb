
#####
# Funkcija za ispravljanje ornitozajeba
# Udruga Biom
# mzec 2015
# ver 1.2
#####

ornitozajeb <- function(vrsta, legit = NA) {
    library(stringdist)

    if (is.na(legit)) {
        legit <- read.csv(file = "legit.csv")
    }

    # mini-funkcija za traženje najbližeg stringa
    najblizi <- function(x, popis) {
        popis[amatch(x, popis, maxDist = Inf)]
    }
    
    # definira regex za ispravljanje dodanih autora i godine u ime vrste
    vrsta_autor_godina <- "(.*) (\\(.*|[A-Z].*)"
    
    # pronalazi neispravne unose među levelima faktora 'vrsta'
    nonlegit_index <- which(!(levels(vrsta) %in% legit_vrste))
    nonlegit <- data.frame(cbind(nonlegit = levels(vrsta)[nonlegit_index],
                                 fixed = NA), 
                           stringsAsFactors = FALSE)
    
    for (i in 1:nrow(nonlegit)) {
        krivi <- nonlegit$nonlegit[i]
        if (grepl(vrsta_autor_godina, krivi)) {
            srednji <- sub(vrsta_autor_godina, "\\1", krivi)
            cat(paste("Mičem autora i godinu:", krivi, "u:", srednji, "\n"))
        } else {
            srednji <- krivi
        }
        if (!is.element(srednji, legit)) {
            automatch <- najblizi(srednji, legit)
            automatch_prompt <- readline(prompt = paste(i, ">",
                                                        srednji, 
                                                        "\n -> ",
                                                        automatch,
                                                        "? (y/novi unos) > "))
            if (automatch_prompt == "y") {
                novi <- as.character(automatch)
                cat(paste("Mijenjam:", srednji, "u:", automatch, "\n"))
            } else if (automatch_prompt != "") {
                novi <- automatch_prompt
                cat(paste("Mijenjam:", krivi, "u:", automatch_prompt, "\n"))
            } else if (automatch_prompt == "") {
                novi <- srednji
                cat("Ostavljam isto ime\n")
            }        
        } else {
            novi <- srednji
        }
        nonlegit$fixed[i] <- novi
    }
    # ispravke <- nonlegit$fixed
    # names(ispravke) <- nonlegit$nonlegit
    return(nonlegit)
}
