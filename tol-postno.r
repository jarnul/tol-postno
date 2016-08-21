#
# Tekijä: Jarno Varis
# Tehty: 20.8.2016
#
# Yksinkertainen R-koodinäyte, joka hakee Patentti- ja rekisterihallituksen avoimesta datasta
# postinumeron perusteella Y-tunnuksia alueelta, jotka on rekisteröity 1.1.2012 jälkeen.
# Hakee korkeintaan 1000 toimialaluokitustietoa.
#
# Lähteet:
# http://avoindata.prh.fi/
# http://www.stat.fi/meta/luokitukset/toimiala/001-2008/
#
# Kehitettävää:
# Käyttäjän syötteen tarkistus, toimialaluokituksiin tekstimuotoisten selitteiden lisäys,
# alkaneiden rekisteröintien alkamispäivämäärän.
#

# Vaatii pakkaukset RJSONIO ja RCurl JSON-tietojen hakuun, XML toimialaluokitusten tietojen hakuun

tiedosto <- "output.txt"

library(RJSONIO)
library(RCurl)
library(XML)
library(stringr)

postinumero <- readline(prompt="Syötä postinumero: ")

url <- paste("http://avoindata.prh.fi:80/bis/v1?totalResults=false&maxResults=1000&resultsFrom=0&streetAddressPostCode=", postinumero, "&companyRegistrationFrom=2012-01-01", sep="")

# haetaan tiedot
raakadata <- getURL(url)

# muutetaan raakadata JSON-muotoon
data <- fromJSON(raakadata)

# viedään rivit data frameen
lopul_data <- do.call(rbind, data[["results"]])

# tyhjä frame-pohja toimialaluokituksien hakuun
tol_tulos <- data.frame(ytunnus = character(),
                        tol = character()
                        )

iter <- 1
counter <- 1

for (rivi in lopul_data[, "detailsUri"]) {
  temp <- getURL(rivi)
  temp <- fromJSON(temp)
  
  # haetaan JSON-datasta Y-tunnus
  temp_ytun <- temp[["results"]][[1]]$businessId
  
  # haetaan toimialaluokitus, jos tieto löytyy
  if (length(temp[["results"]][[1]]$businessLines) != 0) {
    temp_tol <- temp[["results"]][[1]]$businessLines[[1]]$code
    tol_tulos <- rbind(tol_tulos, data.frame(temp_ytun, temp_tol))
  }
  
  # jottei avoimen datan palveluun tule liikaa pyyntöjä liian nopeasti
  if (iter >= 200) {
    print("Odotetaan minuutti, ettei palvelu ruuhkaudu liikaa")
    Sys.sleep(60)
    iter <- 0
  }
  
  # tuloksien seurantaan
  print(cat("Kaikki: ", length(lopul_data[, "detailsUri"]), " Käsitelty: ", counter))
  iter <- iter + 1
  counter <- counter + 1
}

tol_lyh <- as.numeric(substr(tol_tulos[,2], 1, 2))

tulos <- as.data.frame(table(tol_lyh))

# haetaan vielä selkokieliest nimet toimialaluokille
html_tol <- readHTMLTable("http://www.stat.fi/meta/luokitukset/toimiala/001-2008/koko_luokitus.html")[[1]]
html_tol[] <- lapply(html_tol, as.character)

tol_luokat <- subset(html_tol, nchar(html_tol$V1) == 2)

names(tol_luokat)[1] <- "tol_lyh"
names(tol_luokat)[2] <- "kuvaus"

# poistetaan newlinet
tol_luokat$kuvaus <- str_replace_all(tol_luokat$kuvaus, "\n", "")

# yhdistetään luokkakuvaukset tuloksiin
lopul <- merge(x = tulos, y = tol_luokat, by = "tol_lyh", all.x = TRUE)

# tulosten tulostus
print(paste("Alla haetun postinumeron", postinumero, "toimialaluokitukset: "))

print(lopul[order(-lopul$Freq),], row.names = FALSE)

write.table(lopul[order(-lopul$Freq),], tiedosto, sep="\t", row.names = FALSE)
