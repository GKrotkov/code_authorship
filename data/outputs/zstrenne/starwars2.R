library(striprtf)



file_name <- '/Users/zachstrennen/Documents/starwars4.txt'
starwars4_txt <- tolower(readChar(file_name, file.info(file_name)$size))
file_name <- '/Users/zachstrennen/Documents/starwars7.txt'
starwars7_txt <- tolower(readChar(file_name, file.info(file_name)$size))
starwars7_txt <- gsub("maz\'s castle", "mos eisley", starwars7_txt, fixed=TRUE)
starwars4_txt <- gsub(".", "  ", starwars4_txt, fixed=TRUE)
starwars7_txt <- gsub(".", "  ", starwars7_txt, fixed=TRUE)
starwars4_txt <- gsub(":", "  ", starwars4_txt, fixed=TRUE)
starwars7_txt <- gsub(":", "  ", starwars7_txt, fixed=TRUE)
starwars4_txt <- gsub("(", "  ", starwars4_txt, fixed=TRUE)
starwars7_txt <- gsub("(", "  ", starwars7_txt, fixed=TRUE)
starwars4_txt <- gsub(")", "  ", starwars4_txt, fixed=TRUE)
starwars7_txt <- gsub(")", "  ", starwars7_txt, fixed=TRUE)
starwars4_txt <- gsub("\'", "", starwars4_txt, fixed=TRUE)
starwars7_txt <- gsub("\'", "", starwars7_txt, fixed=TRUE)
starwars4_txt <- gsub("death star", "deathstar", starwars4_txt, fixed=TRUE)
starwars7_txt <- gsub("starkiller base", "deathstar", starwars7_txt, fixed=TRUE)
starwars7_txt <- gsub("starkiller", "deathstar", starwars7_txt, fixed=TRUE)
starwars7_txt <- gsub("jakku", "tatooine", starwars7_txt, fixed=TRUE)
starwars4_txt <- gsub("u2028", " ", starwars4_txt, fixed=TRUE)
starwars7_txt <- gsub("u2028", " ", starwars7_txt, fixed=TRUE)
starwars4_txt <- gsub("\n", " ", starwars4_txt)
starwars4_txt <- gsub("\t", " ", starwars4_txt)
starwars7_txt <- gsub("\n", " ", starwars7_txt)
starwars7_txt <- gsub("\t", " ", starwars7_txt)
starwars4_txt <- gsub('[[:punct:] ]+',' ',starwars4_txt)
starwars7_txt <- gsub('[[:punct:] ]+',' ',starwars7_txt)

file_name <- '/Users/zachstrennen/Documents/dune1984.txt'
dune1984_txt <- tolower(readChar(file_name, file.info(file_name)$size))
file_name <- '/Users/zachstrennen/Documents/dune2021.txt'
dune2021_txt <- tolower(readChar(file_name, file.info(file_name)$size))
dune1984_txt <- gsub(".", " ", dune1984_txt, fixed=TRUE)
dune2021_txt <- gsub(".", " ", dune2021_txt, fixed=TRUE)
dune1984_txt <- gsub("u2028", " ", dune1984_txt, fixed=TRUE)
dune2021_txt <- gsub("u2028", " ", dune2021_txt, fixed=TRUE)
dune1984_txt <- gsub("[0-9]+", " ", dune1984_txt)
dune2021_txt <- gsub("[0-9]+", " ", dune2021_txt)
dune2021_txt <- gsub("Salmon Rev. (06/19/2020)", " ", dune2021_txt)
dune1984_txt <- gsub("\n", " ", dune1984_txt)
dune1984_txt <- gsub("\t", " ", dune1984_txt)
dune2021_txt <- gsub("\n", " ", dune2021_txt)
dune2021_txt <- gsub("\t", " ", dune2021_txt)
dune1984_txt <- gsub("\'", "", dune1984_txt, fixed=TRUE)
dune2021_txt <- gsub("\'", "", dune2021_txt, fixed=TRUE)
dune1984_txt <- gsub('[[:punct:] ]+',' ',dune1984_txt)
dune2021_txt <- gsub('[[:punct:] ]+',' ',dune2021_txt)

file_name <- '/Users/zachstrennen/Documents/raidersofthelostark.txt'
raiders_txt <- tolower(readChar(file_name, file.info(file_name)$size))
file_name <- '/Users/zachstrennen/Documents/kingdomofthecrystalskull.txt'
crystal_txt <- tolower(readChar(file_name, file.info(file_name)$size))
raiders_txt <- gsub(".", " ", raiders_txt, fixed=TRUE)
crystal_txt <- gsub(".", " ", crystal_txt, fixed=TRUE)
raiders_txt <- gsub("u2028", " ", raiders_txt, fixed=TRUE)
crystal_txt <- gsub("u2028", " ", crystal_txt, fixed=TRUE)
raiders_txt <- gsub("\n", " ", raiders_txt)
raiders_txt <- gsub("\t", " ", raiders_txt)
crystal_txt <- gsub("\n", " ", crystal_txt)
crystal_txt <- gsub("\t", " ", crystal_txt)
crystal_txt <- gsub("\'", "", crystal_txt, fixed=TRUE)
raiders_txt <- gsub("\'", "", raiders_txt, fixed=TRUE)
crystal_txt <- gsub('[[:punct:] ]+',' ',crystal_txt)
raiders_txt <- gsub('[[:punct:] ]+',' ',raiders_txt)

#https://www.dailyscript.com/scripts/Charlies_Angels.pdf
file_name <- '/Users/zachstrennen/Documents/charlies1.txt'
charlies1 <- tolower(readChar(file_name, file.info(file_name)$size))
charlies1 <- gsub("\n", " ", charlies1)
charlies1 <- gsub("\t", " ", charlies1)
charlies1 <- gsub(".", " ", charlies1, fixed=TRUE)
charlies1 <- gsub("u2028", " ", charlies1, fixed=TRUE)
charlies1 <- gsub("[0-9]+", " ", charlies1)
charlies1 <- gsub("\'", "", charlies1, fixed=TRUE)
charlies1 <- gsub('[[:punct:] ]+',' ',charlies1)
#https://imsdb.com/scripts/Charlie%27s-Angels.html
file_name <- '/Users/zachstrennen/Documents/charlies2.txt'
charlies2 <- tolower(readChar(file_name, file.info(file_name)$size))
charlies2 <- gsub("\n", " ", charlies2)
charlies2 <- gsub("\t", " ", charlies2)
charlies2 <- gsub(".", " ", charlies2, fixed=TRUE)
charlies2 <- gsub("u2028", " ", charlies2, fixed=TRUE)
charlies2 <- gsub("[0-9]+", " ", charlies2)
charlies2 <- gsub("\'", "", charlies2, fixed=TRUE)
charlies2 <- gsub('[[:punct:] ]+',' ',charlies2)

# https://www.scripts.com/script/a_star_is_born_18762
file_name <- '/Users/zachstrennen/Documents/starisborn1.txt'
starisborn1 <- tolower(readChar(file_name, file.info(file_name)$size))
starisborn1 <- gsub("\n", " ", starisborn1)
starisborn1 <- gsub("\t", " ", starisborn1)
starisborn1 <- gsub(".", " ", starisborn1, fixed=TRUE)
starisborn1 <- gsub("u2028", " ", starisborn1, fixed=TRUE)
starisborn1 <- gsub("[0-9]+", " ", starisborn1)
starisborn1 <- gsub("\'", "", starisborn1, fixed=TRUE)
starisborn1 <- gsub('[[:punct:] ]+',' ',starisborn1)
#https://d2bu9v0mnky9ur.cloudfront.net/academy2018/asib/screenplay/asib_wbfomat.pdf
file_name <- '/Users/zachstrennen/Documents/starisborn2.txt'
starisborn2 <- tolower(readChar(file_name, file.info(file_name)$size))
starisborn2 <- gsub("\n", " ", starisborn2)
starisborn2 <- gsub("\t", " ", starisborn2)
starisborn2 <- gsub(".", " ", starisborn2, fixed=TRUE)
starisborn2 <- gsub("u2028", " ", starisborn2, fixed=TRUE)
starisborn2 <- gsub("[0-9]+", " ", starisborn2)
starisborn2 <- gsub("\'", "", starisborn2, fixed=TRUE)
starisborn2 <- gsub('[[:punct:] ]+',' ',starisborn2)

#https://assets.scriptslug.com/live/pdf/scripts/mulan-1998.pdf
file_name <- '/Users/zachstrennen/Documents/mulan1.txt'
mulan1 <- tolower(readChar(file_name, file.info(file_name)$size))
mulan1 <- gsub("\n", " ", mulan1)
mulan1 <- gsub("\t", " ", mulan1)
mulan1 <- gsub(".", " ", mulan1, fixed=TRUE)
mulan1 <- gsub("u2028", " ", mulan1, fixed=TRUE)
mulan1 <- gsub("[0-9]+", " ", mulan1)
mulan1 <- gsub("\'", "", mulan1, fixed=TRUE)
mulan1 <- gsub('[[:punct:] ]+',' ',mulan1)
#https://www.the-editing-room.com/mulan-2020.html
file_name <- '/Users/zachstrennen/Documents/mulan2.txt'
mulan2 <- tolower(readChar(file_name, file.info(file_name)$size))
mulan2 <- gsub("\n", " ", mulan2)
mulan2 <- gsub("\t", " ", mulan2)
mulan2 <- gsub(".", " ", mulan2, fixed=TRUE)
mulan2 <- gsub("u2028", " ", mulan2, fixed=TRUE)
mulan2 <- gsub("[0-9]+", " ", mulan2)
mulan2 <- gsub("\'", "", mulan2, fixed=TRUE)
mulan2 <- gsub('[[:punct:] ]+',' ',mulan2)

#https://sfy.ru/?script=pet_sematary
file_name <-'/Users/zachstrennen/Documents/pet1.txt'
pet1 <- tolower(readChar(file_name, file.info(file_name)$size))
pet1 <- gsub("\n", " ", pet1)
pet1 <- gsub("\t", " ", pet1)
pet1 <- gsub(".", " ", pet1, fixed=TRUE)
pet1 <- gsub("u2028", " ", pet1, fixed=TRUE)
pet1 <- gsub("[0-9]+", " ", pet1)
pet1 <- gsub("\'", "", pet1, fixed=TRUE)
pet1 <- gsub('[[:punct:] ]+',' ',pet1)
#https://assets.scriptslug.com/live/pdf/scripts/pet-sematary-2019.pdf
file_name <-'/Users/zachstrennen/Documents/pet2.txt'
pet2 <- tolower(readChar(file_name, file.info(file_name)$size))
pet2 <- gsub("\n", " ", pet2)
pet2 <- gsub("\t", " ", pet2)
pet2 <- gsub(".", " ", pet2, fixed=TRUE)
pet2 <- gsub("u2028", " ", pet2, fixed=TRUE)
pet2 <- gsub("[0-9]+", " ", pet2)
pet2 <- gsub("\'", "", pet2, fixed=TRUE)
pet2 <- gsub('[[:punct:] ]+',' ',pet2)

#https://www.scripts.com/script-pdf/20539
file_name <-'/Users/zachstrennen/Documents/invisible1.txt'
invisible1 <- tolower(readChar(file_name, file.info(file_name)$size))
invisible1 <- gsub("\n", " ", invisible1)
invisible1 <- gsub("\t", " ", invisible1)
invisible1 <- gsub(".", " ", invisible1, fixed=TRUE)
invisible1 <- gsub("u2028", " ", invisible1, fixed=TRUE)
invisible1 <- gsub("[0-9]+", " ", invisible1)
invisible1 <- gsub("\'", "", invisible1, fixed=TRUE)
invisible1 <- gsub('[[:punct:] ]+',' ',invisible1)
#https://www.the-editing-room.com/the-invisible-man-2020.html
file_name <-'/Users/zachstrennen/Documents/invisible2.txt'
invisible2 <- tolower(readChar(file_name, file.info(file_name)$size))
invisible2 <- gsub("\n", " ", invisible2)
invisible2 <- gsub("\t", " ", invisible2)
invisible2 <- gsub(".", " ", invisible2, fixed=TRUE)
invisible2 <- gsub("u2028", " ", invisible2, fixed=TRUE)
invisible2 <- gsub("[0-9]+", " ", invisible2)
invisible2 <- gsub("\'", "", invisible2, fixed=TRUE)
invisible2 <- gsub('[[:punct:] ]+',' ',invisible2)

#https://imsdb.com/scripts/Evil-Dead.html
file_name <-'/Users/zachstrennen/Documents/evildead1.txt'
evildead1 <- tolower(readChar(file_name, file.info(file_name)$size))
evildead1 <- gsub("\n", " ", evildead1)
evildead1 <- gsub("\t", " ", evildead1)
evildead1 <- gsub(".", " ", evildead1, fixed=TRUE)
evildead1 <- gsub("u2028", " ", evildead1, fixed=TRUE)
evildead1 <- gsub("[0-9]+", " ", evildead1)
evildead1 <- gsub("\'", "", evildead1, fixed=TRUE)
evildead1 <- gsub('[[:punct:] ]+',' ',evildead1)
#https://assets.scriptslug.com/live/pdf/scripts/evil-dead-2013.pdf
file_name <-'/Users/zachstrennen/Documents/evildead2.txt'
evildead2 <- tolower(readChar(file_name, file.info(file_name)$size))
evildead2 <- gsub("\n", " ", evildead2)
evildead2 <- gsub("\t", " ", evildead2)
evildead2 <- gsub(".", " ", evildead2, fixed=TRUE)
evildead2 <- gsub("u2028", " ", evildead2, fixed=TRUE)
evildead2 <- gsub("[0-9]+", " ", evildead2)
evildead2 <- gsub("\'", "", evildead2, fixed=TRUE)
evildead2 <- gsub('[[:punct:] ]+',' ',evildead2)

#https://www.scripts.com/script-pdf/8544
file_name <-'/Users/zachstrennen/Documents/freaky1.txt'
freaky1 <- tolower(readChar(file_name, file.info(file_name)$size))
freaky1 <- gsub("\n", " ", freaky1)
freaky1 <- gsub("\t", " ", freaky1)
freaky1 <- gsub(".", " ", freaky1, fixed=TRUE)
freaky1 <- gsub("u2028", " ", freaky1, fixed=TRUE)
freaky1 <- gsub("[0-9]+", " ", freaky1)
freaky1 <- gsub("\'", "", freaky1, fixed=TRUE)
freaky1 <- gsub('[[:punct:] ]+',' ',freaky1)
#https://www.scripts.com/script-pdf/1426
file_name <-'/Users/zachstrennen/Documents/freaky2.txt'
freaky2 <- tolower(readChar(file_name, file.info(file_name)$size))
freaky2 <- gsub("\n", " ", freaky2)
freaky2 <- gsub("\t", " ", freaky2)
freaky2 <- gsub(".", " ", freaky2, fixed=TRUE)
freaky2 <- gsub("u2028", " ", freaky2, fixed=TRUE)
freaky2 <- gsub("[0-9]+", " ", freaky2)
freaky2 <- gsub("\'", "", freaky2, fixed=TRUE)
freaky2 <- gsub('[[:punct:] ]+',' ',freaky2)

#https://assets.scriptslug.com/live/pdf/scripts/overboard-1987.pdf
file_name <-'/Users/zachstrennen/Documents/overboard1.txt'
overboard1 <- tolower(readChar(file_name, file.info(file_name)$size))
overboard1 <- gsub("\n", " ", overboard1)
overboard1 <- gsub("\t", " ", overboard1)
overboard1 <- gsub(".", " ", overboard1, fixed=TRUE)
overboard1 <- gsub("u2028", " ", overboard1, fixed=TRUE)
overboard1 <- gsub("[0-9]+", " ", overboard1)
overboard1 <- gsub("\'", "", overboard1, fixed=TRUE)
overboard1 <- gsub('[[:punct:] ]+',' ',overboard1)
#https://assets.scriptslug.com/live/pdf/scripts/overboard-2018.pdf
file_name <-'/Users/zachstrennen/Documents/overboard2.txt'
overboard2 <- tolower(readChar(file_name, file.info(file_name)$size))
overboard2 <- gsub("\n", " ", overboard2)
overboard2 <- gsub("\t", " ", overboard2)
overboard2 <- gsub(".", " ", overboard2, fixed=TRUE)
overboard2 <- gsub("u2028", " ", overboard2, fixed=TRUE)
overboard2 <- gsub("[0-9]+", " ", overboard2)
overboard2 <- gsub("\'", "", overboard2, fixed=TRUE)
overboard2 <- gsub('[[:punct:] ]+',' ',overboard2)



#https://assets.scriptslug.com/live/pdf/scripts/spider-man-2002.pdf
file_name <-'/Users/zachstrennen/Documents/spiderman1.txt'
spiderman1 <- tolower(readChar(file_name, file.info(file_name)$size))
spiderman1 <- gsub("\n", " ", spiderman1)
spiderman1 <- gsub("\t", " ", spiderman1)
spiderman1 <- gsub(".", " ", spiderman1, fixed=TRUE)
spiderman1 <- gsub("u2028", " ", spiderman1, fixed=TRUE)
spiderman1 <- gsub("[0-9]+", " ", spiderman1)
spiderman1 <- gsub("\'", "", spiderman1, fixed=TRUE)
spiderman1 <- gsub('[[:punct:] ]+',' ',spiderman1)
#https://www.scriptslug.com/script/spider-man-2-2004
file_name <-'/Users/zachstrennen/Documents/spiderman2.txt'
spiderman2 <- tolower(readChar(file_name, file.info(file_name)$size))
spiderman2 <- gsub("\n", " ", spiderman2)
spiderman2 <- gsub("\t", " ", spiderman2)
spiderman2 <- gsub(".", " ", spiderman2, fixed=TRUE)
spiderman2 <- gsub("u2028", " ", spiderman2, fixed=TRUE)
spiderman2 <- gsub("[0-9]+", " ", spiderman2)
spiderman2 <- gsub("\'", "", spiderman2, fixed=TRUE)
spiderman2 <- gsub('[[:punct:] ]+',' ',spiderman2)

#https://www.scripts.com/script-pdf/13236
file_name <-'/Users/zachstrennen/Documents/mamamia1.txt'
mamamia1 <- tolower(readChar(file_name, file.info(file_name)$size))
mamamia1 <- gsub("\n", " ", mamamia1)
mamamia1 <- gsub("\t", " ", mamamia1)
mamamia1 <- gsub(".", " ", mamamia1, fixed=TRUE)
mamamia1 <- gsub("u2028", " ", mamamia1, fixed=TRUE)
mamamia1 <- gsub("[0-9]+", " ", mamamia1)
mamamia1 <- gsub("\'", "", mamamia1, fixed=TRUE)
mamamia1 <- gsub('[[:punct:] ]+',' ',mamamia1)
#https://www.the-editing-room.com/mamma-mia-here-we-go-again.html
file_name <-'/Users/zachstrennen/Documents/mamamia2.txt'
mamamia2 <- tolower(readChar(file_name, file.info(file_name)$size))
mamamia2 <- gsub("\n", " ", mamamia2)
mamamia2 <- gsub("\t", " ", mamamia2)
mamamia2 <- gsub(".", " ", mamamia2, fixed=TRUE)
mamamia2 <- gsub("u2028", " ", mamamia2, fixed=TRUE)
mamamia2 <- gsub("[0-9]+", " ", mamamia2)
mamamia2 <- gsub("\'", "", mamamia2, fixed=TRUE)
mamamia2 <- gsub('[[:punct:] ]+',' ',mamamia2)

#https://www.scriptslug.com/script/back-to-the-future-1985
file_name <-'/Users/zachstrennen/Documents/bttf1.txt'
bttf1 <- tolower(readChar(file_name, file.info(file_name)$size))
bttf1 <- gsub("\n", " ", bttf1)
bttf1 <- gsub("\t", " ", bttf1)
bttf1 <- gsub(".", " ", bttf1, fixed=TRUE)
bttf1 <- gsub("u2028", " ", bttf1, fixed=TRUE)
bttf1 <- gsub("[0-9]+", " ", bttf1)
bttf1 <- gsub("\'", "", bttf1, fixed=TRUE)
bttf1 <- gsub('[[:punct:] ]+',' ',bttf1)
#http://www.scifiscripts.com/scripts/backtothefuture3.txt
file_name <-'/Users/zachstrennen/Documents/bttf2.txt'
bttf2 <- tolower(readChar(file_name, file.info(file_name)$size))
bttf2 <- gsub("\n", " ", bttf2)
bttf2 <- gsub("\t", " ", bttf2)
bttf2 <- gsub(".", " ", bttf2, fixed=TRUE)
bttf2 <- gsub("u2028", " ", bttf2, fixed=TRUE)
bttf2 <- gsub("[0-9]+", " ", bttf2)
bttf2 <- gsub("\'", "", bttf2, fixed=TRUE)
bttf2 <- gsub('[[:punct:] ]+',' ',bttf2)

#https://sfy.ru/?script=batman_production
file_name <-'/Users/zachstrennen/Documents/batman1.txt'
batman1 <- tolower(readChar(file_name, file.info(file_name)$size))
batman1 <- gsub("\n", " ", batman1)
batman1 <- gsub("\t", " ", batman1)
batman1 <- gsub(".", " ", batman1, fixed=TRUE)
batman1 <- gsub("u2028", " ", batman1, fixed=TRUE)
batman1 <- gsub("[0-9]+", " ", batman1)
batman1 <- gsub("\'", "", batman1, fixed=TRUE)
batman1 <- gsub('[[:punct:] ]+',' ',batman1)
#https://www.dailyscript.com/scripts/batman-returns_shooting.html
file_name <-'/Users/zachstrennen/Documents/batman2.txt'
batman2 <- tolower(readChar(file_name, file.info(file_name)$size))
batman2 <- gsub("\n", " ", batman2)
batman2 <- gsub("\t", " ", batman2)
batman2 <- gsub(".", " ", batman2, fixed=TRUE)
batman2 <- gsub("u2028", " ", batman2, fixed=TRUE)
batman2 <- gsub("[0-9]+", " ", batman2)
batman2 <- gsub("\'", "", batman2, fixed=TRUE)
batman2 <- gsub('[[:punct:] ]+',' ',batman2)

#https://www.imaginatemedia.com/wp-content/uploads/2017/07/The-Fast-and-the-Furious-SCRIPT-2.pdf
file_name <-'/Users/zachstrennen/Documents/ff1.txt'
ff1 <- tolower(readChar(file_name, file.info(file_name)$size))
ff1 <- gsub("\n", " ", ff1)
ff1 <- gsub("\t", " ", ff1)
ff1 <- gsub(".", " ", ff1, fixed=TRUE)
ff1 <- gsub("u2028", " ", ff1, fixed=TRUE)
ff1 <- gsub("[0-9]+", " ", ff1)
ff1 <- gsub("\'", "", ff1, fixed=TRUE)
ff1 <- gsub('[[:punct:] ]+',' ',ff1)
#https://www.scripts.com/script-pdf/1604
file_name <-'/Users/zachstrennen/Documents/ff2.txt'
ff2 <- tolower(readChar(file_name, file.info(file_name)$size))
ff2 <- gsub("\n", " ", ff2)
ff2 <- gsub("\t", " ", ff2)
ff2 <- gsub(".", " ", ff2, fixed=TRUE)
ff2 <- gsub("u2028", " ", ff2, fixed=TRUE)
ff2 <- gsub("[0-9]+", " ", ff2)
ff2 <- gsub("\'", "", ff2, fixed=TRUE)
ff2 <- gsub('[[:punct:] ]+',' ',ff2)

#https://assets.scriptslug.com/live/pdf/scripts/kill-bill-vol-1-2003.pdf
file_name <-'/Users/zachstrennen/Documents/killbill1.txt'
killbill1 <- tolower(readChar(file_name, file.info(file_name)$size))
killbill1 <- gsub("\n", " ", killbill1)
killbill1 <- gsub("\t", " ", killbill1)
killbill1 <- gsub(".", " ", killbill1, fixed=TRUE)
killbill1 <- gsub("u2028", " ", killbill1, fixed=TRUE)
killbill1 <- gsub("[0-9]+", " ", killbill1)
killbill1 <- gsub("\'", "", killbill1, fixed=TRUE)
killbill1 <- gsub('[[:punct:] ]+',' ',killbill1)
#https://assets.scriptslug.com/live/pdf/scripts/kill-bill-vol-2-2004.pdf
file_name <-'/Users/zachstrennen/Documents/killbill2.txt'
killbill2 <- tolower(readChar(file_name, file.info(file_name)$size))
killbill2 <- gsub("\n", " ", killbill2)
killbill2 <- gsub("\t", " ", killbill2)
killbill2 <- gsub(".", " ", killbill2, fixed=TRUE)
killbill2 <- gsub("u2028", " ", killbill2, fixed=TRUE)
killbill2 <- gsub("[0-9]+", " ", killbill2)
killbill2 <- gsub("\'", "", killbill2, fixed=TRUE)
killbill2 <- gsub('[[:punct:] ]+',' ',killbill2)

#https://imsdb.com/scripts/Deadpool.html
file_name <-'/Users/zachstrennen/Documents/deadpool1.txt'
deadpool1 <- tolower(readChar(file_name, file.info(file_name)$size))
deadpool1 <- gsub("\n", " ", deadpool1)
deadpool1 <- gsub("\t", " ", deadpool1)
deadpool1 <- gsub(".", " ", deadpool1, fixed=TRUE)
deadpool1 <- gsub("u2028", " ", deadpool1, fixed=TRUE)
deadpool1 <- gsub("[0-9]+", " ", deadpool1)
deadpool1 <- gsub("\'", "", deadpool1, fixed=TRUE)
deadpool1 <- gsub('[[:punct:] ]+',' ',deadpool1)
#https://genius.com/20th-century-fox-deadpool-2-script-annotated
file_name <-'/Users/zachstrennen/Documents/deadpool2.txt'
deadpool2 <- tolower(readChar(file_name, file.info(file_name)$size))
deadpool2 <- gsub("\n", " ", deadpool2)
deadpool2 <- gsub("\t", " ", deadpool2)
deadpool2 <- gsub(".", " ", deadpool2, fixed=TRUE)
deadpool2 <- gsub("u2028", " ", deadpool2, fixed=TRUE)
deadpool2 <- gsub("[0-9]+", " ", deadpool2)
deadpool2 <- gsub("\'", "", deadpool2, fixed=TRUE)
deadpool2 <- gsub('[[:punct:] ]+',' ',deadpool2)

#https://imsdb.com/scripts/Kung-Fu-Panda.html
file_name <-'/Users/zachstrennen/Documents/panda1.txt'
panda1 <- tolower(readChar(file_name, file.info(file_name)$size))
panda1 <- gsub("\n", " ", panda1)
panda1 <- gsub("\t", " ", panda1)
panda1 <- gsub(".", " ", panda1, fixed=TRUE)
panda1 <- gsub("u2028", " ", panda1, fixed=TRUE)
panda1 <- gsub("[0-9]+", " ", panda1)
panda1 <- gsub("\'", "", panda1, fixed=TRUE)
panda1 <- gsub('[[:punct:] ]+',' ',panda1)
#https://www.screenwritersnetwork.org/wp-content/uploads/2021/08/Kung-Fu-Panda-3-2016.pdf
file_name <-'/Users/zachstrennen/Documents/panda3.txt'
panda3 <- tolower(readChar(file_name, file.info(file_name)$size))
panda3 <- gsub("\n", " ", panda3)
panda3 <- gsub("\t", " ", panda3)
panda3 <- gsub(".", " ", panda3, fixed=TRUE)
panda3 <- gsub("u2028", " ", panda3, fixed=TRUE)
panda3 <- gsub("[0-9]+", " ", panda3)
panda3 <- gsub("\'", "", panda3, fixed=TRUE)
panda3 <- gsub('[[:punct:] ]+',' ',panda3)





doc_id <- c("id1","id2","id3","id4","id5","id6","id7","id8","id9","id10",
            "id11","id12","id13","id14","id15","id16","id17","id18")
text <- c(dune1984_txt,charlies1,starisborn1,mulan1,pet1,invisible1,evildead1, freaky1, overboard1,
          dune2021_txt,charlies2,starisborn2,mulan2,pet2,invisible2,evildead2, freaky2, overboard2)
df <- data.frame(doc_id, text)

text2 <- c(raiders_txt,spiderman1,mamamia1,bttf1,batman1,ff1,killbill1, deadpool1, panda1,
          crystal_txt,spiderman2,mamamia2,bttf2,batman2,ff2,killbill2, deadpool2, panda3)

df2 <- data.frame(doc_id, text2)


doc_id2 <- c("id1","id2")

text3 <- c(starwars4_txt,starwars7_txt)

df3 <- data.frame(doc_id2, text3)

sequel <- paste(text2)
words <- strsplit(sequel, "\\s+")
length(unlist(words))

reboot <- paste(text)
words <- strsplit(reboot, "\\s+")
length(unlist(words))

sw <- paste(text3)
words <- strsplit(sw, "\\s+")
length(unlist(words))

library(tidyverse)
glimpse(sw)
# REBOOTS
# charlies angels
# a star is born
# mulan
# pet semetary
# the invisible man
# the evil dead and evil dead
# freaky friday
# overboard

# sequels
# spider man 1 and 2
# mama mia here we go
# back to the future 1 and 3
# batman and batman returns
# fast and furious and 2 fast
# kill bills
# lego movies
# dead pools
# guardians of the galaxy
# kung fu panda 1 and 3