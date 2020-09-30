

f1 <- read.csv('fosp_breast_2018.csv')
f2 <- read.csv('fosp_breast_2019.csv')

f1[1:4,1:4]
f2[1:4,1:5]

write.table(f1[1:100, c(1:4)], "f1_.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(f2[1:100, c(2:5)], "f2_.txt", sep = "\t", row.names = FALSE, quote = FALSE)

files <- list.files(pattern = '_.txt')
colnames(f1[1:4])
class(f1$UFNASC)
df = data.frame(ESCOLARI=numeric(), IDADE=numeric(), SEXO=numeric(), UFNASC= factor())
df
cn = c('idade', 'sexo')
lt <- list()
for (f in files) {
  tmp <- read.delim(f)
  lt[[paste0(f)]] <- tmp
}
df


lt[1]$f1_.txt$ESCOLARI

save(lt, file = 'lista.RData')
