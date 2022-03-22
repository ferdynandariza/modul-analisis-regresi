###### Bahas Tugas #####
# Kamis, 10 Maret 2022, 10:30-11:50 WIB

##### Nomor 1,1 #####

X <- 0:5
fX <- c(0.3, 0.2, 0, 0.1, 0.15, 0.25) #f(x)

# E(x) = Sigma(x*f(x))
EX <- sum(X*fX)
EX

# E(x^2) = Sigma(x^2 * f(x))
EX2 <- sum((X^2)*fX)
EX2

# Var(x) = E(x^2) - (E(x))^2
varX <- EX2 - EX^2
varX

# sig(x) = Akar dari Var(x)
sigX <- sqrt(varX)
sigX


##### Nomor 1,2 #####

f <- function(x) {1/2} # f(x) = 1/2
# Integral f(x) dari 0 sampai 2
Fx <- integrate(Vectorize(f), lower=0, upper=2) 
# vectorize karena f(x) tidak mengandung x
Fx

# Karena fungsi integrate hanya menerima input fungsi, 
# maka x dan f(x) harus distukan jadi x*f(x) = x*1/2
fEx <- function(x) {x*1/2}
# Integral x*f(x) dari 0 sampai 2
Ex <- integrate(fEx, lower=0, upper=2)
Ex <- Ex$value

# Satukan x^2*f(x) = x^2 * 1/2
fEx_sq <- function(x) {x^2 * 1/2}
# Integral x^2 * f(x) dari 0 sampai 2
Ex_sq <- integrate(fEx_sq, lower=0, upper=2)
Ex_sq <- Ex_sq$value

# Var(x) = E(x^2) - (E(x))^2
var_x <- Ex_sq - Ex^2
var_x

# E(2x+3) = E(2x) + E(3) = 2*E(x) + 3
E2x <- 2*Ex + 3
E2x

# Var(2x+3) = E((2x+3)^2) - (E(2x+3))^2
# var(2x+3) = 4*var(x) + 9*var(1) = 4*var(x)
var_2x <- 4*var_x
var_2x


##### Nomor 1.3 #####

# Diketahui
miu = 10
n = 12; db = n - 1
x.bar = 11.21
s = 0.38
alpha = 0.05

# Hipotesis nol dan tandingan 
#  H0: x.bar = miu
#  H1: x.bar != miu

# Uji hipotesis dan tentukan nilai-p
nilai.t <- (x.bar-miu) / (s/sqrt(n))
nilai.t

# nilai.p < 0.0005
nilai.p <- pt(q=nilai.t, df=db, lower.tail=FALSE)
nilai.p
# nilai.p < alpha, tolak H0
# x.bar != miu

# Selang kepercayaan miu (alpha = 5%)
t.alpha = qt(p=1-alpha/2, df=db)
t.alpha
margin = t.alpha * (s/sqrt(n))
margin
ci.bawah <- miu - margin
ci.atas <- miu + margin
c(ci.bawah, ci.atas)


##### Nomor 1,4 #####

# Matriks A
A <- matrix(data=c(1,3,0,2), ncol=2, byrow=TRUE)
A
B <- matrix(data=c(4,4), ncol=1, byrow = FALSE)
B
t(B)

A_inv <- solve(A)
A_inv
# AX = B -> Ainv A X = Ainv B -> X = Ainv B
X <- A_inv %*% B
X
t(X)

"
Misal: t(X) = [x y]
t(X) A X

[x y] [1 3] [x] = [x  3x+2y] [x] = [x^2 + (3xy+2y^2)] 
      [0 2] [y]              [y]   
      
                = [x^2 + 3xy + 2y^2] = x^2 + 3xy + 2y^2 (skalar)
"

XAX = t(X) %*% A %*% X
AXX = A %*% X %*% t(X)
XXA = (X) %*% t(X) %*%A

# Trace = Jumlah entri diagonal utama
sum(diag(XAX))
sum(diag(AXX))
sum(diag(XXA))

library(matrixcalc)
matrix.trace(XAX)
matrix.trace(AXX)
matrix.trace(XXA)
