###

# source("IC_simulation.R")

## parameter setup

# the number of trials in each condition
N <- 10000

# probabilities of response type 1
p_a <- 0.9
p_A <- 0.95
p_b <- 0.8
p_B <- 0.85

# parameters of processing time distributions
lambda_a1 <- 0.5
lambda_A1 <- 1
lambda_b1 <- 0.7
lambda_B1 <- 1.2

lambda_a0 <- 0.5
lambda_A0 <- 1
lambda_b0 <- 0.7
lambda_B0 <- 1.2

## simulate the outputs and the associated processing times of each single process

# outputs of each single process
C_a <- matrix(rbinom(N*2, 1, p = p_a), ncol = 2)
C_A <- matrix(rbinom(N*2, 1, p = p_A), ncol = 2)
C_b <- matrix(rbinom(N*2, 1, p = p_b), ncol = 2)
C_B <- matrix(rbinom(N*2, 1, p = p_B), ncol = 2)

C <- cbind(C_a, C_A, C_b, C_B)

# processing times of response type 1
D_a1 <- matrix(rexp(n = N*2, rate = lambda_a1), ncol = 2)
D_A1 <- matrix(rexp(n = N*2, rate = lambda_A1), ncol = 2)
D_b1 <- matrix(rexp(n = N*2, rate = lambda_b1), ncol = 2)
D_B1 <- matrix(rexp(n = N*2, rate = lambda_B1), ncol = 2)

D_1 <- cbind(D_a1, D_A1, D_b1, D_B1)

# processing times of response type 0
D_a0 <- matrix(rexp(n = N*2, rate = lambda_a0), ncol = 2)
D_A0 <- matrix(rexp(n = N*2, rate = lambda_A0), ncol = 2)
D_b0 <- matrix(rexp(n = N*2, rate = lambda_b0), ncol = 2)
D_B0 <- matrix(rexp(n = N*2, rate = lambda_B0), ncol = 2)

D_0 <- cbind(D_a0, D_A0, D_b0, D_B0)

# processing times that correspond to the outputs
D <- D_esd <- D_1*C + D_0*(1-C)

## examine if stochastic dominance is fulfilled
D_esd[C == 0] <- Inf
D_esd_a1 <- D_esd[ , 1:2]
D_esd_A1 <- D_esd[ , 3:4]
D_esd_b1 <- D_esd[ , 5:6]
D_esd_B1 <- D_esd[ , 7:8]

P_a1 <- ecdf(as.vector(D_esd_a1))
P_A1 <- ecdf(as.vector(D_esd_A1))
t <- sort(c(as.vector(D_esd_a1), as.vector(D_esd_A1)))
diff <- P_a1(t)-P_A1(t)
print(which(diff>0))

P_b1 <- ecdf(as.vector(D_esd_b1))
P_B1 <- ecdf(as.vector(D_esd_B1))
t <- sort(c(as.vector(D_esd_b1), as.vector(D_esd_B1)))
diff <- P_b1(t)-P_B1(t)
print(which(diff>0))

## simulate responses and RT under different conditions
C_ab <- cbind(C_a[ , 1], C_b[ , 1])
D_ab <- cbind(D[ , 1], D[ , 5])
T_ab <- T_ab1 <- apply(D_ab, MARGIN = 1, FUN = min)
R_ab <- C_ab[apply(D_ab, MARGIN = 1, FUN = which.min)]

C_aB <- cbind(C_a[ , 2], C_B[ , 1])
D_aB <- cbind(D[ , 2], D[ , 7])
T_aB <- T_aB1 <- apply(D_aB, MARGIN = 1, FUN = min)
R_aB <- C_aB[apply(D_aB, MARGIN = 1, FUN = which.min)]

C_Ab <- cbind(C_A[ , 1], C_b[ , 2])
D_Ab <- cbind(D[ , 3], D[ , 6])
T_Ab <- T_Ab1 <- apply(D_Ab, MARGIN = 1, FUN = min)
R_Ab <- C_Ab[apply(D_Ab, MARGIN = 1, FUN = which.min)]

C_AB <- cbind(C_A[ , 2], C_B[ , 2])
D_AB <- cbind(D[ , 4], D[ , 8])
T_AB <- T_AB1 <- apply(D_AB, MARGIN = 1, FUN = min)
R_AB <- C_ab[apply(D_AB, MARGIN = 1, FUN = which.min)]

## calculate the interaction contrast

T_ab1[R_ab == 0] <- Inf
T_aB1[R_aB == 0] <- Inf
T_Ab1[R_Ab == 0] <- Inf
T_AB1[R_AB == 0] <- Inf

P_ab1 <- ecdf(T_ab1)
P_aB1 <- ecdf(T_aB1)
P_Ab1 <- ecdf(T_Ab1)
P_AB1 <- ecdf(T_AB1)

t <- sort(c(T_ab1, T_aB1, T_Ab1, T_AB1))
IC <- P_ab1(t) + P_AB1(t) - P_aB1(t) - P_Ab1(t)
print(which(IC>0))
