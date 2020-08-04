# Two responses that satisfy stochastic dominance
sdom = function(a, A)
{
  if(a$C != 1)
    return(TRUE)
  
  if(A$C != 1)
    return(FALSE)
  
  return(a$D > A$D)
}

# ic fails for a = <3, 1>, A = <1, 1>, b = <7.1, 2>, B = <1.1, 2>. Maybe it is 
# because of a "violation" of SD in B in the errors:
# 
# b = "weak" motion to the left (slow 1) = "strong" motion to the right (fast 2)
# B = "strong" motion to the left (fast 1) = "weak" motion to the right (slow 2)
#
# In the above example, however, b is slower than B. Let's exclude this.
#
# Stronger notion of stochastic dominance
sdom = function(a, A)
{
  if(a$C == 1 & A$C == 1)
    return(a$D > A$D)
  
  if(a$C == 1 & A$C != 1)
    return(FALSE)
  
  if(a$C != 1 & A$C == 1)
    return(TRUE)
  
  return(a$D < A$D)
}

# Two processing times that satisfy stochastic dominance, regardless of response alternative
sdom = function(a, A)
{
  return(a$D > A$D)
}

a = list(D=sample(1:10, size=1), C=sample(1:2, size=1))
A = list(D=sample(1:10, size=1), C=sample(1:2, size=1))
print(list(a=a, A=A, dom=sdom(a, A)))

# Four processing times that satisfy stochastic dominance
sim = function()
{
  a = list(D=sample(1:10, size=1), C=sample(1:2, size=1))
  A = list(D=sample(1:10, size=1), C=sample(1:2, size=1))
  
  b = list(D=sample(1:10, size=1) + 0.1, C=sample(1:2, size=1))
  B = list(D=sample(1:10, size=1) + 0.1, C=sample(1:2, size=1))
  
  if(sdom(a, A) & sdom(b, B))
    return(list(a=a, A=A, b=b, B=B))
  
  # Otherwise, retry
  sim()
}

sim()

# Winner of the race
race = function(A, B)
{
  if(A$D <= B$D)
    return(A)
  
  return(B)
}

# Responses to ab, aB, Ab, AB
resp = function(t = sim())
{
  ab = race(t$a, t$b)
  aB = race(t$a, t$B)
  Ab = race(t$A, t$b)
  AB = race(t$A, t$B)
  
  list(ab=ab, aB=aB, Ab=Ab, AB=AB)
}

resp()

# Interaction contrast
ic = function(ab, aB, Ab, AB, t=5.05)
{
  Fab = (ab$D <= t) & (ab$C == 1)
  FaB = (aB$D <= t) & (aB$C == 1)
  FAb = (Ab$D <= t) & (Ab$C == 1)
  FAB = (AB$D <= t) & (AB$C == 1)

  Fab + FAB <= FaB + FAb
}

# Interaction contrast
ic = function(ab, aB, Ab, AB, t=1:10)
{
  if(ab$C != 1)
    ab$D = Inf
  
  if(aB$C != 1)
    aB$D = Inf
  
  if(Ab$C != 1)
    Ab$D = Inf
  
  if(AB$C != 1)
    AB$D = Inf
  
  Fab = ecdf(ab$D)(t)
  FaB = ecdf(aB$D)(t)
  FAb = ecdf(Ab$D)(t)
  FAB = ecdf(AB$D)(t)
  
  all(Fab + FAB <= FaB + FAb)
}

for(i in 1:10000)
{
  t = sim()
  r = resp(t)
  if(!ic(r$ab, r$aB, r$Ab, r$AB))
  {
    print(t)
    print(r)
  }
}

