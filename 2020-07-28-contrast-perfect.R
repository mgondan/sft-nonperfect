# Two responses that satisfy stochastic dominance
sdom = function(a, A)
{
  return(a > A)
}

# Four processing times that satisfy stochastic dominance
sim = function()
{
  a = sample(1:10, size=1)
  A = sample(1:10, size=1)
  
  b = sample(1:10, size=1) + 0.1
  B = sample(1:10, size=1) + 0.1
  
  if(sdom(a, A) & sdom(b, B))
    return(list(a=a, A=A, b=b, B=B))
  
  # Otherwise, retry
  sim()
}

sim()

# Winner of the race
race = function(A, B)
{
  if(A <= B)
    return(A)
  
  return(B)
}

# Responses to ab, aB, Ab, AB
resp = function(t = sim())
{
  ab = race(t$a, t$b)
  aB = race(t$a, t$B)
  Ab = race(t$A, t$b)
  AB = race(t$A, t$b)
  
  list(ab=ab, aB=aB, Ab=Ab, AB=AB)
}

resp()

# Interaction contrast
ic = function(ab, aB, Ab, AB, t=1:10)
{
  Fab = ecdf(ab)(t)
  FaB = ecdf(aB)(t)
  FAb = ecdf(Ab)(t)
  FAB = ecdf(AB)(t)
  
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

