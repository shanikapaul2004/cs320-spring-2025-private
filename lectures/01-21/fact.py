def fact(n):
  acc = 1
  for i in range(1, n + 1):
    acc = acc * i
  return acc

assert(fact(5) == 120)

def fact(n):
  if n <= 0:
    return 1
  return n * fact(n - 1)

assert(fact(5) == 120)

def fact(n):
  acc = 1
  i = 1
  while i <= n:
    acc = acc * i
    i = i + 1
  return acc

assert(fact(5) == 120)

def fact(n):
  acc = 1
  i = 1
  def while_loop():
    nonlocal acc
    nonlocal i
    acc = acc * i
    i = i + 1
    if i <= n:
      while_loop()
  while_loop()
  return acc

assert(fact(5) == 120)

def fact(n):
  acc = 1
  i = 1
  def while_loop(acc, i):
    acc = acc * i
    i = i + 1
    if i <= n:
      return while_loop(acc, i)
    else:
      return acc
  return while_loop(acc, i)

assert(fact(5) == 120)
