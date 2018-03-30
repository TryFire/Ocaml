class Good:
    u = 0
    p = 0

    def __init__(self, u, p):
        self.u = u
        self.p = p

class Tree:
    res = []
    left = None
    right = None

    def __init__(self, res):
        self.res = res

def construit(t):
    if ob_indice_reel(t.res) == -1:
        return t
    if est_possible(l) == 0:
        return t
    else:
        res1 = [-1 for i in range(len(l))]
        res2 = [-1 for i in range(len(l))]
        res1[ob_indice_reel(l)] = 0
        res2[ob_indice_reel(l)] = 1
        t.left = construit(Tree(res1))
        t.right = construit(Tree(res2))
        return None
max = 0
def visit(t):
    if t == None:
        return 0
    if ob_indice_reel(t.res) == -1:
        return t


B = 14
n = 7
p = [3, 4, 5, 4, 2, 3, 1]
u = [18, 20, 20, 15, 7, 9, 2]

v = [u[i] / float(p[i]) for i in range(n)]

goods = [0] + [Good(u[i], p[i]) for i in range(n)]


def trier():
    for i in range(n):
        for j in range(n - i - 1):
            if v[j] < v[j + 1]:
                temp = v[j + 1]
                v[j + 1] = v[j]
                v[j] = temp

                temp = u[j + 1]
                u[j + 1] = u[j]
                u[j] = temp

                temp = p[j + 1]
                p[j + 1] = p[j]
                p[j] = temp


def ob_solution_admissible(res):
    b = 0
    j = 0
    for i in range(len(res)):
        if res[i] == 1:
            b = b + p[i]

    while (b <= B) & (res[j] == -1) & (j < n):
        res[j] = 1
        if b + p[j] > B:
            res[j] = 0
            break
        else:
            b = b + p[j]
            j = j + 1

    for i in range(len(res)):
        if res[i] == -1:
            res[i] = 0

    return res


def ob_solution_optimale(res):
    b = 0
    j = 0

    for i in range(len(res)):
        if res[i] == 1:
            b = b + p[i]

    while (b <= B) & (j < n):
        res[j] = 1
        if b + p[j] > B:
            break
        else:
            b = b + p[j]
            j = j + 1
    res[j] = (B - b) / float(p[j])
    for i in range(len(res)):
        if res[i] == -1:
            res[i] = 0
    return res


def ob_indice_reel(l):
    for i in range(len(l)):
        if l[i] - int(l[i]) > 0:
            return i
    return -1

def est_possible(l):
    b = 0
    for i in range(len(l)):
        if l[i] == 1:
            b = b + p[i]
    if b>B:
        return 0
    else:
        return 1

trier()
print v
print p
print u

l = [-1 for i in range(n)]

l1 = [-1 for i in range(n)]
print ob_solution_admissible(l)
print l
opt =  ob_solution_optimale(l1)
print opt
print ob_indice_reel(opt)
