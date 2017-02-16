from circle import Circle
from mobius import mobius_on_circ, Transform
import numpy as np

def go(gens, circ, inv):
    levmax = 3
    tag = np.zeros(1000000, dtype=int)
    num = np.zeros(levmax, dtype=int)
    group = np.empty(1000000, dtype=object)
    group[:4] = gens
    tag[:4] = [0, 1, 2, 3]
    for c in circ:
        print(c)

    num[0] = 0
    num[1] = 4

    for lev in range(1, levmax - 1):
        inew = num[lev]
        for iold in range(num[lev - 1], num[lev]):
            for j in range(4):
                if j == inv[tag[iold]]:
                    continue
                group[inew] = Transform.mul(group[iold], gens[j])
                tag[inew] = j
                inew = inew + 1
        num[lev + 1] = inew

    for i in range(num[levmax - 1]):
        for j in range(4):
            if j == inv[tag[i]]:
                continue
            newcirc = mobius_on_circ(group[i], circ[j])
            print(newcirc)

def test():
    theta = np.pi / 6
    s = np.sin(theta)
    c = np.cos(theta)
    a = Transform(1/s, complex(0, c/s), complex(0, -c/s), 1/s)
    b = Transform(1/s, c/s, c/s, 1/s)
    A = a.inverse()
    B = b.inverse()
    r = 1/c
    radius = r/8.0
    c1 = Circle(complex(r,0), radius)
    c2 = Circle(complex(0, r), radius)
    c3 = Circle(complex(-r, 0), radius)
    c4 = Circle(complex(0, -r), radius)
    circ = [c1, c2, c3, c4]
    gens = [a, b, A, B]
    inv = [2, 3, 0, 1]
    go(gens, circ, inv)

if __name__ == "__main__":
    test()
