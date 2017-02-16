import numpy as np
from circle import Circle
#Indras pearls, page 91

class Transform:
    def __init__(self, a, b, c, d):
        self.a = a
        self.b = b
        self.c = c
        self.d = d
        
    def inverse(self):
        a, b, c, d = self.a, self.b, self.c, self.d
        det = a*d - b*c
        return Transform(d/det, -b/det, -c/det, a/det)

    def __str__(self):
        return '{0}, {1}, {2}, {3}'.format(self.a, self.b, self.c, self.d)

    @staticmethod
    def mul(t1, t2):
        a, b, c, d = t1.a, t1.b, t1.c, t1.d
        p, q, r, s = t2.a, t2.b, t2.c, t2.d
        A = a*p + b*r
        B = a*q + b*s
        C = c*p + d*r
        D = c*q + d*s
        return Transform(A, B, C, D)
    

def mobius_on_point(T, z):
    '''
    @param T: matrix
    @param z: complex
    @return: Complex number, after applying mobius transformation
    '''
    if z == np.inf:
        if T.c != 0:
            return T.a / T.c
        else:
            return np.inf
    else:
        num = T.a * z + T.b
        den = T.c*z + T.d
        
        if den == 0:
            return np.inf
        else:
            return num / den
        

def mobius_on_circ(T, C):
    '''
    @param T: matrix
    @param C: Circle
    @return: New Circle, after applying mobius transformation
    '''
    print("T", T)
    print("C", C)
    z = C.cen - (C.rad**2)/(T.d/T.c + C.cen).conjugate()
    print("z", z)
    center = mobius_on_point(T, z)
    radius = abs(center - mobius_on_point(T, C.cen + C.rad))
    return Circle(center, radius)
    
