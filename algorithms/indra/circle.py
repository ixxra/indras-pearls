
class Circle:
    def __init__(self, center, radius):
        self.cen = center
        self.rad = radius

    def __str__(self):
        return '{0}, {1}'.format(self.cen, self.rad)
