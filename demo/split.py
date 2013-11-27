from itertools import izip

def pairwise(iterable):
    "s -> (s0,s1), (s2,s3), (s4, s5), ..."
    a = iter(iterable)
    return izip(a, a)

if __name__ == "__main__":
    line = "0,27 5,5 22,0 4.625,-4.5"
    
    coords = line.split(' ')

    prev = (0,0)
    str = ""

    for elem in coords:
        l = elem.split(',')
        x,y = int(round(float(l[0]))), int(round(float(l[1])))
        xPrev, yPrev = prev

        prev = (x + xPrev, y + yPrev)
        str = str + "({0},{1}),".format(x + xPrev, y + yPrev)

    print str
