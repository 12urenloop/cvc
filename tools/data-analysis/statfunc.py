__author__ = 'Michiel Van den Berghe'

def avg(l):
    return sum(l)/len(l)

def stdev(l):
    a = avg(l)
    return avg([abs(x - a) for x in l])

def diff(l):
    return [i - j for i, j in zip(l[1:], l[:-1])]

if __name__ == '__main__':
    print(avg([1, 2, 3.5, 7, 0]))
    print(stdev([1, 2, 3.5, 7, 0]))
    print(diff([1, 2, 3.5, 7, 0]))