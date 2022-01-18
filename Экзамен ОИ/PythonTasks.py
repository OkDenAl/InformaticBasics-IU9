#2 Задача
def isNumberUppercase(x):
    return (ord(x)>=ord("A")) and (ord(x)<=ord("Z"))

#4 Задача
def remove_repeats(xs):
    res=[]
    for x in xs:
        if (not x in res):
            res.append(x)
    return res

#6 Задача
def my_filter(pred,xs):
    res=[]
    for x in xs:
        if pred(x):
            res.append(x)
    return res

#8 Задача
def mul(x):
    return x*x
print(list(map(mul, [1,2,3,4])))

def my_map1(foo,xs):
    res=[]
    for x in xs:
        res.append(foo(x))
    return res

def plus(*args):
    res=0
    for x in args:
        res+=x
    return res

def my_map2(foo, *args):
    def cars(xss):
        list_cars=[]
        for xs in xss:
            list_cars.append(xs[0])
        return list_cars
    def cdrs(xss):
        list_cdrs=[]
        for xs in xss:
            list_cdrs.append(xs[1:])
        return list_cdrs
    res=[]
    def loop(xss):
        if [] in xss:
            return
        else:
            res.append(foo(*(cars(xss))))
            loop(cdrs(xss))
    loop(args)
    return res

print(my_map1(mul, [5,6,7]))
print(my_map2(plus, [1, 2, 3, 4], [3, 7, 1], [4, 7, 6]))

#9 Задача
def my_sum(a, b):
    return a + b
def pr(a, b):
    return a * b

def my_reduce(action,xss):
    if len(xss)==1:
        return xss[0]
    return(action(xss[0],my_reduce(action,xss[1:])))

print(my_reduce(my_sum, [1, 2, 3, 4, 5]))
print(my_reduce(pr, [1, 2, 3, 4, 5]))
print(my_reduce(min, [1, 2, 3, 4, 5]))
print(my_reduce(max, [1, 2, 3, 4, 5]))
print(my_reduce(lambda x, y: x ** y, [5, 3, 2]))

#11 Задача
def is_even(x):
 return not(x % 2)

def counterPr(pred,xs):
    c=0
    for x in xs:
        if pred(x):
            c+=1
    return c

print(counterPr(is_even, [1, 2, 3, 4, 5, 6]))

#14 Задача
def difference (xs,ys):
    return [x for x in xs if (not x in ys)]

print(difference([1, 2, 3, 4, 5], [2, 3]))

def simmetrialDiff(xs,ys):
    first = [x for x in xs if (not x in ys)]
    second = [y for y in ys if (not y in xs)]
    return first+second
print(simmetrialDiff([1, 2, 3, 4, 5], [2, 3, 6, 7]))

def decart(xs,ys):
    return [(x,y) for x in xs for y in ys]

print(decart([1, 2, 3], [4, 5, 6]))

#23 Задача
def middle_arifmetic(xs):
    sum=0
    for x in xs:
        sum+=x
    return sum/len(xs)
print(middle_arifmetic([1, 2, 5, 6]))

from functools import reduce
def middle_arifm2(xs):
    c=0
    sum=reduce(lambda x,y: x+y,xs)
    return sum / len(xs)
print(middle_arifm2([1, 2, 5, 6]))








