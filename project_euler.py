from time import time, gmtime, strftime


def prb1():
    """ Multiples of 3 and 5 """
    print(sum([n for n in range(1000) if (not n % 3) or (not n % 5)]))


def prb2():
    """ Even Fibonacci numbers """
    fib_num = [0, 1]
    while fib_num[-1] < 4000000:
        fib_num.append(sum(fib_num[-2:]))
    print(sum([n for n in fib_num if not n % 2]))


def prb3():
    """ Largest prime factor """
    import math

    n = 600851475143
    sqrt_n = int(math.sqrt(n)) + 1
    for factor in range(sqrt_n, 2, -2):
        if not n % factor and all(
                factor % i for i in range(3, int(math.sqrt(factor)), 2)):
            print(factor)
            break


def prb4():
    """ Largest palindrome product """
    palinds = [i * j for i in range(100, 1000) for j in range(100, 1000)]
    str_palinds = [str(n) for n in palinds]
    print(max(
        filter(
            lambda n: n[0][:len(n[0]) // 2] == n[0][-1:len(n[0]) // 2 - 1:-1],
            zip(str_palinds, palinds)
        ),
        key=lambda n: n[1])[1]
          )


def prb5():
    """ Smallest multiple """
    from itertools import count

    print(next(n for n in count(start=20, step=20)
               if all(not n % i for i in range(1, 21))
               ))


def prb6():
    """ Sum square difference """
    import math

    list_num = range(101)
    print(int(
        math.pow(sum(list_num), 2) -
        sum(map(lambda n: math.pow(n, 2), list_num))
    ))


def prb7():
    """ 10001st prime """
    from math import sqrt
    from itertools import count

    index_prime = 1  # for the first prime that is 2
    for n in count(start=3, step=2):
        sqrt_n = int(sqrt(n)) + 1
        if all(n % i for i in range(3, sqrt_n, 2)):
            index_prime += 1
            if index_prime == 10001:
                print(n)
                break


def prb8():
    """ Largest product in a series (Not solved) """
    from operator import mul
    from functools import reduce

    str_matrix = """73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
"""
    matrix = [list(map(lambda c: int(c), line)) for line in str_matrix.split()]
    results = []
    adj_num = 13
    print()
    # rows
    for row in matrix:
        for i in range(0, row.__len__() - adj_num + 1):
            results.append(reduce(mul, row[i:i + adj_num]))
            print(row[i:i + adj_num], "rows", results[-1])

    # cols
    for index in range(50):
        a = [row[index] for row in matrix]
        for i in range(0, a.__len__() - adj_num + 1):
            results.append(reduce(mul, a[i:i + adj_num]))
            print(a[i:i + adj_num], "cols", results[-1])

    len_row = matrix[0].__len__()

    # diagonal positive
    for index_row in range(matrix.__len__() - adj_num):
        # positive
        for index_col in range(len_row - adj_num):
            a = [
                matrix[i][j] for i, j in
                zip(range(index_row, index_row + adj_num),
                    range(index_col, index_col + adj_num)
                    )
            ]
            results.append(reduce(mul, a))
            print(a, "negative", results[-1])

        # negative
        for index_col in range(adj_num - 1, len_row):
            a = [
                matrix[i][j] for i, j in
                zip(range(index_row, index_row + adj_num),
                    range(index_col - adj_num + 1, index_col + 1)
                    )
            ]
            results.append(reduce(mul, a))
            print(a, "negative", results[-1])

    print(max(results))


def prb9():
    """ Special Pythagorean triplet """
    pass

if __name__ == "__main__":
    print("Result : ", end='')
    start_time = time()
    prb8()
    end_time = time()
    t = end_time - start_time
    print(strftime("Duration : %H hours %M minutes %S seconds", gmtime(t)))
    print("%.2f seconds" % t)
