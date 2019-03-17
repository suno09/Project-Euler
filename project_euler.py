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
    # rows
    for row in matrix:
        for i in range(0, row.__len__() - adj_num + 1):
            a = row[i:i + adj_num]
            if not a.__contains__(0):
                results.append(reduce(mul, a))
                print(a, "rows", results[-1])

    # cols
    for index in range(50):
        a = [row[index] for row in matrix]
        for i in range(0, a.__len__() - adj_num + 1):
            aa = a[i:i + adj_num]
            if not aa.__contains__(0):
                results.append(reduce(mul, aa))
                print(aa, "cols", results[-1])

    len_row = matrix[0].__len__()

    # diagonal positive
    for index_row in range(matrix.__len__() - adj_num + 1):
        # positive
        for index_col in range(len_row - adj_num + 1):
            a = [
                matrix[i][j] for i, j in
                zip(range(index_row, index_row + adj_num),
                    range(index_col, index_col + adj_num)
                    )
            ]
            if not a.__contains__(0):
                results.append(reduce(mul, a))
                print(a, "positive", results[-1])

        # negative
        for index_col in range(adj_num - 1, len_row):
            a = [
                matrix[i][j] for i, j in
                zip(range(index_row, index_row + adj_num),
                    range(index_col, index_col - adj_num, -1)
                    )
            ]
            if not a.__contains__(0):
                results.append(reduce(mul, a))
                print(a, "negative", results[-1])

    print(max(results))


def prb9():
    """ Special Pythagorean triplet """
    a, b, c = [(x, y, z)
               for x in range(1000 // 3, -1, -1)
               for y in range(x + 1, (1000 - x) // 2 + 1)
               for z in range(y + 1, 1000 - y - x + 1)
               if x + y + z == 1000 and x ** 2 + y ** 2 == z ** 2
               ][0]
    print(f"{a} * {b} * {c} = {a * b * c}")


def prb10():
    """ Summation of primes """
    from math import sqrt

    n = 2000000
    print(sum([p for p in range(3, n, 2)
               if p % 2 and all(p % i for i in range(3, int(sqrt(p)) + 1, 2))
               ]) + 2)


def prb11():
    """ Largest product in a grid """
    from operator import mul
    from functools import reduce

    str_matrix = """08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"""
    matrix = [list(map(int, line.split())) for line in str_matrix.split('\n')]
    results = []
    adj_num = 4
    # rows
    for row in matrix:
        for i in range(0, row.__len__() - adj_num + 1):
            a = row[i:i + adj_num]
            if not a.__contains__(0):
                results.append(reduce(mul, a))
                print(a, "rows", results[-1])

    # cols
    for index in range(20):
        a = [row[index] for row in matrix]
        for i in range(0, a.__len__() - adj_num + 1):
            aa = a[i:i + adj_num]
            if not aa.__contains__(0):
                results.append(reduce(mul, aa))
                print(aa, "cols", results[-1])

    len_row = matrix[0].__len__()

    # diagonal positive
    for index_row in range(matrix.__len__() - adj_num + 1):
        # positive
        for index_col in range(len_row - adj_num + 1):
            a = [
                matrix[i][j] for i, j in
                zip(range(index_row, index_row + adj_num),
                    range(index_col, index_col + adj_num)
                    )
            ]
            if not a.__contains__(0):
                results.append(reduce(mul, a))
                print(a, "positive", results[-1])

        # negative
        for index_col in range(adj_num - 1, len_row):
            a = [
                matrix[i][j] for i, j in
                zip(range(index_row, index_row + adj_num),
                    range(index_col, index_col - adj_num, -1)
                    )
            ]
            if not a.__contains__(0):
                results.append(reduce(mul, a))
                print(a, "negative", results[-1])

    print(max(results))


def prb12():
    """ Highly divisible triangular number """
    from itertools import count
    from math import sqrt

    n = 0
    for i in count(1):
        n += i
        divisors = [True for d in range(1, int(sqrt(n))) if not n % d]
        # True = get two divisors at the same time
        if divisors.__len__() >= 250:  # divisors.__len__() * 2 >= 500
            print(n)
            break


def prb13():
    """ Large sum """
    str_matrix = """37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690"""

    lst = list(map(int, str_matrix.split()))
    print(str(sum(lst))[:10])


def prb14():
    """ Longest Collatz sequence """
    collatz = []
    for n in range(1000000, 0, -1):
        collatz += [[n]]
        while n > 1:
            if not n % 2:
                n //= 2
            else:
                n = 3 * n + 1
            collatz[-1].append(n)

    print(max(collatz, key=lambda c: len(c))[0])


def prb15():
    """ Lattice paths """

    # first solution (very bad)
    def rec(size, i=1, j=0):
        if i < size and j < size:
            return rec(size, i, j + 1) + rec(size, i + 1, j)
        else:
            return 1

    n = 7
    [print(rec(n) * 2) for n in range(1, 20)]
    # second solution
    # for i in range(n*n):


def prb16():
    """ Power digit sum """
    n = 2 ** 1000
    print(sum([int(c) for c in str(n)]))


def prb17():
    """ Number letter counts """
    import re
    from num2words import num2words

    print(sum(
        [len(re.findall(r'([a-z])', num2words(i))) for i in range(1, 1001)]
    ))


def prb18():
    """ Maximum path sum I """

    def dp_bottom_up(l, m):
        if not m:
            return sum(l)
        l = [max(l[i] + m[0][i], l[i + 1] + m[0][i]) for i in range(len(m))]
        return 1 * dp_bottom_up(l, m[1:])

    text = """75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"""

    matrix = [list(map(int, ln.split())) for ln in text.splitlines()]
    matrix_size = len(matrix)
    matrix = [matrix[i] + [0] * (matrix_size - i - 1) for i in
              range(matrix_size)]
    matrix = list(reversed(matrix))
    print(dp_bottom_up(matrix[0], matrix[1:]))


def prb67():
    """ Maximum path sum II """

    def dp_bottom_up(l, m):
        if not m:
            return sum(l)
        l = [max(l[i] + m[0][i], l[i + 1] + m[0][i]) for i in range(len(m))]
        return 1 * dp_bottom_up(l, m[1:])

    with open('p067_triangle.txt', 'r') as f:
        text = f.read()
        matrix = [list(map(int, ln.split())) for ln in text.splitlines()]
        matrix_size = len(matrix)
        matrix = [matrix[i] + [0] * (matrix_size - i - 1) for i in
                  range(matrix_size)]
        matrix = list(reversed(matrix))
        print(dp_bottom_up(matrix[0], matrix[1:]))


def prb19():
    """ Counting Sundays """
    from datetime import date

    # Sunday is the last position = 6
    sum_sundays = 0
    for year in range(1901, 2001):
        for month in range(1, 13):
            if date(year, month, 1).weekday() == 6:
                sum_sundays += 1
    print(sum_sundays)


def prb20():
    """ Factorial digit sum """
    from math import factorial
    print(sum([int(d) for d in str(factorial(100))]))


def prb21():
    """ Amicable numbers """
    from math import sqrt

    list_devisors = []
    for n in range(10000):
        devisors = []
        for i in range(2, int(sqrt(n)) + 1):
            d, m = divmod(n, i)
            if m == 0:
                devisors += [i, d]
        list_devisors.append(sum(set(devisors)) + 1)

    an = []
    for n in range(10000):
        try:
            if (list_devisors.index(n) == list_devisors[n]) and (
                    n != list_devisors.index(n)):
                an += [n, list_devisors[n]]
        except:
            pass

    print(sum(set(an)))


def prb22():
    """ Names scores """

    with open('p022_names.txt', 'r') as r:
        text = r.read()
        sorted_names = sorted([name[1:-1].lower() for name in text.split(',')])
        list_mul_index = [sum(map(lambda s: ord(s) - 96, name))
                          for name in sorted_names]
        print(sum([(i + 1) * list_mul_index[i]
                   for i in range(sorted_names.__len__())]))


def prb23():
    """ Non-abundant sums """
    from math import sqrt

    list_abundant = []
    for n in range(12, 28112):
        devisors = []
        for i in range(2, int(sqrt(n)) + 1):
            d, m = divmod(n, i)
            if m == 0:
                devisors += [i, d]
        s = sum(set(devisors)) + 1
        if s > n:
            list_abundant.append(n)

    sum_two_abundant = []
    for i in range(list_abundant.__len__()):
        a1 = list_abundant[i]
        for j in range(i, list_abundant.__len__()):
            a2 = list_abundant[j]
            if a1 + a2 > 28123:
                break
            sum_two_abundant.append(a1 + a2)

    print(sum(range(28124)) - sum(set(sum_two_abundant)))


def prb24():
    """ Lexicographic permutations """
    from itertools import permutations

    print(list(map(
        "".join,
        permutations(map(str, range(10)))
    ))[999999])


def prb25():
    """ 1000-digit Fibonacci number """
    index = 2
    f1 = 1
    f2 = 1
    while True:
        index += 1
        f3 = f2 + f1
        if len(str(f3)) == 1000:
            break
        f1 = f2
        f2 = f3

    print(index)


def prb26():
    """ Reciprocal cycles """
    list_rc_d = []
    for d in range(2, 1000):
        list_r = [1]
        while list_r[-1] not in list_r[:-1]:
            r = (list_r[-1] * 10) % d
            list_r.append(r)

        list_rc_d.append((d, list_r.__len__() - 1))

    print(max(list_rc_d, key=lambda e: e[1])[0])


def prb27():
    """ Quadratic primes """
    """ formula (n ^ 2 + a * n + b) """
    from math import sqrt, fabs

    list_qp = []
    for a in range(-999, 1000):
        for b in range(-1000, 1001):
            n = 0
            while True:
                f = fabs(n ** 2 + a * n + b)
                sqrt_f = int(sqrt(f)) + 1
                if (f % 2) and all(f % i for i in range(3, sqrt_f, 2)):
                    # print(f)
                    n += 1
                else:
                    break

            list_qp.append((a, b, n))

    a, b, n = max(list_qp, key=lambda e: e[2])
    print(a * b)


def prb28():
    """ Number spiral diagonals """
    n = 1001
    # sum diagonal up right
    l_odd_pow_2 = list(enumerate([i * i for i in range(3, n + 1, 2)], start=1))
    print(sum([i - e * j for j in [0, 2, 4, 6] for e, i in l_odd_pow_2]) + 1)


def prb29():
    """ Distinct powers """
    print(set(a ** b for a in range(2, 101) for b in range(2, 101)).__len__())


def prb30():
    """ Digit fifth powers """
    # upper bound is 6 digits => 100000
    print(sum([p for p in range(2, 1000000)
               if sum(map(lambda d: int(d) ** 5, str(p))) == p]))


def prb31():
    """ Coin sums """
    money = 200
    coins = [1, 2, 5, 10, 20, 50, 100, 200]

    def rec(l: list) -> int:
        if sum(l) == money:
            return 1
        elif sum(l) > money:
            return 0
        else:
            return 0 + sum([rec(l + [coin])
                            for coin in coins[coins.index(l[-1]):]])

    print(sum([rec([coin]) for coin in coins]))


def prb32():
    """ Pandigital products """
    from itertools import permutations

    pandigitals = []
    str_9_digits = map(lambda i: str(i), range(1, 10))
    perms = permutations(str_9_digits)
    str_nums = map(lambda perm: ''.join(perm), perms)
    for str_num in str_nums:
        for i1 in range(1, 5):
            n1 = int(str_num[0:i1])
            for i2 in range(1, 6 - i1):
                n2 = int(str_num[i1:i1 + i2])
                n3 = int(str_num[i1 + i2:])
            if n1 * n2 == n3:
                pandigitals.append(n3)

    print(sum(set(pandigitals)))


def prb33():
    """ Digit cancelling fractions """
    product_n = 1
    product_d = 1

    ns = list(filter(lambda no: no % 10, range(11, 99)))
    for n in ns:
        str_n = str(n)
        for index in [0, 1]:
            char_n = str_n[index]
            ds = list(filter(
                lambda do: (do % 10) and (char_n in str(do)),
                range(n + 1, 100)))
            for d in ds:
                str_d = str(d)
                if n / d == int(str_n[1 - index]) / int(
                        str_d[1 - str_d.index(char_n)]):
                    product_n *= n
                    product_d *= d

    print(divmod(product_d, product_n)[0])


def prb34():
    """ Digit factorials """
    """
    n! + n! + ... > nn... if we have 1 to 6 digits
    so we limit the range from 3 to 10 ^ 6 
    """
    from math import factorial

    print(sum([n for n in range(3, 100000)
               if n == sum(map(lambda d: factorial(d),
                               map(lambda d: int(d), str(n))))]))


def prb35():
    """ Circular primes """
    from math import sqrt
    import re

    def is_prime(num: int) -> bool:
        return (num % 2) and (
            all(num % d for d in range(3, int(sqrt(num)) + 1, 2)))

    lcps = [2, 5]
    for n in range(3, 10 ** 6, 2):
        str_n = str(n)
        if (n not in lcps) and (not bool(re.match(r".*[024568].*", str_n))):
            lcp = [int(str_n[i:] + str_n[0:i]) for i in range(str_n.__len__())]
            if all(is_prime(n_lcp) for n_lcp in lcp):
                lcps += lcp
                # print(n, sorted(lcps))

    print(len(set(lcps)))


def prb36():
    """ Double-base palindromes """
    def is_palindromic(s: str) -> bool:
        return all(s[i] == s[-i-1] for i in range(s.__len__() // 2))

    print(sum([n for n in range(3, 10 ** 6, 2)
          if is_palindromic(str(n)) and is_palindromic(bin(n)[2:])]) + 1)


def prb37():
    """ Truncatable primes """
    from math import sqrt
    import re

    def is_prime(num: int) -> bool:
        return (num == 2) or ((num % 2) and (
            all(num % d for d in range(3, int(sqrt(num)) + 1, 2))))

    ltp = []
    n = 11
    while True:
        str_n = str(n)
        if not bool(re.match(r"(^[1468].+)|(.+[0124568]$)", str_n)):
            if is_prime(n) and all(
                    is_prime(int(str_n[i:])) and is_prime(int(str_n[0:i]))
                    for i in range(1, str_n.__len__())
            ):
                ltp.append(n)

        n += 2
        if ltp.__len__() == 11:
            print(sum(ltp))
            break


def prb38():
    """ Pandigital multiples """
    from itertools import permutations

    def is_concat_product(num: int, n: int, div_n: int,
                          ibegin: int, length: int) -> bool:
        str_num = str(num)
        if n == 0:
            return False
        if ibegin + length > str_num.__len__():
            if ibegin == str_num.__len__():
                return n > 2
            else:
                str_div_n = str((n - 1) * div_n)
                if n == 2:
                    # print('*' * 30)
                    div_n = -1
                return is_concat_product(num, n - 1, div_n,
                                         str_num.index(str_div_n),
                                         str_div_n.__len__() + 1)

        p = int(str_num[ibegin:ibegin + length])
        d, m = divmod(p, n)
        if m or ((d != div_n) and div_n != -1):
            return is_concat_product(num, n, div_n, ibegin, length + 1)
        else:
            # print(f"{d} * {n} = {p}")
            return is_concat_product(num, n + 1, d, ibegin + length, 1)

    pd9ds = list(map(
        lambda perm: int(''.join(perm)),
        permutations(map(lambda d: str(d), range(1, 10)))
    ))

    # print()
    for pd9d in pd9ds.__reversed__():
        if is_concat_product(num=pd9d, n=1, div_n=-1, ibegin=0, length=1):
            print(pd9d)
            break


def prb39():
    """ Integer right triangles """
    from collections import defaultdict

    dict_p = defaultdict(list)
    for p in range(1, 1001):
        for c in range(p // 3, p // 2):
            for b in range(1, ((p - c) // 2) + 1):
                a = p - c - b
                if c ** 2 == a ** 2 + b ** 2:
                    # print(p, [a, b, c])
                    dict_p[a + b + c].append(sorted([a, b, c]))
                    break

    print(max(dict_p.items(), key=lambda d: len(d[1]))[0])


def prb40():
    """ Champernowne's constant """
    from itertools import count
    from functools import reduce

    idf = ""
    for i in count(start=1, step=1):
        idf += str(i)
        if idf.__len__() >= 10 ** 6:
            break

    print(reduce(lambda x, y: x * y, [int(idf[10 ** i - 1]) for i in range(7)]))


def prb41():
    """ Pandigital prime """
    from itertools import permutations
    from math import sqrt

    def is_prime(num: int) -> bool:
        return (num == 2) or ((num % 2) and (
            all(num % d for d in range(3, int(sqrt(num)) + 1, 2))))

    max_pdnd_prime = 0
    for n in range(9, 0, -1):
        str_n_digits = map(lambda i: str(i), range(1, n + 1))
        perms = permutations(str_n_digits)
        pdnds = list(map(lambda perm: int(''.join(perm)), perms))
        for pdnd in reversed(pdnds):
            if is_prime(pdnd):
                max_pdnd_prime = pdnd
                break
        if max_pdnd_prime != 0:
            break

    print(max_pdnd_prime)


def prb42():
    """ Coded triangle numbers """
    ltn = [int(1/2 * n * (n + 1)) for n in range(1, 27)]
    with open("p042_words.txt", "r") as f:
        words = f.read().upper()[1:-1].split('","')
        words_nbrs = [sum(map(lambda c: ord(c) - 64, word)) for word in words]
        print(len(list(filter(lambda n: n in ltn, words_nbrs))))


if __name__ == "__main__":
    print("Result : ", end='')
    start_time = time()
    prb42()
    end_time = time()
    t = end_time - start_time
    print()
    print(
        strftime("Duration : %H hours %M minutes %S seconds", gmtime(t)),
        "(%.2f seconds)" % t
    )
