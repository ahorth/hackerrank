'''
Give a list of coins [c1, c2, c3] where c1 = 1, find the total 
number of ways to make change for an amout m.
'''

def get_first_remainers(coins):
    for j, coin in enumerate(coins):
        if j == len(coins):
            yield coin, tuple()
        yield coin, tuple(coins[j+1:])


amount_seen = {}
def make_change(m, coins):
    coins = tuple(sorted(coins))
    if (m, coins) in amount_seen:
        return amount_seen[(m, coins)]
    elif m==0:
        return 1
    number_of_ways = 0
    for coin, remainers in get_first_remainers(coins):
        amount_to_process = m
        if amount_to_process < coin:
            amount_seen[(amount_to_process, remainers)] = 0
        while amount_to_process >=0:
            number_of_ways += make_change(amount_to_process-coin, remainers)
            amount_to_process -= coin
    amount_seen[(m, coins)] = number_of_ways
    return number_of_ways

if __name__ == "__main__":
    print(make_change(10, tuple([2,5,3,6])))