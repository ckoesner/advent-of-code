value_dict_1 = {
    "A": "F",
    "K": "E",
    "Q": "D",
    "J": "C",
    "T": "B",
}

value_dict_2 = {
    "A": "E",
    "K": "D",
    "Q": "C",
    "J": "0",
    "T": "B",
}


def transform(txt_input):
    lines = txt_input.splitlines()
    lines = [line.strip().split() for line in lines]
    return [(line[0], int(line[1])) for line in lines]


def compute_res(cards, key_function):
    cards.sort(key=lambda x: key_function(x[0]))
    res = 0
    for i in range(len(cards)):
        res += (i+1) * cards[i][1]
    return res


def compute_sort_key_1(hand: str):
    for value in value_dict_1.keys():
        hand = hand.replace(value, value_dict_1[value])

    tuples = compute_num_tuples_per_size(hand)
    return str(value_tuples(tuples)) + hand


def compute_sort_key_2(hand: str):
    if hand == "JJJJJ":
        return "600000"

    jokers = sum([char == "J" for char in hand])

    for value in value_dict_2.keys():
        hand = hand.replace(value, value_dict_2[value])

    tuples = compute_num_tuples_per_size(hand.replace("0", ""))

    if jokers != 0:
        for j in range(5, 0, -1):
            if tuples[j] != 0:
                tuples[j] -= 1
                tuples[j+jokers] = tuples[j+jokers] + 1
                break

    return str(value_tuples(tuples)) + hand


def compute_num_tuples_per_size(hand):
    hand_copy = hand
    tuples = {1: 0, 2: 0, 3: 0, 4: 0, 5: 0}
    for card in hand_copy:
        len1 = len(hand)
        hand = hand.replace(card, "")
        diff = len1 - len(hand)
        if diff != 0:
            tuples[diff] += 1
    return tuples


def value_tuples(tuples):
    if tuples[1] == 5:
        return 0
    if tuples[2] == 1 and tuples[1] == 3:
        return 1
    if tuples[2] == 2:
        return 2
    if tuples[3] == 1 and tuples[1] == 2:
        return 3
    if tuples[3] == 1 and tuples[2] == 1:
        return 4
    if tuples[4] == 1:
        return 5
    if tuples[5] == 1:
        return 6
    return -1


if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        raw_input = f.read()

    transformed = transform(raw_input)

    res1 = compute_res(transformed, compute_sort_key_1)
    print("Result 1:", res1)

    res2 = compute_res(transformed, compute_sort_key_2)
    print("Result 2:", res2)