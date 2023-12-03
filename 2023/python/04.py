def parse_input(txt_input):
    lines = txt_input.splitlines()
    lines = (line.strip() for line in lines)
    return [parse_line(line) for line in lines]


def parse_line(line):
    card, rest = line.split(": ")
    card_num = int(card.split()[1])
    winning_cards, my_cards = rest.split(" | ")
    winning_cards = winning_cards.split(" ")
    my_cards = my_cards.split(" ")

    winning_nums = [int(winning_card) for winning_card in winning_cards if winning_card != ""]
    my_nums = [int(my_card) for my_card in my_cards if my_card != ""]
    return winning_nums, my_nums, card_num


def compute_res1(structured_input):
    res = 0
    for line in structured_input:
        res += compute_line_value_res1(line)
    return res


def compute_line_value_res1(line):
    matching_nums = set(line[0]) & set(line[1])
    if len(matching_nums) == 0:
        return 0
    else:
        return pow(2, len(matching_nums) - 1)


def compute_res2(structured_input):
    card_num_dict = dict()
    for line in structured_input:
        card_num_dict[line[2]] = 1

    for line in structured_input:
        matching_nums = set(line[0]) & set(line[1])
        for i in range(1, len(matching_nums) + 1):
            card_num_dict[line[2] + i] += card_num_dict[line[2]]

    return sum(card_num_dict.values())


if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        raw_input = f.read()

    parsed_input = parse_input(raw_input)

    res1 = compute_res1(parsed_input)
    res2 = compute_res2(parsed_input)

    print("Result 1:", res1)
    print("Result 2:", res2)
