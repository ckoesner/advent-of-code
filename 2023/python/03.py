import re
from math import prod

import numpy as np


def parse_input(txt_input, is_symbol_fun):
    lines = txt_input.splitlines()
    lines = [line.strip() for line in lines]

    symbols = np.ndarray(shape=(len(lines), len(lines[0])), dtype=bool)
    for i in range(0, len(lines)):
        for j in range(0, len(lines[i])):
            symbols[i, j] = is_symbol_fun(lines[i][j])

    numbers = {}
    pattern = re.compile(r'\d+')
    for i in range(0, len(lines)):
        finditer = pattern.finditer(lines[i])
        numbers[i] = [(int(m.group(0)), m.start(0), m.end(0)) for m in finditer]
    return symbols, numbers


def is_symbol(char: str):
    return not (char.isdigit() or char == ".")


def is_symbol_2(char: str):
    return char == "*"


def compute_res1(structured_input):
    symbols, numbers = structured_input

    res = 0
    for line_num in numbers.keys():
        for num in numbers[line_num]:
            if is_part_number(line_num, num, symbols):
                res += num[0]
    return res


def is_part_number(line_num, num, symbols):
    n_rows, n_cols = symbols.shape
    val, start_ind, end_ind = num

    check_range = range(max(0, start_ind - 1), min(end_ind + 1, n_cols))

    above = [symbols[line_num - 1, i] for i in check_range] if line_num - 1 >= 0 else []
    below = [symbols[line_num + 1, i] for i in check_range] if line_num + 1 < n_rows else []

    left = [symbols[line_num, start_ind - 1]] if 0 <= start_ind - 1 else []
    right = [symbols[line_num, end_ind]] if end_ind < n_cols else []

    return any(above + below + left + right)


def compute_res2(structured_input):
    symbols, numbers = structured_input
    n_rows, n_cols = symbols.shape

    res = 0
    for i in range(0, n_rows):
        for j in range(0, n_cols):
            if symbols[i, j]:
                res += get_gear_value(i, j, numbers)

    return res


def get_gear_value(i, j, numbers):
    connected_numbers = []

    for num in numbers[i - 1] + numbers[i] + numbers[i + 1]:
        if set(range(j - 1, j + 2)) & set(range(num[1], num[2])):
            connected_numbers.append(num)

    if len(connected_numbers) == 2:
        return prod([num[0] for num in connected_numbers])
    return 0


if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        raw_input = f.read()

    parsed_input = parse_input(raw_input, is_symbol)
    res1 = compute_res1(parsed_input)

    parsed_input_2 = parse_input(raw_input, is_symbol_2)
    res2 = compute_res2(parsed_input_2)

    print("Result 1:", res1)
    print("Result 2:", res2)
