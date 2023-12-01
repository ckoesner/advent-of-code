from math import prod


# returns
# [
# (game number, [ {green: val1, red: val2, blue: val3}, {...}, {...}, ...),
# (game number, [ {green: val1, red: val2, blue: val3}, {...}, {...}, ...),
# ...
# ]
def parse_input(txt_input):
    lines = txt_input.splitlines()
    lines = (line.strip() for line in lines)
    return [parse_line(line) for line in lines]


# returns
# (game number, [ {green: val1, red: val2, blue: val3}, {...}, {...}, ...),
def parse_line(line):
    game, rest = line.split(": ")
    game_num = int(game.split(" ")[1])
    cube_set_str_list = rest.split("; ")
    return game_num, [get_cube_set_dict(cube_set_str) for cube_set_str in cube_set_str_list]


def get_cube_set_dict(cube_set_str):
    num_color_str_list = cube_set_str.split(", ")
    cube_set_dict = {}
    for num_color_str in num_color_str_list:
        count, name = num_color_str.split(" ")
        cube_set_dict[name] = int(count)
    return cube_set_dict


def compute_res1(structured_input):
    res = 0
    for line in structured_input:
        res += compute_line_value_res1(line)
    return res


def compute_line_value_res1(line):
    test_dict = dict([('red', 12), ('green', 13), ('blue', 14)])

    for col_tuple in line[1]:
        for test_col in test_dict.keys():
            if test_col in col_tuple.keys() and test_dict[test_col] < col_tuple[test_col]:
                return 0

    return line[0]


def compute_res2(structured_input):
    res = 0
    for line in structured_input:
        res += compute_line_value_res2(line)
    return res


def compute_line_value_res2(line):
    vals = dict([("red", 0), ("green", 0), ("blue", 0)])

    for col_tuple in line[1]:
        for test_col in vals.keys():
            if test_col in col_tuple.keys() and vals[test_col] < col_tuple[test_col]:
                vals[test_col] = col_tuple[test_col]

    return prod(vals.values())


if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        raw_input = f.read()

    parsed_input = parse_input(raw_input)

    res1 = compute_res1(parsed_input)
    res2 = compute_res2(parsed_input)

    print("Result 1:", res1)
    print("Result 2:", res2)
