number_dict = {
    "zero": 0,
    "one": 1,
    "two": 2,
    "three": 3,
    "four": 4,
    "five": 5,
    "six": 6,
    "seven": 7,
    "eight": 8,
    "nine": 9
}


def transform(txt_input):
    lines = txt_input.splitlines()
    lines = [line.strip() for line in lines]
    return lines


def compute_res1(transformed_input):
    res = 0
    for line in transformed_input:
        res += compute_line_value_res1(line)
    return res


def compute_line_value_res1(line):
    val1 = 0
    val2 = 0

    for c in line:
        if c.isnumeric():
            val1 = int(c)
            break

    for d in line[::-1]:
        if d.isnumeric():
            val2 = int(d)
            break

    return 10 * val1 + val2


def compute_res2(transformed_input):
    res = 0
    for line in transformed_input:
        res += compute_line_value_res2(line)
    return res


def compute_line_value_res2(line):
    val1 = 0
    val2 = 0
    for i in range(0, len(line)):
        val1 = get_number(line[i:])
        if val1 != -1:
            break

    # n-1 .. 0
    for j in range(len(line)-1, -1, -1):
        val2 = get_number(line[j:])
        if val2 != -1:
            break

    return 10 * val1 + val2


def get_number(cut_line):
    if cut_line[0].isnumeric():
        return int(cut_line[0])

    for number_string in number_dict.keys():
        if cut_line.startswith(number_string):
            return number_dict[number_string]

    return -1


if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        raw_input = f.read()

    transformed = transform(raw_input)

    res1 = compute_res1(transformed)
    res2 = compute_res2(transformed)

    print("Result 1:", res1)
    print("Result 2:", res2)