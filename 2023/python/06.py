import re
from math import prod


def transform(txt_input):
    lines = txt_input.splitlines()
    times = [int(i.group(0)) for i in re.finditer(r'\d+', lines[0])]
    distances = [int(i.group(0)) for i in re.finditer(r'\d+', lines[1])]
    return [(times[i], distances[i]) for i in range(len(times))]


def transform2(txt_input):
    lines = txt_input.splitlines()
    time = int(lines[0].split(":")[1].replace(" ", ""))
    dist = int(lines[1].split(":")[1].replace(" ", ""))
    return time, dist


def compute_res1(transformed_input):
    compute_num_winning(transformed_input[1])
    return prod([compute_num_winning(tuple) for tuple in transformed_input])


def compute_num_winning(tuple):
    values = (i * (tuple[0] - i) for i in range(tuple[0]))
    return sum([value > tuple[1] for value in values])


if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        raw_input = f.read()

    transformed = transform(raw_input)
    res1 = compute_res1(transformed)

    transformed2 = transform2(raw_input)
    res2 = compute_num_winning(transformed2)

    print("Result 1:", res1)
    print("Result 2:", res2)
