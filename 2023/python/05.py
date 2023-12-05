def parse_input(txt_input):
    blocks = txt_input.split("\n\n")
    seeds = [*map(int, blocks[0].split(": ")[1].split(" "))]
    blocks = [*map(parse_block, blocks[1:])]
    return seeds, blocks


def parse_block(block):
    block_lines = block.splitlines()
    val_map = block_lines[1:]
    maps = [[int(x) for x in val.split(" ")] for val in val_map]
    return maps


def compute_res1(structured_input):
    seeds = structured_input[0]
    category_maps = structured_input[1]

    res = seeds
    for category_map in category_maps:
        res = [apply_category_map(mapped_seed, category_map) for mapped_seed in res]

    return min(res)


def apply_category_map(input, category_map):
    for source_dest_map in category_map:
        dest, source, val_range = source_dest_map

        if source <= input < source + val_range:
            return input - source + dest
    return input


def compute_res2(structured_input):
    seeds = structured_input[0]
    category_maps = structured_input[1]

    seed_intervals = [(seeds[i], seeds[i] + seeds[i + 1]) for i in range(0, len(seeds), 2)]

    # Sort source-dest-maps by source for efficient computation
    for category_map in category_maps:
        category_map.sort(key=lambda x: x[1])

    min_vals = [get_min_mapped_value(seed_interval, category_maps) for seed_interval in seed_intervals]
    return min(min_vals)


def get_min_mapped_value(interval, cat_maps):
    left = interval[0]
    right = interval[1]

    # End of recursion
    if len(cat_maps) == 0:
        return left

    # The first map is applied in this function.
    source_dest_maps = cat_maps[0]

    # We need to map the original values, if no mapping applies
    max_mapped_val = source_dest_maps[-1][1] + source_dest_maps[-1][2]
    if max_mapped_val < left:
        return get_min_mapped_value(interval, cat_maps[1:])

    for dest, source, val_range in source_dest_maps:

        # Since the source_dest_maps are ordered, this can only happen if
        # an interval comes before the definition space of all mappings or is completely inbetween.
        if right < source:
            return get_min_mapped_value(interval, cat_maps[1:])

        # Here, the left part of the interval is outside the mapping definition space and needs to be treated as before.
        # Since the right part can again overlap several intervals, the function is applied with the same category
        # mappings. This increases recursion depth.
        if left < source <= right:
            return min(
                get_min_mapped_value((left, source - 1), cat_maps[1:]),
                get_min_mapped_value((source, right), cat_maps),
            )

        # The left part of the interval is inside the mapping definition space.
        # The right part is treated as in the previous condition
        if left < source + val_range < right:
            return min(
                get_min_mapped_value(apply_maps((left, source + val_range), dest, source), cat_maps[1:]),
                get_min_mapped_value((source + val_range, right), cat_maps),
            )

        # The interval is completely inside the mapping definition space.
        if source <= left and right <= source + val_range:
            return get_min_mapped_value(apply_maps(interval, dest, source), cat_maps[1:])

    return -1


def apply_maps(interval, dest, source):
    return interval[0] - source + dest, interval[1] - source + dest


if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        raw_input = f.read()

    parsed_input = parse_input(raw_input)

    res1 = compute_res1(parsed_input)
    print("Result 1:", res1)

    res2 = compute_res2(parsed_input)
    print("Result 2:", res2)
