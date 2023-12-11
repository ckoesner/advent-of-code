neighbor_diffs = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

directions = {
    "|": ((1, 0), (-1, 0)),
    "-": ((0, -1), (0, 1)),
    "L": ((-1, 0), (0, 1)),
    "J": ((-1, 0), (0, -1)),
    "7": ((1, 0), (0, -1)),
    "F": ((1, 0), (0, 1)),
    ".": (),
    "S": ()
}


def transform(txt_input):
    return [[char for char in line] for line in txt_input.splitlines()]


def find_starting_point(field):
    for i in range(len(field)):
        for j in range(len(field[i])):
            if field[i][j] == "S":
                return i, j
    return -1, -1


def compute_loop_element_distance_dict(field, starting_point):
    connected_neighbors = get_connected_neighbors(field, starting_point)

    loop_element_distance_dict = dict()
    loop_element_distance_dict[starting_point] = 0

    add_loop_element_distances_to_dict(
        field,
        starting_point,
        connected_neighbors[0],
        loop_element_distance_dict)

    add_loop_element_distances_to_dict(
        field,
        starting_point,
        connected_neighbors[1],
        loop_element_distance_dict)

    return loop_element_distance_dict


def get_connected_neighbors(field, starting_point):
    neighbors = ((starting_point[0] + diff[0], starting_point[1] + diff[1])
                  for diff in neighbor_diffs)

    return [neighbor
            for neighbor in neighbors
            if is_connected(field, starting_point, neighbor)]


def add_loop_element_distances_to_dict(field, start_tile, current_tile, loop_element_distance_dict):
    previous_tile = start_tile
    i = 1
    while current_tile != start_tile:
        if current_tile not in loop_element_distance_dict.keys():
            loop_element_distance_dict[current_tile] = i
        else:
            if i < loop_element_distance_dict[current_tile]:
                loop_element_distance_dict[current_tile] = i
            else:
                break

        i += 1
        next_tile = get_next(field, previous_tile, current_tile)
        previous_tile = current_tile
        current_tile = next_tile


def get_next(field, previous, current):
    tiles = get_connected_tiles(field, current)
    tiles.remove(previous)
    return tiles[0]


def compute_number_of_contained_tiles(field, distance_set_keys):
    loop_elements = set(distance_set_keys)
    num_inner_tiles = 0
    for i in range(len(field)):
        from_up = False
        on_pipe = False
        count = False

        for j in range(len(field[i])):
            if (i, j) in loop_elements:
                has_upper_connection = is_connected(field, (i - 1, j), (i, j))
                has_lower_connection = is_connected(field, (i + 1, j), (i, j))

                if has_upper_connection and has_lower_connection:
                    count = not count
                else:
                    if has_upper_connection or has_lower_connection:
                        if not on_pipe:
                            from_up = has_upper_connection
                        else:
                            # negates count if pipe came from different side in than it went out
                            count = (from_up ^ has_upper_connection) ^ count
                        on_pipe = not on_pipe
            else:
                num_inner_tiles += 1 if count else 0

    return num_inner_tiles


def is_connected(field, origin, neighbor):
    return origin in get_connected_tiles(field, neighbor)


def get_connected_tiles(field, tile):
    symbol = field[tile[0]][tile[1]]
    diffs = directions[symbol]
    return [(tile[0] + diff[0], tile[1] + diff[1])
            for diff in diffs]


def print_loop(field, loop_elements):
    field_str = ""
    for i in range(len(field)):
        for j in range(len(field[i])):
            field_str += "." if (i, j) not in loop_elements else field[i][j]
        field_str += "\n"
    print(field_str)


if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        raw_input = f.read()

    field_orig = transform(raw_input)
    starting_point = find_starting_point(field_orig)

    loop_element_distance_dict = compute_loop_element_distance_dict(field_orig, starting_point)

    loop_elements = loop_element_distance_dict.keys()
    print_loop(field_orig, loop_elements)

    res1 = max(loop_element_distance_dict.values())
    print("Result 1:", res1)

    # must be replaced manually, so it fits to the corresponding puzzle
    field_orig[starting_point[0]][starting_point[1]] = "|"

    res2 = compute_number_of_contained_tiles(field_orig, loop_elements)
    print("Result 2:", res2)