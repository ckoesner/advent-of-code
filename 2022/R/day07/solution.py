class Command:
  def __init__(self, name, command_input = None, command_output=[]):
    self.name = name
    self.command_input = command_input
    self.command_output = command_output

class File:
  def __init__(self, name, size):
    self.name = name
    self.size = size

class Directory:
  def __init__(self, name, files=[], directories=[], upper_dir = None):
    self.name = name
    self.files = files
    self.directories = directories
    self.upper_dir = upper_dir

def build_command_list_from_input(lines):
  command_list = []
  current_command = None
  current_output_list = []
  for line in lines:
    line_parts = line.replace("\n", "").split(" ")

    if(line_parts[0] == "$"):
      if(len(line_parts) == 3):
        current_command = Command(line_parts[1], line_parts[2])
      else:
        current_command = Command(line_parts[1])
        current_output_list = []
        current_command.command_output = current_output_list
      command_list.append(current_command)
    elif(line_parts[0] == "dir"):
      current_output_list.append(Directory(line_parts[1]))
    else:
      current_output_list.append(File(line_parts[1], line_parts[0]))
  return(command_list)

def print_command_list(command_list):
  for command in command_list:
    if command.command_input == None:
      print(command.name)
      if command.name == "ls":
        for file_or_dir in command.command_output:
          print(file_or_dir.name)
        print("")
    else:
      print(command.name + " " + command.command_input)

def build_dir_structure_from_commands(command_list):
  root_dir = Directory("")
  current_dir = root_dir

  for command in command_list:
    if command.name == "ls":
      current_files = []
      current_dirs = []
      for file_or_dir in command.command_output:
        if isinstance(file_or_dir, File):
          current_dirs.append(file_or_dir)
        if isinstance(file_or_dir, Directory):
          current_dirs.append(file_or_dir)
      current_dir.files = current_files
      current_dir.directories = current_dirs

    if command.name == "cd":
      if command.command_input == "..":
        current_dir = current_dir.upper_dir
      elif command.command_input == "/":
        current_dir = root_dir
      else:
        for subdir in current_dir.directories:
          if subdir.name == command.command_input:
            subdir.upper_dir = current_dir
            current_dir = subdir
  return(root_dir)

def print_tree(root_dir, path):
  for file in root_dir.files:
    print("/" + path + file.name)
  # for directory in root_dir.directories:
  #   print_tree(directory, path + "/" + directory.name)



file1 = open('input2', 'r')
lines = file1.readlines()

command_list = build_command_list_from_input(lines)
# print_command_list(command_list)
dir_tree = build_dir_structure_from_commands(command_list)

for f in dir_tree.files:
  print(f.name)
# print_tree(dir_tree, "")


# p1 = File("John", 36)
# print(p1.name)
# print(p1.size)