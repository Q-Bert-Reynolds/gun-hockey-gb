import csv

def main():
  items = []
  with open("./data/items.csv", encoding='utf8') as file:
    dict_reader = csv.DictReader(file)
    for item in dict_reader:
      items.append(item)

  generate_item_data(items)
  generate_item_strings(items)

def generate_item_strings(items):
  with open("./data/item_strings.asm", "w+", encoding='utf8') as c_file:
    c_file.write("SECTION \"Item Strings\", ROMX, BANK[ITEM_BANK]\n")
    
    c_file.write("\nItemNames::\n")
    for item in items:
      if (len(item["Name"]) > 12):
        print("Warning: \"" + item["Name"] + "\" is more than 12 characters long.")
      c_file.write("DB \"" + item["Name"].upper().replace("É","é") + "\", 0\n")

def generate_item_data(items):
  with open("./data/item_data.asm", "w+") as c_file:
    c_file.write("SECTION \"Item Data\", ROMX, BANK[ITEM_BANK]\n")

    constants = ""
    var_names = ""
    item_data = ""
    for i in range(len(items)):
      item = items[i]
      var_name = item["Name"].replace(" ","").replace(".","").replace("-","").replace("é","e") + "Item"
      var_names += "DW " + var_name + "\n"
      constant = item["Name"].replace("é","e").upper().replace(" ","_").replace(".","").replace("-","_") + "_ITEM"
      constants += constant + " EQU " + str(i+1) + "\n"
      item_data += "\n" + var_name + ":;" + item["Description"] + "\n"
      item_data += "DB " + constant + "\n"
      item_data += "DB ITEM_TYPE_" + item["Type"].upper().replace(" ","_").replace(".","").replace("-","_") + "\n"
      item_data += "DW "
      if item["Cost"]:
        item_data += item["Cost"] + " ;cost\n"
      else:
        item_data += "0 ;can't be sold\n"

    c_file.write("\n" + constants + item_data + "\nItemList:\n" + var_names + "\n")

if __name__ == "__main__":
  main()