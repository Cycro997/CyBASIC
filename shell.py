from basic import run, VER_INFO
from sys import stderr
from time import sleep
from os import stat as file_stats

ver_num = VER_INFO["version"]
rel_date = VER_INFO["release_date"]

print(f"CyBASIC {ver_num} shell ({rel_date.day}/{rel_date.month}/{rel_date.year})")
print("Built using CodePulse's tutorial on YT")
with open("basic.py") as source:
    content = source.read()
    size = file_stats("basic.py").st_size

    print(
        f"{len(content.split("\n")):,} lines, {len(content):,} characters"
        f", {size / 1e3}kB"
    )

sleep(1.25)

while 1:
    text = input(">> ")

    if not text.strip(" ;"): continue

    result, error = run("<stdin>", text, 3)

    if error is not None:
        print(error.as_string(), file=stderr)
        sleep(.5)
    elif result:
        if len(elements := result.elements) == 1:
            print(repr(elements[0]))
        else:
            print(repr(elements))
