from basic import run, VER_INFO
from sys import stderr
from time import sleep

ver_num = VER_INFO["version"]
rel_date = VER_INFO["release_date"]

print("PyLang shell")
print(f"{ver_num} ({rel_date.day}/{rel_date.month}/{rel_date.year})")
print("Built using CodePulse's tutorial on YT")

while 1:
    text = input(">> ")

    result, error = run("<stdin>", text)

    if error is not None:
        print(error.as_string(), file=stderr)
        sleep(.5)
    elif result:
        print(result)
