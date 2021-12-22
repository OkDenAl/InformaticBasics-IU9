import random
import string
symbols = string.ascii_letters + string.digits + string.punctuation
def randomString(len):
    randString = ''
    for i in range(len):
        randString += random.choice(symbols)
    return randString