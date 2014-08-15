#!/usr/bin/env python

ones = [
        "zero",
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine",
        "ten",
        "eleven",
        "twelve",
        "thirteen",
        "fourteen",
        "fifteen",
        "sixteen",
        "seventeen",
        "eighteen",
        "nineteen"
]

tens = [
        "", #For spacing so that tens[1] == "ten"
        "ten",
        "twenty",
        "thirty",
        "fourty",
        "fifty",
        "sixty",
        "seventy",
        "eighty",
        "ninety"
]

units = [
        "", #more spacing
        "hundred",
        "thousand",
        "million"
]

def add_commas(number):
    """
    input: 4500
    output: "4,500"
    """

    number = str(number)
    number = number[::-1] # Reverse number se we can add the commas

    #ugliness
    if len(number) < 4:
        #Less than a thousand
        return number[::-1]
    if len(number) < 7:
        #one comma to be added at number[3]
        return str(number[:3] + "," + number[3:])[::-1]
    if len(number) < 10:
        #Two commas added at number[3] and number[6]
        return str(number[:3] + "," + number[3:6] + "," + number[6:])[::-1]
    if len(number) < 13:
        raise NotImplementedError

def speak_tens(inp):
    if inp > 99:
        raise ValueError

    if inp < 20:
        return ones[inp]
    else:
        one = inp % 10
        ten = inp / 10

        if one == 0:
            # Fix the case that can return a nonsensical number when the last
            # digit is zero
            return tens[ten]
        if ten == 0:
            return ones[one]

        return "%s-%s" % (tens[ten], ones[one])

def speak_hundreds(inp):
    buf = ""

    if inp == "":
        return ""

    if type(inp) != type(int):
        inp = int(inp)

    if inp > 99:
        hundred = inp / 100
        buf += "%s hundred" % ones[hundred]
    if inp % 100 != 0:
        buf += " %s" % speak_tens(inp % 100)

    return buf

def make_speakable(number):
    split = add_commas(number).split(",")

    split = map(speak_hundreds, split)

    #English is a stupid language


    return split
