#!/usr/bin/python3
import sys
import argparse
from scanner import getDictionary

parser = argparse.ArgumentParser(add_help=False)
parser.add_argument('dictionary', metavar='<path_to_dictionary>', type=argparse.FileType('r'))
parser.add_argument('text', metavar='<path_to_text>', type=argparse.FileType('r'))
args = parser.parse_args()

if __name__ == '__main__':
	dictionary=(args.dictionary).read().split()
	scannedText=getDictionary(args.text)

	if len(dictionary)==0:
		sys.stderr.write("Словарь пустой\n")
		sys.exit(1)

	wrongWords={}
	for indexes,word in scannedText.items():
		if word not in dictionary:
			wrongWords[indexes]=word

	if len(wrongWords)==0:
		sys.exit(1)

	lastWrongWwordLine, lastWrongWordColumn = list(wrongWords.keys())[-1]
	maxLength = max(len(str(lastWrongWwordLine)),
				len(str(lastWrongWordColumn)))

	for pos, word in wrongWords.items():
		lineIndex, wordIndex = pos
		print(f'{lineIndex:>{maxLength}}, {wordIndex:>{maxLength}}   {word}')










