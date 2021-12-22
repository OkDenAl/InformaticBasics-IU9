#!/usr/bin/python3
import sys
import argparse
from scanner import getDictionary

parser=argparse.ArgumentParser(add_help=False)
parser.add_argument('source', metavar='<source_path>', type=argparse.FileType('r'), nargs='*')
args = parser.parse_args()

if __name__ == '__main__':
	data=None
	if not sys.stdin.isatty():
		data = sys.stdin
	if len(args.source)>0:
		data=args.source
	if data==None:
		sys.stderr.write("Вы не передали текст для анализа\n")
		sys.exit(1)

	words=[]
	dictionary=getDictionary(data)
	for i,word in dictionary.items():
		if word not in words:
			words.append(word)

	with open('dictionary.txt', 'w+') as file:
		file.write((' '.join(words)).replace(' ', '\n'))



	