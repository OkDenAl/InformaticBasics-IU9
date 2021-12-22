#!/usr/bin/python3
import sys
import argparse
import re
import colorama
from colorama import Fore

def purple(name):
	return Fore.MAGENTA + name + Fore.RESET
def blue(name):
	return Fore.BLUE + name + Fore.RESET
def green(name):
	return Fore.GREEN + name + Fore.RESET
def red(name):
	return Fore.RED + name + Fore.RESET

def get_contents(streams):
	contents = {}
	for stream in streams:
		contents[stream.name] = stream.read().strip()
	return contents

def grep(content, pattern,regExpr, blockRegisterRules, maxCount,showNumber):
	for name, text in content.items():
		lines=text.split('\n')
		counterOfCoincidences=0
		counterOfLines=0
		for line in lines:
			result=''
			counterOfLines+=1
			if blockRegisterRules and pattern.lower() in line.lower() or pattern in line:
				result=(purple(name) + blue(': ') if len(content)>1 else '')\
				+(green(str(counterOfLines)) +blue(': ') if showNumber else '')+line.replace(pattern,red(pattern))
			if maxCount is not None and maxCount == counterOfCoincidences:
				break
			if regExpr is not None:
				if re.findall(regExpr,line):
					result=(purple(name) + blue(': ') if len(content)>1 else '')\
					+(green(str(counterOfLines)) +blue(': ') if showNumber else '') + line.replace(pattern,red(pattern))
			if result:
				print(result)
				counterOfCoincidences+=1


parser=argparse.ArgumentParser(add_help=False)
parser.add_argument('-e', dest='regExpr', type=str)
parser.add_argument('-i', dest='blockRegisterRules', action='store_true')
parser.add_argument('-m',dest='maxCount', type=int)
parser.add_argument('-n',dest='showNumber', action='store_true')
args, unknowns = parser.parse_known_args()


if __name__=='__main__':
	streams = []
	pattern = None
	for unknown in unknowns:
		try:
			streams.append(open(unknown, 'r'))
		except:
			if pattern is None:
				pattern	= unknown
			else:
				sys.stderr.write(f'Не удается открыть указанный файл {unknown}\n')
	if not sys.stdin.isatty() and len(streams) == 0:
		streams.append(sys.stdin)
	if pattern is None:
		sys.stderr.write('Нужно обязательно передать параметр сравнения\n')
	if len(streams)==0:
		sys.stderr.write('Нужно обязательно передать файл в котором мы выполняем поиск\n')

	content=get_contents(streams)
	grep(content, pattern, args.regExpr, args.blockRegisterRules,args.maxCount,args.showNumber)