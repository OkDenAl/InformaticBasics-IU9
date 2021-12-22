#!/usr/bin/python3
import sys
import argparse

def getInfoAboutFile(text):
	return [text.count('\n'),len(text.split()),len(text.encode('utf-8')),len(text)]


def display(res,buffer,name):
	for elem in res:
		print(f'{elem:>{buffer}}', end=' ')
	print (name)	


def wc(content,parametrs):
	results=[]
	if not any(parametrs):
		parametrs[0]=parametrs[1]=parametrs[2]=True

	reguiredIndexes=[]
	i=0
	for parametr in parametrs:
		if parametr:
			reguiredIndexes.append(i)
		i+=1
	totals=[0]*len(reguiredIndexes)
	names=[]

	for name,text in content.items():
		names.append(name)
		info=getInfoAboutFile(text)
		reguiredInfo=[]
		for k in reguiredIndexes:
			reguiredInfo.append(info[k])
		results.append(reguiredInfo)

		for j in range(len(totals)):
			totals[j]+=reguiredInfo[j]

	maxlength=-1		
	for total in totals:
		maxlength=max(maxlength, len(str(total)))
	i=0
	for res in results:
		display(res,maxlength,names[i])
		i+=1
	if len(content) > 1:
		display(totals, maxlength, 'total')


parser=argparse.ArgumentParser(add_help=False)
parser.add_argument('-c', dest='sizeOfObject', action='store_true')
parser.add_argument('-m', dest='amountOfSymbols', action='store_true')
parser.add_argument('-l', dest='amountOfLines', action='store_true')
parser.add_argument('-w', dest='amountOfWords', action='store_true')
args, unknowns=parser.parse_known_args()


def get_contents(streams):
	contents = {}
	for stream in streams:
		contents[stream.name] = stream.read()
	return contents

if __name__=='__main__':
	streams = []
	for unknown in unknowns:
		try:
			streams.append(open(unknown, 'r'))
		except:
			sys.stderr.write(f'Не удается открыть указанный файл {unknown}\n')
	if not sys.stdin.isatty() and len(streams) == 0:
		streams.append(sys.stdin)
	if len(streams)==0:
		sys.stderr.write('Нужно обязательно передать файл в котором мы выполняем поиск\n')
	content=get_contents(streams)
	wc(content,[args.amountOfLines,args.amountOfSymbols,args.sizeOfObject,args.amountOfWords])

