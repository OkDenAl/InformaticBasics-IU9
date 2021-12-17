#!/usr/bin/python3
import sys
import os
import argparse
import errno

SPACE = ' '
TREE_SPACE = '   '
CONNECTOR = '├──'
CONNECTOR_END = '└──'
LINE = '│'
EMPTY_LINE = ' ' * len(LINE)

def makeTree(path,dirOnlyStatus):
	try:
		objects = os.listdir(path)
	except:
		return {'files': []}

	if dirOnlyStatus:
		files=[]
	else:
		files = list(filter(lambda object: os.path.isfile(path + '/'+ str(object)), objects))

	directories = list(filter(lambda object: os.path.isdir(path + '/'+ str(object)), objects))
	directoriesDict={'files': files}

	for directory in directories:
		directoriesDict [str(directory)]=makeTree(path+'/'+str(directory),dirOnlyStatus)
	return directoriesDict

def printTree(dirDict,outPath,dirOnlyStatus,path):
	if outPath is not None:
		sys.stdout=open(outPath,'w')
	print(path)

	def printRecursiveTree(dirDict,beg):
		
		if len(dirDict)> 1:
			directs=list(dirDict.items())[1:]
			for direct, directTree in directs[:-1]:
				print(beg+CONNECTOR+SPACE+ direct)
				printRecursiveTree(directTree,beg+LINE+TREE_SPACE)
			print(beg + (CONNECTOR if len(list(dirDict.items())[0][1])>0  else CONNECTOR_END)  + SPACE + directs[-1][0])
			printRecursiveTree(directs[-1][1],beg+(LINE if len(list(dirDict.items())[0][1])>0  else '')+TREE_SPACE+EMPTY_LINE)

		if len(dirDict['files'])>0:
			for file in dirDict['files'][:-1]:
				print(beg+CONNECTOR+SPACE+file)
			print (beg+ CONNECTOR_END + SPACE + dirDict['files'][-1])

	printRecursiveTree(dirDict,'')

def process_path_argument(parser, argument):
	if os.path.exists(argument):
		if not os.path.isfile(argument):
			parser.error(f'Требуется указать путь до файла')
		elif not os.access(argument, os.W_OK):
			parser.error(f'Нет доступа на запись в файл {argument}')
		else:
			return argument
	else:
		try:
			os.makedirs(os.path.dirname(argument))
		except OSError as error:
			if error.errno != errno.EEXIST:
				parser.error(f'Не удается создать файл {argument}. Ошибка: {error}')
		
		return argument


parser = argparse.ArgumentParser(add_help=False)
parser.add_argument('-d', dest='dirOnly', action='store_true')
parser.add_argument('-o', dest='outputPath', type=lambda path: process_path_argument(parser, path))
args, unknowns = parser.parse_known_args()

if __name__ == '__main__':
	for path in unknowns:
		if not os.access(path, os.R_OK) or not os.path.exists(path) or not os.path.isdir(path):
			sys.stderr.write( f'Не удалось открыть директорию {path} или её не существует \n')
		else:
			printTree(makeTree(path,args.dirOnly), args.outputPath ,args.dirOnly,path)



