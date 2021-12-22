def getWords(line):
	wordStart=-1;
	word=''
	for i,char in enumerate(line,1):
		if char.isalpha() or char.isdigit() or char in ('`', '\'', '’', '-'):
			word+=char
			if wordStart==-1:
				wordStart=i
		elif wordStart!=-1:
			yield wordStart,word
			wordStart=-1 
			word=''
	if wordStart!=-1:
		yield wordStart,word
			

def getDictionary(sources):
	if type(sources)!=list:
		sources=[sources]
	dictionary={}
	for source in sources:
		lines=source.read().split('\n')
		for lineIndex,line in enumerate(lines,1):
			for wordIndex,word in getWords(line):
				dictionary[(lineIndex, wordIndex)]=word
	return dictionary

