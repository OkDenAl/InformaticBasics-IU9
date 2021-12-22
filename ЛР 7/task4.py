#!/usr/bin/python3

def memoize(f):
	knownResults={}
	def helper(*args):
		if args in knownResults:
			print(knownResults)
			return knownResults[args]
		else:
			knownResults[args]=f(*args)
			return knownResults[args]
	return helper

def fact(n):
	if n==1 or n==0:
		return 1
	if n>1:
		return fact(n-1)*n

mem=memoize(fact)
mem(3)
mem(4)
mem(5)
mem(3)
