all:	main.hs
		ghc -O3 main.hs

clean:	
		rm main main.o main.hi
