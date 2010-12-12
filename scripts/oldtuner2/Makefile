CC = ghc
CFLAGS = -O3


all: autotune

autotune:
	cd src;	$(CC) $(CFLAGS) --make Main.hs -o ../autotune

clean:
	rm -f autotune
	cd src; rm -f *.o *.hi
	cd src; rm -f GP/*.o GP/*.hi 
