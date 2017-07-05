VPATH = src

belcher: main.o
	g++ -o $@ $^ -lecl

clean:
	-rm -f belcher main.o
