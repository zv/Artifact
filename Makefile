all: artifact 

artifact: 
	@ echo "==> artifact (compile)"
	mix compile
	@ echo "==> complete";

clean:
	mix clean

test: 
	echo "NO TESTS YET, TRY AGAIN LATER"
