all: artifact

artifact:
	@ echo "==> artifact (compile)"
	mix compile
	@ echo "==> complete";

clean:
	mix clean

test:
	mix test
