all:
	nix-build

combine:
	nix-shell --run "./result/bin/visualizer-server all"

run:
	nix-shell --run "./result/bin/visualizer-server"

clean:
	rm data/*combined*
