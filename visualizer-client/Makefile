all:
	stack build && cp -r `stack path --local-install-root`/bin/visualizer-client.jsexe static

repl:
	NODE_PATH=`pwd`/node_modules stack ghci
