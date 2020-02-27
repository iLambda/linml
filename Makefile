all: clean

# Grammars & plugins
ext: vscode
# Vscode plugin
vscode:
	# Convert yaml to js
	npx js-yaml ext/vscode/syntaxes/linml.tmLanguage.yaml > ext/vscode/syntaxes/linml.tmLanguage.json;
	# Done 
	@echo "ext/vscodeDone !"
	
clean:
	rm -rf bin/;
	dune clean