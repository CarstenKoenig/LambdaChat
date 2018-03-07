distDir := $(shell stack path --dist-dir)
outDir := $(distDir)/build/LambdaChat

ifeq ($(OS),Windows_NT)
    ext := .exe
endif

lambdaChat := $(outDir)/LambdaChat$(ext)

$(lambdaChat): static/chat.js
	stack setup && stack build

dist: $(lambdaChat)
	mkdir -p dist
	mkdir -p dist/static
	cp $(lambdaChat) dist/
	cp static/* dist/static/

clean:
	cd ../Elm && $(MAKE) clean
	rm -rf .stack-work/
	rm -rf static/
	rm -rf dist/


static/chat.js:
	mkdir -p static
	cd ../Elm && $(MAKE) 
	cp ../Elm/content/* static/
