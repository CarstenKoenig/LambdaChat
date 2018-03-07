LambdaChat: static/chat.js exe 

clean:
	cd ../Elm && $(MAKE) clean
	rm -rf .stack-work/
	rm -rf static/


static/chat.js:
	mkdir -p static
	cd ../Elm && $(MAKE) 
	cp ../Elm/content/* static/


exe: setup
	stack build


setup:
	stack setup
