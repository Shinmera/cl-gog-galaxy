windows:
	mkdir -p build ../static/
	cmake -S . -B build/ -G "Visual Studio 16 2019" -A x64
	cmake --build build/
	cp build/Debug/galaxy-c.dll ../static/win-amd64-galaxy-c.dll

clean:
	rm -r build/
