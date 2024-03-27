windows:
	mkdir -p build ../static/
	cmake -S . -B build/ -DCMAKE_BUILD_TYPE=ReleaseWithDebug -G "Visual Studio 16 2019" -A x64 -DGALAXY_SDK=$(GALAXY_SDK)
	cmake --build build/
	cp build/Debug/galaxy-c.dll ../static/galaxy-c-win-amd64.dl

clean:
	rm -r build/
