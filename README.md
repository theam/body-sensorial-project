# Body Sensorial Project

## How to run

1. Clone this repository. `docker` is required.
2. Put the data files into the `data` folder.
3. Build the docker image: `docker build -t bsp .`
4. Attach yourself to the docker container: `docker run -it --entrypoint=/bin/bash bsp`
5. Combine the data files: `cd /app/data-combinator && stack exec data-combinator -- all`
6. Run the server: `cd /app/visualizer-server && stack exec visualizer-server`
7. From a browser access the visualizer URL `http://localhost:8080`
