# Body Sensorial Project

## How to run

1. Clone this repository.
2. Put the data files into the `data` folder.
3. Run `docker build -t bsp .` to build the docker image, **it will combine data automatically**
4. Run `docker run --name bsp -it -p 8081:8081` to run the server
5. From a browser access the visualizer URL `http://localhost:8081`

To stop the docker container, use `docker stop bsp`. If you want to remove the container `docker rm bsp`

