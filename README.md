# BSP

There are two ways of running the dashboard:

* [Natively](#natively)
* [In a Docker container](#docker)

*Before* proceeding to run, all data files (CSVs and FLACs) must be placed in the `resources/data` folder.

## Natively

For running the project natively we need to have installed some external dependencies,
which can be installed using our package manager, be it `apt`, `brew` or other:

* `stack` - The Haskell build tool. (Be sure to run `stack upgrade --git; stack setup` to update it to the last version)
* `python` - Make sure you have installed *Python 2*
* `python-pip` - The Python package manager
* Some Python libraries (by using `pip`)
    * `numpy` - This one depends on `libblas` and `liblapack`. *They have to be installed on your system.*
    * `scipy`
    * `pandas`

Now we can proceed to setup the project:

* `stack build` - To compile the project
* `stack exec bsp -- combine --dataDirectory resources/data` - To combine the data

And we can execute the dashboard server with

* `stack exec bsp -- serve --port 8081`

After this, we can navigate to `http://localhost:8081`.

## Docker

Another possibility is to run this in a Docker container, which will set everything
up for us.

_As said before, data must be placed in the `resources/data` folder._

* `docker build -t bsp .` - To build the docker image. *This will also combine the data files*
* `docker -it -p 8081:8081 bsp` - To run the dashboard server.

After this, we can navigate to `http://localhost:8081`.

