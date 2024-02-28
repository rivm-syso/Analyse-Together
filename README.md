# Analyse Together

With this tool you can explore the sensordata of the Dutch [Measure Together](https://www.samenmeten.nl/node/721) 
project with official measurement data from the National Air
Monitoring Network, or, for example, compare your neighbourhood with
a neighbourhood nearby. 

This tool is an [R Shiny](https://shiny.rstudio.com/) application and
still under heavy development. So, expect that things change often.
For running this tool, please see below.

# Accessing the tool

To access the tool go to this
[link](https://analyseren.samenmeten.nl/). We appreciate your
feedback, use the issues or our [forum](https://forum.samenmeten.nl/t/feedback-samen-analyseren-tool/423).

# Versions and branches

This tool has three versions, maintained in three branches. The
develop branch is the bleeding edge version where you can find the
newest features and try outs. The testing and accpt branches contain
the internal and external testing versions respectively. These version
are used for others besides the developers to test the app. The main
branch will contain the production ready version.

# Running the tool yourself

This is for experts only ;-).  For running this
tool you have two options, run it in you own [RStudio](https://posit.co/products/open-source/rstudio/) environment,
or run a container with the app using [Docker](https://www.docker.com/).

To run the tool, you must also create a database, see below.
While working with the app, you get data from several sources and this
data is stored in the database. As long as you keep the database,
you keep your downloaded data.

## Running using R (or RStudio)

To run this app in R, clone this repository using your Git sofware or
clone directly in RStudio. Then run the app as an regular Shiny App.
You probably need to install some packages, have a look in the upper
part of [global.R](global.R) for the packages you need.

## Running using Docker

When you want the app in a Docker container, then clone repository on
your local system. Our containers are based on
[Rocker](https://rocker-project.org/images/index.html). All needed
packages are installed during the build. You have either to include
the database in the container, or use a volume to mount a local
database into the container.

After cloning the container, build it using Docker and then run. When
running the container dont' forget to port-forward the Shiny port using
`Docker run [...] -p 3838:3838 [...].  After running your Docker
container, point your browser to http://localhost:3838 to see the app.

## Create the database

In the scripts directory you can find several scripts which we use as
tools to maintain and test the app. The scripts
[scripts/make_big_database.R](scripts/make_big_database.R) creates a database containing data from
5 Dutch municipalities. This gives you some data to play with.
Be aware, downloading the data can take a while (hours, in some
cases). To create an empty database, use the script
[scripts/init_database.R](scripts/init_database.R).

## Contributing and Code of Conduct

Contributions of all sorts are most welcome. Issues and pull requests
are the preferred ways of sharing them.

Please note that Analyse Together uses a [Contributor Code of Conduct](CODE_OF_CONDUCT.md) 
By contributing to this project, you agree to abide by its terms.

## Licence

This projects is licenced using the Open Source GPL-3 licence, please
see the [licence](LICENSE.md) for details

