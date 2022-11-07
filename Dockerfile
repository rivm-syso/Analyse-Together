# Image based on Ubuntu LTS (focal), with current R version
FROM rivm-shinyapps-ct/analyse-together:r

# Create folder 
# copy app

RUN mkdir /app
WORKDIR /app
COPY . .

## expose app
EXPOSE 3838

# launch app
CMD ["R", "-e", "shiny::runApp('.' ,host = '0.0.0.0',port=3838)"]