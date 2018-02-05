# Docker

## Build the image
`docker build -t servant-demo .`

## Start the container on port 8080
`docker run -d --name servant-demo -p 127.0.0.1:8080:443 servant-demo`
