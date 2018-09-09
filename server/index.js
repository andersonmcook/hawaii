const fs = require('fs')
const jsonServer = require('json-server')
const path = require('path')

const server = jsonServer.create()

server.all("*", (_req, res, next) => {
  res.header("Access-Control-Allow-Origin", "*");
  return next();
})

server.use([
  jsonServer.defaults(),
  jsonServer.router(path.join(__dirname, 'db.json'))
])

server.listen(3000, () => {
  console.log('serving from http://localhost:3000')
})
