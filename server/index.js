const fs = require('fs')
const jsonServer = require('json-server')
const path = require('path')

const pathToDb = path.join(__dirname, 'db.json')
const server = jsonServer.create()

server.all("*", (_req, res, next) => {
  res.header("Access-Control-Allow-Origin", "*");
  return next();
})

const db = JSON.parse(fs.readFileSync(pathToDb))

server.get('/islands', (_req, res) => {
  res.jsonp(db.islands)
})

server.get('/unauthorized', (_req, res) => {
  res.sendStatus(401)
})

server.get('/not-found', (_req, res) => {
  res.sendStatus(404)
})

server.use([jsonServer.defaults(),jsonServer.router(pathToDb)])

server.listen(3000, () => {
  console.log('serving from http://localhost:3000')
})
