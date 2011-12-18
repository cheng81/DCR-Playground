var http = require('http')
  , fs = require('fs')
  , path = require('path')
  , url = require('url')
  , sio = require('socket.io')
  , DCRClient = require('./dcrdeclient2')

var basepath = path.join(__dirname,"../static")
var Server = module.exports = function(port,dcrport,dcrhost) {
	var s = this
	this.server = http.createServer(function(req,res){
		s.httpReq(req,res)
	}).listen(port||8899)
	this.sio = sio.listen(this.server)
	this.sio.sockets.on('connection',function(sock){
		var dclient = new DCRClient(dcrport,dcrhost)
		dclient.connect(function(){
			console.log('DCR client connected, register socket.io')
			sock.on('disconnect',function(){
				console.log('client disconnected')
				dclient.disconnect()
			})
			sock.on('cmd',function(m){
				var tx = m.tx
				console.log('command',m,tx)
				var method = m.method
				if(dclient[method]) {
					var cb = function(resp) {
						sock.emit('resp',{tx:tx,out:resp})
					}
					var args = m.args
					if(args==true){ args = [] }
					else if(!(args instanceof Array)){ args = [args] }
					args.push(cb)
					dclient[method].apply(dclient,args)
				} else {
					sock.emit('resp',{tx:tx,out:'method_not_found'})
				}
			})
			sock.emit('ready')
		})
	})
	
}
Server.prototype.httpReq = function(req,res) {
	if(req.method=='GET') {
		var rpath = url.parse(req.url)
		console.log('file requested',rpath.pathname)
		var relname = rpath.pathname
		if(relname=='/') {
			relname='/index.html'
		}
		var fpath = path.join(basepath,relname)
		console.log('got request',rpath.pathname,fpath)
		var s = fs.createReadStream(fpath)
		s.on('error',function(e){
			console.log('error reading',e)
			res.writeHead(404)
			res.end()
		})
		s.once('fd',function(){res.writeHead(200)})
		s.pipe(res)
	} else {
		res.writeHead(403)
		res.end()
	}
}