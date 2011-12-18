var net = require('net')
  , util = require('util')
  , EventEmitter = require('events').EventEmitter

var emitter = new EventEmitter()
var Client = module.exports = function(port,host) {
	this.port = port || 8765
	this.host = host || 'localhost'
	this.sock = null
	this.connected = false
	// this.emitter = new EventEmitter()
	//EventEmitter.call(this)
}
//util.inherits(Client,EventEmitter)

var cp = Client.prototype
cp.on = function(ev,cb) {
	emitter.on(ev,cb)
}
cp.emit = function(ev,data) {
	emitter.emit(ev,data)
}
cp.connect = function(cb) {
	var s = this
	this.sock = net.connect(this.port,this.host,function(){
		s.connected = true
		s.sock.setEncoding('utf8')
		if(cb){cb()}
	})
	this.sock.on('close',function(){
		s.connected = false
		s.sock = null
	})
}
cp.disconnect = function() {
	if(this.sock) {
		this.sock.end()
	}
}
cp.listen = function(name,cb) {
	console.log("\t\tstart async notification",name,cb)
	var cb1 = function(r) {
		if(r.dcr==name) {
			console.log('async notification',r)
			cb(r)
		}
	}
	this.on('executed',cb1)
	this.on('exited',cb1)
}
cp.new = function(name,cb) {
	this.rpc(cb,{id:name,method:'new'})
}
cp.init_events = function(id,events,cb) {
	this.rpc(cb,{id:id,method:'init',args:{type:'events',events:events}})
}
cp.init_included = function(id,events,cb) {
	this.rpc(cb,{id:id,method:'init',args:{type:'included',events:events}})
}
cp.make = function(id,type,from,to,pred,cb) {
	this.rpc(cb,{id:id,method:'make',args:{type:type,from:from,to:to,pred:pred}})
}
cp.exec = function(id,name,values,cb) {
	var s = this
	var realcb = function(resp) {
		console.log("\t\tEvent executed?",resp)
		if(resp==true) {
			console.log('emit executed event')
			s.emit('executed',{dcr:id,executed:[name,values]})
		}
		cb(resp)
	}
	this.rpc(realcb,{id:id,method:'exec',args:[name,values]})
}
cp.exit = function(id,cb) {
	var s = this
	var realcb = function(resp) {
		s.emit('exited',{dcr:id,exited:resp})
		cb(resp)
	}
	this.rpc(realcb,{id:id,method:'exit'})
}
cp.state = function(id,cb) {
	this.rpc(cb,{id:id,method:'state'})
}
cp.dcr = function(id,cb) {
	this.rpc(cb,{id:id,method:'dcr'})
}
cp.ids = function(cb) {
	this.rpc(cb,{method:'ids'})
}

cp.rpc = function(cb,payload) {
	this.sock.once('data',function(data){
		console.log("\t\tresponse",data)
		cb(JSON.parse(data))
	})
//	console.log("\t\trequest",payload)
	this.sock.write(JSON.stringify(payload))
}