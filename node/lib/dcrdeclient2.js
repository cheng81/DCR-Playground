var net = require('net')
  , util = require('util')
  , EventEmitter = require('events').EventEmitter


var Reader = function(sock) {
	this.sock = sock
	this.sock.setEncoding('utf8')
	this.buf = ''
	var s = this
	this.sock.on('data',function(data){s.newdata(data)})
	this.emitter = new EventEmitter()
}
Reader.prototype.on = function(ev,callback){this.emitter.on(ev,callback)}
Reader.prototype.once = function(ev,callback){this.emitter.once(ev,callback)}
Reader.prototype.emit = function(ev,data){this.emitter.emit(ev,data)}
Reader.prototype.getMessage = function(data) {
	// var str = ""
	// if(this.buf==null) {
	// 	str = data.toString()
	// } else {
	// 	str = this.bus.toString() + data.toString()
	// }
	// console.log('message',str)
	// return JSON.parse(str)
}
Reader.prototype.buffer = function(data) {
	// if(this.buf==null){this.buf=data; return}
	// var tmp = this.buf
	// this.buf = new Buffer(tmp.length+data.length)
	// tmp.copy(this.buf,0,0,tmp.length)
	// data.copy(this.buf,tmp.length,0,data.length)
}
Reader.prototype.newdata = function(data) {
	console.log('Received data:"'+data+'"')
	if(-1==data.lastIndexOf("\r\n")) {
		this.buf = this.buf + data
		return
	}
	if(data[0]=="\n"){data="\r"+data} //unfortunate case when \r\n was split between two tcp packets...
	var complete = data.lastIndexOf("\r\n")==(data.length-2)
	var msgs = data.split("\r\n").filter(function(m){return m.trim().length>0})
	msgs[0] = this.buf + msgs[0]
	if(complete==false) {
		this.buf = msgs.pop()
	}
	// var msgsNum = (complete) ? msgs.length : msgs.length-1
	for(var i=0;i<msgs.length;i++) {
		var str = msgs[i].trim()
		if(str.length==0) {
			console.log("empty message??")
			continue
		}
		console.log('going to emit message',str)
		var msg = JSON.parse(str)
		if(msg.type=='listen') {
			console.log('listen message',msg.dcr,msg)
			this.emit('update-'+msg.dcr,msg)
		} else {
			this.emit('message',msg)
		}
	}
	
	/*this does not work, since \r\n could not be at the end of the message!
	needs to go through the buffer..*/
	// if("\r\n"==data.slice(data.length-2,data.length).toString()) {
	// 	var msg = this.getMessage(data)
	// 	if(msg.type=='listen') {
	// 		this.emit('update-'+msg.dcr,msg)
	// 	} else {
	// 		this.emit('message',msg)
	// 	}
	// } else {
	// 	this.buffer(data)
	// }
}

var Client = module.exports = function(port,host) {
	this.port = port || 8765
	this.host = host || 'localhost'
	this.sock = null
	this.connected = false
	this.reader = null
	this.emitter = new EventEmitter()
}
var cp = Client.prototype
cp.on = function(ev,cb) {
	this.emitter.on(ev,cb)
}
cp.once = function(ev,cb) {this.emitter.once(ev,cb)}
cp.emit = function(ev,data) {
	this.emitter.emit(ev,data)
}
cp.connect = function(cb) {
	var s = this
	console.log('connecting to',this.port,this.host)
	this.sock = net.connect(this.port,this.host,function(){
		console.log('connected!')
		s.connected = true
		s.reader = new Reader(s.sock)
		s.sock.setEncoding('utf8')
		s.emit('connected',s.sock)
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
	this.rpc(cb,{id:name,method:'listen'},true)
}
cp.unlisten = function(name,cb) {
	var s = this
	this.rpc(function(){
		s.reader.removeAllListeners('update-'+name)
		cb()
	},{id:name,method:'unlisten'})
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

cp.rpc = function(cb,payload,isListen) {
	if(this.connected==false) {
		var s = this
		console.log('still not connected...')
		this.once('connected',function(){s.rpc(cb,payload,isListen)})
		return
	}
	if(isListen && isListen==true) {
		this.reader.on('update-'+payload.id,function(update){
			cb(update)
		})
	} else {
		this.reader.once('message',function(msg){
			cb(msg)
		})
	}
	this.sock.write(JSON.stringify(payload) + "\r\n")
}