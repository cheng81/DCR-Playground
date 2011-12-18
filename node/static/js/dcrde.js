(function($){
	var mk = function(o,path) {
		if(path.length==0){console.log('null path, return'); return}
		var el = path.shift()
		if(!o[el]) {
			console.log('creating',el)
			o[el] = {}
		}
		mk(o[el],path)
	}
	var txid = 0
	var topics = {}
	var getTopic = function(ev) {
		if(!topics[ev]) {
			topics[ev] = $.Callbacks()
		}
		return topics[ev]
	}
//	var evem = $('#evem')
	var DCR = function(host,callback) {
		var s = this
		this.socket = io.connect(host)
		this.socket.on('ready',function(){
			callback(s)
		})
		this.socket.on('resp',function(r){
			var tx = r.tx
			var result = r.out
			console.log('got result',r,tx,result)
			s.emit(s.txev(tx),result)
		})
	}
	mk($,['frza','dcr'])
	$.frza.dcr.boot = function(host,callback) {
		$.frza.dcr.dcrclient = new DCR(host,callback)
		return $.frza.dcr.dcrclient
	}
	
	DCR.prototype.on = function(ev,fn) {
		// $(evem).on(ev,fn)
		var cbs = getTopic(ev)
		cbs.add(fn)
	}
	DCR.prototype.once = function(ev,fn) {
		// console.log('registering once',ev,fn)
		var cbs = getTopic(ev)
		var realFn = function(data) {
			fn(data)
			cbs.remove( realFn )
			delete topics[ev]
		}
		cbs.add(realFn)
	}
	DCR.prototype.emit = function(ev,data) {
		// console.log('emitting data',ev,data)
		// $(evem).trigger(ev,data)
		(getTopic(ev)).fire(data)
	}
	DCR.prototype.txev = function(tx) {
		if(tx) {
			return 'tx_'+tx
		} else {
			txid=txid+1
			return {tx:'tx_'+txid,id:txid}
		}
	}
	DCR.prototype.newtx = function(method,args,callback,all) {
		var _tx = this.txev()
		console.log('newtx',_tx)
		if(!callback){ callback=args; args=true }
		var req = {
			tx: _tx.id,
			method: method,
			args: args
		}
		console.log('req',req)
		if(all && all==true) {
			console.log('async notification?',method)
			this.on(_tx.tx,callback)
		} else {
			this.once(_tx.tx,callback)
		}
		this.socket.emit('cmd',req)
	}
	
	DCR.prototype.new = function(name,callback) {
		this.newtx('new',[name],callback)
	}
	DCR.prototype.make = function(name,type,from,to,pred,callback) {
		this.newtx('make',[name,type,from,to,pred],callback)
	}
	DCR.prototype.init_events = function(name,events,callback) {
		this.newtx('init_events',[name,events],callback)
	}
	DCR.prototype.init_included = function(name,events,callback) {
		this.newtx('init_included',[name,events],callback)
	}
	DCR.prototype.ids = function(callback) {
		this.newtx('ids',callback)
	}
	DCR.prototype.state = function(name,callback) {
		this.newtx('state',[name],callback)
	}
	DCR.prototype.dcr = function(name,callback) {
		if(name==undefined||name==null){throw "dcr(name,callback) -> name cannot be null!"}
		this.newtx('dcr',[name],callback)
	}
	DCR.prototype.exec = function(name,ev,values,callback) {
		this.newtx('exec',[name,ev,values],callback)
	}
	DCR.prototype.exit = function(name,callback) {
		this.newtx('exit',[name],callback)
	}
	DCR.prototype.listen = function(name,callback) {
		this.newtx('listen',[name],callback,true)
	}
	
	DCR.prototype.allDcr = function(callback) {
		var s = this
		var out = []
		this.ids(function(ids){
			var n = ids.length
			console.log('waiting for '+n+' dcrs...')
			if(n==0) {console.log('no dcrs...'); return}
			var next = function() {
				var cur = ids.shift()
				if(cur==undefined){callback(out)}
				else {
					console.log('request dcr',cur)
					s.listen(cur,function(resp){
						console.log('listen',cur,resp)
						s.emit(resp.executed ? 'executed' : 'exited', resp)
					})
					s.dcr(cur,function(dcr){
						out.push(dcr)
						next()
					})
				}
			}
			next()

			// for(var i in ids) {
			// 	var id = ids[i]
			// 	console.log('request dcr',id)
			// 	s.dcr(id,function(dcr){
			// 		console.log('got a dcr',dcr)
			// 		out.push(dcr)
			// 		n = n-1
			// 		if(n==0){callback(out)}
			// 	})
			// 	s.listen(id,function(resp){
			// 		console.log('async dcr update',id,resp)
			// 		s.emit(resp.executed ? 'executed' : 'exited',resp)
			// 	})
			// }
		})
	}
	
	DCR.prototype.makeAll = function(id,rels,cb) {
		if(rels.length==0) {cb(); return}
		var cur = rels.shift()
		var s = this
		this.make(id,cur.k,cur.f,cur.t,cur.pred,function(){
			s.makeAll(id,rels,cb)
		})
	}
	DCR.prototype.create = function(dcrobj) {
		var s = this
		var comp = this._extractEvents(dcrobj)
		var events = comp.events
		var included = comp.included
		var rels = this._extractRelations(dcrobj)
		this.seq(
			function(n) {
				console.log('new dcr',dcrobj.name)
				s.new(dcrobj.name,n)
			},
			function(n) {
				console.log('init events')
				s.init_events(dcrobj.name,events,n)
			},
			function(n) {
				console.log('init included')
				s.init_included(dcrobj.name,included,n)
			},
			function(n) {
				console.log('make all rels')
				s.makeAll(dcrobj.name,rels,n)
			},
			function(n) {
				console.log('dcr made, notifying')
				s.dcr(dcrobj.name,function(dcr){
					s.emit('created',dcr)
					n()
				})
			},
			function(n) {
				console.log('start listening dcr',dcrobj.name)
				s.listen(dcrobj.name,function(resp){
					console.log('async dcr update',dcrobj.name,resp)
					s.emit(resp.executed ? 'executed' : 'exited',resp)
				})
				n()
			}
		)
	}
	DCR.prototype.seq = function() {
		var s = this
		var funs = Array.prototype.slice.call(arguments)
		var next = function() {
			var fn = funs.shift()
			if(fn==undefined){console.log('no more callbacks'); return}
			var thisargs = Array.prototype.slice.call(arguments)
			thisargs.unshift(next)
			fn.apply(s,thisargs)
		}
		next()
	}
	DCR.prototype._extractEvents = function(dcrobj) {
		var events = dcrobj.events
		var newevts = events.map(function(ev){
			var prms = ev.params.map(function(p) {return p.name})
			return {name:ev.name,fields:prms}
		})
		var included = events.filter(function(ev){
			return ev.included==true
		}).map(function(ev){
			return ev.name
		})
		return {events:newevts,included:included}
	}
	DCR.prototype._extractRelations = function(dcrobj) {
		var evparams = {}
		var events = dcrobj.events
		events.forEach(function(ev){
			evparams[ev.name] = ev.params.map(function(p){return p.name})
		})
		var rels = []
		events.forEach(function(ev){
			var from = ev.name
			ev.relations.forEach(function(rel){
				var type = rel.name
				reacts = rel.reactions.map(function(r){
					var to = r.evtname
					var pred = []
					for(var i=0;i<r.params.length;i++) {
						pred.push(evparams[from][i] + "=" + r.params[i].name)
					}
					return {k:type,f:from,t:to,pred:pred.join(' & ')}
				})
				rels = rels.concat(reacts)
			})
		})
		rels = rels.map(function(rel){
			if(rel.k=='condition'||rel.k=='milestone') {
				return {
					k:rel.k,
					f:rel.t,
					t:rel.f,
					pred:rel.pred
				}
			}
			return rel
		})
		return rels
	}
})(jQuery)

