(function($){
	var mk = function(o,path) {
		if(path.length==0){return}
		var el = path.shift()
		if(!o[el]) {
			o[el] = {}
		}
		mk(o[el],path)
	}
	mk($,['frza','dcr','ui'])
	
	var bindclick = function(selector,context,fun) {
		if(!fun) {fun=context; context=null}
		if(context!=null) {
			$(selector,context).click(function(ev){
				ev.preventDefault()
				fun.call(this,ev)
				return false
			})
		} else {
			$(selector).click(function(ev){
				ev.preventDefault()
				fun.call(this,ev)
				return false
			})
		}
	}
	var bind = function(client) {
		bindclick('div.dcr input.closedcr[type="button"]',function(){
			var name = this.name
			client.exit(name,function(){
				$('#'+name).hide('slow')
			})
		})
		bindclick('form.execfrm input[type="submit"]',function(){
			var frm = $(this).parent()
			var dcrid = $('input[name="dcrid"]',$(frm)).val()
			var evname = $('input[name="eventname"]',$(frm)).val()
			var values = $('.pvalue',$(frm))
			var obj = {}
			$(values).each(function(_i,el){
				var name = el.name
				var val = $(el).val()
				obj[name] = val
				console.log('param',name,val,el)
			})
			
			console.log('exec event',dcrid,evname,obj)
			client.exec(dcrid,evname,obj,function(r) {
				console.log('event executed?',r)
			})
		})

	}
	
	var makeTabs = function(name,init) {
		console.log('making tabs...',name)
		$('#'+name+'state').tabs()
		if(init) {
			$('#'+name+'forms').accordion({header:'dt'})
			// $('#'+name+'forms').tabs({
			// 				tabTemplate: '<dt><a href="#{href}">#{label}</a></dt>',
			// 				panelTemplate: '<dl></dl>'
			// 			})
		}
	}
	
	$.frza.dcr.ui.boot = function(client) {
		client.on('executed',function(resp){
			var dcr = $('#'+resp.dcr)
			client.state(resp.dcr,function(st){
				var state = $('div.state',dcr)
				$('ul.included',state).html($('#included').render(st))
				$('ul.responses',state).html($('#responses').render(st))
				$('ul.executed',state).html($('#executed').render(st))
				// state.html(
				// 					$('#dcrState').render(st)
				// 				)
				makeTabs(resp.dcr)
			})
		})
		client.on('exited',function(resp){
			$('#'+resp.dcr).hide('slow')
		})
		client.on('created',function(dcr){
			$('#dcrs').prepend(
				$('#dcrTemplate').render(dcr)
			)
			var context = $('#'+dcr.name)
//			console.log('dcr created',context)
			bindclick('input.closedcr[type="button"]',context,function(){
				client.exit(dcr.name,function(){
					$('#'+dcr.name).hide('slow')
				})
			})
			bindclick('form.execfrm input[type="submit"]',context,function(){
				var frm = $(this).parent()
				var dcrid = dcr.name
				var evname = $('input[name="eventname"]',$(frm)).val()
				var values = $('.pvalue',$(frm))
				var obj = {}
				$(values).each(function(_i,el){
					var name = el.name
					var val = $(el).val()
					obj[name] = val
					console.log('param',name,val,el)
				})

				console.log('exec event',dcrid,evname,obj)
				client.exec(dcrid,evname,obj,function(r) {
					console.log('event executed?',r)
				})
			})
			makeTabs(dcr.name,true)
		})
		client.allDcr(function(dcrs){
			$('#dcrs').html(
				$('#dcrTemplate').render(dcrs)
			)
			bind(client)
			dcrs.forEach(function(dcr){
				makeTabs(dcr.name,true)
			})
		})
	}
	
})(jQuery)