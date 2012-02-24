function parseEvent(pathname,handler){
    console.log(pathname);
    if(typeof handler[pathname] === 'function'){
	return handler[pathname]();
    }else{
	return "404 invalid handler request";
    }
}
exports.parseEvent = parseEvent;