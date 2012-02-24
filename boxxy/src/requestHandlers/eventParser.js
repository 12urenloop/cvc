function parseEvent(pathname,handler){
    console.log(pathname);
    if(typeof handler[pathname] === 'function'){
	handler[pathname]();
    }else{
	console.log("invalid handler request");
    }
}
exports.parseEvent = parseEvent;