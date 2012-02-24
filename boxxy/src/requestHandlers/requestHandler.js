function add_view_client(){
    //check if ID already exist
    console.log("add view client with ID: ");
}

function remove_view_client(){
    //remove ID
    //check if ID already exist
    console.log("remove view clien with ID: ");
}
function default_handler(){
    console.log("request default request handler");
}

exports.add_view_client = add_view_client;
exports.remove_view_client = remove_view_client;
exports.default_handler = default_handler;