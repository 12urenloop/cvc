var client = new Faye.Client('http://localhost:9000/faye', { retry: 5, timeout: 120 });

client.subscribe('/updates', updatesHandler);

var loops;
var currentLoop;
var items;

function updatesHandler(msg) {
    console.log('Update received');
    JSON.stringify(msg);
    loops = msg.loops;
    items = msg.items;
    currentLoop = msg.currentLoop;

    render();
}

client.bind('transport:up', function() {
    console.log("Admin Connected");
    client.publish('/status', { type: 'updaterequest' });
});

function newItem(name, tag) {
    var name = $('#newItem #name').attr('value');    

    if (name === '') {
        $('#newItem .error').text('Name please');
    } else if ($('#' + name).length !== 0) {
        $('#newItem .error').text('Name is not unique');
    } else {
        $('#newItem .error').text('');
        tag = $('#newItem').children(".tag:checked").val();

        $('#items').append(itemDiv(name, null, tag));
    }
}

function deleteItem(itemDiv) {
    itemDiv.remove();

    client.publish('/items/delete', { itemName:itemDiv.attr('id') })
}

function deleteLoop(loopDiv) {
    loopDiv.remove();

    client.publish('/loops/delete', { loopName:loopDiv.attr('id') });
}

function newLoop() {
    var name = $('#newLoop #name').attr('value');

    if (name === '') {
        $('#newLoop .error').text('Name please');
    } else if ($('#' + name).length !== 0) {
        $('#newLoop .error').text('Name is not unique');
    } else {
        $('#newLoop .error').text('');

        $('#loops').append(loopDiv(name, [], undefined));
    }
}

function playLoop(loopDiv) {
    var loopName = loopDiv.attr('id');
    if (loopName != currentLoop) {
        client.publish('/loops/play', { loopName:loopName });
    }
}

function playItem(itemDiv) {
    var itemName = itemDiv.attr('id');

    client.publish('/items/play', { itemName:itemName });
}

function saveItem(itemDiv) {
    var itemName = itemDiv.attr('id');
    var itemTag = itemDiv.attr('class');
    var content = itemDiv.children('.' + itemTag).val();

    var item = { tag: itemTag };
    item[itemTag] = content;

    client.publish('/items/save', { itemName:itemName, item:item });
}

function saveLoop(loopDiv) {
    var loopName = loopDiv.attr('id');
    var loopItems = [];
    var valid = true;
    loopDiv.children('.items').children('.item').each(function() {
        var item = {};
        item.item = $(this).find('.loopItemName').val();
        item.duration = parseInt($(this).find('.loopItemDuration').val());
        if (item.item === '' || isNaN(item.duration)) {
            loopDiv.children('.error').first().text('There is an invalid item'); // TODO: index?
            valid = false;
            return;
        } else if (items[item.item] === undefined) {
            valid = false;
            loopDiv.children('.error').first().text('There is no item ' + item.item);
            return;
        } else {
            loopItems.push(item);
        }
    });
    if (valid && loopItems.length <= 0) {
        loopDiv.children('.error').first().text('There are no items');
    }
    if (valid && loopItems.length > 0) {
        loopDiv.children('.error').first().text('');
        client.publish('/loops/save', { loop:loopName, items:loopItems })
    }
}

function moveUp(item) {
    item.prev().before(item);
}

function moveDown(item) {
    item.next().after(item);
}

function addItemToLoop(loop) {
    loop.children('.items').append(loopItemDiv(null));
}

function itemDiv(name, item, tag) {
    var url, html
    if (item === null) {
        url = '';
        html = '';
    } else {
        tag = item.tag;
        if (tag === 'html') {
            html = item.html;
        } else if (tag === 'url') {
            url = item.url;
        }
    }

    var div = $('<div>');
    div.attr('class', 'item');
    div.attr('id', name);
    div.attr('class', tag);
    div.append('<h4>' + name + ' (' + tag + ')</h4>');

    var input = $('<input>');
    if (tag === 'html') {
        input = $('<textarea>');
        input.css('display', 'block');
    }
    input.attr('name', tag);
    input.attr('class', tag);
    if (tag === 'html') {
        input.attr('rows', '5');
        input.attr('cols', '60');
        input.attr('value', html);
    } else if (tag === 'url') {
        input.attr('type', 'text');
        input.attr('value', url);
    }
    div.append(input);    

    var saveButton = $('<input>');
    saveButton.attr('type', 'submit');
    saveButton.attr('value', 'Save');
    saveButton.attr('onClick', 'javascript:saveItem($(this).parent())');
    div.append(saveButton);

    var cantDelete = false;
    for (item in loops[currentLoop]) {
        cantDelete = cantDelete || loops[currentLoop][item].item === name;
    }
    if (!cantDelete) {
        div.append(getItemDelButton());   
    }

    var playButton = $('<input>');
    playButton.attr('type', 'submit');
    playButton.attr('value', 'Play');
    playButton.attr('onClick', 'javascript:playItem($(this).parent())');
    div.append(playButton);

    return div;
}

function loopItemDiv(item) {
    var itemDuration = '';
    var itemName = '';
    if (item !== null) {
        itemDuration = item.duration;
        itemName = item.item;
    }

    var div = $('<div>');
    div.attr('class', 'item');

    div.append('Item: ');
    var itemNameDiv = $('<input>');
    itemNameDiv.attr('type', 'text');
    itemNameDiv.attr('name', 'name');
    itemNameDiv.attr('class', 'loopItemName');
    itemNameDiv.attr('value', itemName);
    div.append(itemNameDiv);

    div.append('Duration: ');
    var itemDurationDiv = $('<input>');
    itemDurationDiv.attr('type', 'text');
    itemDurationDiv.attr('name', 'duration');
    itemDurationDiv.attr('class', 'loopItemDuration');
    itemDurationDiv.attr('value', itemDuration);
    div.append(itemDurationDiv);

    div.append(getUpButton());
    div.append(getDownButton());
    div.append(getDelButton());

    return div;
}

function loopDiv(name, loop) {
    var div = $('<div>');
    div.attr('class', 'loop');
    div.attr('id', name);
    div.append('<h3>' + name + '</h3>');

    var itemsDiv = $('<div>');
    itemsDiv.attr('class', 'items');
    for (itemName in loop) {
        itemsDiv.append(loopItemDiv(loop[itemName]));
    }
    div.append(itemsDiv);

    var addItemButton = $('<input>');

    var saveButton = $('<input>');
    saveButton.attr('type', 'submit');
    saveButton.attr('value', 'New Item');
    saveButton.attr('onClick', 'javascript:addItemToLoop($(this).parent())');
    div.append(saveButton);

    var saveButton = $('<input>');
    saveButton.attr('type', 'submit');
    saveButton.attr('value', 'Save');
    saveButton.attr('onClick', 'javascript:saveLoop($(this).parent())');
    div.append(saveButton);

    if (name !== currentLoop) {
        div.append(getLoopDelButton());

        var playButton = $('<input>');
        playButton.attr('type', 'submit');
        playButton.attr('value', 'Play');
        playButton.attr('onClick', 'javascript:playLoop($(this).parent())');
        div.append(playButton);
    }

    var error = $('<span class="error">');
    div.append(error);

    return div;
}

function getDelButton() {
    var del = $('<input>');
    del.attr('type', 'submit');
    del.attr('value', 'Delete');
    del.attr('onClick', 'javascript:$(this).parent().remove()');
    return del;
}

function getItemDelButton() {
    var btn = getDelButton();
    btn.attr('onClick', 'javascript:deleteItem($(this).parent());');
    return btn;
}

function getLoopDelButton() {
    var btn = getDelButton();
    btn.attr('onClick', 'javascript:deleteLoop($(this).parent())');
    return btn;
}

function getUpButton() {
    var del = $('<input>');
    del.attr('type', 'submit');
    del.attr('value', 'Up');
    del.attr('onClick', 'javascript:moveUp($(this).parent())');
    return del;
}

function getDownButton() {
    var del = $('<input>');
    del.attr('type', 'submit');
    del.attr('value', 'Down');
    del.attr('onClick', 'javascript:moveDown($(this).parent())');
    return del;
}

function render() {
    $('#loops').html('');
    $('#items').html('');

    for (name in loops) {
        $('#loops').append(loopDiv(name, loops[name]));
    }

    for (name in items) {
        $('#items').append(itemDiv(name, items[name], items[name].tag));
    }
}

render();