var client = new Faye.Client('http://localhost:8000/faye');


client.subscribe('/loops/updates', updateHandler);

var loops;
var currentLoop;

function updateHandler(msg) {
    loops = msg.loops;
    currentLoop = msg.currentLoop;

    render();
}

client.bind('transport:up', function() {
    client.publish('/loops', { type: 'updaterequest' })
});

function addItemToLoop(name) {
    $('#' + name + ' .items').append(itemDivString(null));
}

function saveLoop(name) {
    var items = [];

    $('#' + name + ' .items .item').each(function() {
        var item = {}
        item["url"] = $(this).children('.url').attr('value');
        item["duration"] = parseInt($(this).children('.duration').attr('value'));

        if (item.url !== '' && isFinite(item.duration)) {
            items.push(item);
        }
    });

    if (items.length > 0) {
        $('#' + name).children('.error').text('');
        client.publish('/loops/save', { items: items, loop: name })
    } else {
        $('#' + name).children('.error').text('Won\'t save, no valid items');
    }
}

function deleteLoop(name) {
    if (confirm('Are you sure')) {
        client.publish('/loops/delete', { loop: name });
    }
}

function newLoop(name) {
    var name = $('#newLoop #name').attr('value');

    if (name === '') {
        $('#newLoop .error').text('Name please');
    } else if ($('#' + name).length !== 0) {
        $('#newLoop .error').text('Name is not unique');
    } else {
        $('#newLoop .error').text('');

        $('#loops').append(loopDivString(name, [], undefined));
    }
}

function playLoop(name) {
    client.publish('/loops/play', { loop: name });
}

function moveUp(item) {
    item.prev().before(item);
}

function moveDown(item) {
    item.next().after(item);
}

function itemDivString(item) {
    var url, duration
    if (item === null) {
        url = '';
        duration = '';
    } else {
        url = item.url;
        duration = item.duration;
    }

    var div = '<div class="item">';
    div += '<input type="text" class="url" value="' + url + '" />';
    div += '<input type="text" class="duration" value="' + duration + '" />';
    div += '<span onClick="javascript:$(this).parent().remove()">Delete</span>';
    div += '<span onClick="javascript:moveUp($(this).parent())">Up</span>';
    div += '<span onClick="javascript:moveDown($(this).parent())">Down</span>';
    div += '</div>';
    return div;
}

function loopDivString(name, loop, currentLoop) {
    var div = '<div class="loop" ';
    div += 'id="' + name + '" >'
    div += '<h3>' + name;
    console.log(currentLoop);
    if (name === currentLoop) {
        div += ' (playing)';
    }
    div += '</h3>';

    div += '<div class="items">';
    for (item in loop) {
        div += itemDivString(loop[item]);
    }
    div += '</div>';

    div += '<input type="submit" value="New Item"';
    div += 'onClick="javascript:addItemToLoop(\'' + name + '\');" />'

    div += '<input type="submit" value="Save Loop"';
    div += 'onClick="javascript:saveLoop(\'' + name + '\');" />'

    if (name !== currentLoop) {
        div += '<input type="submit" value="Delete Loop"';
        div += 'onClick="javascript:deleteLoop(\'' + name + '\');" />'

        div += '<input type="submit" value="Play Loop"';
        div += 'onClick="javascript:playLoop(\'' + name + '\');" />'
    }

    div += '<span class="error"></span>'

    div += '</div>';
    return div;
}

function render() {
    $('#loops').html('');

    for (name in loops) {
        $('#loops').append(loopDivString(name, loops[name], currentLoop));
    }
}

render();