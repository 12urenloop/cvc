$(document).ready(function() {
    $.ajax({
        url: "/js/timeline.json",
        success: function( data ) {
            var timeline = $('#cd-timeline');
            timeline.empty();

            $.each( data, function(i, e) {
                var block = $('<div>', { class: 'cd-timeline-block ' + e.align })
                    .append(
                        $('<div>', { class: 'cd-timeline-img ' + e.icon.background })
                            .append(
                                $('<img>', { src: e.icon.image })
                            )
                    );

                var content = $('<div>', { class: 'cd-timeline-content' })
                    .append(e.content)
                    .append(
                        $('<span>', { class: 'cd-date' })
                        .append(e.time)
                        );

                var content_link;
                if (e.link_to == undefined) {
                  content_link = content;
                } else {
                  content_link = $('<a>', { href: e.link_to })
                    .append(content);
                }

                block.append(content_link);

                timeline.append(block);
            });
        },
        dataType: "json"
    }).fail(function( jqXHR, textStatus, errorThrown ) {
        console.log(textStatus);
    });
});
