$(document).ready(function() {
    $.ajax({
        url: "/js/timeline.json",
        success: function( data ) {
            var timeline = $('#cd-timeline');
            timeline.empty();

            $.each( data, function(i, e) {
                var time = new Date(
                    e.time.year,
                    e.time.month,
                    e.time.day,
                    e.time.hour,
                    e.time.minute,
                    0
                );

                var isPassed = Date.now() > time.getTime();
                var background;

                if(isPassed){
                  background = "cd-grey";
                } else {
                  background = e.icon.background;
                }

                
                var block = $('<div>', { class: 'cd-timeline-block ' + e.align })
                    .append(
                        $('<div>', { class: 'cd-timeline-img ' + background })
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
