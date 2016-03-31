$(document).ready(function() {
    $.ajax({
        url: "/js/timeline.json",
        success: function( data ) {
            var timeline = $('#cd-timeline');
            timeline.empty();

            $.each( data, function(i, e) {
                var block = $('<div>', { class: 'cd-timeline-block' })
                    .append(
                        $('<div>', { class: 'cd-timeline-img ' + e.icon.background })
                            .append(
                                $('<img>', { src: e.icon.image })
                            )
                    ).append(
                        $('<div>', { class: 'cd-timeline-content' })
                            .append(e.content)
                            .append(
                                $('<span>', { class: 'cd-date' })
                                    .append(e.time)
                            )
                    );

                timeline.append(block);
            });
        },
        dataType: "json"
    }).fail(function( jqXHR, textStatus, errorThrown ) {
        console.log(textStatus);
    });
});
