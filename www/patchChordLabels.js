Shiny.addCustomMessageHandler('patchChordLabels', function(message) {
    setTimeout(function() {
    // Select all text elements in the chord diagram
    d3.selectAll('#chordPlot text').each(function() {
        var text = d3.select(this).text();
        if (text.indexOf('<br>') !== -1) {
        var lines = text.split('<br>');
        d3.select(this).text(null);
        for (var i = 0; i < lines.length; i++) {
            d3.select(this)
            .append('tspan')
            .attr('x', 0)
            .attr('dy', i === 0 ? 0 : '1.2em')
            .text(lines[i]);
        }
        }
    });
    }, 300); // Wait for rendering
});