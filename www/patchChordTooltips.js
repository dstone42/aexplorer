Shiny.addCustomMessageHandler('patchChordTooltips', function(message) {
    setTimeout(function() {
    console.log('Patching chord tooltips');
    d3.selectAll('.chord-tooltip').remove();

    // Get all label text elements (second <g> in the svg)
    var text = d3.selectAll('#chordPlot text').nodes();

    // Attach handlers to each pie-slice, using index to get label
    d3.selectAll('#chordPlot g.pie-slice').each(function(d, i) {
        var labelNode = text[i];
        // If labelNode has tspans, join their text with a space
        var tspans = labelNode.querySelectorAll('tspan');
        var label;
        if (tspans.length > 0) {
            label = Array.from(tspans).map(t => t.textContent).join(' ');
        } else {
            label = labelNode.textContent;
        }
        var value = d && d.value ? d.value : (d && d.endAngle && d.startAngle ? (d.endAngle - d.startAngle) : '');
        function formatNumber(x) {
        return x && !isNaN(x) ? x.toLocaleString() : x;
        }
        d3.select(this)
        .on('mouseover', function(data) {
            var tooltip = d3.select('body').append('div')
            .attr('class', 'chord-tooltip')
            .style('position', 'absolute')
            .style('background', '#fff')
            .style('border', '1px solid #ccc')
            .style('padding', '6px 10px')
            .style('border-radius', '4px')
            .style('pointer-events', 'none')
            .style('font-size', '13px')
            .html('<b>' + label + '</b>' + (value ? '<br>Co-occurrence: ' + formatNumber(value) : ''));
            tooltip.style('left', (event.pageX + 10) + 'px')
                .style('top', (event.pageY - 20) + 'px');
        })
        .on('mousemove', function(data) {
            d3.select('.chord-tooltip')
            .style('left', (event.pageX + 10) + 'px')
            .style('top', (event.pageY - 20) + 'px');
        })
        .on('mouseout', function() {
            d3.selectAll('.chord-tooltip').remove();
        });
    });
    }, 400);
});