// !preview r2d3 data= data.frame(dest_name = c("Austin Bergstrom Intl", "Chicago Ohare Intl", "Dallas Fort Worth Intl", "Eagle Co Rgnl", "Fort Lauderdale Hollywood Intl", "General Edward Lawrence Logan Intl"), n = c(365, 1455, 7257,  103,  182,  274))

var layer_left   = 0.02;
var layer_top    = 0.05;
var layer_height = 0.90;
var layer_width  = 0.96;

var actual_max = d3.max(data, function (d) {return d.n; });
var col_heigth = height / data.length * layer_height;
var col_width  = (width /actual_max) * layer_width;
var col_top    = height * layer_top;
var col_left   = width * layer_left;

var bars = svg.selectAll('rect').data(data);

bars.enter().append('rect')
    .attr('width', function(d) { return d.n * col_width; })
    .attr('height',col_heigth * 0.9)
    .attr('y', function(d, i) { return i * col_heigth + col_top; })
    .attr('x', col_left)
    .attr('fill', '#99CCFF')
    .attr("id", function(d) {return d.y; })
    .on("mouseover", function(){
        d3.select(this)
          .attr('fill', '#ffb14e');
    })
    .on("mouseout", function(){
        d3.select(this)
          .attr('fill', '#99CCFF');
    });

bars.exit().remove();

bars.transition()
  .duration(500)
    .attr('width', function(d) { return d.n * col_width; })
    .attr('height',col_heigth * 0.9)
    .attr('y', function(d, i) { return i * col_heigth + col_top; })
    .attr('x', col_left)
    .attr('fill', '#99CCFF')
    .attr("id", function(d) {return d.y; });

var txt = svg.selectAll('text').data(data);

txt.enter().append('text')
      .attr('x', col_left * 1.1)
      .attr('y', function(d, i) { return i * col_heigth + (col_heigth / 2) + col_top; })
      .style('font-size', '14px') 
      .text(function(d) {return d.dest_name + ' - ' + d.n; });
      
txt.exit().remove();

txt.transition()
  .duration(1000)
      .attr('x', col_left * 1.1)
      .attr('y', function(d, i) { return i * col_heigth + (col_heigth / 2) + col_top; })
      .text(function(d) {return d.dest_name + ' - ' + d.n; });
