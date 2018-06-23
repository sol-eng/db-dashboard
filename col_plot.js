// !preview r2d3 data=data.frame(n = c(10,20,30,40), month = c('jan', 'feb', 'mar', 'apr'))

var layer_left   = 0.001;
var layer_top    = 0.1;
var layer_height = 0.85;
var layer_width  = 0.99;

var actual_max_y = d3.max(data, function (d) {return d.n; });
var col_width    = layer_width * Math.ceil(width / data.length);
var col_heigth   = (height /actual_max_y) * layer_height;
var col_upper    = height - (actual_max_y * col_heigth);
var col_top      = (height * layer_top) - col_upper;

svg.selectAll()
  .data(data)
  .enter().append('a')      
    .append('rect')
      .attr( 'height', function(d) {return (d.n * col_heigth); })
      .attr( 'width',  col_width)
      .attr( 'x',      function(d, i) {return (i * col_width) + (width * layer_left); })
      .attr( 'y',      function(d) {return (height -  (d.n * col_heigth) + col_top); })
      //.attr('fill', '#0072B2')
      .attr('fill', 'white')
      .attr('opacity', function(d) { return d.n / actual_max_y })
      .attr( 'stroke', 'blue')
      .attr("d", function(d) { return d.month; })
      .on("click", function(){
        Shiny.setInputValue(
          "bar_clicked", 
          d3.select(this).attr("d"),
          {priority: "event"}
        );
      })    
      .on("mouseover", function(){
          d3.select(this)
            .attr('opacity', 1)
            .attr('fill', '#ffb14e');
      })
      .on("mouseout", function(){
          d3.select(this)
            .attr('opacity', function(d) { return d.n / actual_max_y; })
            .attr('fill', '#0072B2');
      });      
      
      
svg.selectAll()
  .data(data)
  .enter().append('a')
    .append('text')
    .attr( 'x',      function(d, i) {return (i * col_width) + (width * layer_left) + (col_width * 0.5); })
    .attr('y', function(d) {return height * 0.99;})
    .style('font-size', '12px') 
    .text(function(d) {return d.month;})
    .style('font-family', 'sans-serif');  
    
// Numeric labels

var totals = svg.selectAll().data(data);

totals.enter().append('text')
      .attr( 'x',      function(d, i) {return (i * col_width) + (width * layer_left) + (col_width * 0.5); })
      .attr( 'y',      function(d) {return (height -  (d.n * col_heigth) + col_top); })
      .text(function(d) {return d.n; })
      .style('font-size', '12px') 
      .style('font-family', 'sans-serif');  
      
totals.exit().remove();

totals.transition()
  .duration(1000)
      .attr( 'x',      function(d, i) {return (i * col_width) + (width * layer_left) + (col_width * 0.5); })
      .attr( 'y',      function(d) {return (height -  (d.n * col_heigth) + col_top); })
      .text(function(d) {return d.n; })
      .attr("d", function(d) { return d.dest; });

// Title
svg.append('text')
  .attr('x', width * 0.01)             
  .attr('y', height * 0.05)
  .style('font-size', '18px') 
  .style('font-family', 'sans-serif')
  .text('Total flights');
  
//Sub-title
svg.append('text')
  .attr('x', width * 0.99)             
  .attr('y', height * 0.05)
  .attr('text-anchor', 'end')
  .style('font-size', '12px') 
  .style('font-family', 'sans-serif')
  .text('Click bar for details');
