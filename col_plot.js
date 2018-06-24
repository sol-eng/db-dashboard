// !preview r2d3 data=data.frame(n = c(5000,2000,3000,4000), label = c('jan', 'feb', 'mar', 'apr'))

var layer_left   = 0.05;
var layer_top    = 0.1;
var layer_height = 0.85;
var layer_width  = 0.9;

function actual_max() {return d3.max(data, function (d) {return d.n; }); }
function col_width()  {return width / data.length * layer_width;}
function col_heigth() {return (height /actual_max()) * layer_height; }

var col_top    = height * layer_top;
var col_left   = width * layer_left;

var cols = svg.selectAll('rect').data(data);

cols.enter().append('rect')
  .attr('height', function(d) {return (d.n * col_heigth()); })
  .attr('width', col_width())
  .attr('x', function(d, i) {return (i * col_width()) + (width * layer_left); })
  .attr('y', function(d) {return col_top + ((actual_max() - d.n) * col_heigth()); })
  .attr('fill', '#0072B2')
  .attr('opacity', function(d) { return d.n / actual_max() })
  .attr('stroke', 'white')
  .attr('d', function(d) { return d.label; })
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
        .attr('opacity', function(d) { return d.n / actual_max() })
        .attr('fill', '#0072B2');
  });      
      
cols.exit().remove();

cols.transition()
  .duration(500)
  .attr('height', function(d) {return (d.n * col_heigth()); })
  .attr('width', col_width())
  .attr('x', function(d, i) {return (i * col_width()) + (width * layer_left); })
  .attr('y', function(d) {return col_top + ((actual_max() - d.n) * col_heigth()); })
  .attr('fill', '#0072B2')
  .attr('opacity', function(d) { return d.n / actual_max() })
  .attr('stroke', 'white')
  .attr('d', function(d) { return d.label; });    

// Identity labels

var txt = svg.selectAll('text').data(data);

txt.enter().append('text')
    .attr('x', function(d, i) {return (i * col_width()) + (width * layer_left) + (col_width() * 0.5); })
    .attr('y', function(d) {return height * 0.99;})
    .style('font-size', '10px') 
    .text(function(d) {return d.label;})
    .style('font-family', 'sans-serif')
    .attr('text-anchor', 'middle');
      
      
txt.exit().remove();

txt.transition()
  .duration(500)
    .attr('x', function(d, i) {return (i * col_width()) + (width * layer_left) + (col_width() * 0.5); })
    .attr('y', function(d) {return height * 0.99;})
    .style('font-size', '10px') 
    .text(function(d) {return d.label;})
    .style('font-family', 'sans-serif')
    .attr('text-anchor', 'middle');

// Numeric labels

var totals = svg.selectAll().data(data);

totals.enter().append('text')
      .attr('x', function(d, i) {return (i * col_width()) + (width * layer_left) + (col_width() * 0.5); })
      .attr('y', function(d) {return (height -  (d.n * col_heigth()) + col_top); })
      .text(function(d) {return d.n; })
      .attr('text-anchor', 'middle')
      .style('font-size', '10px') 
      .style('font-family', 'sans-serif')
      ;  
      
totals.exit().remove();

totals.transition()
  .duration(500)
      .attr( 'x',      function(d, i) {return (i * col_width()) + (width * layer_left) + (col_width() * 0.5); })
      .attr( 'y',      function(d) {return (height -  (d.n * col_heigth()) + col_top); })
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
