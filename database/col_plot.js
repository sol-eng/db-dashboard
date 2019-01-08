// !preview r2d3 data=data.frame(y = c(5000,2000,3000,4000), x = c(1,2,4,5), label = c('jan', 'feb', 'mar', 'apr'))

var layer_left   = 0.01;
    layer_top    = 0.2;
    layer_height = 0.7;
    layer_width  = 0.97;

function svg_height() {return parseInt(svg.style('height'))}
function svg_width() {return parseInt(svg.style('width'))}
function actual_max() {return d3.max(data, function (d) {return d.y; }); }
function col_width()  {return svg_width()  / data.length * layer_width;}
function col_heigth() {return (svg_height() /actual_max()) * layer_height; }

function col_top()  {return svg_height() * layer_top; }
function col_left() {return svg_width()  * layer_left;}

var cols = svg.selectAll('rect').data(data);

cols.enter().append('rect')
  .attr('height', function(d) {return (d.y * col_heigth()); })
  .attr('width', col_width())
  .attr('x', function(d, i) {return (i * col_width()) + (svg_width()* layer_left); })
  .attr('y', function(d) {return col_top() + ((actual_max() - d.y) * col_heigth()); })
  .attr('fill', '#009E73')
  .attr('opacity', 0.5)
  .attr('stroke', 'white')
  .attr('d', function(d) { return d.x; })
  .on("click", function(){
    Shiny.setInputValue(
      "column_clicked", 
      d3.select(this).attr("d"),
      {priority: "event"}
    );
  })    
  .on("mouseenter", function(){
      d3.select(this)
        .attr('opacity', 1)
        .attr('fill', '#ffb14e');
  })
  .on("mouseleave", function(){
      d3.select(this)
        .attr('opacity', 0.5)
        .attr('fill', '#009E73');
  });      
      
cols.exit().remove();

cols.transition()
  .duration(500)
  .attr('height', function(d) {return (d.y * col_heigth()); })
  .attr('width', col_width())
  .attr('x', function(d, i) {return (i * col_width()) + (svg_width()* layer_left); })
  .attr('y', function(d) {return col_top() + ((actual_max() - d.y) * col_heigth()); })
  .attr('fill', '#009E73')
  .attr('opacity', 0.5)
  .attr('stroke', 'white');

// Identity labels

var txt = svg.selectAll('text').data(data);

txt.enter().append('text')
    .attr('x', function(d, i) {return (i * col_width()) + (svg_width()* layer_left) + (col_width() * 0.5); })
    .attr('y', function(d) {return svg_height()* 0.95;})
    .style('font-size', '10px') 
    .text(function(d) {return d.label;})
    .style('font-family', 'sans-serif')
    .attr('text-anchor', 'middle');
      
      
txt.exit().remove();

txt.transition()
  .duration(500)
    .attr('x', function(d, i) {return (i * col_width()) + (svg_width()* layer_left) + (col_width() * 0.5); })
    .attr('y', function(d) {return svg_height()* 0.95;})
    .style('font-size', '10px') 
    .text(function(d) {return d.label;})
    .style('font-family', 'sans-serif')
    .attr('text-anchor', 'middle');

// Numeric labels

var totals = svg.selectAll('totals').data(data);

totals.enter().append('text')
      .attr('x', function(d, i) {return (i * col_width()) + (svg_width()* layer_left) + (col_width() * 0.5); })
      .attr('y', function(d) {return (col_top() * 0.9) + ((actual_max() - d.y) * col_heigth()); })
      .attr('text-anchor', 'middle')
      .style('font-size', '10px') 
      .style('font-family', 'sans-serif')
      .text(function(d) {return d.y; });  
      
totals.exit().remove();

totals.transition()
  .duration(500)
      .attr('x', function(d, i) {return (i * col_width()) + (svg_width()* layer_left) + (col_width() * 0.5); })
      .attr('y', function(d) {return col_top() + ((actual_max() - d.y) * col_heigth()); })
      .text(function(d) {return d.y; });

// Title
svg.append('text')
  .attr('x', svg_width()* 0.01)             
  .attr('y', svg_height()* 0.05)
  .style('font-size', '18px') 
  .style('font-family', 'sans-serif')
  .text('Total flights');
  
//Sub-title
svg.append('text')
  .attr('x', svg_width()* 0.99)             
  .attr('y', svg_height()* 0.05)
  .attr('text-anchor', 'end')
  .style('font-size', '12px') 
  .style('font-family', 'sans-serif')
  .text('Click bar for details');
