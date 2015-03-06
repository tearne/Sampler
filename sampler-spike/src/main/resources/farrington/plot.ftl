<!DOCTYPE html>
<meta charset="utf-8">
<style>
body {
	font: 10px sans-serif;
}

.axis path, .axis line {
	fill: none;
	stroke: #000;
	shape-rendering: crispEdges;
}

.x.axis path {
	display: none;
}

.line {
	fill: none;
	stroke-width: 1.5px;
}
</style>
	<script type="text/javascript">
		var jsonData = ${jsonData}
	</script>
<body>
	<script src="http://d3js.org/d3.v3.js"></script>
	<script>
		var margin = 
			{top : 20, right : 80, bottom : 30, left : 50}, 
			width = 800 - margin.left - margin.right, 
			height = 300 - margin.top - margin.bottom;

		var parseYearMonth = d3.time.format("%Y-%m").parse;

		var x = d3.time.scale().range([ 0, width ]);
		var y = d3.scale.linear().range([ height, 0 ]);

		var xAxis = d3.svg.axis().scale(x).orient("bottom");
		var yAxis = d3.svg.axis().scale(y).orient("left");

		var line = d3.svg.line()
			.x(function(d) {return x(d.x);})
			.y(function(d) {return y(d.y);});

		var svg = d3.select("body")
			.append("svg")
			.attr("width",width + margin.left + margin.right)
			.attr("height",height + margin.top + margin.bottom)
			.append("g")
			.attr("transform","translate(" + margin.left + "," + margin.top + ")");

		var colour = d3.scale.category10();

		jsonData.month.forEach(function(d, i) {
			jsonData.month[i] = parseYearMonth(d)
		});

		var lines = [ "actual", "threshold" ];

		var linesData = lines.map(function(name) {
			return {
				"name" : name,
				"values" : jsonData["month"].map(function(month, index) {
					return {
						y : jsonData[name][index],
						x : month
					};
				})
			};
		});
		
		linesData.forEach(function(d) {
			d.values.sort();
			d.values.reverse();
		});

		x.domain(d3.extent(jsonData.month));
		y.domain(d3.extent(jsonData.actual.concat(jsonData.threshold)));

		svg.append("g")
			.attr("class", "x axis")
			.attr("transform","translate(0," + height + ")")
			.call(xAxis);

		svg.append("g")
			.attr("class", "y axis")
			.call(yAxis)
			.append("text")
			.attr("transform", "rotate(-90)")
			.attr("y", 6)
			.attr("dy",".71em")
			.style("text-anchor", "end")
			.text("Value");

		var linePlots = svg.selectAll(".path")
			.data(linesData)
			.enter()
			.append("g")
			
		linePlots.append("path")
			.attr("class", "line")
			.attr("d", function(d) {
				return line(d.values);
			})
			.attr("stroke", function(d) {
				return colour(d.name);
			});
		
		linePlots.append("text")
			.datum(function(d) {
				return {name: d.name, value: d.values[d.values.length - 1]}; })
			.attr("transform", function(d) { 
				return "translate(" + x(d.value.x) + "," + y(d.value.y) + ")"; })
			.attr("x", 3)
			.attr("dy", ".35em")
			.text(function(d) {return d.name; })
	</script>
</body>