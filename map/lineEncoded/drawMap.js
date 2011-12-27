(function($){
  var bounds = {
    xMin: undefined, 
    xMax: undefined,
    yMin: undefined,
    yMax: undefined
  }

  var width = 600;
  var height = 300;

  var paper = Raphael(2, 2, width, height);

  var translatePoint = function(point){
    var xScale = width / (bounds.xMax - bounds.xMin);
    var yScale = height / (bounds.yMax - bounds.yMin);
    return {
      x: (point.x - bounds.xMin) * xScale,
      y: (point.y  - bounds.yMin) * yScale
    };
  }

  var printPoint = function(point){
      return point.x + " " + point.y;
  }
  
  var max = function(left,right){
    if(left == undefined)
      return right;

    if(right == undefined)
      return left;

    return Math.max(left,right);
  }

  var min = function(left,right){
    if(left == undefined)
      return right;

    if(right == undefined)
      return left;

    return Math.min(left,right);
  }

  var getBounds = function(data){
    return _.reduce(data.segments, function(memo, segment){
      return _.reduce(segment.points, function(memo, point){
        var xMin = min(memo.xMin, point.x);
        var yMin = min(memo.yMin, point.y);
        var xMax = max(memo.xMax, point.x);
        var yMax = max(memo.yMax, point.y);
        return {
          xMin: xMin,
          xMax: xMax,
          yMin: yMin,
          yMax: yMax
        }
      }, memo);
    }, bounds);

  }

  $.getJSON("lineencoded.js",function(data){
    bounds = getBounds(data)
    console.log(bounds);
    _.each(data.regions, function(region){
      console.log(region.name);
      var pathString = "";
      var points = _.flatten(_.map(region.segments, function(segmentId){
        var segment = _.find(data.segments, function(segment){
          return segment.id === segmentId;
        });
        return segment.points;
      }));
      
      for(i = 0; i < points.length; i+=1){
        var point = translatePoint(points[i]);
        if(i ===0){
          pathString = "M " + printPoint(point)
        }	
        else{
          pathString = pathString + "L " + printPoint(point)
        }
      }
      pathString = pathString + "Z";
      var path = paper.path(pathString);
      path.animate({fill: "#fff", stroke: "#666"}, 5);
      
      console.log(points);
    });
    console.log(data);
  });

  console.log("test");
}(jQuery))
