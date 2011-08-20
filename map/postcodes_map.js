function loadFile(map, number){
  var colors = []
    colors.push("#FF4848")
    colors.push("#9669FE")
    colors.push("#23819C")
    colors.push("#01F33E")
    colors.push("#DFE32D")
    colors.push("#DFE32D")
    colors.push("#FF800D")
    colors.push("#4A9586")

    $.getJSON("./postcodes/"+number+".js", function(data){
      var shape = data

      var shapeCoords = [];
      var parts = shape.parts
      var partCount = parts.length

      for(var partIndex =0; partIndex < partCount; partIndex+=1){
        var partCoords = []
      var points = parts[partIndex].points;
    var pointCount = points.length
      for(var pointIndex = 0; pointIndex < pointCount; pointIndex+=1){
        var point = points[pointIndex];
        if(point != undefined){
          partCoords.push(new google.maps.LatLng(point.Y, point.X));
        }
      }
    shapeCoords.push(partCoords)
      }
    bermudaTriangle = new google.maps.Polygon({
      paths: shapeCoords,
                    strokeColor: colors["#000000"], 
                    strokeOpacity: 0.8,
                    strokeWeight: 2,
                    fillColor: colors[shape.recordNumber % 8],
                    fillOpacity: 0.35
    });

    bermudaTriangle.setMap(map);

    })
  /*jvar shapeCount = mapPoints.length
    for(var shapeIndex = 0; shapeIndex < shapeCount; shapeIndex+=1){
    var triangleCoords = [];
    var firstShape = mapPoints[shapeIndex];
    var pointCount = firstShape.length;
    for(var pointIndex = 0; pointIndex < pointCount; pointIndex+=1){
    var point = firstShape[pointIndex];
    if(point != undefined){
    triangleCoords.push(new google.maps.LatLng(point[1], point[0]));
    }
    }
    shapes.push(triangleCoords)
    }*/
  // Construct the polygon
  // Note that we don't specify an array or arrays, but instead just
  // a simple array of LatLngs in the paths property
  /*  bermudaTriangle = new google.maps.Polygon({
      paths: shapes,
      strokeColor: colors[fileNum], 
      strokeOpacity: 0.8,
      strokeWeight: 2,
      fillColor: colors[fileNum],
      fillOpacity: 0.35
      });

      bermudaTriangle.setMap(map);
      });

*/
}
var latlng = new google.maps.LatLng(-29.000221252441406,141.00265502929688);
var myOptions = {
  zoom: 4,
  center: latlng,
  mapTypeId: google.maps.MapTypeId.ROADMAP
};
var map = new google.maps.Map(document.getElementById("map_canvas"),
    myOptions);

for(var i=1689;i < 2050;i++){
loadFile(map,i)
}

