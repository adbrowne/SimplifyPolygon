function loadFile(fileNum,map){
  var colors = []
  colors.push("#FF4848")
  colors.push("#9669FE")
  colors.push("#23819C")
  colors.push("#01F33E")
  colors.push("#DFE32D")
  colors.push("#DFE32D")
  colors.push("#FF800D")
  colors.push("#4A9586")
  
  $.getJSON("./mapdata/"+fileNum+"_simple.js", function(data){
    var mapPoints = data.mappoints;
    var shapes = []

    var shapeCount = mapPoints.length
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
    }
  // Construct the polygon
  // Note that we don't specify an array or arrays, but instead just
  // a simple array of LatLngs in the paths property
  bermudaTriangle = new google.maps.Polygon({
    paths: shapes,
                  strokeColor: colors[fileNum], 
                  strokeOpacity: 0.8,
                  strokeWeight: 2,
                  fillColor: colors[fileNum],
                  fillOpacity: 0.35
  });

  bermudaTriangle.setMap(map);
  });


}
var latlng = new google.maps.LatLng(-29.000221252441406,141.00265502929688);
var myOptions = {
  zoom: 4,
  center: latlng,
  mapTypeId: google.maps.MapTypeId.ROADMAP
};
var map = new google.maps.Map(document.getElementById("map_canvas"),
    myOptions);

for(var fileNum = 0; fileNum <= 7; fileNum++){
  loadFile(fileNum, map)
}

