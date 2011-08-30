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

    $.getJSON("./all_0_5.js", function(data){
      var shapes = data.shapes;

      var shapeCount = shapes.length
      for(var shapeIndex = 0; shapeIndex < shapeCount; shapeIndex +=1){
        var shape = shapes[shapeIndex]

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
                    strokeColor: "#000000",
                    strokeOpacity: 0.35,
                    strokeWeight: 0.5,
                    fillColor: colors[shape.recordNumber % 8],
                    fillOpacity: 0.35
    });

    bermudaTriangle.setMap(map);

      }
    })
}
var latlng = new google.maps.LatLng(-29.000221252441406,141.00265502929688);
var myOptions = {
  zoom: 4,
  center: latlng,
  mapTypeId: google.maps.MapTypeId.ROADMAP
};
var map = new google.maps.Map(document.getElementById("map_canvas"),
    myOptions);

loadFile(map)

