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

    $.getJSON("./union_encoded.js", function(data){
      var parts = data;
      var allCoords = []
      var partCount = parts.length

      for(var partIndex =0; partIndex < partCount; partIndex+=1){
      
        var partCoords = []
      var encodedPoints = parts[partIndex];
      var points = google.maps.geometry.encoding.decodePath(encodedPoints);
    var pointCount = points.length
      for(var pointIndex = 0; pointIndex < pointCount; pointIndex+=1){
        var point = points[pointIndex];
        if(point != undefined){
          partCoords.push(new google.maps.LatLng(point.lat(), point.lng()));
        }
      }
      allCoords.push(partCoords);
      }
    bermudaTriangle = new google.maps.Polygon({
      paths: allCoords,
                    strokeColor: "#FF0000",
                    strokeOpacity: 0.35,
                    strokeWeight: 0.5,
                    fillColor: "#FF0000",
                    fillOpacity: 0.35
    });

    bermudaTriangle.setMap(map);

      }
    )
}
var latlng = new google.maps.LatLng(-29.000221252441406,141.00265502929688);
var myOptions = {
  zoom: 4,
  center: latlng,
  mapTypeId: google.maps.MapTypeId.ROADMAP
};
var map = new google.maps.Map(document.getElementById("map_canvas"),
    myOptions);

var encoded = google.maps.geometry.encoding.encodePath([new google.maps.LatLng(-34.59553987981164, 151.09722811874997)])

console.log(encoded)
loadFile(map)

