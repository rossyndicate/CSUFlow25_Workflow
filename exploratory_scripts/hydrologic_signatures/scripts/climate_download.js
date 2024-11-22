// Google Earth Engine script, copy and paste into GEE if you want to run it.
var start = '2000-01-01';
var end = '2024-01-01';

  // daily precipitation, evapotranspiration
var gridmet = ee.ImageCollection("IDAHO_EPSCOR/GRIDMET")
  .filterDate(start, end)
  .select('pr', 'eto', 'tmmn', 'tmmx', 'srad', 'vpd')

// swe, more precipitation, more srad, vpd
var daymet = ee.ImageCollection("NASA/ORNL/DAYMET_V4")
  .filterDate(start, end)

// Function to apply reduceRegions on each image in the collection
var reduceRegionsFunction = function(image) {
  // Perform the reduceRegions operation
  var reduced = image.reduceRegions({
    collection: basins,
    reducer: ee.Reducer.mean(),
    //reducer: ee.Reducer.sum(),  // Specify the reducer (e.g., mean, median, etc.)
    scale: 500                   // Gridmet is 4000m pixels, but this should resample to 500m before calculating
  });

  // Add a property to each feature to identify the date of the image
  return reduced.map(function(feature) {
    feature = feature.setGeometry(null) // remove geometry to save space
    return feature.set('date', image.date().format('YYYY-MM-dd'));
  });
};

// Map the reduceRegions function over the collection
//var reducedCollection = daymet.map(reduceRegionsFunction).flatten(); // choose gridmet or daymet
var reducedCollection = gridmet.map(reduceRegionsFunction).flatten();

// Export it
Export.table.toDrive(reducedCollection, 'gridmetSum_' + start + '_' + end, 'CWCB')
