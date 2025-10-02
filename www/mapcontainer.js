function showMapContainer() {
  var mapContainer = document.getElementById('windanalyzer_map_container');
  if (mapContainer) {
    mapContainer.classList.add('visible'); // Adiciona a classe para mostrar
  }
}

function hideMapContainer() {
  var mapContainer = document.getElementById('windanalyzer_map_container');
  if (mapContainer) {
    mapContainer.classList.remove('visible'); // Remove a classe para ocultar
  }
}