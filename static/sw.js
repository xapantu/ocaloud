this.addEventListener('install', function(event) {
  event.waitUntil(
    caches.open('v2').then(function(cache) {
      return cache.addAll([
        '/css/ocaloud.css',
		'/',
		'/p/california',
		'/v/california',
		'/ocaloudcore.js'
      ]);
    })
  );
});
