// module WebRTC.MediaStream
//
exports.hasUserMedia = (typeof navigator === "object") && (typeof navigator.mediaDevices === "object") && (typeof navigator.mediaDevices.getUserMedia === "function")

exports._getUserMedia = function(constraints) {
    return function(onError, onSuccess) {
        navigator.mediaDevices.getUserMedia(constraints)
            .then(function(mediaStream) {
                onSuccess(mediaStream);
            })
            .catch(function(e) {
                onError(e);
            });

        // Canceller
        return function(cancelError, cancelerError, cancelerSuccess) {
            cancelerSuccess();
        };
    }
};


exports.stopMediaStream = function(mediaStream) {
    return function() {
        if (mediaStream.stop)
            mediaStream.stop();
        else {
            mediaStream.getAudioTracks().forEach(function (track) {
                track.stop();
            });
            mediaStream.getVideoTracks().forEach(function (track) {
                track.stop();
            });
        }
    };
};


exports.playAudioStream = function(mediaStream) {
    return function() {
        var player = new Audio();
        player.autoplay = true;
		player.srcObject = mediaStream;
    };
};


exports.createObjectURL = function(blob) {
    return function() {
        return URL.createObjectURL(blob);
    };
};
