// module WebRTC.RTC
//
exports.hasRTC = typeof RTCPeerConnection === "function"

exports.newRTCPeerConnection = function(psConfig) {
    return function() {
        function nativeIceServer(psIceServer) {
            return psIceServer.value0;
        };
        return new RTCPeerConnection({
            bundlePolicy: psConfig.bundlePolicy.value0,
            iceServers: psConfig.iceServers.map(nativeIceServer),
            iceCandidatePoolSize: psConfig.iceCandidatePoolSize,
            iceTransportPolicy: psConfig.iceTransportPolicy.value0, // TODO
            peerIdentity: psConfig.peerIdentity.value0
        });
    };
};


exports.closeRTCPeerConnection = function(pc) {
    return function() {
        pc.close();
    };
};


exports.onclose = function(handler) {
    return function(pc) {
        return function() {
            pc.oniceconnectionstatechange = function() {
                if(pc.iceConnectionState == 'disconnected' || pc.iceConnectionState == 'closed') {
                    handler();
                }
            }
        };
    };
};


exports.addStream = function(stream) {
    return function(pc) {
        return function() {
            pc.addStream(stream);
            //stream.getTracks().forEach(function(track) {
                //pc.addTrack(track);
            //});
        };
    };
};


exports._signalingState = function(pc) {
    return function() {
        return pc.signalingState;
    };
};


exports.onicecandidate = function(f) {
    return function(pc) {
        return function() {
            pc.onicecandidate = function(event) {
                f(event)();
            };
        };
    };
};


exports.ontrack = function(f) {
    return function(pc) {
        return function() {
            pc.ontrack = function(event) {
                f(event)();
            };
        };
    };
};


exports._createOffer = function(pc) {
    return function(error, success) {
    pc.createOffer()
        .then(function(offer) {
            success(new RTCSessionDescription(offer))();
        })
        .catch(error);
        return function(a, b, cancelerSuccess) { cancellerSuccess(); };
    };
};


exports._createAnswer = function(pc) {
    return function(error, success) {
        pc.createAnswer(success, error);
        return function(a, b, cancelerSuccess) { cancellerSuccess(); };
    };
};


exports._setLocalDescription = function(desc) { return function(pc) {
    return function(error, success) {
        pc.setLocalDescription(desc, success, error);
        return function(a, b, cancelerSuccess) { cancellerSuccess(); };
    };
};};


exports._setRemoteDescription = function(desc) { return function(pc) {
    return function(error, success) {
        pc.setRemoteDescription(desc, success, error);
        return function(a, b, cancelerSuccess) { cancellerSuccess(); };
    };
};};


exports._iceEventCandidate = function(nothing) {
    return function(just) {
        return function(e) {
            return e.candidate ? just(e.candidate) : nothing;
        };
    };
};


exports.addIceCandidate = function(c) {
    return function(pc) {
        return function() {
            pc.addIceCandidate(new RTCIceCandidate(c));
        };
    };
};


exports.newRTCSessionDescription = function(s) {
    return new RTCSessionDescription(s);
};


exports.createDataChannel = function(s) {
    return function(pc) {
        return function() {
            var dc = pc.createDataChannel(s);
            return dc;
        };
    };
};


exports.send = function(s) {
    return function(dc) {
        return function() {
            if (dc.readyState != "open") return;
            dc.send(s);
        };
    };
};


exports.onmessage = function(f) {
    return function(dc) {
        return function() {
            dc.onmessage = function(m) {
                f(m.data)();
            };
        };
    };
};


exports.ondatachannel = function(handler) {
    return function(pc) {
        return function() {
            pc.ondatachannel = function(ev) {
                handler(ev.channel)();
            };
        };
    };
}


exports.onopen = function(handler) {
    return function(dataChannel) {
        return function() {
            dataChannel.onopen = handler;
        };
    };
}