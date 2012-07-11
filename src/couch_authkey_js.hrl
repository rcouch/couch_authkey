-define(COUCH_AUTKEY_VIEW_BY_KEY, {[{<<"map">>, <<"
    function(doc) {
        if (doc.auth_keys) {
            for (var i=0; i < doc.auth_keys.length; i++) {
                var key = doc.auth_keys[i];
                emit(key.id, key);
            }
        }
    }">>
}]}).
