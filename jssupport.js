function jsmangle (s) {
    return s
	.replace (/ /g, '__')
	.replace (/\?/g, '_Q');
}
