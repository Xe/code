proc gen-oline { name passwordhash } {
	return "operator \"$name\" {\n\tuser = \"*@*\";\n\tpassword = \"$passwordhash\";\n\tsnomask = \"+bcdfFknrsSuWxyz\";\n\tflags = encrypted, need_ssl;\n\tprivset = \"admin\";\n};"
}

proc gen-cnline { servername host mypass theirpass hubmask } {
	return "connect \"$servername\" {\n\thost = \"$host\";\n\taccept_password = \"$theirpass\";\n\tport = 6697;\n\tflags = topicburst, autoconn, ssl;\n\thub_mask = \"$hubmask\";\n\tclass = \"server\";\n};"
}

proc gen-mline { servername sid description netname netdesc helpchan helpurl ssldcount } {
	
}

