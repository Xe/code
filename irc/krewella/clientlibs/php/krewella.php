<?php

/**
 * Base Krewella Exception because apparently one-file one-function libraries need them.
 */
class KrewellaBaseException extends Exception {};

/**
 * Exception but with details. Takes and holds a code and a message.
 * If you make code anything but a number and message anything but a string you deserve whatever breaks.
 */
class KrewellaDetailedException extends KrewellaBaseException {
	public $code;
	public $message;

	public function __construct($code, $message) {
		$this->code = $code;
		$this->message = $message;
	}
};

/**
 * Thrown if we have no hostname to use. You need to have http as part of it or things will break.
 */
class KrewellaNoHostException extends KrewellaBaseException {};

/**
 * Thrown if we have no API key.
 */
class KrewellaNoAPIKeyException extends KrewellaBaseException {};

/**
 * Thrown if Krewella says it doesn't know that network.
 */
class KrewellaNoSuchNetworkException extends KrewellaBaseException {};

/**
 * Thrown if some real weid crap happened.
 */
class KrewellaSomeRealWeirdCrapHappenedException extends KrewellaDetailedException {};

/**
 * Take in a network name, channel name and message string, Return true or throw one of the above
 * exceptions if something abormal happens.
 */
function krewella_message($network, $channel, $message) {
	if(!defined('KREWELLA_HOST')) {
		if(!getenv('KREWELLA_HOST')) {
			throw new KrewellaNoHostException();
		} else {
			$host = getenv('KREWELLA_HOST');
		}
	} else {
		$host = constant('KREWELLA_HOST');
	}

	if(!defined('KREWELLA_KEY')) {
		if(!getenv('KREWELLA_KEY')) {
			throw new KrewellaNoAPIKeyException();
		} else {
			$key = getenv('KREWELLA_KEY');
		}
	} else {
		$key = constant('KREWELLA_KEY');
	}

	$ch = curl_init($host . '/message/' . $network . '/' . $channel);

	curl_setopt($ch, CURLOPT_HTTPHEADER, array(
		"X-Krewella-Auth: $key",
		'Content-Type: application/json',
	));
	curl_setopt($ch, CURLOPT_POSTFIELDS, $message);
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);
	curl_setopt($ch, CURLOPT_POST, TRUE);

	$reply = curl_exec($ch);
	$code = curl_getinfo($ch, CURLINFO_HTTP_CODE);

	if ($code == 404) {
		throw new KrewellaNoSuchNetworkException();
	} elseif ($code != 200) {
		throw new KrewellaSomeRealWeirdCrapHappenedException($code, $message);
	}

	curl_close($ch);

	return TRUE;
}
