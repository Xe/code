import concurrent.futures
import json
import socket
import struct

PROXY_TIMEOUT = 9
PROXY_JUDGE = "http://ip4.me/"
PROXY_GOOD_RESPONSE = "200 OK"
SOCKS_JUDGE = "188.165.3.145"
SOCKS_JUDGE_PORT = 80
CONNECT_PORTS = [6667, 6697, 443]


def big_recv(sock, amt):
    data = ""
    while amt > 0:
        tmp = sock.recv(amt)
        if tmp == "":
            return ""
        amt -= len(tmp)
        data += tmp

    return tmp


def try_close(sock):
    try:
        sock.close()
    except Exception:
        pass


def try_http(ip, port):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(PROXY_TIMEOUT)
    sock.connect((ip, port))
    sock.sendall("GET %s HTTP/1.1\r\n" % PROXY_JUDGE)
    sock.sendall("Host: %s\r\n" % PROXY_JUDGE)
    sock.sendall("Connection: Close\r\n")
    sock.sendall("User-Agent: Mozilla\r\n")
    sock.sendall("\r\n")
    data = sock.recv(4096)
    good = False
    if PROXY_GOOD_RESPONSE.lower() in data.lower():
        good = True

    try:
        sock.close()
    except Exception:
        pass

    return good


def try_socks4(ip, port):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(PROXY_TIMEOUT)
    sock.connect((ip, port))
    data = struct.pack("!bb", 0x04, 0x01)
    data += struct.pack("!H", SOCKS_JUDGE_PORT)
    data += socket.inet_aton(SOCKS_JUDGE)
    data += struct.pack("!b", 0x00)
    sock.sendall(data)
    data = big_recv(sock, 8)
    results = struct.unpack("!bbbbbbbb", data)
    good = True
    if results[0] != 0x00 or results[1] != 0x5A:
        good = False

    try:
        sock.close()
    except Exception:
        pass

    return good


def try_socks5(ip, port):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(PROXY_TIMEOUT)
    sock.connect((ip, port))
    data = struct.pack("!bbb", 0x05, 0x01, 0x00)
    sock.sendall(data)
    resp = struct.unpack("!bb", big_recv(sock, 2))
    if resp[0] != 0x05 or resp[1] != 0x00:
        try_close(sock)
        return False

    data = struct.pack("!bbbb", 0x05, 0x01, 0x00, 0x01)
    data += socket.inet_aton(SOCKS_JUDGE)
    data += struct.pack("!H", SOCKS_JUDGE_PORT)
    print("Data=%r" % data)
    sock.sendall(data)
    resp = struct.unpack("!bb", big_recv(sock, 2))
    good = True
    if resp[0] != 0x00 or resp[1] != 0x00:
        good = False

    try_close(sock)

    return good


def try_connect(ip, port):
    ports = []
    for c_port in CONNECT_PORTS:
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(PROXY_TIMEOUT)
            sock.connect((ip, port))

            data = "CONNECT %s:%d HTTP/1.1\r\n" % (SOCKS_JUDGE, c_port)
            data += "\r\n"
            sock.sendall(data)

            reply = sock.recv(4096)
            if "200" in reply:
                ports.append(c_port)

            try_close(sock)
        except Exception:
            import traceback
            #traceback.print_exc()

    return ports


def check_proxy(ip, port, timeout=PROXY_TIMEOUT):
    """
    I have found 9 to be a reasonable timeout over the years.

    @return ((ip, port), working, types, connect_ports)
    """
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(timeout)
    try:
        sock.connect((ip, port))
    except (socket.error, socket.timeout):
        return { "proxy": {"ip": ip, "port": port, "working": False}, "methods": [], "external_ports": [] }
    finally:
        try:
            sock.close()
        except Exception:
            pass

    methods = []

    for meth in [("http", try_http), ("socks4", try_socks4), ("socks5", try_socks5)]:
        good = False
        try:
            good = meth[1](ip, port)
        except Exception as e:
            import traceback
            #traceback.print_exc()

        if good:
            methods.append(meth[0])

    ports = []

    try:
        ports = try_connect(ip, port)
        if ports:
            methods.append("connect")
    except Exception:
        import traceback
        traceback.print_exc()

    good = len(methods) > 0

    return { "proxy": {"ip": ip, "port": port, "working": True}, "methods": methods, "external_ports": ports }
