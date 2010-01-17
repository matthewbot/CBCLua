import socket
import threading

INTERACT_PORT = 6754
MULTICAST_ADDR = ("239.2.1.1", 13590)

class CBCConnection(threading.Thread):
	def __init__(self, callbacks, ip):
		threading.Thread.__init__(self, name="CBCConnection")
		self.daemon = True
		self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)
		self.sockbuf = ""
		self.callbacks = callbacks
		self.ip = ip
		self.closed = False
		self.start()
	
	def close(self):
		self.closed = True
		self.sock.shutdown(socket.SHUT_RDWR)
		
	def send_expr(self, expr):
		self.write_line("EXPR")
		self.write_data(expr)
		
	def send_stop(self):
		self.write_line("STOPTASKS")
		
	def run(self):
		self.sock.connect((self.ip, INTERACT_PORT))
		name = self.recv_line()
		version = self.recv_line()
		self.callbacks.on_net_version(name, version)
		
		while True:
			line = self.recv_line()
			if line == "RESULT":
				result = self.recv_data()
				self.callbacks.on_net_result(result)
			elif line == "ERROR":
				error = self.recv_data()
				self.callbacks.on_net_error(error)
			elif line == None:
				break
				
		if not self.closed:
			self.callbacks.on_net_connerror()
		self.sock.close()
				
	def recv_line(self):
		while True:
			(line, newline, remaining) = self.sockbuf.partition("\n")
			if newline == "\n":
				break	
			data = self.sock.recv(1024)
			if data == "":
				return None
			self.sockbuf += data
		self.sockbuf = remaining
		return line
		
	def recv_data(self):
		amt = int(self.recv_line())
		while len(self.sockbuf) < amt:
			data = self.sock.recv(1024)
			if data == "":
				return None
			self.sockbuf += data
		
		result = self.sockbuf[:amt]
		self.sockbuf = self.sockbuf[amt:]
		return result
		
	def write_line(self, msg):
		self.sock.sendall(msg + "\n")
		
	def write_data(self, data):
		self.write_line(str(len(data)))
		self.sock.sendall(data)
		
		
class CBCFinder(threading.Thread):
	def __init__(self, callbacks):
		threading.Thread.__init__(self, name="CBCFinder")
		self.daemon = True
		self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
		self.callbacks = callbacks
		
		self.start()
		
	def send_request(self):
		self.sock.sendto("interact", MULTICAST_ADDR)
		
	def run(self):
		while True:
			(msg, (ip, port)) = self.sock.recvfrom(128)
			[name, conns] = msg.split(",")
			self.callbacks.on_net_cbcresponse(name, ip, conns)		
