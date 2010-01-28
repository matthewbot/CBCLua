import socket
import threading
import os

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
		
	def send_runmain(self):
		self.write_line("RUNMAIN")
		
	def send_stop(self):
		self.write_line("STOPTASKS")
		
	def send_button_down(self, button):
		self.write_line("BUTTONDOWN")
		self.write_line(button)
		
	def send_button_up(self, button):
		self.write_line("BUTTONUP")
		self.write_line(button)
		
	def send_clear_code(self):
		self.write_line("CLEARCODE")
		
	def send_reset_env(self):
		self.write_line("RESETENV")
		
	def send_download(self, rootpath):
		self.send_stop()
		self.send_clear_code()
	
		for curpath, subdirs, filenames in os.walk(rootpath):
			if curpath == rootpath:
				cbccurpath = ""
			else:
				cbccurpath = curpath[len(rootpath)+1:] + "/" # cbc-relative path is the current path sans the root path
			curpath += "/"
			
			for subdir in subdirs:
				cbcdir = cbccurpath + subdir
				self.make_code_dir(cbcdir)
				
			for filename in filenames:
				cbcfilepath = cbccurpath + filename
				localfilepath = curpath + filename
				self.put_file(cbcfilepath, localfilepath)	
				
		self.send_reset_env()
	
	def make_code_dir(self, codedir):		
		self.write_line("MKCODEDIR")
		self.write_line(codedir)
		
	def put_file(self, cbcfilepath, localfilepath):
		self.write_line("PUTCODE")
		self.write_line(cbcfilepath)
		
		f = open(localfilepath, "r")
		self.write_data(f.read())
		f.close()
		
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
			elif line == "PRINT":
				msg = self.recv_data()
				self.callbacks.on_net_print(msg)
			elif line is None:
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
		self.sock.bind(("", 0))
		self.callbacks = callbacks
		
		self.start()
		
	def send_request(self):
		self.sock.sendto("interact", MULTICAST_ADDR)
		
	def run(self):
		while True:
			(msg, (ip, port)) = self.sock.recvfrom(128)
			[name, conns] = msg.split(",")
			self.callbacks.on_net_cbcresponse(name, ip, conns)		
