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
		self.sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
		self.sockbuf = ""
		self.callbacks = callbacks
		self.ip = ip
		self.closed = False
		self.host = None
		self.start()
	
	def close(self):
		self.closed = True
		self.sock.shutdown(socket.SHUT_RDWR)
		
	def get_host(self):
		return self.host
		
	def send_expr(self, expr):
		self.write_line("EXPR")
		self.write_data(expr)
		
	def send_runmain(self):
		self.write_line("RUNMAIN")
		
	def send_stop(self):
		self.write_line("STOPTASKS")
		
	def send_stop_task(self, taskid):
		self.write_line("STOPTASK")
		self.write_line(str(taskid))
		
	def send_button_down(self, button):
		self.write_line("BUTTONDOWN")
		self.write_line(button)
		
	def send_button_up(self, button):
		self.write_line("BUTTONUP")
		self.write_line(button)
		
	def send_clear_code(self):
		self.write_line("CLEARCODE")
		
	def send_clear_display(self):
		self.write_line("CLEARDISP")
		
	def send_reset(self):
		self.write_line("RESET")
	
	def send_make_code_dir(self, codedir):		
		self.write_line("MKCODEDIR")
		self.write_line(codedir)
		
	def send_put_file(self, cbcfilepath, localfilepath):
		self.write_line("PUTCODE")
		self.write_line(cbcfilepath)
		
		f = open(localfilepath, "r")
		self.write_data(f.read())
		f.close()
		
	def run(self):
		try:
			self.sock.connect((self.ip, INTERACT_PORT))
			name = self.recv_line()
			version = self.recv_line()
			self.host = self.recv_line()
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
				elif line == "TASKLIST":
					tasklist = self.recv_tasklist()
					self.callbacks.on_net_tasklist(tasklist)
				elif line == "DISPLAYCLEAR":
					self.callbacks.on_net_display_clear()
				elif line == "BEEP":
					self.callbacks.on_net_beep()
				elif line is None:
					break
		except IOError:
			pass
			
		if not self.closed:
			self.callbacks.on_net_connerror()
		self.sock.close()
				
	def recv_tasklist(self):
		tasklist = []
	
		while True:
			taskid = self.recv_line()
			if taskid == "":
				break
			taskid = int(taskid)
			taskname = self.recv_line()
			taskstate = self.recv_line()
			tasktype = self.recv_line()
			tasklist.append((taskid, taskname, taskstate, tasktype))
		return tasklist
				
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
