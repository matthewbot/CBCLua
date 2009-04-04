#ifndef __CBCSERIAL_H__
#define __CBCSERIAL_H__

#define CREATE_UART_TX "/tmp/uart1tx"
#define CREATE_UART_RX "/tmp/uart1rx"

void serial_init();
void serial_quit();

int serial_read(char *data, int count);
int serial_write(char *data, int count);

char serial_read_byte();
void serial_wryte_byte(char byte);

#endif
