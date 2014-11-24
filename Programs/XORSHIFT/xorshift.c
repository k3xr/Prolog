/* Simple C implementation of the xorshift algorithm for encrypting/decrypting 16-bit blocks of data using a 64-bit key.

 *

 * Who: Miguel García Remesal (mgremesal@fi.upm.es)

 * When: November 7, 2014.

 */

#include <stdio.h>
#include <stdint.h>

void xorshift_encrypt(uint16_t data, uint64_t key, uint16_t *edata) {

	int8_t i;
	uint8_t tmp;
	uint16_t carry;

	// Initialize edata.
	*edata = data;

	// For each bit of the data buffer to be encrypted do as follows:
	for (i = 16; i > 0; i--) {

		// Get the lower three bits of the data buffer.
		tmp = *edata & 0x07;

		// Use such value as an index to retrieve the key byte to be used.
		tmp = 0xFF & (key >> (8 * tmp));

		// XOR the chosen key byte with the most significant byte of the data buffer.
		*edata ^= (uint16_t)(tmp << 8);

		// Record the value of the least significant bit of the data buffer.
		carry = *edata & 0x0001;

		// Right shift the data buffer 1 position.
		*edata >>= 1;

		// Adjust the most significant bit of the data buffer. Note that this is actually a circular shift.

		if (carry)
			*edata |= 0x8000;
		else

			*edata &= 0x7FFF;
	}

	// Return control to the caller.
	return;
}



void xorshift_decrypt(uint16_t data, uint64_t key, uint16_t *ddata) {

	int8_t i;
	uint8_t tmp; 
	uint16_t carry;

	// Initialize ddata;
	*ddata = data;

	// For each bit of the data buffer to be decrypted do as follows:
	for (i = 16; i > 0; i--) {

		// Record the value of the most significant bit of the data buffer.
		carry = *ddata & 0x8000;

		// Left shift the data buffer 1 position.
		*ddata <<= 1;

		// Adjust the least significant bit of the data bit. Note that this is actually a circular shift.

		if (carry)
			*ddata |= 0x0001;
		else
			*ddata &= 0xFFFE;

		// Get the lower three bits of the least significant byte of the data buffer.
		tmp = *ddata & 0x07;

		// Use such value as an index to retrieve the key byte to be used.
		tmp = 0xFF & (key >> (8 * tmp));

		// XOR the chosen key byte with the most significant byte of the data buffer.
		*ddata ^= (uint16_t)(tmp << 8);
	}

	// Return control to the caller.

}


void main(int argc, char *argv) {

	uint16_t data = 0x5A7B, edata, ddata;
	uint64_t key = 0x0123456789ABCDEF;

	// Encrypt.
	xorshift_encrypt(data, key, &edata);

	// Print encrypted value.
	fprintf(stdout, "xorshift_encrypt(0x%04x, 0x%016llx) = 0x%04x.\n", data, key, edata);

	// Decrypt.
	xorshift_decrypt(edata, key, &ddata);

	// Print decrypted value.
	fprintf(stdout, "xorshift_decrypt(0x%04x, 0x%016llx) = 0x%04x.\n", edata, key, ddata);

	// Return control to the OS.

	return;

}