/*
	Copyright (c) 2008, Adobe Systems Incorporated
	Copyright (c) 2011, Pimm Hogeling and Edo Rivai
	Copyright (c) 2011, Cameron Desrochers
	All rights reserved.

	Redistribution and use in source and binary forms, with or without 
	modification, are permitted provided that the following conditions are
	met:

	* Redistributions of source code must retain the above copyright notice, 
	this list of conditions and the following disclaimer.

	* Redistributions in binary form must reproduce the above copyright
	notice, this list of conditions and the following disclaimer in the 
	documentation and/or other materials provided with the distribution.

	* Neither the name of Adobe Systems Incorporated nor the names of its 
	contributors may be used to endorse or promote products derived from 
	this software without specific prior written permission.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
	IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
	THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
	PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
	CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
	EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
	PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
	PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
	LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
	NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
	SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package;
import flash.display.Bitmap;
import flash.display.BitmapData;
import flash.geom.Rectangle;
import flash.Lib;
import flash.Memory;
import flash.system.ApplicationDomain;
import flash.utils.ByteArray;
import flash.utils.Endian;
import DeflateStream;


/**
 * Class that converts BitmapData into a valid PNG
 */
class OptimizedPNGEncoder
{
	// TODO: Unroll tight loops
	
	
	private static inline var CRC_TABLE_END = 256 * 4;
	private static inline var CHUNK_START = CRC_TABLE_END;
	private static var data : ByteArray;
	
	
	/**
	 * Creates a PNG image from the specified BitmapData.
	 * Uses flash.Memory to speed things up
	 *
	 * @param image The BitmapData that will be converted into the PNG format.
	 * @return a ByteArray representing the PNG encoded image data.
	 * @playerversion Flash 10
	 */
	public static function encode(img:BitmapData):ByteArray
	{
		// Save current domain memory and restore it after, to avoid
		// conflicts with other components using domain memory
		var oldFastMem = ApplicationDomain.currentDomain.domainMemory;
		
		// Data will be select()ed for use with fast memory
		// The first 256 * 4 bytes are the CRC table
		// Inner chunk data is appended to the CRC table, starting at CHUNK_START
		
		initialize();		// Sets up data var & CRC table
		
		
		// Create output byte array
		var png:ByteArray = new ByteArray();
		
		writePNGSignature(png);
		
		// Build chunks (get stored in data starting at CHUNK_START)
		
		var chunkLength = buildIHDRChunk(img);
		writeChunk(png, 0x49484452, chunkLength);
		
		chunkLength = buildIDATChunk(img);
		writeChunk(png, 0x49444154, chunkLength);
		
		writeChunk(png, 0x49454E44, 0);
		
		ApplicationDomain.currentDomain.domainMemory = oldFastMem;
		
		png.position = 0;
		return png;
	}

	private static inline function writePNGSignature(png : ByteArray)
	{
		png.writeUnsignedInt(0x89504e47);
		png.writeUnsignedInt(0x0D0A1A0A);
	}
	
	
	private static inline function buildIHDRChunk(img : BitmapData)
	{
		var chunkLength = 13;
		data.length = Std.int(Math.max(CHUNK_START + chunkLength, ApplicationDomain.MIN_DOMAIN_MEMORY_LENGTH));
		Memory.select(data);
		
		writeI32BE(CHUNK_START, img.width);
		writeI32BE(CHUNK_START + 4, img.height);
		
		Memory.setByte(CHUNK_START + 8, 8);		// Bit depth
		
		if (img.transparent) {
			Memory.setByte(CHUNK_START + 9, 6);		// RGBA colour type
		}
		else {
			Memory.setByte(CHUNK_START + 9, 2);		// RGB colour type
		}
		
		Memory.setByte(CHUNK_START + 10, 0);	// Compression method (always 0 -> zlib)
		Memory.setByte(CHUNK_START + 11, 0);	// Filter method (always 0)
		Memory.setByte(CHUNK_START + 12, 0);	// No interlacing
		
		return chunkLength;
	}
	
	
	// Copies length bytes (all by default) from src into flash.Memory at the specified offset
	private static inline function memcpy(src : ByteArray, offset : UInt, length : UInt = 0) : Void
	{
		src.readBytes(ApplicationDomain.currentDomain.domainMemory, offset, length);
	}
	
	// Writes one integer into flash.Memory at the given address, in big-endian order
	private static inline function writeI32BE(addr: UInt, value : UInt) : Void
	{
		Memory.setByte(addr, value >>> 24);
		Memory.setByte(addr + 1, value >>> 16);
		Memory.setByte(addr + 2, value >>> 8);
		Memory.setByte(addr + 3, value);
	}
	
	
	private static inline function buildIDATChunk(img : BitmapData)
	{
		var width = img.width;
		var height = img.height;
		
		var bytesPerPixel = img.transparent ? 4 : 3;
		
		// Length of IDAT data: 3 or 4 bytes per pixel + 1 byte per scanline
		var length : UInt = width * height * bytesPerPixel + height;
		
		// Size needed to store byte array of bitmap
		var scratchSize : UInt = width * height * 4;
		var totalScratchSize = scratchSize + DeflateStream.SCRATCH_MEMORY_SIZE;
		
		// Memory layout:
		// CHUNK_START: Deflated data (written last)
		// CHUNK_START + deflated data buffer: scatch (raw image bytes)
		// CHUNK_START + deflated data buffer + scratchSize: deflate scratch
		// CHUNK_START + deflated data buffer + totalScratchSize: Uncompressed PNG-format image data
		
		data.length = Std.int(Math.max(CHUNK_START + DeflateStream.maxOutputBufferSize(length) + totalScratchSize + length, ApplicationDomain.MIN_DOMAIN_MEMORY_LENGTH));
		Memory.select(data);
		
		var scratchAddrStart : UInt = CHUNK_START + DeflateStream.maxOutputBufferSize(length);
		var addrStart : UInt = scratchAddrStart + totalScratchSize;
		
		var addr = addrStart;
		var scratchAddr = scratchAddrStart;
		
		var imgBytes = img.getPixels(new Rectangle(0, 0, width, height));
		imgBytes.position = 0;
		memcpy(imgBytes, scratchAddr);
		
		if (img.transparent) {
			for (i in 0 ... height) {
				Memory.setByte(addr, 0);		// No filter
				addr += 1;
				
				// Copy line, moving alpha byte to end
				for (j in 0 ... width) {
					Memory.setI32(addr, Memory.getI32(scratchAddr) >>> 8);
					Memory.setByte(addr + 3, Memory.getByte(scratchAddr + 0));
					addr += 4;
					scratchAddr += 4;
				}
			}
		}
		else {
			for (i in 0 ... height) {
				Memory.setByte(addr, 0);		// No filter
				addr += 1;
				
				// Copy line
				for (j in 0 ... width) {
					Memory.setByte(addr + 0, Memory.getByte(scratchAddr + 1));
					Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2));
					Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3));
					addr += 3;
					scratchAddr += 4;
				}
			}
		}
		
		var deflateScratchAddrStart = scratchAddrStart + scratchSize;
		var deflateStream = new DeflateStream(FAST, true, deflateScratchAddrStart, CHUNK_START);
		deflateStream.fastWriteBlock(addrStart, addrStart + length, true);
		var range = deflateStream.fastFinalize();
		
		return range.len();
	}
	
	

	private static inline function writeChunk(png : ByteArray, type : UInt, chunkLength : UInt) : Void
	{
		var len = chunkLength;
		
		png.writeUnsignedInt(len);
		png.writeUnsignedInt(type);
		if (len != 0) {
			data.position = CHUNK_START;
			data.readBytes(png, png.position, chunkLength);
			png.position += len;
		}
		
		var c : UInt = 0xFFFFFFFF;
		
		// Unroll first four iterations from type bytes, rest use chunk data
		c = crcTable(c ^ (type >>> 24)) ^ (c >>> 8);
		c = crcTable(c ^ ((type >>> 16) & 0xFF)) ^ (c >>> 8);
		c = crcTable(c ^ ((type >>> 8) & 0xFF)) ^ (c >>> 8);
		c = crcTable(c ^ (type & 0xFF)) ^ (c >>> 8);
		
		if (len != 0) {
			for (i in CHUNK_START ... len + CHUNK_START) {
				c = crcTable(c ^ Memory.getByte(i)) ^ (c >>> 8);
			}
		}
		c ^= 0xFFFFFFFF;
		
		png.writeUnsignedInt(c);
	}
	
	
	
	private static var crcComputed = false;
	
	private static inline function initialize() : Void
	{
		if (!crcComputed) {
			data = new ByteArray();
			data.length = Std.int(Math.max(CHUNK_START, ApplicationDomain.MIN_DOMAIN_MEMORY_LENGTH));
		}
		
		Memory.select(data);
		
		if (!crcComputed) {
			var c : UInt;
			for (n in 0 ... 256) {
				c = n;
				
				for (k in 0 ... 8) {
					if (c & 1 == 1) {
						c = 0xedb88320 ^ (c >>> 1);
					} else {
						c >>>= 1;
					}
				}
				
				Memory.setI32(n << 2, c);
			}
			
			crcComputed = true;
		}
	}
	
	private static inline function crcTable(index : UInt) : UInt
	{
		return Memory.getI32((index & 0xFF) << 2);
	}
}






























