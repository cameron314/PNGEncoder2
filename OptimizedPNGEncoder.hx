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


/**
 * Class that converts BitmapData into a valid PNG
 */	
class OptimizedPNGEncoder {
	/**
	 * Creates a PNG image from the specified BitmapData.
	 * Uses flash.Memory to speed things up (so beware of conflicts)
	 *
	 * @param image The BitmapData that will be converted into the PNG format.
	 * @return a ByteArray representing the PNG encoded image data.
	 * @playerversion Flash 9.0
	 */			
	public static function encode(img:BitmapData):ByteArray {
		// Create output byte array
		var png:ByteArray = new ByteArray();
		
		writePNGSignature(png);
		
		// Build IHDR chunk
		var IHDR = buildIHDRChunk(img);
		writeChunk(png, 0x49484452, IHDR);
		
		var IDAT = buildIDATChunk(img);
		
		writeChunk(png, 0x49444154, IDAT);
		
		// Build IEND chunk
		writeChunk(png, 0x49454E44, null);
		
		png.position = 0;
		return png;
	}

	private static inline function writePNGSignature(png : ByteArray)
	{
		png.writeUnsignedInt(0x89504e47);
		png.writeUnsignedInt(0x0D0A1A0A);
	}
	
	
	private static inline function buildIHDRChunk(img : BitmapData) : ByteArray
	{
		var IHDR:ByteArray = new ByteArray();
		IHDR.length = 13;
		
		IHDR.writeInt(img.width);
		IHDR.writeInt(img.height);
		IHDR.writeUnsignedInt(0x08060000); // 32bit RGBA
		IHDR.writeByte(0);
		
		return IHDR;
	}
	
	
	// Copies length bytes (all by default) from src into flash.Memory at the specified offset
	private static inline function memcpy(src : ByteArray, offset : UInt, ?length : UInt) : Void
	{
		src.readBytes(ApplicationDomain.currentDomain.domainMemory, offset, length);
	}
	
	
	private static inline function buildIDATChunk(img : BitmapData)
	{
		var width = img.width;
		var height = img.height;
		
		// Length of IDAT: 4 bytes per pixel + 1 byte per scanline
		var length : UInt = width * height * 4 + height;
		
		// Size needed to store byte array of bitmap
		var scratchSize : UInt = width * height * 4;
		
		var IDAT:ByteArray = new ByteArray();		// IDAT + scratch at end
		IDAT.length = Std.int(Math.max(length + scratchSize, ApplicationDomain.MIN_DOMAIN_MEMORY_LENGTH));
		Memory.select(IDAT);
		
		var addr : UInt = 0;
		var scratchAddr = length;
		
		var imgBytes = img.getPixels(new Rectangle(0, 0, width, height));
		imgBytes.position = 0;
		memcpy(imgBytes, scratchAddr);
		
		if (img.transparent) {
			for (i in 0 ... height) {
				Memory.setByte(addr, 0);		// No filter
				addr += 1;
				
				// Copy line, moving alpha byte to end
				for (j in 0 ... width) {
					Memory.setByte(addr + 0, Memory.getByte(scratchAddr + 1));
					Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2));
					Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3));
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
				
				// Copy line, moving alpha byte to end
				for (j in 0 ... width) {
					Memory.setByte(addr + 0, Memory.getByte(scratchAddr + 1));
					Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2));
					Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3));
					Memory.setByte(addr + 3, 0xFF);
					addr += 4;
					scratchAddr += 4;
				}
			}
		}
		
		IDAT.length = length;
		IDAT.compress();
		
		return IDAT;
	}
	
	
	
	private static var crcTable:Array<UInt>;
	private static var crcTableComputed:Bool  = false;

	private static inline function writeChunk(png:ByteArray, type:UInt, data:ByteArray):Void {
		var c:UInt;
		if (!crcTableComputed) {
			crcTableComputed = true;
			crcTable = [];
			//for (var n:UInt = 0; n < 256; n++) {
			for (n in 0...256) {
				c = n;
				//for (var k:UInt = 0; k < 8; k++) {
				for (k in 0...8) {
					if (1 == c & 1) {
						c = 0xedb88320 ^ (c >>> 1);
					} else {
						c >>>= 1;
					}
				}
				crcTable[n] = c;
				
				//Lib.trace(c);
			}
		}
		var len:UInt = 0;
		if (data != null) {
			len = data.length;
		}
		png.writeUnsignedInt(len);
		var p:UInt = png.position;
		png.writeUnsignedInt(type);
		if ( data != null ) {
			png.writeBytes(data);
		}
		var e:UInt = png.position;
		png.position = p;
		c = 0xffffffff;
		
		// First four bytes are from type, rest are chunk data
		c = crcTable[(c ^ (type >>> 24)) & 0xff] ^ (c >>> 8);
		c = crcTable[(c ^ ((type >>> 16) & 0xFF)) & 0xff] ^ (c >>> 8);
		c = crcTable[(c ^ ((type >>> 8) & 0xFF)) & 0xff] ^ (c >>> 8);
		c = crcTable[(c ^ (type & 0xFF)) & 0xff] ^ (c >>> 8);
		
		//for (var i:Int = 0; i < (e-p); i++) {
		if (data != null) {
			data.length = Std.int(Math.max(len, ApplicationDomain.MIN_DOMAIN_MEMORY_LENGTH));
			Memory.select(data);
			
			data.position = 0;
			for (i in 0...len) {
				c = crcTable[(c ^ Memory.getByte(i)) & 0xff] ^ (c >>> 8);
			}
		}
		c ^= 0xffffffff;
		
		png.position = e;
		png.writeUnsignedInt(c);
	}
}
