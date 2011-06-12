/*
	Copyright (c) 2008, Adobe Systems Incorporated
	Copyright (c) 2011, Pimm Hogeling and Edo Rivai
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
import flash.Memory;
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
	
	
	private static inline function buildIHDRChunk(img : BitmapData)
	{
		var IHDR:ByteArray = new ByteArray();
		IHDR.length = 13;
		
		IHDR.writeInt(img.width);
		IHDR.writeInt(img.height);
		IHDR.writeUnsignedInt(0x08060000); // 32bit RGBA
		IHDR.writeByte(0);
		
		return IHDR;
	}
	
	
	private static inline function buildIDATChunk(img : BitmapData)
	{
		// Length of IDAT: 4 bytes per pixel + 1 byte per scanline
		var length = img.width * img.height * 4 + img.height;
		
		// Size needed to store byte array of bitmap
		var scratchSize = img.width * img.height * 4;
		
		var IDAT:ByteArray = new ByteArray();		// IDAT + scratch at end
		IDAT.length = Std.int(Math.max(length + scratchSize, 0x400));	// Minimum size for Memory.select
		
		Memory.select(IDAT);
		var addr = 0;
		
		var scratchAddr = length;
		
		var imgBytes = img.getPixels(new Rectangle(0, 0, img.width, img.height));
		imgBytes.position = 0;
		imgBytes.readBytes(IDAT, scratchAddr);
		
		if ( img.transparent ) {
			for (i in 0...img.height) {
				Memory.setByte(addr, 0);		// No filter
				addr += 1;
				
				// Copy line, moving alpha byte to end
				for (j in 0...img.width) {
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
			for(i in 0...img.height) {
				Memory.setByte(addr, 0);		// No filter
				addr += 1;
				
				// Copy line, moving alpha byte to end
				for (j in 0...img.width) {
					Memory.setByte(addr + 0, Memory.getByte(scratchAddr + 1));
					Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2));
					Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3));
					Memory.setByte(addr + 3, 0xFF);
					addr += 4;
					scratchAddr += 4;
				}
			}
		}
		
		Memory.setByte(0, 0);
		
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
						c = cast(cast(0xedb88320, UInt) ^ 
							cast(c >>> 1, UInt), UInt);
					} else {
						c = cast(c >>> 1, UInt);
					}
				}
				crcTable[n] = c;
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
		//for (var i:Int = 0; i < (e-p); i++) {
		for (i in 0...(e - p)) {
			c = cast(crcTable[
				(c ^ png.readUnsignedByte()) & 
				cast(0xff, UInt)] ^ cast(c >>> 8, UInt), UInt);
		}
		c = cast(c^cast(0xffffffff, UInt), UInt);
		png.position = e;
		png.writeUnsignedInt(c);
	}
}
