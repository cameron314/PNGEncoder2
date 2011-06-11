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
import flash.utils.ByteArray;


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
		IHDR.writeInt(img.width);
		IHDR.writeInt(img.height);
		IHDR.writeUnsignedInt(0x08060000); // 32bit RGBA
		IHDR.writeByte(0);
		
		return IHDR;
	}
	
	
	private static inline function buildIDATChunk(img : BitmapData)
	{
		// Build IDAT chunk
		var IDAT:ByteArray = new ByteArray();
		
		for(i in 0...img.height) {
			IDAT.writeByte(0);		// No filter
			
			var p:UInt;			// Current pixel value in ARGB format
			
			if ( !img.transparent ) {
				for(j in 0...img.width) {
					p = img.getPixel(j,i);
					IDAT.writeUnsignedInt(
						cast(((p&0xFFFFFF) << 8)|0xFF, UInt));
				}
			} else {
				for(j in 0...img.width) {
					p = img.getPixel32(j,i);
					IDAT.writeUnsignedInt(
						cast(((p&0xFFFFFF) << 8)|
						(p>>>24), UInt));
				}
			}
		}
		
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
