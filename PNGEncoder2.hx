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
import flash.display.Sprite;
import flash.display.Stage;
import flash.errors.Error;
import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.ProgressEvent;
import flash.geom.Rectangle;
import flash.Lib;
import flash.Memory;
import flash.system.ApplicationDomain;
import flash.system.System;
import flash.utils.ByteArray;
import flash.utils.Endian;
import DeflateStream;


// TODO: Make sure all public methods are *not* inlined (they wouldn't be accessible from a swc in flash)

/**
 * Converts BitmapData objects into valid PNGs
 */
class PNGEncoder2 extends EventDispatcher
{
	private static inline var CRC_TABLE_END = 256 * 4;
	private static inline var DEFLATE_SCRATCH = CRC_TABLE_END;
	private static inline var CHUNK_START = DEFLATE_SCRATCH + DeflateStream.SCRATCH_MEMORY_SIZE;
	private static var data : ByteArray;
	private static var sprite : Sprite;		// Used to listen to ENTER_FRAME events
	private static var encoding = false;
	
	// FAST compression level is recommended (and default)
	public static var level : CompressionLevel;
	
	private var img : BitmapData;
	public var png : ByteArray;
	private var deflateStream : DeflateStream;
	private var currentY : Int;
	private var step : Int;
	
	/**
	 * Creates a PNG image from the specified BitmapData.
	 * Highly optimized for speed.
	 *
	 * @param image The BitmapData that will be converted into the PNG format.
	 * @return a ByteArray representing the PNG encoded image data.
	 * @playerversion Flash 10
	 */
	public static function encode(image : BitmapData) : ByteArray
	{
		return _encode(image);
	}
	
	
	/**
	 * Creates a PNG image from the specified BitmapData without blocking.
	 * Highly optimized for speed.
	 *
	 * @param image The BitmapData that will be converted into the PNG format.
	 * @return a PNGEncoder2 object that dispatches COMPLETE and PROGRESS events.
	 * @playerversion Flash 10
	 */
	public static function encodeAsync(image : BitmapData) : PNGEncoder2
	{
		return new PNGEncoder2(image);
	}
	
	
	private static inline function _encode(img : BitmapData) : ByteArray
	{
		// Save current domain memory and restore it after, to avoid
		// conflicts with other components using domain memory
		var oldFastMem = ApplicationDomain.currentDomain.domainMemory;
		
		var png = beginEncoding(img);
		
		// Initialize stream for IDAT chunks
		var deflateStream = DeflateStream.createEx(level, DEFLATE_SCRATCH, CHUNK_START, true);
		
		writeIDATChunk(img, 0, img.height, deflateStream, png);
		
		endEncoding(png);
		
		Memory.select(oldFastMem);
		return png;
	}
	
	private static inline function beginEncoding(img : BitmapData) : ByteArray
	{
		if (encoding) {
			throw new Error("Only one PNG can be encoded at once");
		}
		
		encoding = true;
		
		
		if (level == null) {
			level = FAST;
		}
		
		// Data will be select()ed for use with fast memory
		// The first 256 * 4 bytes are the CRC table
		// Inner chunk data is appended to the CRC table, starting at CHUNK_START
		
		initialize();		// Sets up data var & CRC table
		
		// Create output byte array
		var png:ByteArray = new ByteArray();
		
		writePNGSignature(png);
		
		writeIHDRChunk(img, png);
		
		return png;
	}
	
	
	private static inline function endEncoding(png : ByteArray)
	{
		writeIENDChunk(png);
		
		encoding = false;
		
		png.position = 0;
	}
	
	
	
	private function new(image : BitmapData)
	{
		super();
		
		_new(image);
	}
	
	private inline function _new(image : BitmapData)
	{
		var oldFastMem = ApplicationDomain.currentDomain.domainMemory;
		
		img = image;
		png = beginEncoding(img);
		currentY = 0;
		
		deflateStream = DeflateStream.createEx(level, DEFLATE_SCRATCH, CHUNK_START, true);
		
		if (img.width > 0) {
			// Determine proper step
			var startTime = Lib.getTimer();
			
			// Write first ~15K pixels to see how fast it is
			var height = Std.int(Math.min(15 * 1024 / img.width, img.height));
			writeIDATChunk(img, 0, height, deflateStream, png);
			
			var endTime = Lib.getTimer();
			var ms = endTime - startTime;
			
			// Use 90% of available milliseconds per frame
			var targetMs = Std.int(1 / new Stage().frameRate * 1000 * 0.9);
			
			// Ensure step is at least 8K pixels at once
			step = Math.ceil(Math.max(targetMs / ms * height, 8 * 1024 / img.width));
			
			currentY = height;
		}
		else {
			// Zero-width
			step = img.height;
		}
		
		sprite.addEventListener(Event.ENTER_FRAME, onEnterFrame);
		
		Memory.select(oldFastMem);
	}
	
	
	private function onEnterFrame(e : Event)
	{
		_onEnterFrame();
	}
	
	private inline function _onEnterFrame()
	{
		var oldFastMem = ApplicationDomain.currentDomain.domainMemory;
		Memory.select(data);
		
		var bytesPerPixel = img.transparent ? 4 : 3;
		var totalBytes = bytesPerPixel * img.width * img.height;
		
		if (currentY >= img.height) {
			// Finished encoding the entire image in the initial setup
			Memory.select(oldFastMem);
			dispatchEvent(new ProgressEvent(ProgressEvent.PROGRESS, false, false, totalBytes, totalBytes));
			
			Memory.select(data);
			finalize(oldFastMem);
		}
		else {
			var next = Std.int(Math.min(currentY + step, img.height));
			writeIDATChunk(img, currentY, next, deflateStream, png);
			currentY = next;
			
			var currentBytes = bytesPerPixel * img.width * currentY;
			
			Memory.select(oldFastMem);
			dispatchEvent(new ProgressEvent(ProgressEvent.PROGRESS, false, false, currentBytes, totalBytes));
			
			Memory.select(data);
			finalize(oldFastMem);
		}
		
		Memory.select(oldFastMem);
	}
	
	
	private inline function finalize(oldFastMem : ByteArray)
	{
		if (currentY >= img.height) {
			sprite.removeEventListener(Event.ENTER_FRAME, onEnterFrame);
			
			endEncoding(png);
			
			Memory.select(oldFastMem);
			dispatchEvent(new Event(Event.COMPLETE));
		}
	}
	
	

	private static inline function writePNGSignature(png : ByteArray)
	{
		png.writeUnsignedInt(0x89504e47);
		png.writeUnsignedInt(0x0D0A1A0A);
	}
	
	
	private static inline function writeIHDRChunk(img : BitmapData, png : ByteArray)
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
		
		writeChunk(png, 0x49484452, chunkLength);
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
	
	
	private static function writeIDATChunk(img : BitmapData, startY : Int, endY : Int, deflateStream: DeflateStream, png : ByteArray)
	{
		_writeIDATChunk(img, startY, endY, deflateStream, png);
	}
	
	private static inline function _writeIDATChunk(img : BitmapData, startY : Int, endY : Int, deflateStream: DeflateStream, png : ByteArray)
	{
		var width = img.width;
		var height = endY - startY;
		var region = new Rectangle(0, startY, width, height);
		
		var bytesPerPixel = img.transparent ? 4 : 3;
		
		// Length of IDAT data: 3 or 4 bytes per pixel + 1 byte per scanline
		var length : UInt = width * height * bytesPerPixel + height;
		
		// Size needed to store byte array of bitmap
		var scratchSize : UInt = width * height * 4;
		
		// Memory layout:
		// DEFLATE_SCRATCH: Deflate stream scratch memory
		// CHUNK_START: Deflated data (written last)
		// CHUNK_START + deflated data buffer: scratch (raw image bytes)
		// CHUNK_START + deflated data buffer + scratchSize: Uncompressed PNG-format image data
		
		data.length = Std.int(Math.max(CHUNK_START + deflateStream.maxOutputBufferSize(length) + scratchSize + length, ApplicationDomain.MIN_DOMAIN_MEMORY_LENGTH));
		Memory.select(data);
		
		var scratchAddr : Int = CHUNK_START + deflateStream.maxOutputBufferSize(length);
		var addrStart : Int = scratchAddr + scratchSize;
		
		var addr = addrStart;
		var end8 = (width & 0xFFFFFFF4) - 8;		// Floor to nearest 8, then subtract 8
		var j;
		
		//var startTime = Lib.getTimer();
		
		var imgBytes = img.getPixels(region);
		imgBytes.position = 0;
		memcpy(imgBytes, scratchAddr);
		
		//var endTime = Lib.getTimer();
		//trace("Blitting pixel data into fast mem took " + (endTime - startTime) + "ms");
		
		//startTime = Lib.getTimer();
		if (img.transparent) {
			for (i in 0 ... height) {
				Memory.setByte(addr, 1);		// Sub filter
				addr += 1;
				
				if (width > 0) {
					// Do first pixel (4 bytes) manually (sub formula is different)
					Memory.setI32(addr, Memory.getI32(scratchAddr) >>> 8);
					Memory.setByte(addr + 3, Memory.getByte(scratchAddr + 0));
					addr += 4;
					scratchAddr += 4;
				
					// Copy line, moving alpha byte to end, and applying filter
					j = 1;
					while (j < end8) {
						Memory.setByte(addr + 0, Memory.getByte(scratchAddr + 1) - Memory.getByte(scratchAddr - 3));
						Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - Memory.getByte(scratchAddr - 2));
						Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - Memory.getByte(scratchAddr - 1));
						Memory.setByte(addr + 3, Memory.getByte(scratchAddr + 0) - Memory.getByte(scratchAddr - 4));
						
						Memory.setByte(addr + 4, Memory.getByte(scratchAddr + 5) - Memory.getByte(scratchAddr + 1));
						Memory.setByte(addr + 5, Memory.getByte(scratchAddr + 6) - Memory.getByte(scratchAddr + 2));
						Memory.setByte(addr + 6, Memory.getByte(scratchAddr + 7) - Memory.getByte(scratchAddr + 3));
						Memory.setByte(addr + 7, Memory.getByte(scratchAddr + 4) - Memory.getByte(scratchAddr + 0));
						
						Memory.setByte(addr +  8, Memory.getByte(scratchAddr +  9) - Memory.getByte(scratchAddr + 5));
						Memory.setByte(addr +  9, Memory.getByte(scratchAddr + 10) - Memory.getByte(scratchAddr + 6));
						Memory.setByte(addr + 10, Memory.getByte(scratchAddr + 11) - Memory.getByte(scratchAddr + 7));
						Memory.setByte(addr + 11, Memory.getByte(scratchAddr +  8) - Memory.getByte(scratchAddr + 4));
						
						Memory.setByte(addr + 12, Memory.getByte(scratchAddr + 13) - Memory.getByte(scratchAddr +  9));
						Memory.setByte(addr + 13, Memory.getByte(scratchAddr + 14) - Memory.getByte(scratchAddr + 10));
						Memory.setByte(addr + 14, Memory.getByte(scratchAddr + 15) - Memory.getByte(scratchAddr + 11));
						Memory.setByte(addr + 15, Memory.getByte(scratchAddr + 12) - Memory.getByte(scratchAddr +  8));
						
						Memory.setByte(addr + 16, Memory.getByte(scratchAddr + 17) - Memory.getByte(scratchAddr + 13));
						Memory.setByte(addr + 17, Memory.getByte(scratchAddr + 18) - Memory.getByte(scratchAddr + 14));
						Memory.setByte(addr + 18, Memory.getByte(scratchAddr + 19) - Memory.getByte(scratchAddr + 15));
						Memory.setByte(addr + 19, Memory.getByte(scratchAddr + 16) - Memory.getByte(scratchAddr + 12));
						
						Memory.setByte(addr + 20, Memory.getByte(scratchAddr + 21) - Memory.getByte(scratchAddr + 17));
						Memory.setByte(addr + 21, Memory.getByte(scratchAddr + 22) - Memory.getByte(scratchAddr + 18));
						Memory.setByte(addr + 22, Memory.getByte(scratchAddr + 23) - Memory.getByte(scratchAddr + 19));
						Memory.setByte(addr + 23, Memory.getByte(scratchAddr + 20) - Memory.getByte(scratchAddr + 16));
						
						Memory.setByte(addr + 24, Memory.getByte(scratchAddr + 25) - Memory.getByte(scratchAddr + 21));
						Memory.setByte(addr + 25, Memory.getByte(scratchAddr + 26) - Memory.getByte(scratchAddr + 22));
						Memory.setByte(addr + 26, Memory.getByte(scratchAddr + 27) - Memory.getByte(scratchAddr + 23));
						Memory.setByte(addr + 27, Memory.getByte(scratchAddr + 24) - Memory.getByte(scratchAddr + 20));
						
						Memory.setByte(addr + 28, Memory.getByte(scratchAddr + 29) - Memory.getByte(scratchAddr + 25));
						Memory.setByte(addr + 29, Memory.getByte(scratchAddr + 30) - Memory.getByte(scratchAddr + 26));
						Memory.setByte(addr + 30, Memory.getByte(scratchAddr + 31) - Memory.getByte(scratchAddr + 27));
						Memory.setByte(addr + 31, Memory.getByte(scratchAddr + 28) - Memory.getByte(scratchAddr + 24));
						
						
						addr += 32;
						scratchAddr += 32;
						j += 8;
					}
					while (j < width) {
						Memory.setByte(addr + 0, Memory.getByte(scratchAddr + 1) - Memory.getByte(scratchAddr - 3));
						Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - Memory.getByte(scratchAddr - 2));
						Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - Memory.getByte(scratchAddr - 1));
						Memory.setByte(addr + 3, Memory.getByte(scratchAddr + 0) - Memory.getByte(scratchAddr - 4));
						addr += 4;
						scratchAddr += 4;
						++j;
					}
				}
			}
		}
		else {
			for (i in 0 ... height) {
				Memory.setByte(addr, 1);		// Sub filter
				addr += 1;
				
				if (width > 0) {
					// Do first pixel (3 bytes) manually (sub formula is different)
					Memory.setByte(addr + 0, Memory.getByte(scratchAddr + 1));
					Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2));
					Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3));
					addr += 3;
					scratchAddr += 4;
					
					// Copy line
					j = 1;
					while (j < end8) {
						Memory.setByte(addr + 0, Memory.getByte(scratchAddr + 1) - Memory.getByte(scratchAddr - 3));
						Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - Memory.getByte(scratchAddr - 2));
						Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - Memory.getByte(scratchAddr - 1));
						
						Memory.setByte(addr + 3, Memory.getByte(scratchAddr + 5) - Memory.getByte(scratchAddr + 1));
						Memory.setByte(addr + 4, Memory.getByte(scratchAddr + 6) - Memory.getByte(scratchAddr + 2));
						Memory.setByte(addr + 5, Memory.getByte(scratchAddr + 7) - Memory.getByte(scratchAddr + 3));
						
						Memory.setByte(addr + 6, Memory.getByte(scratchAddr +  9) - Memory.getByte(scratchAddr + 5));
						Memory.setByte(addr + 7, Memory.getByte(scratchAddr + 10) - Memory.getByte(scratchAddr + 6));
						Memory.setByte(addr + 8, Memory.getByte(scratchAddr + 11) - Memory.getByte(scratchAddr + 7));
						
						Memory.setByte(addr +  9, Memory.getByte(scratchAddr + 13) - Memory.getByte(scratchAddr +  9));
						Memory.setByte(addr + 10, Memory.getByte(scratchAddr + 14) - Memory.getByte(scratchAddr + 10));
						Memory.setByte(addr + 11, Memory.getByte(scratchAddr + 15) - Memory.getByte(scratchAddr + 11));
						
						Memory.setByte(addr + 12, Memory.getByte(scratchAddr + 17) - Memory.getByte(scratchAddr + 13));
						Memory.setByte(addr + 13, Memory.getByte(scratchAddr + 18) - Memory.getByte(scratchAddr + 14));
						Memory.setByte(addr + 14, Memory.getByte(scratchAddr + 19) - Memory.getByte(scratchAddr + 15));
						
						Memory.setByte(addr + 15, Memory.getByte(scratchAddr + 21) - Memory.getByte(scratchAddr + 17));
						Memory.setByte(addr + 16, Memory.getByte(scratchAddr + 22) - Memory.getByte(scratchAddr + 18));
						Memory.setByte(addr + 17, Memory.getByte(scratchAddr + 23) - Memory.getByte(scratchAddr + 19));
						
						Memory.setByte(addr + 18, Memory.getByte(scratchAddr + 25) - Memory.getByte(scratchAddr + 21));
						Memory.setByte(addr + 19, Memory.getByte(scratchAddr + 26) - Memory.getByte(scratchAddr + 22));
						Memory.setByte(addr + 20, Memory.getByte(scratchAddr + 27) - Memory.getByte(scratchAddr + 23));
						
						Memory.setByte(addr + 21, Memory.getByte(scratchAddr + 29) - Memory.getByte(scratchAddr + 25));
						Memory.setByte(addr + 22, Memory.getByte(scratchAddr + 30) - Memory.getByte(scratchAddr + 26));
						Memory.setByte(addr + 33, Memory.getByte(scratchAddr + 31) - Memory.getByte(scratchAddr + 27));
						
						addr += 24;
						scratchAddr += 32;
						j += 8;
					}
					while (j < width) {
						Memory.setByte(addr + 0, Memory.getByte(scratchAddr + 1) - Memory.getByte(scratchAddr - 3));
						Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - Memory.getByte(scratchAddr - 2));
						Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - Memory.getByte(scratchAddr - 1));
						addr += 3;
						scratchAddr += 4;
						++j;
					}
				}
			}
		}
		
		//endTime = Lib.getTimer();
		//trace("Copying pixel data into RGBA format with filter took " + (endTime - startTime) + "ms");
		
		//var startTime = Lib.getTimer();
		
		deflateStream.fastWrite(addrStart, addrStart + length);
		
		var lastChunk = endY == img.height;
		var range = lastChunk ? deflateStream.fastFinalize() : deflateStream.peek();
		writeChunk(png, 0x49444154, range.len());
		
		if (!lastChunk) {
			deflateStream.release();
		}
		
		//var endTime = Lib.getTimer();
		//trace("Compression took " + (endTime - startTime) + "ms");
	}
	
	
	private static inline function writeIENDChunk(png : ByteArray)
	{
		writeChunk(png, 0x49454E44, 0);
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
			var i = CHUNK_START;
			var end = CHUNK_START + len;
			var end16 = CHUNK_START + (len & 0xFFFFFFF0);	// Floor to nearest 16
			while (i < end16) {
				c = crcTable(c ^ Memory.getByte(i)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 1)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 2)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 3)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 4)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 5)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 6)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 7)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 8)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 9)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 10)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 11)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 12)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 13)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 14)) ^ (c >>> 8);
				c = crcTable(c ^ Memory.getByte(i + 15)) ^ (c >>> 8);
				i += 16;
			}
			while (i < end) {
				c = crcTable(c ^ Memory.getByte(i)) ^ (c >>> 8);
				++i;
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
				
				// 8 iterations
				if (c & 1 == 1) c = 0xedb88320 ^ (c >>> 1);
				else c >>>= 1;
				if (c & 1 == 1) c = 0xedb88320 ^ (c >>> 1);
				else c >>>= 1;
				if (c & 1 == 1) c = 0xedb88320 ^ (c >>> 1);
				else c >>>= 1;
				if (c & 1 == 1) c = 0xedb88320 ^ (c >>> 1);
				else c >>>= 1;
				if (c & 1 == 1) c = 0xedb88320 ^ (c >>> 1);
				else c >>>= 1;
				if (c & 1 == 1) c = 0xedb88320 ^ (c >>> 1);
				else c >>>= 1;
				if (c & 1 == 1) c = 0xedb88320 ^ (c >>> 1);
				else c >>>= 1;
				if (c & 1 == 1) c = 0xedb88320 ^ (c >>> 1);
				else c >>>= 1;
				
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
