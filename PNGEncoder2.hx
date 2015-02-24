/*
	Copyright (c) 2008, Adobe Systems Incorporated
	Copyright (c) 2011, Pimm Hogeling and Edo Rivai
	Copyright (c) 2011-2015, Cameron Desrochers
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
import flash.events.IEventDispatcher;
import flash.events.ProgressEvent;
import flash.events.TimerEvent;
import flash.geom.Rectangle;
import flash.Lib;
import flash.Memory;
import flash.system.ApplicationDomain;
import flash.system.System;
import flash.utils.ByteArray;
import flash.utils.Endian;
import flash.utils.IDataInput;
import flash.utils.IDataOutput;
import flash.utils.Timer;
import flash.Vector;
import DeflateStream;


class PNGKeywords
{
	public static var TITLE = "Title";		// Short (one line) title or caption for image
	public static var AUTHOR = "Author";	// Name of image's creator
	public static var DESCRIPTION = "Description";	// Description of image (possibly long)
	public static var COPYRIGHT = "Copyright";		// Copyright notice
	public static var CREATION_TIME = "Creation Time";	// Time of original image creation
	public static var SOFTWARE = "Software";		// Software used to create the image
	public static var DISCLAIMER = "Disclaimer";	// Legal disclaimer
	public static var WARNING = "Warning";	//	Warning of nature of content
	public static var SOURCE = "Source";	// Device used to create the image
	public static var COMMENT = "Comment";	// Miscellaneous comment
}

// Separate public interface from private implementation because all
// members appear as public in SWC
class PNGEncoder2 extends EventDispatcher
{
	// For internal use only. Do not access.
	private var __impl : PNGEncoder2Impl;
	
	public static var level : CompressionLevel;
	
	// Provide both HaXe and AS3 properties to access the PNG result for reading
	@:protected private inline function get_png() { return __impl.png; }
	@:protected public var png(get_png, null) : IDataOutput;
	@:getter(png) private function flGetPng() { return get_png(); }
	
	// Provide both HaXe and AS3 properties to access the target FPS (read/write)
	@:protected private inline function get_targetFPS() { return __impl.targetFPS; }
	@:protected private inline function set_targetFPS(fps : Int) { return __impl.targetFPS = fps; }
	@:protected public var targetFPS(get_targetFPS, set_targetFPS) : Int;
	@:getter(targetFPS) private function flGetTargetFPS() { return get_targetFPS(); }
	@:setter(targetFPS) private function flSetTargetFPS(fps : Int) { set_targetFPS(fps); }
	
	
	/**
	 * Creates a PNG image from the specified BitmapData.
	 * If the BitmapData's transparent property is true, then a 32-bit
	 * PNG (i.e. with alpha) is generated, otherwise a (generally samller)
	 * 24-bit PNG is generated.
	 * Highly optimized for speed.
	 *
	 * @param image The BitmapData that will be converted into the PNG format.
	 * @param outPng An IDataOutput (e.g. ByteArray) object that the PNG encoded image data will be written to.
	 * @playerversion Flash 10
	 */
	public static function encode(image : BitmapData, outPng : IDataOutput) : Void
	{
		PNGEncoder2Impl.level = level;
		PNGEncoder2Impl.encode(image, outPng, null);
	}
	
	/**
	 * Creates a PNG image from the specified BitmapData and metadata.
	 * If the BitmapData's transparent property is true, then a 32-bit
	 * PNG (i.e. with alpha) is generated, otherwise a (generally samller)
	 * 24-bit PNG is generated.
	 * Highly optimized for speed.
	 *
	 * @param image The BitmapData that will be converted into the PNG format.
	 * @param metadata An object that will be treated as key-value pairs of keyword-value metadata.
	 * @param outPng An IDataOutput (e.g. ByteArray) object that the PNG encoded image data will be written to.
	 * @playerversion Flash 10
	 */
	public static function encodeWithMetadata(image : BitmapData, metadata : Dynamic, outPng : IDataOutput) : Void
	{
		PNGEncoder2Impl.level = level;
		PNGEncoder2Impl.encode(image, outPng, metadata);
	}
	
	
	/**
	 * Creates a PNG image from the specified BitmapData without blocking.
	 * If the BitmapData's transparent property is true, then a 32-bit
	 * PNG (i.e. with alpha) is generated, otherwise a (generally samller)
	 * 24-bit PNG is generated.
	 * Highly optimized for speed.
	 *
	 * @param image The BitmapData that will be converted into the PNG format.
	 * @param outPng An IDataOutput (e.g. ByteArray) object that the PNG encoded image data will be written to.
	 * @return a PNGEncoder2 object that dispatches COMPLETE and PROGRESS events.
	 * The encoder object allows the targetFPS to be set, and has a 'png'
	 * property to access the encoded data once the COMPLETE event has fired.
	 * @playerversion Flash 10
	 */
	public static function encodeAsync(image : BitmapData, outPng : IDataOutput) : PNGEncoder2
	{
		return new PNGEncoder2(image, outPng, null);
	}
	
	
	/**
	 * Creates a PNG image from the specified BitmapData and metadata,
	 * without blocking.
	 * If the BitmapData's transparent property is true, then a 32-bit
	 * PNG (i.e. with alpha) is generated, otherwise a (generally samller)
	 * 24-bit PNG is generated.
	 * Highly optimized for speed.
	 *
	 * @param image The BitmapData that will be converted into the PNG format.
	 * @param metadata An object that will be treated as key-value pairs of keyword-value metadata.
	 * @param outPng An IDataOutput (e.g. ByteArray) object that the PNG encoded image data will be written to.
	 * @return a PNGEncoder2 object that dispatches COMPLETE and PROGRESS events.
	 * The encoder object allows the targetFPS to be set, and has a 'png'
	 * property to access the encoded data once the COMPLETE event has fired.
	 * @playerversion Flash 10
	 */
	public static function encodeAsyncWithMetadata(image : BitmapData, metadata : Dynamic, outPng : IDataOutput) : PNGEncoder2
	{
		return new PNGEncoder2(image, outPng, metadata);
	}
	
	
#if DECODER
	/**
	 * Provides a simple, synchronous (but fast) decoder for image files
	 * created using PNGEncoder2 (though not, alas, arbitrary PNGs).
	 *
	 * @param pngBytes The raw bytes of the PNG encoded image data.
	 * @return A BitmapData representing the decoded image.
	 * @playerversion Flash 10
	 */
	public static inline function decode(pngBytes : IDataInput) : BitmapData
	{
		return PNGEncoder2Impl.decode(pngBytes);
	}
#end

	/**
	 * Clears any long-term cached memory (e.g. CRC tables) in order
	 * to reduce memory usage. This is only needed in resource-constrained
	 * environments; it's faster to leave the cache intact between
	 * encodings.
	*/
	public static function freeCachedMemory()
	{
		PNGEncoder2Impl.freeCachedMemory();
	}
	
	
	private inline function new(image : BitmapData, outPng : IDataOutput, metadata : Dynamic)
	{
		super();
		
		PNGEncoder2Impl.level = level;
		__impl = new PNGEncoder2Impl(image, outPng, this, metadata);
	}
}


// The actual implementation of all PNG functionality (completely refactored
// and improved (by Cameron) from the HaXe port of the original AS3 version --
// new features include fast performance, 24- and 32-bit PNG support, the
// Paeth filter, and adaptive asynchronous encoding)
@:protected private class PNGEncoder2Impl
{
	// Domain memory (aka "fast memory") layout:
	// 0:           CRC table (256 4-byte entries)
	// 1024:        Deflate stream (fixed) scratch memory
	// CHUNK_START: Compressed image data to be written to the next chunk in the output
	// After chunk: Additional scratch memory used to construct the chunk
	
	private static inline var CRC_TABLE_END = 256 * 4;
	private static inline var DEFLATE_SCRATCH = CRC_TABLE_END;
	private static inline var CHUNK_START = DEFLATE_SCRATCH + DeflateStream.SCRATCH_MEMORY_SIZE;
	
	private static inline var FRAME_AVG_SMOOTH_COUNT = 4;	// Number of frames to calculate averages from. Must be power of 2
	private static inline var FIRST_UPDATE_PIXELS = 20 * 1024;			// Encode this many pixels right away on the first frame
	private static inline var MIN_PIXELS_PER_UPDATE = 20 * 1024;		// Always compress at least this many pixels per chunk
	private static var data : ByteArray;	// The select()ed working memory
	private static var sprite : Sprite;		// Used purely to listen to ENTER_FRAME events
	private static var encoding = false;	// Keeps track of global state, to ensure only one PNG can be encoded at once
	private static var region : Rectangle;	// Re-used rectangle for async update function (avoids per-frame allocation)
	
	private static var pendingAsyncEncodings : Vector<PNGEncoder2Impl> = new Vector<PNGEncoder2Impl>();
	
	// FAST compression level is default
	public static var level : CompressionLevel;
	
	//
	// Asynchronous encoder member variables:
	//
	public var png : IDataOutput;	// Where the resulting PNG data is written for the asynchronous encoder
	public var targetFPS : Int;		// The desired number of frames-per-second during asynchronous encoding (will attempt to achieve this)
	
	private var img : BitmapData;	// The input image
	private var dispatcher : IEventDispatcher;		// The dispatcher to use for dispatching PROGRESS and COMPLETE events
	private var deflateStream : DeflateStream;		// To compress each chunk's data
	private var currentY : Int;						// The current scanline in the input image (tracks progress)
	private var msPerFrame : Vector<Int>;			// Keeps track of last FRAME_AVG_SMOOTH_COUNT measures of milliseconds between sequential frame
	private var msPerFrameIndex : Int;				// Index into msPerFrame at which to store next measure
	private var msPerLine : Vector<Float>;			// Keeps track of last FRAME_AVG_SMOOTH_COUNT measures (one per chunk) of average milliseconds to process a single line
	private var msPerLineIndex : Int;				// Index into msPerLine at which to store next measure
	private var updatesPerFrame : Vector<Int>;		// Keeps track of last FRAME_AVG_SMOOTH_COUNT measures of the number of updates (i.e. chunks) that can be processed per frame (generally just one except at very low frame rates)
	private var updatesPerFrameIndex : Int;			// Index into updatesPerFrame at which to store next measure
	private var updates : Int;						// Total number of updates (i.e. chunks) done *since the last frame was entered*
	private var lastFrameStart : Int;				// Lib.getTimer() value. Used to calculate millisecond delta between two frames
	private var step : Int;							// Number of scanlines to process during the next update (in order to approximate targetFPS, but without wasting cycles)
	private var done : Bool;						// Whether there's any more scanlines to process or not
	private var metadata : Dynamic;					// Treated as key-value pairs of tEXt/iTXt metadata
	
	private var frameCount : Int;					// Total number of frames that have elapsed so far during the encoding
	
	
	public static inline function encode(img : BitmapData, png : IDataOutput, metadata : Dynamic) : Void
	{
		// Save current domain memory and restore it after, to avoid
		// conflicts with other components using domain memory
		var oldFastMem = ApplicationDomain.currentDomain.domainMemory;
		
		beginEncoding(img, png, metadata);
		
		// Initialize stream for IDAT chunks
		var deflateStream = DeflateStream.createEx(level, DEFLATE_SCRATCH, CHUNK_START, true);
		
		writeIDATChunk(img, 0, img.height, deflateStream, png);
		
		endEncoding(png);
		
		deflateStream = null;		// Just in case this helps the garbage collector...
		
		Memory.select(oldFastMem);
	}
	
#if DECODER
	public static inline function decode(pngBytes : IDataInput) : BitmapData
	{
		//var start = Lib.getTimer();
		
		// Assumes valid PNG created by PNGEncoder2 -- only the most basic error checking is done!
		var failed = false;
		if (pngBytes.bytesAvailable < 16) {
			failed = true;
		}
		
		if (!failed && (pngBytes.readInt() != 0x89504e47 || pngBytes.readInt() != 0x0D0A1A0A)) {
			failed = true;
		}
		//trace("Basic prelude: " + (Lib.getTimer() - start) + "ms");
		//start = Lib.getTimer();
		
		var bmp : BitmapData = null;
		if (!failed) {
			// Extract the dimensions, bit depth, and all the IDAT chunks
			var width = -1;
			var height = -1;
			var transparent = false;
			var idatData = new ByteArray();
			var chunkLength = pngBytes.readUnsignedInt();
			var chunkType = pngBytes.readUnsignedInt();
			if (chunkType != 0x49484452 /* IHDR */) {
				failed = true;
			}
			while (chunkType != 0x49454E44 /* IEND */) {
				if (chunkType == 0x49484452 /* IHDR */) {
					if (chunkLength != 13) {
						failed = true;
					}
					
					width = pngBytes.readInt();
					height = pngBytes.readInt();
					pngBytes.readByte();		// Ignore bit depth (constant)
					transparent = pngBytes.readUnsignedByte() == 6;
					pngBytes.readByte(); pngBytes.readByte(); pngBytes.readByte();		// Ignore other options (constant)
					// Resize to maximum length to avoid too many resizes when there's lots of chunks
					idatData.length = transparent ? (height * width * 4 + height) : (height * width * 3 + height);
				}
				else if (chunkType == 0x49444154 /* IDAT */) {
					pngBytes.readBytes(idatData, idatData.position, chunkLength);
					idatData.position += chunkLength;
				}
				else {
					for (i in 0 ... chunkLength) {
						pngBytes.readByte();
					}
				}
				pngBytes.readUnsignedInt();		// Ignore CRC-32 of chunk
				chunkLength = pngBytes.readUnsignedInt();
				chunkType = pngBytes.readUnsignedInt();
			}
			
			//trace("Chunk parsing & copying: " + (Lib.getTimer() - start) + "ms");
			
			if (width == 0 || height == 0) {
				bmp = new BitmapData(width, height, transparent, 0x00FFFFFF);
			}
			else if (!failed) {
				// Decompress the data (is this as fast as it could be? Seems a tad
				// slower than I was expecting... but still relatively quick, I suppose)
				//start = Lib.getTimer();
				idatData.uncompress();
				//trace("uncompress(): " + (Lib.getTimer() - start) + "ms");
				//start = Lib.getTimer();
				
				// Reverse the Paeth filter (and add in alpha values if the PNG was non-transparent,
				// otherwise move the alpha byte from the end to the beginning).
				// Note: PNG is RGB(A), Flash wants ARGB, and get/setI32 are little-endian.
				var oldFastMem = ApplicationDomain.currentDomain.domainMemory;
				var addr = 0;
				
				if (transparent) {
					// Since the uncompressed data is > than the final bitmap size,
					// we can undo the filter in-place
					var destAddr : Int = 0;
					Memory.select(idatData);
					
					// First line
					++addr;		// Skip filter byte (it's always sub for the first line)
					Memory.setI32(destAddr, rotl8(Memory.getI32(addr)));	// first pixel
					var widthBy4 = width * 4;
					var endAddr = destAddr + widthBy4;
					addr += 4;
					destAddr += 4;
					var endAddr64 = destAddr + ((widthBy4 - 1) & 0xFFFFFFC0);
					while (destAddr != endAddr64) {
						Memory.setI32(destAddr, byteAdd4(rotl8(Memory.getI32(addr)), Memory.getI32(destAddr - 4)));
						Memory.setI32(destAddr + 4, byteAdd4(rotl8(Memory.getI32(addr + 4)), Memory.getI32(destAddr)));
						Memory.setI32(destAddr + 8, byteAdd4(rotl8(Memory.getI32(addr + 8)), Memory.getI32(destAddr + 4)));
						Memory.setI32(destAddr + 12, byteAdd4(rotl8(Memory.getI32(addr + 12)), Memory.getI32(destAddr + 8)));
						Memory.setI32(destAddr + 16, byteAdd4(rotl8(Memory.getI32(addr + 16)), Memory.getI32(destAddr + 12)));
						Memory.setI32(destAddr + 20, byteAdd4(rotl8(Memory.getI32(addr + 20)), Memory.getI32(destAddr + 16)));
						Memory.setI32(destAddr + 24, byteAdd4(rotl8(Memory.getI32(addr + 24)), Memory.getI32(destAddr + 20)));
						Memory.setI32(destAddr + 28, byteAdd4(rotl8(Memory.getI32(addr + 28)), Memory.getI32(destAddr + 24)));
						Memory.setI32(destAddr + 32, byteAdd4(rotl8(Memory.getI32(addr + 32)), Memory.getI32(destAddr + 28)));
						Memory.setI32(destAddr + 36, byteAdd4(rotl8(Memory.getI32(addr + 36)), Memory.getI32(destAddr + 32)));
						Memory.setI32(destAddr + 40, byteAdd4(rotl8(Memory.getI32(addr + 40)), Memory.getI32(destAddr + 36)));
						Memory.setI32(destAddr + 44, byteAdd4(rotl8(Memory.getI32(addr + 44)), Memory.getI32(destAddr + 40)));
						Memory.setI32(destAddr + 48, byteAdd4(rotl8(Memory.getI32(addr + 48)), Memory.getI32(destAddr + 44)));
						Memory.setI32(destAddr + 52, byteAdd4(rotl8(Memory.getI32(addr + 52)), Memory.getI32(destAddr + 48)));
						Memory.setI32(destAddr + 56, byteAdd4(rotl8(Memory.getI32(addr + 56)), Memory.getI32(destAddr + 52)));
						Memory.setI32(destAddr + 60, byteAdd4(rotl8(Memory.getI32(addr + 60)), Memory.getI32(destAddr + 56)));
						addr += 64;
						destAddr += 64;
					}
					while (destAddr != endAddr) {
						Memory.setI32(destAddr, byteAdd4(rotl8(Memory.getI32(addr)), Memory.getI32(destAddr - 4)));
						addr += 4;
						destAddr += 4;
					}
					
					// Other lines:
					for (i in 1 ... height) {
						++addr;	// Skip filter byte (always Paeth here)
						// Do first pixel (4 bytes) manually (formula is different)
						Memory.setI32(destAddr, byteAdd4(rotl8(Memory.getI32(addr)), Memory.getI32(destAddr - widthBy4)));
						endAddr = destAddr + widthBy4;
						addr += 4;
						destAddr += 4;
						endAddr64 = destAddr + ((widthBy4 - 1) & 0xFFFFFFC0);
						while (destAddr != endAddr64) {
							Memory.setI32(destAddr, byteAdd4(rotl8(Memory.getI32(addr)), paethPredictor4(Memory.getI32(destAddr - 4), Memory.getI32(destAddr - widthBy4), Memory.getI32(destAddr - 4 - widthBy4))));
							Memory.setI32(destAddr + 4, byteAdd4(rotl8(Memory.getI32(addr + 4)), paethPredictor4(Memory.getI32(destAddr    ), Memory.getI32(destAddr + 4 - widthBy4), Memory.getI32(destAddr     - widthBy4))));
							Memory.setI32(destAddr + 8, byteAdd4(rotl8(Memory.getI32(addr + 8)), paethPredictor4(Memory.getI32(destAddr + 4), Memory.getI32(destAddr + 8 - widthBy4), Memory.getI32(destAddr + 4 - widthBy4))));
							Memory.setI32(destAddr + 12, byteAdd4(rotl8(Memory.getI32(addr + 12)), paethPredictor4(Memory.getI32(destAddr + 8), Memory.getI32(destAddr + 12 - widthBy4), Memory.getI32(destAddr + 8 - widthBy4))));
							Memory.setI32(destAddr + 16, byteAdd4(rotl8(Memory.getI32(addr + 16)), paethPredictor4(Memory.getI32(destAddr + 12), Memory.getI32(destAddr + 16 - widthBy4), Memory.getI32(destAddr + 12 - widthBy4))));
							Memory.setI32(destAddr + 20, byteAdd4(rotl8(Memory.getI32(addr + 20)), paethPredictor4(Memory.getI32(destAddr + 16), Memory.getI32(destAddr + 20 - widthBy4), Memory.getI32(destAddr + 16 - widthBy4))));
							Memory.setI32(destAddr + 24, byteAdd4(rotl8(Memory.getI32(addr + 24)), paethPredictor4(Memory.getI32(destAddr + 20), Memory.getI32(destAddr + 24 - widthBy4), Memory.getI32(destAddr + 20 - widthBy4))));
							Memory.setI32(destAddr + 28, byteAdd4(rotl8(Memory.getI32(addr + 28)), paethPredictor4(Memory.getI32(destAddr + 24), Memory.getI32(destAddr + 28 - widthBy4), Memory.getI32(destAddr + 24 - widthBy4))));
							Memory.setI32(destAddr + 32, byteAdd4(rotl8(Memory.getI32(addr + 32)), paethPredictor4(Memory.getI32(destAddr + 28), Memory.getI32(destAddr + 32 - widthBy4), Memory.getI32(destAddr + 28 - widthBy4))));
							Memory.setI32(destAddr + 36, byteAdd4(rotl8(Memory.getI32(addr + 36)), paethPredictor4(Memory.getI32(destAddr + 32), Memory.getI32(destAddr + 36 - widthBy4), Memory.getI32(destAddr + 32 - widthBy4))));
							Memory.setI32(destAddr + 40, byteAdd4(rotl8(Memory.getI32(addr + 40)), paethPredictor4(Memory.getI32(destAddr + 36), Memory.getI32(destAddr + 40 - widthBy4), Memory.getI32(destAddr + 36 - widthBy4))));
							Memory.setI32(destAddr + 44, byteAdd4(rotl8(Memory.getI32(addr + 44)), paethPredictor4(Memory.getI32(destAddr + 40), Memory.getI32(destAddr + 44 - widthBy4), Memory.getI32(destAddr + 40 - widthBy4))));
							Memory.setI32(destAddr + 48, byteAdd4(rotl8(Memory.getI32(addr + 48)), paethPredictor4(Memory.getI32(destAddr + 44), Memory.getI32(destAddr + 48 - widthBy4), Memory.getI32(destAddr + 44 - widthBy4))));
							Memory.setI32(destAddr + 52, byteAdd4(rotl8(Memory.getI32(addr + 52)), paethPredictor4(Memory.getI32(destAddr + 48), Memory.getI32(destAddr + 52 - widthBy4), Memory.getI32(destAddr + 48 - widthBy4))));
							Memory.setI32(destAddr + 56, byteAdd4(rotl8(Memory.getI32(addr + 56)), paethPredictor4(Memory.getI32(destAddr + 52), Memory.getI32(destAddr + 56 - widthBy4), Memory.getI32(destAddr + 52 - widthBy4))));
							Memory.setI32(destAddr + 60, byteAdd4(rotl8(Memory.getI32(addr + 60)), paethPredictor4(Memory.getI32(destAddr + 56), Memory.getI32(destAddr + 60 - widthBy4), Memory.getI32(destAddr + 56 - widthBy4))));
							addr += 64;
							destAddr += 64;
						}
						while (destAddr != endAddr) {
							Memory.setI32(destAddr, byteAdd4(rotl8(Memory.getI32(addr)), paethPredictor4(Memory.getI32(destAddr - 4), Memory.getI32(destAddr - widthBy4), Memory.getI32(destAddr - 4 - widthBy4))));
							addr += 4;
							destAddr += 4;
						}
					}
					//trace("Reverse filters (32-bit): " + (Lib.getTimer() - start) + "ms");
					
					// Copy into a BitmapData!
					Memory.select(oldFastMem);
					idatData.position = 0;
					bmp = new BitmapData(width, height, transparent, 0x00FFFFFF);
					bmp.setPixels(new Rectangle(0, 0, width, height), idatData);
				}
				else {	// 24-bit
					//var start = Lib.getTimer();
					
					var destStart = idatData.length;
					var destAddr : Int = destStart;
					// Since Flash wants ARGB (4-byte) values for each pixel,
					// we can't decode on-place :-(
					idatData.length = idatData.length + width * height * 4;
					Memory.select(idatData);
					
					// First line
					++addr;		// Skip filter byte (it's always sub for the first line)
					// First pixel has different formula
					Memory.setI16(destAddr, Memory.getByte(addr) << 8);
					Memory.setByte(destAddr + 2, Memory.getByte(addr + 1));
					Memory.setByte(destAddr + 3, Memory.getByte(addr + 2));
					
					var widthBy4 = width * 4;
					var endAddr = destAddr + widthBy4;
					addr += 3;
					destAddr += 4;
					var endAddr64 = destAddr + ((widthBy4 - 1) & 0xFFFFFFC0);
					
					// Rest of first line
					--addr;		// Offset addr by one so that when reading 32-bit little-endian RGB value,
								// we can read a random byte in the alpha (XRGB) which is OK because it's
								// ignored (but we do it to get the RGB offset properly)
					while (destAddr != endAddr64) {
						Memory.setI32(destAddr, byteAdd4(Memory.getI32(addr), Memory.getI32(destAddr - 4)));
						Memory.setI32(destAddr + 4, byteAdd4(Memory.getI32(addr + 3), Memory.getI32(destAddr)));
						Memory.setI32(destAddr + 8, byteAdd4(Memory.getI32(addr + 6), Memory.getI32(destAddr + 4)));
						Memory.setI32(destAddr + 12, byteAdd4(Memory.getI32(addr + 9), Memory.getI32(destAddr + 8)));
						Memory.setI32(destAddr + 16, byteAdd4(Memory.getI32(addr + 12), Memory.getI32(destAddr + 12)));
						Memory.setI32(destAddr + 20, byteAdd4(Memory.getI32(addr + 15), Memory.getI32(destAddr + 16)));
						Memory.setI32(destAddr + 24, byteAdd4(Memory.getI32(addr + 18), Memory.getI32(destAddr + 20)));
						Memory.setI32(destAddr + 28, byteAdd4(Memory.getI32(addr + 21), Memory.getI32(destAddr + 24)));
						Memory.setI32(destAddr + 32, byteAdd4(Memory.getI32(addr + 24), Memory.getI32(destAddr + 28)));
						Memory.setI32(destAddr + 36, byteAdd4(Memory.getI32(addr + 27), Memory.getI32(destAddr + 32)));
						Memory.setI32(destAddr + 40, byteAdd4(Memory.getI32(addr + 30), Memory.getI32(destAddr + 36)));
						Memory.setI32(destAddr + 44, byteAdd4(Memory.getI32(addr + 33), Memory.getI32(destAddr + 40)));
						Memory.setI32(destAddr + 48, byteAdd4(Memory.getI32(addr + 36), Memory.getI32(destAddr + 44)));
						Memory.setI32(destAddr + 52, byteAdd4(Memory.getI32(addr + 39), Memory.getI32(destAddr + 48)));
						Memory.setI32(destAddr + 56, byteAdd4(Memory.getI32(addr + 42), Memory.getI32(destAddr + 52)));
						Memory.setI32(destAddr + 60, byteAdd4(Memory.getI32(addr + 45), Memory.getI32(destAddr + 56)));
						addr += 48;
						destAddr += 64;
					}
					while (destAddr != endAddr) {
						Memory.setI32(destAddr, byteAdd4(Memory.getI32(addr), Memory.getI32(destAddr - 4)));
						addr += 3;
						destAddr += 4;
					}
					++addr;		// Un-offset addr
					
					// Remaining lines:
					for (i in 1 ... height) {
						++addr;		// Skip filter byte, always Paeth here
						
						// Do first pixel manually (formula is different)
						Memory.setI16(destAddr, (Memory.getByte(addr) + Memory.getByte(destAddr + 1 - widthBy4)) << 8);
						Memory.setByte(destAddr + 2, Memory.getByte(addr + 1) + Memory.getByte(destAddr + 2 - widthBy4));
						Memory.setByte(destAddr + 3, Memory.getByte(addr + 2) + Memory.getByte(destAddr + 3 - widthBy4));
						
						endAddr = destAddr + widthBy4;
						addr += 3;
						destAddr += 4;
						endAddr64 = destAddr + ((widthBy4 - 1) & 0xFFFFFFC0);
						
						--addr;
						while (destAddr != endAddr64) {
							Memory.setI32(destAddr, byteAdd4(Memory.getI32(addr), paethPredictor3Hi(Memory.getI32(destAddr - 4), Memory.getI32(destAddr - widthBy4), Memory.getI32(destAddr - 4 - widthBy4))));
							Memory.setI32(destAddr + 4, byteAdd4(Memory.getI32(addr + 3), paethPredictor3Hi(Memory.getI32(destAddr), Memory.getI32(destAddr + 4 - widthBy4), Memory.getI32(destAddr - widthBy4))));
							Memory.setI32(destAddr + 8, byteAdd4(Memory.getI32(addr + 6), paethPredictor3Hi(Memory.getI32(destAddr + 4), Memory.getI32(destAddr + 8 - widthBy4), Memory.getI32(destAddr + 4 - widthBy4))));
							Memory.setI32(destAddr + 12, byteAdd4(Memory.getI32(addr + 9), paethPredictor3Hi(Memory.getI32(destAddr + 8), Memory.getI32(destAddr + 12 - widthBy4), Memory.getI32(destAddr + 8 - widthBy4))));
							Memory.setI32(destAddr + 16, byteAdd4(Memory.getI32(addr + 12), paethPredictor3Hi(Memory.getI32(destAddr + 12), Memory.getI32(destAddr + 16 - widthBy4), Memory.getI32(destAddr + 12 - widthBy4))));
							Memory.setI32(destAddr + 20, byteAdd4(Memory.getI32(addr + 15), paethPredictor3Hi(Memory.getI32(destAddr + 16), Memory.getI32(destAddr + 20 - widthBy4), Memory.getI32(destAddr + 16 - widthBy4))));
							Memory.setI32(destAddr + 24, byteAdd4(Memory.getI32(addr + 18), paethPredictor3Hi(Memory.getI32(destAddr + 20), Memory.getI32(destAddr + 24 - widthBy4), Memory.getI32(destAddr + 20 - widthBy4))));
							Memory.setI32(destAddr + 28, byteAdd4(Memory.getI32(addr + 21), paethPredictor3Hi(Memory.getI32(destAddr + 24), Memory.getI32(destAddr + 28 - widthBy4), Memory.getI32(destAddr + 24 - widthBy4))));
							Memory.setI32(destAddr + 32, byteAdd4(Memory.getI32(addr + 24), paethPredictor3Hi(Memory.getI32(destAddr + 28), Memory.getI32(destAddr + 32 - widthBy4), Memory.getI32(destAddr + 28 - widthBy4))));
							Memory.setI32(destAddr + 36, byteAdd4(Memory.getI32(addr + 27), paethPredictor3Hi(Memory.getI32(destAddr + 32), Memory.getI32(destAddr + 36 - widthBy4), Memory.getI32(destAddr + 32 - widthBy4))));
							Memory.setI32(destAddr + 40, byteAdd4(Memory.getI32(addr + 30), paethPredictor3Hi(Memory.getI32(destAddr + 36), Memory.getI32(destAddr + 40 - widthBy4), Memory.getI32(destAddr + 36 - widthBy4))));
							Memory.setI32(destAddr + 44, byteAdd4(Memory.getI32(addr + 33), paethPredictor3Hi(Memory.getI32(destAddr + 40), Memory.getI32(destAddr + 44 - widthBy4), Memory.getI32(destAddr + 40 - widthBy4))));
							Memory.setI32(destAddr + 48, byteAdd4(Memory.getI32(addr + 36), paethPredictor3Hi(Memory.getI32(destAddr + 44), Memory.getI32(destAddr + 48 - widthBy4), Memory.getI32(destAddr + 44 - widthBy4))));
							Memory.setI32(destAddr + 52, byteAdd4(Memory.getI32(addr + 39), paethPredictor3Hi(Memory.getI32(destAddr + 48), Memory.getI32(destAddr + 52 - widthBy4), Memory.getI32(destAddr + 48 - widthBy4))));
							Memory.setI32(destAddr + 56, byteAdd4(Memory.getI32(addr + 42), paethPredictor3Hi(Memory.getI32(destAddr + 52), Memory.getI32(destAddr + 56 - widthBy4), Memory.getI32(destAddr + 52 - widthBy4))));
							Memory.setI32(destAddr + 60, byteAdd4(Memory.getI32(addr + 45), paethPredictor3Hi(Memory.getI32(destAddr + 56), Memory.getI32(destAddr + 60 - widthBy4), Memory.getI32(destAddr + 56 - widthBy4))));
							addr += 48;
							destAddr += 64;
						}
						while (destAddr != endAddr) {
							Memory.setI32(destAddr, byteAdd4(Memory.getI32(addr), paethPredictor3Hi(Memory.getI32(destAddr - 4), Memory.getI32(destAddr - widthBy4), Memory.getI32(destAddr - 4 - widthBy4))));
							addr += 3;
							destAddr += 4;
						}
						++addr;
					}
					//trace("Reverse filters (32-bit): " + (Lib.getTimer() - start) + "ms");
					
					// Copy into a BitmapData!
					Memory.select(oldFastMem);
					idatData.position = destStart;
					bmp = new BitmapData(width, height, transparent, 0x00FFFFFF);
					bmp.setPixels(new Rectangle(0, 0, width, height), idatData);
				}
			}
		}
		return bmp;
	}
	
	private static inline function rotl8(x : Int) { return (x << 8) | (x >>> 24); }
	
	private static inline function byteAdd4(a : UInt, b : UInt)
	{
		return (((a & 0xFF00FF00) + (b & 0xFF00FF00)) & 0xFF00FF00) | (((a & 0x00FF00FF) + (b & 0x00FF00FF)) & 0x00FF00FF);
	}
	
	private static inline function paethPredictor3Hi(a : Int, b : Int, c : Int)
	{
		var pa = abs((b & 0x0000FF00) - (c & 0x0000FF00));
		var pb = abs((a & 0x0000FF00) - (c & 0x0000FF00));
		var pc = abs((a & 0x0000FF00) + (b & 0x0000FF00) - ((c << 1) & 0x0001FE00));
		var notACond = (((pb - pa) | (pc - pa)) >> 31) & 0x0000FF00;
		var notBCond = ((pc - pb) >> 31) & 0x0000FF00;
		
		pa = abs((b & 0x00FF0000) - (c & 0x00FF0000));
		pb = abs((a & 0x00FF0000) - (c & 0x00FF0000));
		pc = abs((a & 0x00FF0000) + (b & 0x00FF0000) - ((c << 1) & 0x01FE0000));
		notACond |= (((pb - pa) | (pc - pa)) >> 31) & 0x00FF0000;
		notBCond |= ((pc - pb) >> 31) & 0x00FF0000;
		
		pa = abs(((b >> 8) & 0x00FF0000) - ((c >> 8) & 0x00FF0000));
		pb = abs(((a >> 8) & 0x00FF0000) - ((c >> 8) & 0x00FF0000));
		pc = abs(((a >> 8) & 0x00FF0000) + ((b >> 8) & 0x00FF0000) - ((c >> 7) & 0x01FE0000));
		notACond |= (((pb - pa) | (pc - pa)) >> 31) & 0xFF000000;
		notBCond |= ((pc - pb) >> 31) & 0xFF000000;
		
		//return pa <= pb && pa <= pc ? a : (pb <= pc ? b : c);
		return (a & ~notACond) | (b & notACond & ~notBCond) | (c & notACond & notBCond);
	}
#end
	
	private static inline function beginEncoding(img : BitmapData, png : IDataOutput, metadata : Dynamic)
	{
		if (encoding) {
			throw new Error("Only one PNG can be encoded at once (are you encoding asynchronously while attempting to encode another PNG synchronously?)");
			
			// This limitation is in place to make the implementation simpler;
			// there is only one domain memory chunk across all PNG encoding
			// (because it contains cached values like the CRC table).
		}
		
		if (png == ApplicationDomain.currentDomain.domainMemory) {
			throw new Error("Cannot use domain memory as PNG output");
		}
		
		encoding = true;
		
		
		if (level == null) {
			// Use default if no level explicitly specified
#if (FAST_ONLY || !(FAST_ONLY || NORMAL_ONLY || GOOD_ONLY))
			level = FAST;
#elseif NORMAL_ONLY
			level = NORMAL;
#elseif GOOD_ONLY
			level = GOOD;
#end
		}
		
		// Data will be select()ed for use with fast memory
		// The first 256 * 4 bytes are the CRC table
		// Inner chunk data is appended to the CRC table, starting at CHUNK_START
		
		initialize();		// Sets up data var & CRC table
		
		writePNGSignature(png);
		
		writeIHDRChunk(img, png);
		
		writeMetadataChunks(metadata, png);
	}
	
	
	private static inline function endEncoding(png : IDataOutput)
	{
		writeIENDChunk(png);
		
		encoding = false;
	}
	
	
	
	public inline function new(image : BitmapData, png : IDataOutput, dispatcher : IEventDispatcher, metadata : Dynamic)
	{
		targetFPS = 20;					// Default, can be overridden
		_new(image, png, dispatcher, metadata);		// Constructors are slow -- delegate to function
	}
	
	private function _new(image : BitmapData, png : IDataOutput, dispatcher : IEventDispatcher, metadata : Dynamic)
	{
		fastNew(image, png, dispatcher, metadata);
	}
	
	private inline function fastNew(image : BitmapData, png : IDataOutput, dispatcher : IEventDispatcher, metadata : Dynamic)
	{
		img = image;
		this.png = png;
		this.dispatcher = dispatcher;
		this.metadata = metadata;
		
		if (encoding) {
			// Add to queue for later!
			pendingAsyncEncodings.push(this);
		}
		else {
			lastFrameStart = Lib.getTimer();
			
			// Preserve current domain memory so that we can leave it as we found
			// it once we've finished using it
			var oldFastMem = ApplicationDomain.currentDomain.domainMemory;
			
			beginEncoding(img, png, metadata);
			currentY = 0;
			frameCount = 0;
			done = false;
			msPerFrame = new Vector<Int>(FRAME_AVG_SMOOTH_COUNT, true);
			msPerFrameIndex = 0;
			msPerLine = new Vector<Float>(FRAME_AVG_SMOOTH_COUNT, true);
			msPerLineIndex = 0;
			updatesPerFrame = new Vector<Int>(FRAME_AVG_SMOOTH_COUNT, true);
			updatesPerFrameIndex = 0;
			
			// Note that this effectively freezes the compression level for the
			// duration of the encoding (even if the static level member changes)
			deflateStream = DeflateStream.createEx(level, DEFLATE_SCRATCH, CHUNK_START, true);
			
			// Get notified of new frames, and timer events
			sprite.addEventListener(Event.ENTER_FRAME, onEnterFrame);
			
			// We write data in chunks (one per update), starting with one
			// chunk right now in order to gather some statistics up front
			// to make an informed estimate for the next update's step size.
			
			// Note that small images may be entirely encoded in this step,
			// but we don't dispatch any events until the next update in
			// order to give the client an opportunity to attach event listeners.
			
			if (img.width > 0 && img.height > 0) {
				// Determine proper start step
				var startTime = Lib.getTimer();
				
				// Write first ~20K pixels to see how fast it is
				var height = Math.ceil(Math.min(FIRST_UPDATE_PIXELS / img.width, img.height));
				writeIDATChunk(img, 0, height, deflateStream, png);
				
				var endTime = Lib.getTimer();
				updateMsPerLine(endTime - startTime, height);
				
				// Use unmeasured FPS as guestimate to seed msPerFrame
				var fps = Lib.current == null || Lib.current.stage == null ? 24 : Lib.current.stage.frameRate;
				updateMsPerFrame(Std.int(1.0 / fps * 1000));
				
				updateUpdatesPerFrame(1);
				
				updateStep();
				
				currentY = height;
			}
			else {
				// A dimension is 0
				step = img.height;
			}
			
			updates = 0;
			
			Memory.select(oldFastMem);		// Play nice!
		}
	}
	
	
	// Updates the msPerLine vector with a new measure
	private inline function updateMsPerLine(ms : Int, lines : Int)
	{
		if (lines != 0) {
			if (ms <= 0) {
				// Can occasionally happen because timer resolution on Windows is limited to 10ms
				ms = 5;		// Guess!
			}
			
			msPerLine[msPerLineIndex] = ms * 1.0 / lines;
			msPerLineIndex = (msPerLineIndex + 1) & (FRAME_AVG_SMOOTH_COUNT - 1);	// Cheap modulus
		}
	}
	
	// Updates the msPerFrame vector with a new measure
	private inline function updateMsPerFrame(ms : Int)
	{
		msPerFrame[msPerFrameIndex] = ms;
		msPerFrameIndex = (msPerFrameIndex + 1) & (FRAME_AVG_SMOOTH_COUNT - 1);		// Cheap modulus
	}
	
	// Updates the updatesPerFrame vector with a new measure
	private inline function updateUpdatesPerFrame(updates : Int)
	{
		updatesPerFrame[updatesPerFrameIndex] = updates;
		updatesPerFrameIndex = (updatesPerFrameIndex + 1) & (FRAME_AVG_SMOOTH_COUNT - 1);
	}
	
	
	// Makes an informed estimate of how many scanlines should be processed during
	// the next update, and sets the "step" member variable to that value
	private inline function updateStep()
	{
		// Updates are always executing as fast as possible in order to saturate the
		// event loop/render cycle (aka the "elastic racetrack": see http://www.craftymind.com/2008/04/18/updated-elastic-racetrack-for-flash-9-and-avm2/)
		// So, instead of changing the update frequency, we predict the best number of
		// scanlines to process (the "step") during the next update based on past evidence
		// of how long it takes to process one scanline. We monitor the FPS to determine
		// how closely we're approaching the targetFPS, and thus which direction we should
		// adjust towards. We push against the targetFPS as much as possible in order to
		// saturate the AVM2's cycle and so minimize idling time and maximize encoding speed).
		
		// Data: We have the last FRAME_AVG_SMOOTH_COUNT measurements of various
		// statistics that we can use to calculate moving averages
		
		
		var avgMsPerFrame = 0.0;
		var count = 0;
		
		for (ms in msPerFrame) {
			if (ms > 0) {		// Discount measurements of 0 (can happen with imprecise platform timers)
				avgMsPerFrame += ms;
				++count;
			}
		}
		
		if (count != 0) {		// Make sure we have sufficient data
			avgMsPerFrame /= count;
			
			// Check our current empirical FPS against the target
			// (with 15% leeway in favour of a slightly slower FPS)
			var targetMs = 1000.0 / targetFPS;
			if (avgMsPerFrame > targetMs * 1.15) {
				// Too slow, pull back a bit
				targetMs -= avgMsPerFrame - targetMs;		// Error delta
			}
			
			var avgUpdatesPerFrame = 0.0;
			count = 0;
			for (ups in updatesPerFrame) {
				if (ups > 0) {
					avgUpdatesPerFrame += ups;
					++count;
				}
			}
			if (count != 0) {
				avgUpdatesPerFrame /= count;
				
				var avgMsPerLine = 0.0;
				count = 0;
				for (ms in msPerLine) {
					if (ms > 0) {
						avgMsPerLine += ms;
						++count;
					}
				}
				if (count != 0) {
					avgMsPerLine /= count;
					
					// Calculate step; must include at least MIN_PIXELS_PER_FRAME, and must be >= 1
					// In dimensional analysis, the estimate works out to:
					//     ? scanlinesPerUpdate = scanlinesPerMs * targetMsPerFrame * framesPerUpdate
					step = Math.ceil(Math.max(targetMs / avgMsPerLine / avgUpdatesPerFrame, MIN_PIXELS_PER_UPDATE / img.width));
				}
				else {
					step = Math.ceil(MIN_PIXELS_PER_UPDATE / img.width);
				}
			}
			else {
				step = Math.ceil(MIN_PIXELS_PER_UPDATE / img.width);
			}
		}
		else {		// Not enough data, just use bare minimum for step
			step = Math.ceil(MIN_PIXELS_PER_UPDATE / img.width);
		}
	}
	
	
	private function onEnterFrame(e : Event)
	{
		updateFrameInfo();
		update();
	}
	
	
	private inline function updateFrameInfo()
	{
		if (!done) {
			++frameCount;
			
			var now = Lib.getTimer();
			updateMsPerFrame(now - lastFrameStart);
			lastFrameStart = now;
			
			updateUpdatesPerFrame(updates);
			updates = 0;
		}
	}
	
	
	private inline function update()
	{
		// Need to check if we've finished or not since it's possible (if we're
		// attached to a timer) for a timer event to be dispatched before we stop
		// it, but get processed after we've finished (e.g. if there's two timer
		// events in the event queue and the first one finishes the work, the second
		// will still cause this function to be entered since it was generated
		// before we stopped the timer).
		if (!done) {
			var start = Lib.getTimer();
			
			++updates;
			
			// Play nice with others, and preserve the domain memory
			var oldFastMem = ApplicationDomain.currentDomain.domainMemory;
			Memory.select(data);
			
			// Queue events (dispatched at end) instead of dispatching them inline
			// because during a call to dispatchEvent *other* pending events
			// might be dispatched too, possibly resulting in this method being
			// called again in a re-entrant fashion (which doesn't play nicely
			// with storing/retrieving oldFastMem).
			var progressEvent : ProgressEvent = null;
			var completeEvent : Event = null;
			
			var bytesPerPixel = img.transparent ? 4 : 3;
			var totalBytes = bytesPerPixel * img.width * img.height;
			
			if (currentY >= img.height) {
				// Finished encoding the entire image in the initial setup
				progressEvent = new ProgressEvent(ProgressEvent.PROGRESS, false, false, totalBytes, totalBytes);
				completeEvent = finalize();
			}
			else {
				var next = Std.int(Math.min(currentY + step, img.height));
				writeIDATChunk(img, currentY, next, deflateStream, png);
				currentY = next;
				
				var currentBytes = bytesPerPixel * img.width * currentY;
				
				progressEvent = new ProgressEvent(ProgressEvent.PROGRESS, false, false, currentBytes, totalBytes);
				completeEvent = finalize();
				
				updateMsPerLine(Lib.getTimer() - start, step);
				updateStep();
			}
			
			Memory.select(oldFastMem);
			
			// With `done` stable and domain memory properly set, we can now safely
			// handle re-entrancy (thank goodness Flash is single threaded or this
			// would be even more of a nightmare)
			if (progressEvent != null) {
				dispatcher.dispatchEvent(progressEvent);
				progressEvent = null;
			}
			if (completeEvent != null) {
				dispatcher.dispatchEvent(completeEvent);
				completeEvent = null;
			}
			
			if (done) {
				//trace("Async encoding finished on frame " + __frame + " (targetFPS was " + targetFPS + ")");
				
				// Clear some references to give the garbage collector an easier time
				dispatcher = null;		// This removes a circular reference, which might save a mark-and-sweep step
				img = null;
				deflateStream = null;
				msPerFrame = null;
				msPerLine = null;
				updatesPerFrame = null;
				
				if (!encoding && pendingAsyncEncodings.length > 0) {
					// Need to check `encoding` just in case someone started encoding another PNG
					// in the async COMPLETED event handler
					var next = pendingAsyncEncodings.shift();
					next._new(next.img, next.png, next.dispatcher, next.metadata);
				}
			}
		}
	}
	
	
	// Only finalizes the encoding if there's nothing left to encode
	private inline function finalize() : Event
	{
		var result : Event = null;
		if (currentY >= img.height) {
			done = true;
			
			sprite.removeEventListener(Event.ENTER_FRAME, onEnterFrame);
			
			endEncoding(png);
			
			result = new Event(Event.COMPLETE);
			
			//trace("Async completed over " + frameCount + " frame(s)");
		}
		return result;
	}
	
	

	private static inline function writePNGSignature(png : IDataOutput)
	{
		// See PNG spec for details
		png.writeUnsignedInt(0x89504e47);
		png.writeUnsignedInt(0x0D0A1A0A);
	}
	
	
	private static inline function writeIHDRChunk(img : BitmapData, png : IDataOutput)
	{
		var chunkLength = 13;		// 13-byte header
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
	
	
	private static function writeMetadataChunks(metadata : Dynamic, png : IDataOutput)
	{
		if (metadata != null) {
			var chunkData = new ByteArray();
			Memory.select(data);
			
			var fields = Reflect.fields(metadata);
			if (fields == null || fields.length == 0) {
				var cls = Type.getClass(metadata);
				if (cls != null) {
					fields = Type.getInstanceFields(cls);
				}
			}
			for (field in fields) {
				// Check if the field matches the constraints
				// outlined in http://www.libpng.org/pub/png/spec/iso/index-object.html#11textinfo
				
				if (field.length < 1 || field.length > 79) {
					continue;
				}
				
				chunkData.clear();
				chunkData.position = 0;
				var validChars = true;
				for (i in 0 ... field.length) {
					var c = field.charCodeAt(i);
					if (!(c >= 32 && c <= 126 || c >= 161 && c <= 255)) {
						validChars = false;
						break;
					}
					chunkData.writeByte(c);
				}
				if (!validChars) {
					continue;
				}
				
				var rawValue : Dynamic = Reflect.field(metadata, field);
				if (rawValue == null || Reflect.isFunction(rawValue)) {
					continue;
				}
				
				// Convert value to string and normalize newlines
				var value = Std.string(rawValue);
				value = StringTools.replace(value, "\r\n", "\n");				
				value = StringTools.replace(value, "\r", "\n");
				
				var isLatin1 = true;
				for (i in 0 ... value.length) {
					var c = value.charCodeAt(i);
					if (c < 0 || c > 255) {
						isLatin1 = false;
						break;
					}
				}
				
				if (isLatin1) {
					// tEXt
					chunkData.writeByte(0);		// Null separator
					for (i in 0 ... value.length) {
						chunkData.writeByte(value.charCodeAt(i));
					}
				}
				else {
					// iTXt
					//chunkData.writeByte(0);	// Null separator
					//chunkData.writeByte(0);	// No compression
					//chunkData.writeByte(0);	// Compression method
					//chunkData.writeByte(0);	// No language tag
					chunkData.writeInt(0);
					
					chunkData.writeByte(0);		// No translated keyword
					chunkData.writeUTFBytes(value);
				}
				
				var minLength : UInt = CHUNK_START + chunkData.length;
				if (data.length < minLength) {
					data.length = Std.int(Math.max(CHUNK_START + chunkData.length, ApplicationDomain.MIN_DOMAIN_MEMORY_LENGTH));
					Memory.select(data);
				}
				chunkData.position = 0;
				memcpy(chunkData, CHUNK_START);
				writeChunk(png, isLatin1 ? 0x74455874 : 0x69545874, chunkData.length);
			}
		}
	}
	
	
	// Copies length bytes (all by default) from src into flash.Memory at the specified offset
	private static inline function memcpy(src : ByteArray, offset : UInt, length : UInt = 0) : Void
	{
		src.readBytes(ApplicationDomain.currentDomain.domainMemory, offset, length);
	}
	
	// Writes one integer into flash.Memory at the given address, in big-endian order
	// (domain memory is always little-endian)
	private static inline function writeI32BE(addr: UInt, value : UInt) : Void
	{
		Memory.setByte(addr, value >>> 24);
		Memory.setByte(addr + 1, value >>> 16);
		Memory.setByte(addr + 2, value >>> 8);
		Memory.setByte(addr + 3, value);
	}
	
	
	private static function writeIDATChunk(img : BitmapData, startY : Int, endY : Int, deflateStream: DeflateStream, png : IDataOutput)
	{
		_writeIDATChunk(img, startY, endY, deflateStream, png);
	}
	
	private static inline function _writeIDATChunk(img : BitmapData, startY : Int, endY : Int, deflateStream: DeflateStream, png : IDataOutput)
	{
		var width = img.width;
		var bufferedStartY = startY == 0 ? 0 : startY - 1;	// To give access to previous row
		var height = endY - startY;
		var bufferedHeight = endY - bufferedStartY;
		region.y = bufferedStartY;
		region.width = width;
		region.height = bufferedHeight;
		
		var widthBy4 = width << 2;
		
		var bytesPerPixel = img.transparent ? 4 : 3;
		
		// Length of IDAT data: 3 or 4 bytes per pixel + 1 byte per scanline
		var length : UInt = width * height * bytesPerPixel + height;
		
		// Add a dummy byte for 24-bit PNGs (so that we can write one past the end
		// without hurting anything)
		var extra : UInt = img.transparent ? 0 : 1;
		
		// Size needed to store byte array of bitmap
		var scratchSize : UInt = width * bufferedHeight * 4;
		
		// Memory layout:
		// DEFLATE_SCRATCH: Deflate stream scratch memory
		// CHUNK_START: Deflated data (written last)
		// CHUNK_START + deflated data buffer: scratch (raw image bytes)
		// CHUNK_START + deflated data buffer + scratchSize: Uncompressed PNG-format image data
		
		data.length = Std.int(Math.max(CHUNK_START + deflateStream.maxOutputBufferSize(length) + scratchSize + length + extra, ApplicationDomain.MIN_DOMAIN_MEMORY_LENGTH));
		Memory.select(data);
		
		var scratchAddr : Int = CHUNK_START + deflateStream.maxOutputBufferSize(length);
		var addrStart : Int = scratchAddr + scratchSize;
		
		var addr = addrStart;
		
		var imgBytes = img.getPixels(region);
		imgBytes.position = 0;
		memcpy(imgBytes, scratchAddr);
		
		if (bufferedStartY != startY) {
			scratchAddr += width * 4;
		}
		
		var endAddr;
		var endAddr64;
		if (img.transparent) {
			if (bufferedStartY == startY) {
				// Do first line of image separately (no row above)
				Memory.setByte(addr, 1);		// Sub filter
				addr += 1;
				
				if (width > 0 && height > 0) {
					// Do first pixel (4 bytes) manually (sub formula is different)
					Memory.setI32(addr, rotr8(Memory.getI32(scratchAddr)));
					endAddr = addr + widthBy4;
					addr += 4;
					scratchAddr += 4;
					endAddr64 = addr + ((widthBy4 - 1) & 0xFFFFFFC0);
					
					// Copy line, moving alpha byte to end, and applying filter
					while (addr != endAddr64) {
						Memory.setI32(addr, rotr8(byteSub4(Memory.getI32(scratchAddr), Memory.getI32(scratchAddr - 4))));
						Memory.setI32(addr + 4, rotr8(byteSub4(Memory.getI32(scratchAddr + 4), Memory.getI32(scratchAddr))));
						Memory.setI32(addr + 8, rotr8(byteSub4(Memory.getI32(scratchAddr + 8), Memory.getI32(scratchAddr + 4))));
						Memory.setI32(addr + 12, rotr8(byteSub4(Memory.getI32(scratchAddr + 12), Memory.getI32(scratchAddr + 8))));
						Memory.setI32(addr + 16, rotr8(byteSub4(Memory.getI32(scratchAddr + 16), Memory.getI32(scratchAddr + 12))));
						Memory.setI32(addr + 20, rotr8(byteSub4(Memory.getI32(scratchAddr + 20), Memory.getI32(scratchAddr + 16))));
						Memory.setI32(addr + 24, rotr8(byteSub4(Memory.getI32(scratchAddr + 24), Memory.getI32(scratchAddr + 20))));
						Memory.setI32(addr + 28, rotr8(byteSub4(Memory.getI32(scratchAddr + 28), Memory.getI32(scratchAddr + 24))));
						Memory.setI32(addr + 32, rotr8(byteSub4(Memory.getI32(scratchAddr + 32), Memory.getI32(scratchAddr + 28))));
						Memory.setI32(addr + 36, rotr8(byteSub4(Memory.getI32(scratchAddr + 36), Memory.getI32(scratchAddr + 32))));
						Memory.setI32(addr + 40, rotr8(byteSub4(Memory.getI32(scratchAddr + 40), Memory.getI32(scratchAddr + 36))));
						Memory.setI32(addr + 44, rotr8(byteSub4(Memory.getI32(scratchAddr + 44), Memory.getI32(scratchAddr + 40))));
						Memory.setI32(addr + 48, rotr8(byteSub4(Memory.getI32(scratchAddr + 48), Memory.getI32(scratchAddr + 44))));
						Memory.setI32(addr + 52, rotr8(byteSub4(Memory.getI32(scratchAddr + 52), Memory.getI32(scratchAddr + 48))));
						Memory.setI32(addr + 56, rotr8(byteSub4(Memory.getI32(scratchAddr + 56), Memory.getI32(scratchAddr + 52))));
						Memory.setI32(addr + 60, rotr8(byteSub4(Memory.getI32(scratchAddr + 60), Memory.getI32(scratchAddr + 56))));
						addr += 64;
						scratchAddr += 64;
					}
					while (addr != endAddr) {
						Memory.setI32(addr, rotr8(byteSub4(Memory.getI32(scratchAddr), Memory.getI32(scratchAddr - 4))));
						addr += 4;
						scratchAddr += 4;
					}
				}
			}
			
			// Other lines:
			for (i in 1 ... bufferedHeight) {
				Memory.setByte(addr, 4);		// Paeth filter
				addr += 1;
				
				if (width > 0) {
					// Do first pixel (4 bytes) manually (formula is different)
					Memory.setI32(addr, rotr8(byteSub4(Memory.getI32(scratchAddr), Memory.getI32(scratchAddr - widthBy4))));
					endAddr = addr + widthBy4;
					addr += 4;
					scratchAddr += 4;
					endAddr64 = addr + ((widthBy4 - 1) & 0xFFFFFFC0);
				
					// Copy line, moving alpha byte to end, and applying filter
					while (addr != endAddr64) {
						Memory.setI32(addr, rotr8(byteSub4(Memory.getI32(scratchAddr), paethPredictor4(Memory.getI32(scratchAddr - 4), Memory.getI32(scratchAddr - widthBy4), Memory.getI32(scratchAddr - 4 - widthBy4)))));
						Memory.setI32(addr + 4, rotr8(byteSub4(Memory.getI32(scratchAddr + 4), paethPredictor4(Memory.getI32(scratchAddr), Memory.getI32(scratchAddr + 4 - widthBy4), Memory.getI32(scratchAddr - widthBy4)))));
						Memory.setI32(addr + 8, rotr8(byteSub4(Memory.getI32(scratchAddr + 8), paethPredictor4(Memory.getI32(scratchAddr + 4), Memory.getI32(scratchAddr + 8 - widthBy4), Memory.getI32(scratchAddr + 4 - widthBy4)))));
						Memory.setI32(addr + 12, rotr8(byteSub4(Memory.getI32(scratchAddr + 12), paethPredictor4(Memory.getI32(scratchAddr + 8), Memory.getI32(scratchAddr + 12 - widthBy4), Memory.getI32(scratchAddr + 8 - widthBy4)))));
						Memory.setI32(addr + 16, rotr8(byteSub4(Memory.getI32(scratchAddr + 16), paethPredictor4(Memory.getI32(scratchAddr + 12), Memory.getI32(scratchAddr + 16 - widthBy4), Memory.getI32(scratchAddr + 12 - widthBy4)))));
						Memory.setI32(addr + 20, rotr8(byteSub4(Memory.getI32(scratchAddr + 20), paethPredictor4(Memory.getI32(scratchAddr + 16), Memory.getI32(scratchAddr + 20 - widthBy4), Memory.getI32(scratchAddr + 16 - widthBy4)))));
						Memory.setI32(addr + 24, rotr8(byteSub4(Memory.getI32(scratchAddr + 24), paethPredictor4(Memory.getI32(scratchAddr + 20), Memory.getI32(scratchAddr + 24 - widthBy4), Memory.getI32(scratchAddr + 20 - widthBy4)))));
						Memory.setI32(addr + 28, rotr8(byteSub4(Memory.getI32(scratchAddr + 28), paethPredictor4(Memory.getI32(scratchAddr + 24), Memory.getI32(scratchAddr + 28 - widthBy4), Memory.getI32(scratchAddr + 24 - widthBy4)))));
						Memory.setI32(addr + 32, rotr8(byteSub4(Memory.getI32(scratchAddr + 32), paethPredictor4(Memory.getI32(scratchAddr + 28), Memory.getI32(scratchAddr + 32 - widthBy4), Memory.getI32(scratchAddr + 28 - widthBy4)))));
						Memory.setI32(addr + 36, rotr8(byteSub4(Memory.getI32(scratchAddr + 36), paethPredictor4(Memory.getI32(scratchAddr + 32), Memory.getI32(scratchAddr + 36 - widthBy4), Memory.getI32(scratchAddr + 32 - widthBy4)))));
						Memory.setI32(addr + 40, rotr8(byteSub4(Memory.getI32(scratchAddr + 40), paethPredictor4(Memory.getI32(scratchAddr + 36), Memory.getI32(scratchAddr + 40 - widthBy4), Memory.getI32(scratchAddr + 36 - widthBy4)))));
						Memory.setI32(addr + 44, rotr8(byteSub4(Memory.getI32(scratchAddr + 44), paethPredictor4(Memory.getI32(scratchAddr + 40), Memory.getI32(scratchAddr + 44 - widthBy4), Memory.getI32(scratchAddr + 40 - widthBy4)))));
						Memory.setI32(addr + 48, rotr8(byteSub4(Memory.getI32(scratchAddr + 48), paethPredictor4(Memory.getI32(scratchAddr + 44), Memory.getI32(scratchAddr + 48 - widthBy4), Memory.getI32(scratchAddr + 44 - widthBy4)))));
						Memory.setI32(addr + 52, rotr8(byteSub4(Memory.getI32(scratchAddr + 52), paethPredictor4(Memory.getI32(scratchAddr + 48), Memory.getI32(scratchAddr + 52 - widthBy4), Memory.getI32(scratchAddr + 48 - widthBy4)))));
						Memory.setI32(addr + 56, rotr8(byteSub4(Memory.getI32(scratchAddr + 56), paethPredictor4(Memory.getI32(scratchAddr + 52), Memory.getI32(scratchAddr + 56 - widthBy4), Memory.getI32(scratchAddr + 52 - widthBy4)))));
						Memory.setI32(addr + 60, rotr8(byteSub4(Memory.getI32(scratchAddr + 60), paethPredictor4(Memory.getI32(scratchAddr + 56), Memory.getI32(scratchAddr + 60 - widthBy4), Memory.getI32(scratchAddr + 56 - widthBy4)))));
						addr += 64;
						scratchAddr += 64;
					}
					while (addr != endAddr) {
						Memory.setI32(addr, rotr8(byteSub4(Memory.getI32(scratchAddr), paethPredictor4(Memory.getI32(scratchAddr - 4), Memory.getI32(scratchAddr - widthBy4), Memory.getI32(scratchAddr - 4 - widthBy4)))));
						addr += 4;
						scratchAddr += 4;
					}
				}
			}
		}
		else {
			if (bufferedStartY == startY) {
				// Do first line of image separately (no row above)
				Memory.setByte(addr, 1);		// Sub filter
				addr += 1;
				
				if (width > 0 && height > 0) {
					// Do first pixel (3 bytes) manually (sub formula is different)
					Memory.setI16(addr, Memory.getUI16(scratchAddr + 1));
					Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3));
					endAddr = addr + width * 3;
					addr += 3;
					scratchAddr += 4;
					
					// Copy line.
					// Offset scratch address so that the extra byte that we read goes into the right (unused) spot
					++scratchAddr;
					endAddr64 = scratchAddr + ((width * 3 - 1) & 0xFFFFFFC0);
					while (scratchAddr != endAddr64) {
						Memory.setI32(addr, byteSub4(Memory.getI32(scratchAddr), Memory.getI32(scratchAddr - 4)));
						Memory.setI32(addr + 3, byteSub4(Memory.getI32(scratchAddr + 4), Memory.getI32(scratchAddr)));
						Memory.setI32(addr + 6, byteSub4(Memory.getI32(scratchAddr + 8), Memory.getI32(scratchAddr + 4)));
						Memory.setI32(addr + 9, byteSub4(Memory.getI32(scratchAddr + 12), Memory.getI32(scratchAddr + 8)));
						Memory.setI32(addr + 12, byteSub4(Memory.getI32(scratchAddr + 16), Memory.getI32(scratchAddr + 12)));
						Memory.setI32(addr + 15, byteSub4(Memory.getI32(scratchAddr + 20), Memory.getI32(scratchAddr + 16)));
						Memory.setI32(addr + 18, byteSub4(Memory.getI32(scratchAddr + 24), Memory.getI32(scratchAddr + 20)));
						Memory.setI32(addr + 21, byteSub4(Memory.getI32(scratchAddr + 28), Memory.getI32(scratchAddr + 24)));
						Memory.setI32(addr + 24, byteSub4(Memory.getI32(scratchAddr + 32), Memory.getI32(scratchAddr + 28)));
						Memory.setI32(addr + 27, byteSub4(Memory.getI32(scratchAddr + 36), Memory.getI32(scratchAddr + 32)));
						Memory.setI32(addr + 30, byteSub4(Memory.getI32(scratchAddr + 40), Memory.getI32(scratchAddr + 36)));
						Memory.setI32(addr + 33, byteSub4(Memory.getI32(scratchAddr + 44), Memory.getI32(scratchAddr + 40)));
						Memory.setI32(addr + 36, byteSub4(Memory.getI32(scratchAddr + 48), Memory.getI32(scratchAddr + 44)));
						Memory.setI32(addr + 39, byteSub4(Memory.getI32(scratchAddr + 52), Memory.getI32(scratchAddr + 48)));
						Memory.setI32(addr + 42, byteSub4(Memory.getI32(scratchAddr + 56), Memory.getI32(scratchAddr + 52)));
						Memory.setI32(addr + 45, byteSub4(Memory.getI32(scratchAddr + 60), Memory.getI32(scratchAddr + 56)));
						addr += 48;
						scratchAddr += 64;
					}
					while (addr != endAddr) {
						Memory.setI32(addr, byteSub4(Memory.getI32(scratchAddr), Memory.getI32(scratchAddr - 4)));
						addr += 3;
						scratchAddr += 4;
					}
					--scratchAddr;
				}
			}
			
			// Other lines:
			for (i in 1 ... bufferedHeight) {
				Memory.setByte(addr, 4);		// Paeth filter
				addr += 1;
				
				if (width > 0) {
					// Do first pixel (3 bytes) manually (formula is different)
					Memory.setI32(addr, byteSub4(Memory.getI32(scratchAddr + 1), Memory.getI32(scratchAddr + 1 - widthBy4)));
					endAddr = addr + width * 3;
					addr += 3;
					scratchAddr += 4;
					
					// Copy line, applying filter
					++scratchAddr;
					endAddr64 = scratchAddr + ((width * 3 - 1) & 0xFFFFFFC0);
					while (scratchAddr != endAddr64) {
						Memory.setI32(addr, byteSub4(Memory.getI32(scratchAddr), paethPredictor3Lo(Memory.getI32(scratchAddr - 4), Memory.getI32(scratchAddr - widthBy4), Memory.getI32(scratchAddr - 4 - widthBy4))));
						Memory.setI32(addr + 3, byteSub4(Memory.getI32(scratchAddr + 4), paethPredictor3Lo(Memory.getI32(scratchAddr), Memory.getI32(scratchAddr + 4 - widthBy4), Memory.getI32(scratchAddr - widthBy4))));
						Memory.setI32(addr + 6, byteSub4(Memory.getI32(scratchAddr + 8), paethPredictor3Lo(Memory.getI32(scratchAddr + 4), Memory.getI32(scratchAddr + 8 - widthBy4), Memory.getI32(scratchAddr + 4 - widthBy4))));
						Memory.setI32(addr + 9,  byteSub4(Memory.getI32(scratchAddr + 12), paethPredictor3Lo(Memory.getI32(scratchAddr + 8), Memory.getI32(scratchAddr + 12 - widthBy4), Memory.getI32(scratchAddr + 8 - widthBy4))));
						Memory.setI32(addr + 12, byteSub4(Memory.getI32(scratchAddr + 16), paethPredictor3Lo(Memory.getI32(scratchAddr + 12), Memory.getI32(scratchAddr + 16 - widthBy4), Memory.getI32(scratchAddr + 12 - widthBy4))));
						Memory.setI32(addr + 15, byteSub4(Memory.getI32(scratchAddr + 20), paethPredictor3Lo(Memory.getI32(scratchAddr + 16), Memory.getI32(scratchAddr + 20 - widthBy4), Memory.getI32(scratchAddr + 16 - widthBy4))));
						Memory.setI32(addr + 18, byteSub4(Memory.getI32(scratchAddr + 24), paethPredictor3Lo(Memory.getI32(scratchAddr + 20), Memory.getI32(scratchAddr + 24 - widthBy4), Memory.getI32(scratchAddr + 20 - widthBy4))));
						Memory.setI32(addr + 21, byteSub4(Memory.getI32(scratchAddr + 28), paethPredictor3Lo(Memory.getI32(scratchAddr + 24), Memory.getI32(scratchAddr + 28 - widthBy4), Memory.getI32(scratchAddr + 24 - widthBy4))));
						Memory.setI32(addr + 24, byteSub4(Memory.getI32(scratchAddr + 32), paethPredictor3Lo(Memory.getI32(scratchAddr + 28), Memory.getI32(scratchAddr + 32 - widthBy4), Memory.getI32(scratchAddr + 28 - widthBy4))));
						Memory.setI32(addr + 27, byteSub4(Memory.getI32(scratchAddr + 36), paethPredictor3Lo(Memory.getI32(scratchAddr + 32), Memory.getI32(scratchAddr + 36 - widthBy4), Memory.getI32(scratchAddr + 32 - widthBy4))));
						Memory.setI32(addr + 30, byteSub4(Memory.getI32(scratchAddr + 40), paethPredictor3Lo(Memory.getI32(scratchAddr + 36), Memory.getI32(scratchAddr + 40 - widthBy4), Memory.getI32(scratchAddr + 36 - widthBy4))));
						Memory.setI32(addr + 33, byteSub4(Memory.getI32(scratchAddr + 44), paethPredictor3Lo(Memory.getI32(scratchAddr + 40), Memory.getI32(scratchAddr + 44 - widthBy4), Memory.getI32(scratchAddr + 40 - widthBy4))));
						Memory.setI32(addr + 36, byteSub4(Memory.getI32(scratchAddr + 48), paethPredictor3Lo(Memory.getI32(scratchAddr + 44), Memory.getI32(scratchAddr + 48 - widthBy4), Memory.getI32(scratchAddr + 44 - widthBy4))));
						Memory.setI32(addr + 39, byteSub4(Memory.getI32(scratchAddr + 52), paethPredictor3Lo(Memory.getI32(scratchAddr + 48), Memory.getI32(scratchAddr + 52 - widthBy4), Memory.getI32(scratchAddr + 48 - widthBy4))));
						Memory.setI32(addr + 42, byteSub4(Memory.getI32(scratchAddr + 56), paethPredictor3Lo(Memory.getI32(scratchAddr + 52), Memory.getI32(scratchAddr + 56 - widthBy4), Memory.getI32(scratchAddr + 52 - widthBy4))));
						Memory.setI32(addr + 45, byteSub4(Memory.getI32(scratchAddr + 60), paethPredictor3Lo(Memory.getI32(scratchAddr + 56), Memory.getI32(scratchAddr + 60 - widthBy4), Memory.getI32(scratchAddr + 56 - widthBy4))));
						addr += 48;
						scratchAddr += 64;
					}
					while (addr != endAddr) {
						Memory.setI32(addr, byteSub4(Memory.getI32(scratchAddr), paethPredictor3Lo(Memory.getI32(scratchAddr - 4), Memory.getI32(scratchAddr - widthBy4), Memory.getI32(scratchAddr - 4 - widthBy4))));
						addr += 3;
						scratchAddr += 4;
					}
					--scratchAddr;
				}
			}
		}
		
		deflateStream.fastWrite(addrStart, addrStart + length);
		
		var lastChunk = endY == img.height;
		var range = lastChunk ? deflateStream.fastFinalize() : deflateStream.peek();
		writeChunk(png, 0x49444154, range.len());
		
		if (!lastChunk) {
			deflateStream.release();
		}
	}
	
	private static inline function rotr8(x) { return (x >>> 8) | (x << 24); }
	
	private static inline function byteSub4(a : UInt, b : UInt)
	{
		return ((((a & 0xFF00FF00) | 0x00010000) - (b & 0xFF00FF00)) & 0xFF00FF00) | ((((a & 0x00FF00FF) | 0x01000100) - (b & 0x00FF00FF)) & 0x00FF00FF);
	}
	
	/*private static inline function paethPredictor(a, b, c)
	{
		// a = left, b = above, c = upper left
		var p = a + b - c;        // initial estimate
		var pa = abs(p - a),      // distances to a, b, c
		    pb = abs(p - b),
		    pc = abs(p - c);
		
		// return nearest of a,b,c, breaking ties in order a,b,c.
		
		// Jumps replaced with bitwise operations to improve speed
		//return pa <= pb && pa <= pc ? a : (pb <= pc ? b : c);
		var notACond = ((pb - pa) | (pc - pa)) >> 31;
		var notBCond = (pc - pb) >> 31;
		return (a & ~notACond) | (b & notACond & ~notBCond) | (c & notACond & notBCond);
	}*/
	
	private static inline function paethPredictor4(a : Int, b : Int, c : Int)
	{
		var pa = abs((b & 0x000000FF) - (c & 0x000000FF));
		var pb = abs((a & 0x000000FF) - (c & 0x000000FF));
		var pc = abs((a & 0x000000FF) + (b & 0x000000FF) - ((c << 1) & 0x000001FE));
		var notACond = (((pb - pa) | (pc - pa)) >> 31) & 0x000000FF;
		var notBCond = ((pc - pb) >> 31) & 0x000000FF;
		
		pa = abs((b & 0x0000FF00) - (c & 0x0000FF00));
		pb = abs((a & 0x0000FF00) - (c & 0x0000FF00));
		pc = abs((a & 0x0000FF00) + (b & 0x0000FF00) - ((c << 1) & 0x0001FE00));
		notACond |= (((pb - pa) | (pc - pa)) >> 31) & 0x0000FF00;
		notBCond |= ((pc - pb) >> 31) & 0x0000FF00;
		
		pa = abs((b & 0x00FF0000) - (c & 0x00FF0000));
		pb = abs((a & 0x00FF0000) - (c & 0x00FF0000));
		pc = abs((a & 0x00FF0000) + (b & 0x00FF0000) - ((c << 1) & 0x01FE0000));
		notACond |= (((pb - pa) | (pc - pa)) >> 31) & 0x00FF0000;
		notBCond |= ((pc - pb) >> 31) & 0x00FF0000;
		
		pa = abs(((b >> 8) & 0x00FF0000) - ((c >> 8) & 0x00FF0000));
		pb = abs(((a >> 8) & 0x00FF0000) - ((c >> 8) & 0x00FF0000));
		pc = abs(((a >> 8) & 0x00FF0000) + ((b >> 8) & 0x00FF0000) - ((c >> 7) & 0x01FE0000));
		notACond |= (((pb - pa) | (pc - pa)) >> 31) & 0xFF000000;
		notBCond |= ((pc - pb) >> 31) & 0xFF000000;
		
		//return pa <= pb && pa <= pc ? a : (pb <= pc ? b : c);
		return (a & ~notACond) | (b & notACond & ~notBCond) | (c & notACond & notBCond);
	}
	
	private static inline function paethPredictor3Lo(a : Int, b : Int, c : Int)
	{
		var pa = abs((b & 0x000000FF) - (c & 0x000000FF));
		var pb = abs((a & 0x000000FF) - (c & 0x000000FF));
		var pc = abs((a & 0x000000FF) + (b & 0x000000FF) - ((c << 1) & 0x000001FE));
		var notACond = (((pb - pa) | (pc - pa)) >> 31) & 0x000000FF;
		var notBCond = ((pc - pb) >> 31) & 0x000000FF;
		
		pa = abs((b & 0x0000FF00) - (c & 0x0000FF00));
		pb = abs((a & 0x0000FF00) - (c & 0x0000FF00));
		pc = abs((a & 0x0000FF00) + (b & 0x0000FF00) - ((c << 1) & 0x0001FE00));
		notACond |= (((pb - pa) | (pc - pa)) >> 31) & 0x0000FF00;
		notBCond |= ((pc - pb) >> 31) & 0x0000FF00;
		
		pa = abs((b & 0x00FF0000) - (c & 0x00FF0000));
		pb = abs((a & 0x00FF0000) - (c & 0x00FF0000));
		pc = abs((a & 0x00FF0000) + (b & 0x00FF0000) - ((c << 1) & 0x01FE0000));
		notACond |= (((pb - pa) | (pc - pa)) >> 31) & 0x00FF0000;
		notBCond |= ((pc - pb) >> 31) & 0x00FF0000;
		
		//return pa <= pb && pa <= pc ? a : (pb <= pc ? b : c);
		return (a & ~notACond) | (b & notACond & ~notBCond) | (c & notACond & notBCond);
	}
	
	private static inline function abs(a)
	{
		// From http://graphics.stanford.edu/~seander/bithacks.html#IntegerAbs
		var mask = a >> 31;
		return (a + mask) ^ mask;
	}
	
	
	private static inline function writeIENDChunk(png : IDataOutput)
	{
		writeChunk(png, 0x49454E44, 0);
	}
	

	// Writes a PNG chunk into the output PNG
	private static inline function writeChunk(png : IDataOutput, type : Int, chunkLength : Int) : Void
	{
		var len = chunkLength;
		
		png.writeUnsignedInt(len);
		png.writeUnsignedInt(type);
		if (len != 0) {
			data.position = CHUNK_START;
			png.writeBytes(data, CHUNK_START, chunkLength);
		}
		
		// Calculate CRC-32 checksum of chunk
		var c = 0xFFFFFFFF;
		
		// Unroll first four iterations from type bytes, the rest use chunk data
		c = crcTable(c ^ (type >>> 24)) ^ (c >>> 8);
		c = crcTable(c ^ ((type >>> 16) & 0xFF)) ^ (c >>> 8);
		c = crcTable(c ^ ((type >>> 8) & 0xFF)) ^ (c >>> 8);
		c = crcTable(c ^ (type & 0xFF)) ^ (c >>> 8);
		
		if (len != 0) {
			var i = CHUNK_START;
			var end = CHUNK_START + len;
			var end16 = CHUNK_START + (len & 0xFFFFFFF0);	// Floor to nearest 16
			
			// Unroll 16 iterations at a time for speed
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
			
			// Do remaining iterations one-by-one
			while (i < end) {
				c = crcTable(c ^ Memory.getByte(i)) ^ (c >>> 8);
				++i;
			}
		}
		c ^= 0xFFFFFFFF;
		
		png.writeUnsignedInt(c);
	}
	
	
	// Whether the CRC table has been pre-computed yet
	private static var crcComputed = false;
	
	// Selects the domain memory, and performs any additional static initialization needed
	private static inline function initialize() : Void
	{
		if (!crcComputed) {
			region = new Rectangle(0, 0, 1, 1);
			sprite = new Sprite();
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
	
	// Looks up an entry in the CRC table given an index
	private static inline function crcTable(index : Int) : Int
	{
		return Memory.getI32((index & 0xFF) << 2);
	}
	
	public static inline function freeCachedMemory()
	{
		if (encoding) {
			throw new Error("Cached resources cannot be freed while an image is being encoded");
		}
		
		if (crcComputed) {
			if (ApplicationDomain.currentDomain.domainMemory == data) {
				Memory.select(null);
			}
			region = null;
			sprite = null;
			data = null;
			crcComputed = false;
		}
	}
}
