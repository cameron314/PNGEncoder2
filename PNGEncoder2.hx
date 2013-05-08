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
import flash.utils.IDataOutput;
import flash.utils.Timer;
import flash.Vector;
import DeflateStream;


// Separate public interface from private implementation because all
// members appear as public in SWC
class PNGEncoder2 extends EventDispatcher
{
	// For internal use only. Do not access.
	private var __impl : PNGEncoder2Impl;
	
	public static var level : CompressionLevel;
	
	// Provide both HaXe and AS3 properties to access the target FPS (read/write)
	@:protected private inline function getTargetFPS() return __impl.targetFPS
	@:protected private inline function setTargetFPS(fps : Int) return __impl.targetFPS = fps
	@:protected public var targetFPS(getTargetFPS, setTargetFPS) : Int;
	@:getter(targetFPS) private function flGetTargetFPS() return getTargetFPS()
	@:setter(targetFPS) private function flSetTargetFPS(fps : Int) setTargetFPS(fps)
	
	
	/**
	 * Creates a PNG image from the specified BitmapData.
	 * If the BitmapData's transparent property is true, then a 32-bit
	 * PNG (i.e. with alpha) is generated, otherwise a (generally samller)
	 * 24-bit PNG is generated.
	 * Highly optimized for speed.
	 *
	 * @param image The BitmapData that will be converted into the PNG format.
	 * @return a ByteArray representing the PNG encoded image data.
	 * @playerversion Flash 10
	 */
	public static function encode(image : BitmapData, outPng : IDataOutput) : Void
	{
		PNGEncoder2Impl.level = level;
		PNGEncoder2Impl.encode(image, outPng);
	}
	
	
	/**
	 * Creates a PNG image from the specified BitmapData without blocking.
	 * If the BitmapData's transparent property is true, then a 32-bit
	 * PNG (i.e. with alpha) is generated, otherwise a (generally samller)
	 * 24-bit PNG is generated.
	 * Highly optimized for speed.
	 *
	 * @param image The BitmapData that will be converted into the PNG format.
	 * @return a PNGEncoder2 object that dispatches COMPLETE and PROGRESS events.
	 * The encoder object allows the targetFPS to be set, and has a 'png'
	 * property to access the encoded data once the COMPLETE event has fired.
	 * @playerversion Flash 10
	 */
	public static function encodeAsync(image : BitmapData, outPng : IDataOutput) : PNGEncoder2
	{
		return new PNGEncoder2(image, outPng);
	}
	
	
	private inline function new(image : BitmapData, outPng : IDataOutput)
	{
		super();
		
		PNGEncoder2Impl.level = level;
		__impl = new PNGEncoder2Impl(image, outPng, this);
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
	private var timer : Timer;						// Used to trigger an update as often as possible
	
	private var frameCount : Int;					// Total number of frames that have elapsed so far during the encoding
	
	public static inline function encode(img : BitmapData, png : IDataOutput) : Void
	{
		// Save current domain memory and restore it after, to avoid
		// conflicts with other components using domain memory
		var oldFastMem = ApplicationDomain.currentDomain.domainMemory;
		
		beginEncoding(img, png);
		
		// Initialize stream for IDAT chunks
		var deflateStream = DeflateStream.createEx(level, DEFLATE_SCRATCH, CHUNK_START, true);
		
		writeIDATChunk(img, 0, img.height, deflateStream, png);
		
		endEncoding(png);
		
		deflateStream = null;		// Just in case this helps the garbage collector...
		
		Memory.select(oldFastMem);
	}
	
	private static inline function beginEncoding(img : BitmapData, png : IDataOutput)
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
	}
	
	
	private static inline function endEncoding(png : IDataOutput)
	{
		writeIENDChunk(png);
		
		encoding = false;
	}
	
	
	
	public inline function new(image : BitmapData, png : IDataOutput, dispatcher : IEventDispatcher)
	{
		_new(image, png, dispatcher);		// Constructors are slow -- delegate to function
	}
	
	private function _new(image : BitmapData, png : IDataOutput, dispatcher : IEventDispatcher)
	{
		fastNew(image, png, dispatcher);
	}
	
	private inline function fastNew(image : BitmapData, png : IDataOutput, dispatcher : IEventDispatcher)
	{
		img = image;
		this.png = png;
		this.dispatcher = dispatcher;
		
		if (encoding) {
			// Add to queue for later!
			pendingAsyncEncodings.push(this);
		}
		else {
			lastFrameStart = Lib.getTimer();
			
			// Preserve current domain memory so that we can leave it as we found
			// it once we've finished using it
			var oldFastMem = ApplicationDomain.currentDomain.domainMemory;
			
			beginEncoding(img, png);
			currentY = 0;
			frameCount = 0;
			done = false;
			msPerFrame = new Vector<Int>(FRAME_AVG_SMOOTH_COUNT, true);
			msPerFrameIndex = 0;
			msPerLine = new Vector<Float>(FRAME_AVG_SMOOTH_COUNT, true);
			msPerLineIndex = 0;
			updatesPerFrame = new Vector<Int>(FRAME_AVG_SMOOTH_COUNT, true);
			updatesPerFrameIndex = 0;
			targetFPS = 20;		// Default, can be overridden
			
			// Note that this effectively freezes the compression level for the
			// duration of the encoding (even if the static level member changes)
			deflateStream = DeflateStream.createEx(level, DEFLATE_SCRATCH, CHUNK_START, true);
			
			// Get notified of new frames, and timer events
			sprite.addEventListener(Event.ENTER_FRAME, onEnterFrame);
			timer = new Timer(1);		// Really means "update as often as possible"
			timer.addEventListener(TimerEvent.TIMER, onTimer);
			timer.start();
			
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
				var height = Math.ceil(Math.min(20 * 1024 / img.width, img.height));
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
			if (ms == 0) {
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
	}
	
	private function onTimer(e : TimerEvent)
	{
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
		// Need to check if we've finished or not since it's possible
		// for the timer event to be dispatched before we stop it, but get
		// processed after we've finished (e.g. if there's two timer events
		// in the event queue and the first one finishes the work, the second
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
				// Clear some references to give the garbage collector an easier time
				dispatcher = null;		// This removes a circular reference, which might save a mark-and-sweep step
				timer = null;
				img = null;
				deflateStream = null;
				msPerFrame = null;
				msPerLine = null;
				updatesPerFrame = null;
				
				if (!encoding && pendingAsyncEncodings.length > 0) {
					// Need to check `encoding` just in case someone started encoding another PNG
					// in the async COMPLETED event handler
					var next = pendingAsyncEncodings.shift();
					next._new(next.img, next.png, next.dispatcher);
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
			timer.removeEventListener(TimerEvent.TIMER, onTimer);
			timer.stop();
			
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
		
		// Size needed to store byte array of bitmap
		var scratchSize : UInt = width * bufferedHeight * 4;
		
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
		
		var imgBytes = img.getPixels(region);
		imgBytes.position = 0;
		memcpy(imgBytes, scratchAddr);
		
		if (bufferedStartY != startY) {
			scratchAddr += width * 4;
		}
		
		if (img.transparent) {
			if (bufferedStartY == startY) {
				// Do first line of image separately (no row above)
				Memory.setByte(addr, 1);		// Sub filter
				addr += 1;
				
				if (width > 0 && height > 0) {
					// Do first pixel (4 bytes) manually (sub formula is different)
					Memory.setI32(addr, Memory.getI32(scratchAddr) >>> 8);
					Memory.setByte(addr + 3, Memory.getByte(scratchAddr));
					addr += 4;
					scratchAddr += 4;
					
					// Copy line, moving alpha byte to end, and applying filter
					j = 1;
					while (j < end8) {
						Memory.setByte(addr    , Memory.getByte(scratchAddr + 1) - Memory.getByte(scratchAddr - 3));
						Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - Memory.getByte(scratchAddr - 2));
						Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - Memory.getByte(scratchAddr - 1));
						Memory.setByte(addr + 3, Memory.getByte(scratchAddr    ) - Memory.getByte(scratchAddr - 4));
						
						Memory.setByte(addr + 4, Memory.getByte(scratchAddr + 5) - Memory.getByte(scratchAddr + 1));
						Memory.setByte(addr + 5, Memory.getByte(scratchAddr + 6) - Memory.getByte(scratchAddr + 2));
						Memory.setByte(addr + 6, Memory.getByte(scratchAddr + 7) - Memory.getByte(scratchAddr + 3));
						Memory.setByte(addr + 7, Memory.getByte(scratchAddr + 4) - Memory.getByte(scratchAddr    ));
						
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
						Memory.setByte(addr    , Memory.getByte(scratchAddr + 1) - Memory.getByte(scratchAddr - 3));
						Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - Memory.getByte(scratchAddr - 2));
						Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - Memory.getByte(scratchAddr - 1));
						Memory.setByte(addr + 3, Memory.getByte(scratchAddr    ) - Memory.getByte(scratchAddr - 4));
						addr += 4;
						scratchAddr += 4;
						++j;
					}
				}
			}
			
			// Other lines:
			for (i in 1 ... bufferedHeight) {
				Memory.setByte(addr, 4);		// Paeth filter
				addr += 1;
				
				if (width > 0) {
					// Do first pixel (4 bytes) manually (formula is different)
					Memory.setByte(addr    , Memory.getByte(scratchAddr + 1) - Memory.getByte(scratchAddr + 1 - widthBy4));
					Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - Memory.getByte(scratchAddr + 2 - widthBy4));
					Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - Memory.getByte(scratchAddr + 3 - widthBy4));
					Memory.setByte(addr + 3, Memory.getByte(scratchAddr    ) - Memory.getByte(scratchAddr     - widthBy4));
					addr += 4;
					scratchAddr += 4;
				
					// Copy line, moving alpha byte to end, and applying filter
					j = 1;
					while (j < end8) {
						Memory.setByte(addr    , Memory.getByte(scratchAddr + 1) - paethPredictor(Memory.getByte(scratchAddr - 3), Memory.getByte(scratchAddr + 1 - widthBy4), Memory.getByte(scratchAddr - 3 - widthBy4)));
						Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - paethPredictor(Memory.getByte(scratchAddr - 2), Memory.getByte(scratchAddr + 2 - widthBy4), Memory.getByte(scratchAddr - 2 - widthBy4)));
						Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - paethPredictor(Memory.getByte(scratchAddr - 1), Memory.getByte(scratchAddr + 3 - widthBy4), Memory.getByte(scratchAddr - 1 - widthBy4)));
						Memory.setByte(addr + 3, Memory.getByte(scratchAddr    ) - paethPredictor(Memory.getByte(scratchAddr - 4), Memory.getByte(scratchAddr     - widthBy4), Memory.getByte(scratchAddr - 4 - widthBy4)));
						
						Memory.setByte(addr + 4, Memory.getByte(scratchAddr + 5) - paethPredictor(Memory.getByte(scratchAddr + 1), Memory.getByte(scratchAddr + 5 - widthBy4), Memory.getByte(scratchAddr + 1 - widthBy4)));
						Memory.setByte(addr + 5, Memory.getByte(scratchAddr + 6) - paethPredictor(Memory.getByte(scratchAddr + 2), Memory.getByte(scratchAddr + 6 - widthBy4), Memory.getByte(scratchAddr + 2 - widthBy4)));
						Memory.setByte(addr + 6, Memory.getByte(scratchAddr + 7) - paethPredictor(Memory.getByte(scratchAddr + 3), Memory.getByte(scratchAddr + 7 - widthBy4), Memory.getByte(scratchAddr + 3 - widthBy4)));
						Memory.setByte(addr + 7, Memory.getByte(scratchAddr + 4) - paethPredictor(Memory.getByte(scratchAddr    ), Memory.getByte(scratchAddr + 4 - widthBy4), Memory.getByte(scratchAddr     - widthBy4)));
						
						Memory.setByte(addr + 8,  Memory.getByte(scratchAddr + 9 ) - paethPredictor(Memory.getByte(scratchAddr + 5), Memory.getByte(scratchAddr + 9  - widthBy4), Memory.getByte(scratchAddr + 5 - widthBy4)));
						Memory.setByte(addr + 9,  Memory.getByte(scratchAddr + 10) - paethPredictor(Memory.getByte(scratchAddr + 6), Memory.getByte(scratchAddr + 10 - widthBy4), Memory.getByte(scratchAddr + 6 - widthBy4)));
						Memory.setByte(addr + 10, Memory.getByte(scratchAddr + 11) - paethPredictor(Memory.getByte(scratchAddr + 7), Memory.getByte(scratchAddr + 11 - widthBy4), Memory.getByte(scratchAddr + 7 - widthBy4)));
						Memory.setByte(addr + 11, Memory.getByte(scratchAddr + 8 ) - paethPredictor(Memory.getByte(scratchAddr + 4), Memory.getByte(scratchAddr + 8  - widthBy4), Memory.getByte(scratchAddr + 4 - widthBy4)));
						
						Memory.setByte(addr + 12, Memory.getByte(scratchAddr + 13) - paethPredictor(Memory.getByte(scratchAddr + 9 ), Memory.getByte(scratchAddr + 13 - widthBy4), Memory.getByte(scratchAddr + 9  - widthBy4)));
						Memory.setByte(addr + 13, Memory.getByte(scratchAddr + 14) - paethPredictor(Memory.getByte(scratchAddr + 10), Memory.getByte(scratchAddr + 14 - widthBy4), Memory.getByte(scratchAddr + 10 - widthBy4)));
						Memory.setByte(addr + 14, Memory.getByte(scratchAddr + 15) - paethPredictor(Memory.getByte(scratchAddr + 11), Memory.getByte(scratchAddr + 15 - widthBy4), Memory.getByte(scratchAddr + 11 - widthBy4)));
						Memory.setByte(addr + 15, Memory.getByte(scratchAddr + 12) - paethPredictor(Memory.getByte(scratchAddr + 8 ), Memory.getByte(scratchAddr + 12 - widthBy4), Memory.getByte(scratchAddr + 8  - widthBy4)));
						
						Memory.setByte(addr + 16, Memory.getByte(scratchAddr + 17) - paethPredictor(Memory.getByte(scratchAddr + 13), Memory.getByte(scratchAddr + 17 - widthBy4), Memory.getByte(scratchAddr + 13 - widthBy4)));
						Memory.setByte(addr + 17, Memory.getByte(scratchAddr + 18) - paethPredictor(Memory.getByte(scratchAddr + 14), Memory.getByte(scratchAddr + 18 - widthBy4), Memory.getByte(scratchAddr + 14 - widthBy4)));
						Memory.setByte(addr + 18, Memory.getByte(scratchAddr + 19) - paethPredictor(Memory.getByte(scratchAddr + 15), Memory.getByte(scratchAddr + 19 - widthBy4), Memory.getByte(scratchAddr + 15 - widthBy4)));
						Memory.setByte(addr + 19, Memory.getByte(scratchAddr + 16) - paethPredictor(Memory.getByte(scratchAddr + 12), Memory.getByte(scratchAddr + 16 - widthBy4), Memory.getByte(scratchAddr + 12 - widthBy4)));
						
						Memory.setByte(addr + 20, Memory.getByte(scratchAddr + 21) - paethPredictor(Memory.getByte(scratchAddr + 17), Memory.getByte(scratchAddr + 21 - widthBy4), Memory.getByte(scratchAddr + 17 - widthBy4)));
						Memory.setByte(addr + 21, Memory.getByte(scratchAddr + 22) - paethPredictor(Memory.getByte(scratchAddr + 18), Memory.getByte(scratchAddr + 22 - widthBy4), Memory.getByte(scratchAddr + 18 - widthBy4)));
						Memory.setByte(addr + 22, Memory.getByte(scratchAddr + 23) - paethPredictor(Memory.getByte(scratchAddr + 19), Memory.getByte(scratchAddr + 23 - widthBy4), Memory.getByte(scratchAddr + 19 - widthBy4)));
						Memory.setByte(addr + 23, Memory.getByte(scratchAddr + 20) - paethPredictor(Memory.getByte(scratchAddr + 16), Memory.getByte(scratchAddr + 20 - widthBy4), Memory.getByte(scratchAddr + 16 - widthBy4)));
						
						Memory.setByte(addr + 24, Memory.getByte(scratchAddr + 25) - paethPredictor(Memory.getByte(scratchAddr + 21), Memory.getByte(scratchAddr + 25 - widthBy4), Memory.getByte(scratchAddr + 21 - widthBy4)));
						Memory.setByte(addr + 25, Memory.getByte(scratchAddr + 26) - paethPredictor(Memory.getByte(scratchAddr + 22), Memory.getByte(scratchAddr + 26 - widthBy4), Memory.getByte(scratchAddr + 22 - widthBy4)));
						Memory.setByte(addr + 26, Memory.getByte(scratchAddr + 27) - paethPredictor(Memory.getByte(scratchAddr + 23), Memory.getByte(scratchAddr + 27 - widthBy4), Memory.getByte(scratchAddr + 23 - widthBy4)));
						Memory.setByte(addr + 27, Memory.getByte(scratchAddr + 24) - paethPredictor(Memory.getByte(scratchAddr + 20), Memory.getByte(scratchAddr + 24 - widthBy4), Memory.getByte(scratchAddr + 20 - widthBy4)));
						
						Memory.setByte(addr + 28, Memory.getByte(scratchAddr + 29) - paethPredictor(Memory.getByte(scratchAddr + 25), Memory.getByte(scratchAddr + 29 - widthBy4), Memory.getByte(scratchAddr + 25 - widthBy4)));
						Memory.setByte(addr + 29, Memory.getByte(scratchAddr + 30) - paethPredictor(Memory.getByte(scratchAddr + 26), Memory.getByte(scratchAddr + 30 - widthBy4), Memory.getByte(scratchAddr + 26 - widthBy4)));
						Memory.setByte(addr + 30, Memory.getByte(scratchAddr + 31) - paethPredictor(Memory.getByte(scratchAddr + 27), Memory.getByte(scratchAddr + 31 - widthBy4), Memory.getByte(scratchAddr + 27 - widthBy4)));
						Memory.setByte(addr + 31, Memory.getByte(scratchAddr + 28) - paethPredictor(Memory.getByte(scratchAddr + 24), Memory.getByte(scratchAddr + 28 - widthBy4), Memory.getByte(scratchAddr + 24 - widthBy4)));
						
						addr += 32;
						scratchAddr += 32;
						j += 8;
					}
					
					while (j < width) {
						Memory.setByte(addr    , Memory.getByte(scratchAddr + 1) - paethPredictor(Memory.getByte(scratchAddr - 3), Memory.getByte(scratchAddr + 1 - widthBy4), Memory.getByte(scratchAddr - 3 - widthBy4)));
						Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - paethPredictor(Memory.getByte(scratchAddr - 2), Memory.getByte(scratchAddr + 2 - widthBy4), Memory.getByte(scratchAddr - 2 - widthBy4)));
						Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - paethPredictor(Memory.getByte(scratchAddr - 1), Memory.getByte(scratchAddr + 3 - widthBy4), Memory.getByte(scratchAddr - 1 - widthBy4)));
						Memory.setByte(addr + 3, Memory.getByte(scratchAddr    ) - paethPredictor(Memory.getByte(scratchAddr - 4), Memory.getByte(scratchAddr     - widthBy4), Memory.getByte(scratchAddr - 4 - widthBy4)));
						
						addr += 4;
						scratchAddr += 4;
						++j;
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
						Memory.setByte(addr + 23, Memory.getByte(scratchAddr + 31) - Memory.getByte(scratchAddr + 27));
						
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
			
			// Other lines:
			for (i in 1 ... bufferedHeight) {
				Memory.setByte(addr, 4);		// Paeth filter
				addr += 1;
				
				if (width > 0) {
					// Do first pixel (3 bytes) manually (formula is different)
					Memory.setByte(addr    , Memory.getByte(scratchAddr + 1) - Memory.getByte(scratchAddr + 1 - widthBy4));
					Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - Memory.getByte(scratchAddr + 2 - widthBy4));
					Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - Memory.getByte(scratchAddr + 3 - widthBy4));
					addr += 3;
					scratchAddr += 4;
				
					// Copy line, applying filter
					j = 1;
					while (j < end8) {
						Memory.setByte(addr    , Memory.getByte(scratchAddr + 1) - paethPredictor(Memory.getByte(scratchAddr - 3), Memory.getByte(scratchAddr + 1 - widthBy4), Memory.getByte(scratchAddr - 3 - widthBy4)));
						Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - paethPredictor(Memory.getByte(scratchAddr - 2), Memory.getByte(scratchAddr + 2 - widthBy4), Memory.getByte(scratchAddr - 2 - widthBy4)));
						Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - paethPredictor(Memory.getByte(scratchAddr - 1), Memory.getByte(scratchAddr + 3 - widthBy4), Memory.getByte(scratchAddr - 1 - widthBy4)));
						
						Memory.setByte(addr + 3, Memory.getByte(scratchAddr + 5) - paethPredictor(Memory.getByte(scratchAddr + 1), Memory.getByte(scratchAddr + 5 - widthBy4), Memory.getByte(scratchAddr + 1 - widthBy4)));
						Memory.setByte(addr + 4, Memory.getByte(scratchAddr + 6) - paethPredictor(Memory.getByte(scratchAddr + 2), Memory.getByte(scratchAddr + 6 - widthBy4), Memory.getByte(scratchAddr + 2 - widthBy4)));
						Memory.setByte(addr + 5, Memory.getByte(scratchAddr + 7) - paethPredictor(Memory.getByte(scratchAddr + 3), Memory.getByte(scratchAddr + 7 - widthBy4), Memory.getByte(scratchAddr + 3 - widthBy4)));
						
						Memory.setByte(addr + 6,  Memory.getByte(scratchAddr + 9 ) - paethPredictor(Memory.getByte(scratchAddr + 5), Memory.getByte(scratchAddr + 9  - widthBy4), Memory.getByte(scratchAddr + 5 - widthBy4)));
						Memory.setByte(addr + 7,  Memory.getByte(scratchAddr + 10) - paethPredictor(Memory.getByte(scratchAddr + 6), Memory.getByte(scratchAddr + 10 - widthBy4), Memory.getByte(scratchAddr + 6 - widthBy4)));
						Memory.setByte(addr + 8, Memory.getByte(scratchAddr + 11) - paethPredictor(Memory.getByte(scratchAddr + 7), Memory.getByte(scratchAddr + 11 - widthBy4), Memory.getByte(scratchAddr + 7 - widthBy4)));
						
						Memory.setByte(addr + 9,  Memory.getByte(scratchAddr + 13) - paethPredictor(Memory.getByte(scratchAddr + 9 ), Memory.getByte(scratchAddr + 13 - widthBy4), Memory.getByte(scratchAddr + 9  - widthBy4)));
						Memory.setByte(addr + 10, Memory.getByte(scratchAddr + 14) - paethPredictor(Memory.getByte(scratchAddr + 10), Memory.getByte(scratchAddr + 14 - widthBy4), Memory.getByte(scratchAddr + 10 - widthBy4)));
						Memory.setByte(addr + 11, Memory.getByte(scratchAddr + 15) - paethPredictor(Memory.getByte(scratchAddr + 11), Memory.getByte(scratchAddr + 15 - widthBy4), Memory.getByte(scratchAddr + 11 - widthBy4)));
						
						Memory.setByte(addr + 12, Memory.getByte(scratchAddr + 17) - paethPredictor(Memory.getByte(scratchAddr + 13), Memory.getByte(scratchAddr + 17 - widthBy4), Memory.getByte(scratchAddr + 13 - widthBy4)));
						Memory.setByte(addr + 13, Memory.getByte(scratchAddr + 18) - paethPredictor(Memory.getByte(scratchAddr + 14), Memory.getByte(scratchAddr + 18 - widthBy4), Memory.getByte(scratchAddr + 14 - widthBy4)));
						Memory.setByte(addr + 14, Memory.getByte(scratchAddr + 19) - paethPredictor(Memory.getByte(scratchAddr + 15), Memory.getByte(scratchAddr + 19 - widthBy4), Memory.getByte(scratchAddr + 15 - widthBy4)));
						
						Memory.setByte(addr + 15, Memory.getByte(scratchAddr + 21) - paethPredictor(Memory.getByte(scratchAddr + 17), Memory.getByte(scratchAddr + 21 - widthBy4), Memory.getByte(scratchAddr + 17 - widthBy4)));
						Memory.setByte(addr + 16, Memory.getByte(scratchAddr + 22) - paethPredictor(Memory.getByte(scratchAddr + 18), Memory.getByte(scratchAddr + 22 - widthBy4), Memory.getByte(scratchAddr + 18 - widthBy4)));
						Memory.setByte(addr + 17, Memory.getByte(scratchAddr + 23) - paethPredictor(Memory.getByte(scratchAddr + 19), Memory.getByte(scratchAddr + 23 - widthBy4), Memory.getByte(scratchAddr + 19 - widthBy4)));
						
						Memory.setByte(addr + 18, Memory.getByte(scratchAddr + 25) - paethPredictor(Memory.getByte(scratchAddr + 21), Memory.getByte(scratchAddr + 25 - widthBy4), Memory.getByte(scratchAddr + 21 - widthBy4)));
						Memory.setByte(addr + 19, Memory.getByte(scratchAddr + 26) - paethPredictor(Memory.getByte(scratchAddr + 22), Memory.getByte(scratchAddr + 26 - widthBy4), Memory.getByte(scratchAddr + 22 - widthBy4)));
						Memory.setByte(addr + 20, Memory.getByte(scratchAddr + 27) - paethPredictor(Memory.getByte(scratchAddr + 23), Memory.getByte(scratchAddr + 27 - widthBy4), Memory.getByte(scratchAddr + 23 - widthBy4)));
						
						Memory.setByte(addr + 21, Memory.getByte(scratchAddr + 29) - paethPredictor(Memory.getByte(scratchAddr + 25), Memory.getByte(scratchAddr + 29 - widthBy4), Memory.getByte(scratchAddr + 25 - widthBy4)));
						Memory.setByte(addr + 22, Memory.getByte(scratchAddr + 30) - paethPredictor(Memory.getByte(scratchAddr + 26), Memory.getByte(scratchAddr + 30 - widthBy4), Memory.getByte(scratchAddr + 26 - widthBy4)));
						Memory.setByte(addr + 23, Memory.getByte(scratchAddr + 31) - paethPredictor(Memory.getByte(scratchAddr + 27), Memory.getByte(scratchAddr + 31 - widthBy4), Memory.getByte(scratchAddr + 27 - widthBy4)));
						
						addr += 24;
						scratchAddr += 32;
						j += 8;
					}
					
					while (j < width) {
						Memory.setByte(addr    , Memory.getByte(scratchAddr + 1) - paethPredictor(Memory.getByte(scratchAddr - 3), Memory.getByte(scratchAddr + 1 - widthBy4), Memory.getByte(scratchAddr - 3 - widthBy4)));
						Memory.setByte(addr + 1, Memory.getByte(scratchAddr + 2) - paethPredictor(Memory.getByte(scratchAddr - 2), Memory.getByte(scratchAddr + 2 - widthBy4), Memory.getByte(scratchAddr - 2 - widthBy4)));
						Memory.setByte(addr + 2, Memory.getByte(scratchAddr + 3) - paethPredictor(Memory.getByte(scratchAddr - 1), Memory.getByte(scratchAddr + 3 - widthBy4), Memory.getByte(scratchAddr - 1 - widthBy4)));
						
						addr += 3;
						scratchAddr += 4;
						++j;
					}
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
	
	
	private static inline function paethPredictor(a, b, c)
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
}

