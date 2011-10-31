/*
   Copyright (c) 2011, Cameron Desrochers
   All rights reserved.
   
   Redistribution and use in source and binary forms, with or without modification, are
   permitted provided that the following conditions are met:
   
      1. Redistributions of source code must retain the above copyright notice, this list of
         conditions and the following disclaimer.
      
      2. Redistributions in binary form must reproduce the above copyright notice, this list
         of conditions and the following disclaimer in the documentation and/or other materials
	     provided with the distribution.
   
   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
   OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
   AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
   WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Certain parts of this file are based on the zlib source.
As such, here is the zlib license in its entirety (from zlib.h):

  zlib.h -- interface of the 'zlib' general purpose compression library
  version 1.2.5, April 19th, 2010

  Copyright (C) 1995-2010 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu


  The data format used by the zlib library is described by RFCs (Request for
  Comments) 1950 to 1952 in the files http://www.ietf.org/rfc/rfc1950.txt
  (zlib format), rfc1951.txt (deflate format) and rfc1952.txt (gzip format).
*/




// References:
// - RFC 1950 (zlib) and 1951 (DEFLATE)
// - The standard zlib implementation (available from http://zlib.net/)
// - "An Explanation of the Deflate Algorithm" (http://www.zlib.net/feldspar.html)
// - "Length-Limitted Huffman Codes" (http://cbloomrants.blogspot.com/2010/07/07-02-10-length-limitted-huffman-codes.html)
// - "Length-Limitted Huffman Codes Heuristic" (http://cbloomrants.blogspot.com/2010/07/07-03-10-length-limitted-huffman-codes.html)
// - A C implementation of an in-place Huffman code length generator (http://ww2.cs.mu.oz.au/~alistair/inplace.c)
// - FastLZ source: http://fastlz.googlecode.com/svn/trunk/fastlz.c
// - Reverse Parallel algorithm for reversing bits (http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel)
// - Wikipedia article on canonical Huffman codes (http://en.wikipedia.org/wiki/Canonical_Huffman_code)
// - http://cstheory.stackexchange.com/questions/7420/relation-between-code-length-and-symbol-weight-in-a-huffman-code
// - MurmurHash3: http://code.google.com/p/smhasher/source/browse/trunk/MurmurHash3.cpp

package;

import flash.errors.ArgumentsError;
import flash.errors.Error;
import flash.Lib;
import flash.Memory;
import flash.system.ApplicationDomain;
import flash.utils.ByteArray;
import flash.utils.Endian;

enum CompressionLevel {
	UNCOMPRESSED;		// Fastest
	FAST;				// Huffman coding only
	NORMAL;				// Huffman + fast LZ77 compression
	GOOD;				// Huffman + good LZ77 compression
}


class MemoryRange {
	public var offset : Int;
	public var end : Int;
	
	public function new(offset : Int, end : Int)
	{
		this.offset = offset;
		this.end = end;
	}
	
	public inline function len() { return end - offset; }
}



// Compliant with RFC 1950 (ZLIB) and RFC 1951 (DEFLATE)
class DeflateStream
{
	private static inline var MAX_SYMBOLS_IN_TREE = 286;
	private static inline var LENGTH_CODES = 29;
	private static inline var MIN_LENGTH = 3;	// Minimum number of repeating symbols to use a length/distance pair
	private static inline var MAX_LENGTH = 258;
	private static inline var LENGTHS = 256;
	
	public static inline var SCRATCH_MEMORY_SIZE : Int = (32 + 19 + MAX_SYMBOLS_IN_TREE * 2 + LENGTHS + MIN_LENGTH + 512) * 4;		// Bytes
	
	private static inline var DISTANCE_OFFSET : Int = MAX_SYMBOLS_IN_TREE * 4;		// Where distance lookup is stored in scratch memory
	private static inline var CODE_LENGTH_OFFSET : Int = DISTANCE_OFFSET + 32 * 4;
	private static inline var HUFFMAN_SCRATCH_OFFSET : Int = CODE_LENGTH_OFFSET + 19 * 4;
	private static inline var LENGTH_EXTRA_BITS_OFFSET : Int = HUFFMAN_SCRATCH_OFFSET + MAX_SYMBOLS_IN_TREE * 4;
	private static inline var DIST_EXTRA_BITS_OFFSET : Int = LENGTH_EXTRA_BITS_OFFSET + (LENGTHS + MIN_LENGTH) * 4;
	// Next offset: DIST_EXTRA_BITS_OFFSET + 512 * 4
	
	private static inline var OUTPUT_BYTES_BEFORE_NEW_BLOCK : Int = 48 * 1024;
	private static inline var MAX_UNCOMPRESSED_BYTES_PER_BLOCK : UInt = 65535;
	private static inline var ADLER_MAX : Int = 65521;		// Largest prime smaller than 65536
	private static inline var MAX_CODE_LENGTH : Int = 15;
	private static inline var MAX_CODE_LENGTH_CODE_LENGTH : Int = 7;
	private static inline var CODE_LENGTH_ORDER = [ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 ];
	private static inline var EOB = 256;		// End of block symbol
	private static inline var HASH_SIZE_BITS = 13;
	private static inline var HASH_SIZE = 1 << HASH_SIZE_BITS;		// # of 4-byte slots
	private static inline var HASH_MASK = HASH_SIZE - 1;
	private static inline var WINDOW_SIZE = 32768;
	
	private var level : CompressionLevel;
	private var zlib : Bool;
	private var startAddr : UInt;
	private var currentAddr : Int;
	private var scratchAddr : Int;
	private var blockInProgress : Bool;
	private var blockStartAddr : Int;
	
	private var literalLengthCodes : Int;		// Count
	private var distanceCodes : Int;			// Count
	
	// For calculating Adler-32 sum
	private var s1 : UInt;
	private var s2 : UInt;
	
	private var bitOffset : Int;
	
	// TODO: Add compression ratio (keep track of bits written vs. bits seen)
	
	
	// Returns a new deflate stream (assumes no other code uses flash.Memory for
	// the duration of the lifetime of the stream). No manual memory management
	// is required.
	public static function create(level : CompressionLevel, writeZLIBInfo = false) : DeflateStream
	{
		var mem = new ByteArray();
		mem.length = ApplicationDomain.MIN_DOMAIN_MEMORY_LENGTH;
		Memory.select(mem);
		
		return createEx(level, 0, SCRATCH_MEMORY_SIZE, writeZLIBInfo);
	}
	
	// Returns a new deflate stream configured to use pre-selected memory (already
	// selected with flash.Memory). The size of the pre-selected memory will
	// automatically expand as needed to accomodate the stream as it grows.
	// Up to SCRATCH_MEMORY_SIZE bytes will be used at scratchAddr.
	// If scratchAddr is past startAddr, then it is the caller's reponsiblity to
	// ensure that there's enough room between startAddr and scratchAddr for all of
	// the compressed data (thus ensuring that no automatic expansion is needed) --
	// use maxOutputBufferSize() to calculate how much space is needed.
	public static function createEx(level : CompressionLevel, scratchAddr : Int, startAddr : Int, writeZLIBInfo = false) : DeflateStream
	{
		return new DeflateStream(level, writeZLIBInfo, scratchAddr, startAddr);
	}
	
	
	private function new(level : CompressionLevel, writeZLIBInfo : Bool, scratchAddr : Int, startAddr : Int)
	{
		_new(level, writeZLIBInfo, scratchAddr, startAddr);
	}
	
	
	private function _new(level : CompressionLevel, writeZLIBInfo : Bool, scratchAddr : Int, startAddr : Int)
	{
		fastNew(level, writeZLIBInfo, scratchAddr, startAddr);
	}
	
	private inline function fastNew(level : CompressionLevel, writeZLIBInfo : Bool, scratchAddr : Int, startAddr : Int)
	{
		this.level = level;
		this.zlib = writeZLIBInfo;
		this.scratchAddr = scratchAddr;
		this.startAddr = startAddr;
		this.currentAddr = startAddr;
		
		HuffmanTree.scratchAddr = scratchAddr + HUFFMAN_SCRATCH_OFFSET;
		
		// Ensure at least 10 bytes for possible zlib data, block header bits,
		// and final empty block.
		// writeBits requires up to 3 bytes past what is needed
		var mem = ApplicationDomain.currentDomain.domainMemory;
		var minLength : UInt = startAddr + 15;
		if (mem.length < minLength) {
			mem.length = minLength;
		}
		
		distanceCodes = -1;
		blockInProgress = false;
		blockStartAddr = currentAddr;
		bitOffset = 0;
		s1 = 1;
		s2 = 0;
		
		if (zlib) {
			writeByte(0x78);		// CMF with compression method 8 (deflate) 32K sliding window
			writeByte(0x9C);		// FLG: Check bits, no dict, default algorithm
		}
		
		setupStaticScratchMem();
		
		// writeBits relies on first byte being initiazed to 0
		Memory.setByte(currentAddr, 0);
	}
	
	
	// For best compression, write large chunks of data at once (there
	// is no internal buffer for performance reasons)
	public function write(bytes : ByteArray)
	{
		// Put input bytes into fast mem *after* a gap for output bytes.
		// This allows multiple calls to update without needing to pre-declare an input
		// buffer big enough (i.e. streaming).
		var offset = currentAddr + _maxOutputBufferSize(bytes.bytesAvailable);
		var end = offset + bytes.bytesAvailable;
		
		// Reserve space
		var mem = ApplicationDomain.currentDomain.domainMemory;
		var uend : UInt = end;
		if (mem.length < uend) {
			mem.length = end;
		}
		
		memcpy(bytes, offset);
		return fastWrite(offset, end);
	}
	
	
	// Updates the stream with a compressed representation of bytes
	// between the from and to indexes (of selected memory).
	// For best compression, write large chunks of data at once (there
	// is no internal buffering for performance reasons)
	public function fastWrite(offset : Int, end : Int)
	{
		_fastWrite(offset, end);
	}
	
	
	private inline function _fastWrite(offset : Int, end : Int)
	{
		if (level == UNCOMPRESSED) {
			_fastWriteUncompressed(offset, end);
		}
		else {
			_fastWriteCompressed(offset, end);
		}
	}
	
	
	// Returns a memory range for the currently written data in the output buffer.
	// Guaranteed to always start at the original startAddr passed to createEx
	public function peek() : MemoryRange
	{
		return new MemoryRange(startAddr, currentAddr);
	}
	
	
	// Releases all written bytes from the output buffer.
	// Use peek() to read the bytes before releasing them.
	public function release() : Void
	{
		if (bitOffset > 0) {
			// Copy in-progress byte to start
			Memory.setByte(startAddr, Memory.getByte(currentAddr));
		}
		else {
			Memory.setByte(startAddr, 0);
		}
		
		// Push back block start so that the distance between currentAddr
		// and blockStartAddr remains consistent (it is only used for counting
		// the number of bytes in the current block, so an invalid addr is OK)
		blockStartAddr = startAddr - (currentAddr - blockStartAddr);
		
		currentAddr = startAddr;
	}
	
	
	// Pre-condition: blockInProgress should always be false
	private inline function _fastWriteUncompressed(offset : Int, end : Int)
	{
		if (zlib) {
			updateAdler32(offset, end);
		}
		
		// Ensure room to start the blocks and write all data
		var blockOverhead = 8;		// 1B block header + 4B header + 3B max needed by writeBits
		var totalLen : Int = end - offset;
		var blocks = Math.ceil(totalLen / MAX_UNCOMPRESSED_BYTES_PER_BLOCK);
		var minimumSize : UInt = totalLen + blockOverhead * blocks;
		var mem = ApplicationDomain.currentDomain.domainMemory;
		var freeSpace : UInt = mem.length - currentAddr;
		if (freeSpace < minimumSize) {
			mem.length = currentAddr + minimumSize;
		}
		
		while (end - offset > 0) {
			var len = Std.int(Math.min(end - offset, MAX_UNCOMPRESSED_BYTES_PER_BLOCK));
			
			beginBlock();
			
			// Write uncompressed header info
			writeShort(len);
			writeShort(~len);
			
			// Write data (loop unrolled for speed)
			var cappedEnd = offset + len;
			var cappedEnd32 = offset + (len & 0xFFFFFFE0);		// Floor to nearest 32
			var i = offset;
			while (i < cappedEnd32) {
				Memory.setI32(currentAddr, Memory.getI32(i));
				Memory.setI32(currentAddr + 4, Memory.getI32(i + 4));
				Memory.setI32(currentAddr + 8, Memory.getI32(i + 8));
				Memory.setI32(currentAddr + 12, Memory.getI32(i + 12));
				Memory.setI32(currentAddr + 16, Memory.getI32(i + 16));
				Memory.setI32(currentAddr + 20, Memory.getI32(i + 20));
				Memory.setI32(currentAddr + 24, Memory.getI32(i + 24));
				Memory.setI32(currentAddr + 28, Memory.getI32(i + 28));
				currentAddr += 32;
				i += 32;
			}
			while (i < cappedEnd) {
				Memory.setByte(currentAddr, Memory.getByte(i));
				++currentAddr;
				++i;
			}
			
			endBlock();
			
			offset += len;
		}
	}
	
	
	private inline function _fastWriteCompressed(offset : Int, end : Int)
	{
		var len = end - offset;
		
		// Make sure there's enough room in the output
		var mem = ApplicationDomain.currentDomain.domainMemory;
		if (_maxOutputBufferSize(len) > mem.length - currentAddr) {
			mem.length = _maxOutputBufferSize(len) + currentAddr;
		}
		
		if (zlib) {
			updateAdler32(offset, end);
		}
		
		if (level == FAST) {
			_fastWriteFast(offset, end);
		}
		else if (level == NORMAL) {
			_fastWriteNormal(offset, end);
		}
		else if (level == GOOD) {
			_fastWriteGood(offset, end);
		}
		else {
			throw new Error("Compression level not supported");
		}
	}
	
	
	private inline function _fastWriteFast(offset : Int, end : Int)
	{
		//var startTime = Lib.getTimer();
		
		// Write data in bytesBeforeBlockCheck byte increments -- this allows
		// us to efficiently check the current output block size only periodically
		
		var bytesBeforeBlockCheck = 2048;
		var i = offset;
		var endCheck;
		while (end - offset > bytesBeforeBlockCheck) {
			endCheck = offset + bytesBeforeBlockCheck;
			
			if (!blockInProgress) {
				beginBlock();
				
				// Write Huffman trees into the stream as per RFC 1951
				// Estimate bytes (assume ~50% compression ratio)
				createAndWriteHuffmanTrees(offset, Std.int(Math.min(end, offset + OUTPUT_BYTES_BEFORE_NEW_BLOCK * 2)));
			}
			
			while (i < endCheck) {
				// Write data (loop unrolled for speed)
				writeSymbol(Memory.getByte(i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				writeSymbol(Memory.getByte(++i));
				++i;
			}
			
			offset += bytesBeforeBlockCheck;
			
			if (currentBlockLength() > OUTPUT_BYTES_BEFORE_NEW_BLOCK) {
				endBlock();
			}
		}
		
		if (i < end) {
			if (!blockInProgress) {
				beginBlock();
				
				createAndWriteHuffmanTrees(offset, end);
			}
			
			while (i < end) {
				writeSymbol(Memory.getByte(i));
				++i;
			}
			
			if (currentBlockLength() > OUTPUT_BYTES_BEFORE_NEW_BLOCK) {
				endBlock();
			}
		}
		
		//var endTime = Lib.getTimer();
		//trace("Writing Huffman-encoded data took " + (endTime - startTime) + "ms");
	}
	
	
	private inline function _fastWriteNormal(offset : Int, end : Int)
	{
		var cappedEnd, safeEnd;
		
		var symbol;
		var length;
		var lengthInfo;
		var distance;
		var distanceInfo;
		var hashOffset;
		var i, j, k;
		
		// blockInProgress should always be false here
		
		
		// Instead of writing symbols directly to output stream, write them
		// to a temporary buffer instead. This lets us compute exact symbol
		// likelihood statistics for creating the Huffman trees, which are
		// then used to encode the buffer into the final output stream.
		// Symbols in the temporary buffer are all two-bytes, and can be literals
		// or lengths. Lengths are represented by 512 + the actual length value.
		// Lengths are immediately followed by a two-byte distance (actual value).
		
		var hashAddr = currentAddr + _maxOutputBufferSize(end - offset) - HASH_SIZE * 4;
		var bufferAddr = hashAddr - OUTPUT_BYTES_BEFORE_NEW_BLOCK * 2 * 2;
		var currentBufferAddr;
		
		// Initialize hash table to invalid data (HASH_SIZE is divisble by 8)
		i = hashAddr + HASH_SIZE * 4 - 32;
		while (i >= hashAddr) {
			Memory.setI32(i, -1);
			Memory.setI32(i + 4, -1);
			Memory.setI32(i + 8, -1);
			Memory.setI32(i + 12, -1);
			Memory.setI32(i + 16, -1);
			Memory.setI32(i + 20, -1);
			Memory.setI32(i + 24, -1);
			Memory.setI32(i + 28, -1);
			i -= 32;
		}
		
		while (end - offset > 0) {
			// Assume ~50% compression ratio
			cappedEnd = Std.int(Math.min(end, offset + OUTPUT_BYTES_BEFORE_NEW_BLOCK * 2));
			safeEnd = cappedEnd - 4;		// Can read up to 4 ahead without worrying
			
			
			// Phase 1: Use LZ77 compression to determine literals, lengths, and distances to
			// later be encoded. Put these in a temporary output buffer, and track the frequency
			// of each symbol
			
			clearSymbolFrequencies();
			currentBufferAddr = bufferAddr;
			
			i = offset;
			
			while (i < safeEnd) {
				hashOffset = LZHash.hash(i, HASH_MASK);
				j = Memory.getI32(hashAddr + hashOffset);
				
				if (j >= 0 && Memory.getI32(j) == Memory.getI32(i)) {
					length = 4;
					j += 4;
					k = i + 4;
					while (k < cappedEnd && Memory.getByte(j) == Memory.getByte(k) && length < MAX_LENGTH) {
						++j;
						++k;
						++length;
					}
					
					// Update hash before incrementing
					Memory.setI32(hashAddr + hashOffset, i);
					
					distance = k - j;
					if (distance <= WINDOW_SIZE) {
						incSymbolFrequency(Memory.getUI16(scratchAddr + LENGTH_EXTRA_BITS_OFFSET + (length << 2) + 2));
						
						distanceInfo = getDistanceInfo(distance);
						incSymbolFrequency(distanceInfo >>> 24, DISTANCE_OFFSET);
						
						Memory.setI32(currentBufferAddr, (length | 512) | (distance << 16));
						currentBufferAddr += 4;
						
						i += length;
						
						if (i < safeEnd) {
							// Update hash after
							Memory.setI32(hashAddr + LZHash.hash(i - 1, HASH_MASK), i - 1);
						}
					}
					else {
						symbol = Memory.getByte(i);
						Memory.setI16(currentBufferAddr, symbol);
						incSymbolFrequency(symbol);
						
						currentBufferAddr += 2;
						++i;
					}
				}
				else {
					// No luck with hash table. Output literal:
					symbol = Memory.getByte(i);
					Memory.setI16(currentBufferAddr, symbol);
					incSymbolFrequency(symbol);
					
					Memory.setI32(hashAddr + hashOffset, i);
					
					currentBufferAddr += 2;
					++i;
				}
			}
			
			while (i < cappedEnd) {
				symbol = Memory.getByte(i);
				Memory.setI16(currentBufferAddr, symbol);
				incSymbolFrequency(symbol);
				currentBufferAddr += 2;
				++i;
			}
			
			
			// Phase 2: Encode buffered data to output stream
			
			beginBlock();
			createAndWriteHuffmanTrees(offset, cappedEnd);
			
			i = bufferAddr;
			while (i + 64 <= currentBufferAddr) {	// Up to 16 length/distance pairs, or 32 literals
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
			}
			while (i < currentBufferAddr) {
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
			}
			
			endBlock();
			
			
			offset = cappedEnd;
		}
	}
	
	
	private inline function _fastWriteGood(offset : Int, end : Int)
	{
		var cappedEnd, safeEnd;
		
		var symbol;
		var length, lengthInfo;
		var distanceInfo;
		var hashOffset;
		var i, j, k;
		
		// Works just like _fastWriteNormal, but uses a different hash (LZHash)
		
		var hashAddr = currentAddr + _maxOutputBufferSize(end - offset) - LZHash.MEMORY_SIZE;
		var bufferAddr = hashAddr - OUTPUT_BYTES_BEFORE_NEW_BLOCK * 2 * 2;
		var currentBufferAddr;
		
		var hash = new LZHash(hashAddr, MAX_LENGTH, WINDOW_SIZE);
		if (offset < end - LZHash.MAX_LOOKAHEAD) {
			// Note that if this if is not entered, we cannot call the
			// searchAndUpdate method, but we won't since the first inner loop
			// will never be entered
			hash.initLookahead(offset, end);
		}
		
		while (end - offset > 0) {
			// Assume ~50% compression ratio
			cappedEnd = Std.int(Math.min(end, offset + OUTPUT_BYTES_BEFORE_NEW_BLOCK * 2));
			safeEnd = cappedEnd - LZHash.MAX_LOOKAHEAD;
			
			
			// Phase 1: Use LZ77 compression to determine literals, lengths, and distances to
			// later be encoded. Put these in a temporary output buffer, and track the frequency
			// of each symbol
			
			clearSymbolFrequencies();
			currentBufferAddr = bufferAddr;
			
			i = offset;
			
			while (i < safeEnd) {
				hash.searchAndUpdate(i, cappedEnd);
				
				if (Memory.getUI16(hash.resultAddr) >= LZHash.MIN_MATCH_LENGTH) {
					length = Memory.getUI16(hash.resultAddr);
					incSymbolFrequency(Memory.getUI16(scratchAddr + LENGTH_EXTRA_BITS_OFFSET + (length << 2) + 2));
					
					distanceInfo = getDistanceInfo(Memory.getUI16(hash.resultAddr + 2));
					incSymbolFrequency(distanceInfo >>> 24, DISTANCE_OFFSET);
					
					Memory.setI32(currentBufferAddr, Memory.getI32(hash.resultAddr) | 512);
					currentBufferAddr += 4;
					
					i += length;
				}
				else {
					// No luck with hash table. Output literal:
					symbol = Memory.getByte(i);
					Memory.setI16(currentBufferAddr, symbol);
					incSymbolFrequency(symbol);
					
					currentBufferAddr += 2;
					++i;
				}
			}
			
			while (i < cappedEnd) {
				symbol = Memory.getByte(i);
				Memory.setI16(currentBufferAddr, symbol);
				incSymbolFrequency(symbol);
				currentBufferAddr += 2;
				++i;
			}
			
			
			// Phase 2: Encode buffered data to output stream
			
			beginBlock();
			createAndWriteHuffmanTrees(offset, cappedEnd);
			
			i = bufferAddr;
			while (i + 64 <= currentBufferAddr) {	// Up to 16 length/distance pairs, or 32 literals
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
			}
			while (i < currentBufferAddr) {
				symbol = writeTemporaryBufferSymbol(i);
				i += 2 + ((symbol & 512) >>> 8);
			}
			
			endBlock();
			
			
			offset = cappedEnd;
		}
	}
	
	
	private inline function writeTemporaryBufferSymbol(i : Int) : Int
	{
		var length, distance, lengthInfo, distanceInfo;
		var symbol = Memory.getUI16(i);
		if ((symbol & 512) != 0) {
			// Length/distance pair
			length = symbol ^ 512;
			lengthInfo = Memory.getI32(scratchAddr + LENGTH_EXTRA_BITS_OFFSET + (length << 2));
			writeSymbol(lengthInfo >>> 16);
			writeBits(length - (lengthInfo & 0x1FFF), (lengthInfo & 0xFF00) >>> 13);
			
			distance = Memory.getUI16(i + 2);
			distanceInfo = getDistanceInfo(distance);
			writeSymbol(distanceInfo >>> 24, DISTANCE_OFFSET);
			writeBits(distance - (distanceInfo & 0xFFFF), (distanceInfo & 0xFF0000) >>> 16);
		}
		else {
			// Literal
			writeSymbol(symbol);
		}
		
		return symbol;
	}
	
	
	private inline function beginBlock(lastBlock = false)
	{
		blockInProgress = true;
		
		if (level == CompressionLevel.UNCOMPRESSED) {
			if (bitOffset == 0) {
				// Current output is aligned to byte, indicating
				// last write may not have been made using writeBits;
				// writeBits requires that next byte be zero
				Memory.setByte(currentAddr, 0);
			}
			
			// Write BFINAL bit and BTYPE (00)
			writeBits(lastBlock ? 1 : 0, 3);		// Uncompressed
			
			// Align to byte boundary
			if (bitOffset > 0) {
				writeBits(0, 8 - bitOffset);
			}
		}
		else {
			writeBits(4 | (lastBlock ? 1 : 0), 3);		// Dynamic Huffman tree compression
		}
		
		blockStartAddr = currentAddr;
	}
	
	
	// Call only once. After called, no other methods should be called
	public function finalize() : ByteArray
	{
		var range = fastFinalize();
		
		var result = new ByteArray();
		if (zlib) {
			result.endian = BIG_ENDIAN;		// Network byte order (RFC 1950)
		}
		else {
			result.endian = LITTLE_ENDIAN;
		}
		
		var mem = ApplicationDomain.currentDomain.domainMemory;
		mem.position = range.offset;
		mem.readBytes(result, 0, range.len());
		
		result.position = 0;
		return result;
	}
	
	
	// Call only once. After called, no other methods should be called
	public function fastFinalize() : MemoryRange
	{
		if (blockInProgress) {
			endBlock();
		}
		
		// It's easier to always write one empty block at the end than to force
		// the caller to know in advance which block is the last one.
		writeEmptyBlock(true);
		
		// Align to byte boundary
		if (bitOffset > 0) {
			++currentAddr;		// active byte is now last byte
		}
		
		if (zlib) {
			// Write Adler-32 sum in big endian order
			// adlerSum = (s2 << 16) | s1;
			writeByte(s2 >>> 8);
			writeByte(s2);
			writeByte(s1 >>> 8);
			writeByte(s1);
		}
		
		return new MemoryRange(startAddr, currentAddr);
	}
	
	
	// Does not include SCRATCH_MEMORY_SIZE
	// Note that if the data is written in very small chunks
	// then the maximum output size may be exceeded (because
	// no buffering is done internally, a large number of blocks
	// may be created, causing enough overhead to overflow the max)
	public function maxOutputBufferSize(inputByteCount : Int)
	{
		return _maxOutputBufferSize(inputByteCount);
	}
	
	private inline function _maxOutputBufferSize(inputByteCount : Int)
	{
		var blockCount;
		var blockOverhead;
		var multiplier = 1;
		var extraScratch = 0;
		
		if (level == UNCOMPRESSED) {
			blockOverhead = 8;		// 1B block header + 4B header + 3B max needed by writeBits
			blockCount = Math.ceil(inputByteCount / MAX_UNCOMPRESSED_BYTES_PER_BLOCK);
		}
		else {
			if (level == FAST) {
				// Worst case:
				blockCount = Math.ceil(inputByteCount * 2 / OUTPUT_BYTES_BEFORE_NEW_BLOCK);
			}
			else {
				blockCount = Math.ceil(inputByteCount / (OUTPUT_BYTES_BEFORE_NEW_BLOCK * 2));
				
				if (level == NORMAL) {
					extraScratch = HASH_SIZE * 4 + OUTPUT_BYTES_BEFORE_NEW_BLOCK * 2 * 2;
				}
				else if (level == GOOD) {
					extraScratch = LZHash.MEMORY_SIZE + OUTPUT_BYTES_BEFORE_NEW_BLOCK * 2 * 2;
				}
			}
			
			// Using Huffman compression with max 15 bits can't possibly
			// exceed twice the uncompressed length. Margin of 300 includes
			// header/footer data (think 285 length/literal codes at 7 bits max each),
			// rounded up for good luck.
			multiplier = 2;
			blockOverhead = 300;
		}
		
		// Include extra block to account for the one written during finalize()
		return inputByteCount * multiplier + blockOverhead * (blockCount + 1) + extraScratch;
	}
	
	
	private inline function endBlock()
	{
		if (level != UNCOMPRESSED) {
			writeSymbol(EOB);
		}
		
		blockInProgress = false;
	}
	
	
	private inline function currentBlockLength()
	{
		return blockInProgress ? currentAddr - blockStartAddr : 0;
	}
	
	
	private inline function writeEmptyBlock(lastBlock : Bool)
	{
		if (blockInProgress) {
			endBlock();
		}
		
		var currentLevel = level;
		level = UNCOMPRESSED;
		beginBlock(lastBlock);
		writeShort(0);
		writeShort(~0);
		endBlock();
		level = currentLevel;
	}
	
	
	private inline function setupStaticScratchMem()
	{
		if (level == NORMAL || level == GOOD) {
			// Write length code, extra bits and lower bound that must be subtracted to
			// get the extra bits value for all the possible lengths.
			// This information is stuffed into 4 bytes per length, addressable
			// by Memory.getI32(scratchAddr + LENGTH_EXTRA_BITS_OFFSET + length * 4).
			// Upper 2 bytes: Length code
			// Lower 2 bytes: The upper 3 bits are the extra bit count, and the lower 13 are
			// the lower bound.
			
			var offset = scratchAddr + LENGTH_EXTRA_BITS_OFFSET;
			
			// 3 ... 10:
			Memory.setI32(offset + 3 * 4, (257 << 16) | 3);
			Memory.setI32(offset + 4 * 4, (258 << 16) | 4);
			Memory.setI32(offset + 5 * 4, (259 << 16) | 5);
			Memory.setI32(offset + 6 * 4, (260 << 16) | 6);
			Memory.setI32(offset + 7 * 4, (261 << 16) | 7);
			Memory.setI32(offset + 8 * 4, (262 << 16) | 8);
			Memory.setI32(offset + 9 * 4, (263 << 16) | 9);
			Memory.setI32(offset + 10 * 4, (264 << 16) | 10);
			
			// 11 ... 258 (258 is special-cased right after):
			var base = 11;
			var symbol = 265;
			for (extraBits in 1 ... 6) {
				for (_ in 0 ... 4) {
					//Lib.trace("Lengths " + base + "-" + (base + (1 << extraBits) - 1) + ": code: " + symbol + "; extra bits: " + extraBits);
					for (i in base ... base + (1 << extraBits)) {
						Memory.setI32(offset + i * 4, (symbol << 16) | (extraBits << 13) | base);
					}
					base += 1 << extraBits;
					++symbol;
				}
			}
			
			Memory.setI32(offset + 258 * 4, (285 << 16) | 258);		// 258
			
			
			
			
			
			// Write distance code, extra bits and lower bound that must be subtracted to
			// get the extra bits value for all the possible distances.
			// This information is stuffed into 4 bytes per length, accessible
			// by getDistanceInfo(distance).
			// Uppermost byte: Distance code
			// Next byte: Extra bit count
			// Lower 2 bytes: The lower bound
			
			offset = scratchAddr + DIST_EXTRA_BITS_OFFSET;
			
			
			// 1-4:
			Memory.setI32(offset + 1 * 4, (0 << 24) | 1);
			Memory.setI32(offset + 2 * 4, (1 << 24) | 2);
			Memory.setI32(offset + 3 * 4, (2 << 24) | 3);
			Memory.setI32(offset + 4 * 4, (3 << 24) | 4);
			
			// 5-256
			base = 5;
			symbol = 4;
			for (extraBits in 1 ... 7) {
				for (_ in 0 ... 2) {
					for (i in base ... base + (1 << extraBits)) {
						Memory.setI32(offset + i * 4, (symbol << 24) | (extraBits << 16) | base);
					}
					base += 1 << extraBits;
					++symbol;
				}
			}
			
			offset += 256 * 4;
			
			// 257-32768 (in chunks of 128 distances)
			for (extraBits in 7 ... 14) {
				for (_ in 0 ... 2) {
					for (i in base >>> 7 ... (base >>> 7) + (1 << (extraBits - 7))) {
						Memory.setI32(offset + i * 4, (symbol << 24) | (extraBits << 16) | base);
					}
					base += 1 << extraBits;
					++symbol;
				}
			}
		}
	}
	
	
	private inline function getDistanceInfo(distance : Int)
	{
		return Memory.getI32(scratchAddr + DIST_EXTRA_BITS_OFFSET +
			((distance <= 256 ? distance : 256 + ((distance - 1) >>> 7)) << 2)
		);
	}
	
	
	private function createAndWriteHuffmanTrees(offset : Int, end : Int)
	{
		_createAndWriteHuffmanTrees(offset, end);
	}
	
	private inline function _createAndWriteHuffmanTrees(offset : Int, end : Int)
	{
		literalLengthCodes = createLiteralLengthTree(offset, end);
		distanceCodes = createDistanceTree(offset, end);
		
		var codeLengthCodes = createCodeLengthTree(literalLengthCodes, distanceCodes);
		
		
		// HLIT
		writeBits(literalLengthCodes - 257, 5);
		
		// HDIST
		if (distanceCodes == 0) {
			writeBits(0, 5);		// Minimum one distance code
		}
		else {
			writeBits(distanceCodes - 1, 5);
		}
		
		// HCLEN
		writeBits(codeLengthCodes - 4, 4);
		
		// Write code lengths of code length code
		for (rank in CODE_LENGTH_ORDER) {
			writeBits(Memory.getUI16(scratchAddr + CODE_LENGTH_OFFSET + rank * 4), 3);
		}
		
		// Write (compressed) code lengths of literal/length codes
		for (i in 0 ... literalLengthCodes) {
			writeSymbol(Memory.getUI16(scratchAddr + i * 4), CODE_LENGTH_OFFSET);
		}
		
		// Write (compressed) code lengths of distance codes
		if (distanceCodes == 0) {
			writeSymbol(0, CODE_LENGTH_OFFSET);
		}
		else {
			for (i in 0 ... distanceCodes) {
				writeSymbol(Memory.getUI16(scratchAddr + DISTANCE_OFFSET + i * 4), CODE_LENGTH_OFFSET);
			}
		}
	}
	
	
	// Writes up to 25 bits into the stream (bits must be zero-padded to 32 bits)
	private inline function writeBits(bits : Int, bitCount : Int)
	{
		var current = Memory.getByte(currentAddr);
		current |= bits << bitOffset;
		Memory.setI32(currentAddr, current);
		bitOffset += bitCount;
		currentAddr += bitOffset >>> 3; 	// divided by 8
		bitOffset &= 0x7;		// modulus 8
	}
	
	private inline function writeByte(byte : Int)
	{
		Memory.setByte(currentAddr, byte);
		++currentAddr;
	}
	
	private inline function writeShort(num : Int)
	{
		Memory.setI16(currentAddr, num);
		currentAddr += 2;
	}
	
	private inline function writeSymbol(symbol : Int, scratchOffset = 0)
	{
		// For codes, get codelength. All symbols are <= 2 bytes
		var compressed = Memory.getI32(scratchAddr + scratchOffset + symbol * 4);
		writeBits(compressed >>> 16, compressed & 0xFFFF);
	}
	
	
	// From zlib's adler32.c:
	private static inline var NMAX = 5552;	// NMAX is the largest n such that 255n(n+1)/2 + (n+1)(ADDLER_MAX-1) <= 2^32-1
	
	private inline function updateAdler32(offset : Int, end : Int)
	{
		// Adapted directly from zlib's adler32.c implementation. Most of the
		// optimization tricks are taken straight from there.
		
		while (offset + NMAX <= end) {
			// NMAX is evenly divisible by 16
			do16Adler(offset, offset + NMAX);
			
			s1 %= ADLER_MAX;
			s2 %= ADLER_MAX;
			
			offset += NMAX;
		}
		
		if (offset != end) {
			for (i in offset ... end) {
				s1 += Memory.getByte(i);
				s2 += s1;
			}
			
			s1 %= ADLER_MAX;
			s2 %= ADLER_MAX;
		}
	}
	
	private inline function do16Adler(i, end)
	{
		// (end - i) must be divisible by 16
		
		while (i < end) {
			// Use math to calculate s1 and s2 updates in one batch.
			// This turns out to be slightly faster than straight-up unrolling.
			s2 += (s1 << 4) + // * 16
				Memory.getByte(i    ) * 16 +
				Memory.getByte(i + 1) * 15 +
				Memory.getByte(i + 2) * 14 +
				Memory.getByte(i + 3) * 13 +
				Memory.getByte(i + 4) * 12 +
				Memory.getByte(i + 5) * 11 +
				Memory.getByte(i + 6) * 10 +
				Memory.getByte(i + 7) * 9 +
				Memory.getByte(i + 8) * 8 +
				Memory.getByte(i + 9) * 7 +
				Memory.getByte(i + 10) * 6 +
				Memory.getByte(i + 11) * 5 +
				Memory.getByte(i + 12) * 4 +
				Memory.getByte(i + 13) * 3 +
				Memory.getByte(i + 14) * 2 +
				Memory.getByte(i + 15);
			s1 += Memory.getByte(i) +
				Memory.getByte(i + 1) +
				Memory.getByte(i + 2) +
				Memory.getByte(i + 3) +
				Memory.getByte(i + 4) +
				Memory.getByte(i + 5) +
				Memory.getByte(i + 6) +
				Memory.getByte(i + 7) +
				Memory.getByte(i + 8) +
				Memory.getByte(i + 9) +
				Memory.getByte(i + 10) +
				Memory.getByte(i + 11) +
				Memory.getByte(i + 12) +
				Memory.getByte(i + 13) +
				Memory.getByte(i + 14) +
				Memory.getByte(i + 15);
			i += 16;
		}
	}
	
	
	private inline function createLiteralLengthTree(offset : Int, end : Int)
	{
		var n = 0;
		
		if (level == FAST) {
			n = 257;		// 256 literals plus EOB
			
			// Literals
			for (value in 0 ... 256) {		// Literals
				Memory.setI32(scratchAddr + value * 4, 10);
			}
			
			// Weight EOB less than more common literals
			Memory.setI32(scratchAddr + EOB * 4, 1);
			
			// Sample given bytes to estimate literal weights
			var len = end - offset;
			var sampleFrequency;		// Sample every nth byte
			if (len <= 16 * 1024) {
				// Small sample, calculate exactly
				sampleFrequency = 1;
			}
			else if (len <= 100 * 1024) {
				sampleFrequency = 5;		// Use prime to avoid hitting a pattern as much as possible -- not sure if this helps, but hey
			}
			else {
				sampleFrequency = 11;
			}
			
			var byte;
			var samples = Std.int(len / sampleFrequency);
			var end16 = samples & 0xFFFFFFF0;		// Floor to nearest 16
			var i = 0;
			while (i < end16) {
				byte = Memory.getByte(offset + i * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 1) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 2) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 3) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 4) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 5) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 6) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 7) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 8) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 9) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 10) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 11) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 12) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 13) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 14) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				byte = Memory.getByte(offset + (i + 15) * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				i += 16;
			}
			while (i < samples) {
				byte = Memory.getByte(offset + i * sampleFrequency);
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				++i;
			}
		}
		else if (level == NORMAL || level == GOOD) {
			n = 257;		// Must include all symbols up to EOB
			
			// Find last non-zero weight (all symbols up to that point will have to be included)
			for (symbol in 257 ... 286) {
				if (Memory.getI32(scratchAddr + symbol * 4) > 0) {
					n = symbol + 1;
				}
			}
			
			// Weight EOB lowest
			Memory.setI32(scratchAddr + EOB * 4, 1);
			
			// Make sure all used symbols are weighted >= 2
			for (symbol in 0 ... n) {
				if (symbol != EOB && Memory.getI32(scratchAddr + symbol * 4) > 0) {
					Memory.setI32(scratchAddr + symbol * 4, Memory.getI32(scratchAddr + symbol * 4) + 2);
				}
			}
		}
		
		
		HuffmanTree.weightedAlphabetToCodes(scratchAddr, scratchAddr + n * 4, MAX_CODE_LENGTH);
		
		return n;
	}
	
	
	// Resets all symbol frequencies (literal, EOB, length, distance) to 0
	private inline function clearSymbolFrequencies()
	{
		for (symbol in 0 ... 286) {
			Memory.setI32(scratchAddr + (symbol << 2), 0);
		}
		for (symbol in 0 ... 30) {
			Memory.setI32(scratchAddr + DISTANCE_OFFSET + (symbol << 2), 0);
		}
	}
	
	
	private inline function incSymbolFrequency(symbol : Int, scratchOffset = 0)
	{
		var addr = scratchAddr + scratchOffset + (symbol << 2);
		Memory.setI32(addr, Memory.getI32(addr) + 1);
	}
	
	
	private inline function createDistanceTree(offset : Int, end : Int)
	{
		var scratchOffset = scratchAddr + DISTANCE_OFFSET;
		var n = 0;
		
		if (level == NORMAL || level == GOOD) {
			// Find last non-zero weight (all symbols up to that point will have to be included)
			for (symbol in 0 ... 30) {
				if (Memory.getI32(scratchOffset + symbol * 4) > 0) {
					n = symbol + 1;
				}
			}
		}
		
		HuffmanTree.weightedAlphabetToCodes(scratchOffset, scratchOffset + n * 4, MAX_CODE_LENGTH);
		
		return n;
	}
	
	
	private inline function createCodeLengthTree(literalLengthCodes, distanceCodes)
	{
		for (len in 0 ... 19) {
			Memory.setI32(scratchAddr + CODE_LENGTH_OFFSET + len * 4, 1);
		}
		
		var index;
		for (i in 0 ... literalLengthCodes) {
			// Increment weight for literal/length code's code length
			index = scratchAddr + CODE_LENGTH_OFFSET + Memory.getUI16(scratchAddr + i * 4) * 4;
			Memory.setI32(index, Memory.getI32(index) + 1);
		}
		for (i in 0 ... distanceCodes) {
			// Increment weight for distance code's code length
			index = scratchAddr + CODE_LENGTH_OFFSET + Memory.getUI16(scratchAddr + DISTANCE_OFFSET + i * 4) * 4;
			Memory.setI32(index, Memory.getI32(index) + 1);
		}
		
		var offset = scratchAddr + CODE_LENGTH_OFFSET;
		HuffmanTree.weightedAlphabetToCodes(offset, offset + 19 * 4, MAX_CODE_LENGTH_CODE_LENGTH);
		return 19;
	}
	
	
	// Copies length bytes (all by default) from src into flash.Memory at the specified offset
	private static inline function memcpy(src : ByteArray, offset : Int, length : Int = 0) : Void
	{
		src.readBytes(ApplicationDomain.currentDomain.domainMemory, offset, length);
	}
}


@:protected private class LZHash
{
	private static inline var HASH_BITS = 15;
	private static inline var HASH_SIZE = 1 << HASH_BITS;
	private static inline var HASH_MASK = HASH_SIZE - 1;
	private static inline var LOG_SLOT = 4;
	private static inline var SLOTS = 1 << LOG_SLOT;
	private static inline var SLOT_MASK = SLOTS - 1;
	private static inline var LOOKAHEADS = 3;			// In addition to main search. Do not change; implementation is hardcoded to this value for speed
	private static inline var SCRATCH_SIZE = (LOOKAHEADS + 1) * 4;
	private static inline var SCRATCH_MASK = SCRATCH_SIZE - 1;
	
	public static inline var MEMORY_SIZE = (HASH_SIZE * (SLOTS + 1)) * 4 + SCRATCH_SIZE;
	public static inline var MAX_LOOKAHEAD = 4 + LOOKAHEADS;		// Bytes
	public static inline var MIN_MATCH_LENGTH = 4;
	
	
	private var addr : Int;
	private var baseResultAddr : Int;
	private var maxMatchLength : Int;
	private var windowSize : Int;
	
	public var resultAddr : Int;
	
	
	
	// Memory layout:
	// HASH_SIZE slot lists
	// Each slot list is composed of a 16-bit index to the next slot to be written,
	// followed by a 16-bit count (of filled slots)
	// followed by SLOTS 4-byte slots containing pointers back to the original
	// data
	
	
	
	// Addr must point to an allocated memory region of size MEMORY_SIZE
	public function new(addr, maxMatchLength, windowSize)
	{
		this.addr = addr;
		this.maxMatchLength = maxMatchLength;
		this.windowSize = windowSize;
		
		baseResultAddr = resultAddr = addr + MEMORY_SIZE - SCRATCH_SIZE;
		
		clearTable();
	}
	
	
	// Must be called exactly once before doing anything else.
	// Cap must be > i + MAX_LOOKAHEAD
	public inline function initLookahead(i : Int, cap : Int)
	{
		var hashOffset;
		
		for (_ in 0 ... LOOKAHEADS) {
			hashOffset = calcHashOffset(i);
			_search(i, hashOffset, cap);
			_update(i, hashOffset);
			
			++i;
			resultAddr = nextResultAddr(resultAddr);
		}
	}
	
	private inline function nextResultAddr(resultAddr : Int)
	{
		return baseResultAddr + (((resultAddr - baseResultAddr) + 4) & SCRATCH_MASK);
	}
	
	
	private function clearTable()
	{
		_clearTable();
	}
	
	private inline function _clearTable()
	{
		// Initialize hash table to empty slot lists (HASH_SIZE is divisble by 8)
		var i = addr;
		var end = addr + MEMORY_SIZE;
		while (i < end) {
			Memory.setI32(i, 0);
			Memory.setI32(i + 4 * (SLOTS + 1), 0);
			Memory.setI32(i + 8 * (SLOTS + 1), 0);
			Memory.setI32(i + 12 * (SLOTS + 1), 0);
			Memory.setI32(i + 16 * (SLOTS + 1), 0);
			Memory.setI32(i + 20 * (SLOTS + 1), 0);
			Memory.setI32(i + 24 * (SLOTS + 1), 0);
			Memory.setI32(i + 28 * (SLOTS + 1), 0);
			i += 32 * (SLOTS + 1);
		}
	}
	
	
	// Searches the hash for the best match of bytes starting at i, and also updates the hash
	// for future searches. A 32-bit ((distance << 16) | length) pair is written to memory at
	// resultAddr; Length portion will be < MIN_MATCH_LENGTH if no match was found
	public inline function searchAndUpdate(i : Int, cap : Int)
	{
		// Calculate next result for lookahead cache
		var hashOffset = calcHashOffset(i + LOOKAHEADS);
		_search(i + LOOKAHEADS, hashOffset, cap);
		_update(i + LOOKAHEADS, hashOffset);
		
		
		// After incrementing, current result (calculated LOOKAHEADS steps ago) will
		// be at resultAddr
		resultAddr = nextResultAddr(resultAddr);
		
		if (Memory.getUI16(resultAddr) >= MIN_MATCH_LENGTH) {
			var length = Memory.getUI16(resultAddr);
			if (Memory.getUI16(nextResultAddr(resultAddr)) > length ||
				Memory.getUI16(nextResultAddr(resultAddr + 4)) > length + 1 ||
				Memory.getUI16(nextResultAddr(resultAddr + 8)) > length + 2
			) {
				Memory.setI32(resultAddr, 0);	// Defer to better length coming up
			}
			else if (i + length < cap - MAX_LOOKAHEAD) {
				// Found match. Update hash with every byte (updates are pretty fast)
				for (k in i + LOOKAHEADS + 1 ... i + length) {
					update(k);
				}
				
				i += length;
				
				// Cache will be invalid after jump in i; repopulate
				resultAddr = nextResultAddr(resultAddr);		// Fill up slots after (and up to) current result slot
				initLookahead(i, cap);
			}
		}
	}
	
	
	private inline function _search(i : Int, hashOffset : Int, cap : Int)
	{
		var slots = Memory.getUI16(hashOffset);
		var first4 = Memory.getI32(i);
		
		
		var length, distance;
		
		var longestLength = 0;
		var longestEndPosition = -1;
		
		// Look at all the slots, finding the longest match
		
		// TODO: Instead of using brute-force search for hash collisions, use something more clever (sorted substrings?) to speed up searches
		// TODO: Improve hash spread (and therefore speed) by hashing on first four bytes instead of first three
		// TODO: Improve compression by using PAETH filter
		
		var j, k;
		var p = hashOffset + 4;
		var end = p + (slots << 2);
		while (p < end) {
			j = Memory.getI32(p);
			if (Memory.getI32(j) == first4 && i - j <= windowSize) {
				// A match! Determine how long it is
				length = 4;
				j += 4;
				k = i + 4;
				while (k + 4 <= cap && Memory.getI32(j) == Memory.getI32(k) && length + 4 <= maxMatchLength) {
					length += 4;
					j += 4;
					k += 4;
				}
				
				while (k < cap && Memory.getByte(j) == Memory.getByte(k) && length < maxMatchLength) {
					++length;
					++j;
					++k;
				}
				
				// Check if this match is longer than another, or equal with shorter distance
				if (length > longestLength || (length == longestLength && k - j < i - (longestEndPosition - longestLength))) {
					longestLength = length;
					longestEndPosition = j;
				}
			}
			p += 4;
		}
		
		// length: longestLength,
		// distance: i - (longestEndPosition - longestLength)
		Memory.setI32(resultAddr, ((i - (longestEndPosition - longestLength)) << 16) | longestLength);
	}
	
	
	public inline function update(i : Int)
	{
		_update(i, calcHashOffset(i));
	}
	
	
	private inline function _update(i : Int, hashOffset : Int)
	{
		// The slot index wraps around (circular buffer).
		// This insures the oldest (most likely to be more than
		// windowSize away) entries are overwritten when the slots
		// are already full
		
		var bookkeeping = Memory.getI32(hashOffset);
		var nextIndex = bookkeeping >>> 16;
		var count = bookkeeping & 0xFFFF;
		
		Memory.setI32(hashOffset + 4 + (nextIndex << 2), i);
		
		// Update bookkeeping
		++nextIndex;
		count = (count + 1) ^ (count >>> LOG_SLOT);		// Increment count to ceiling of SLOTS
		
		Memory.setI32(hashOffset, ((nextIndex & SLOT_MASK) << 16) | count);
	}
	
	
	
	// Returns the index into the hash table for the first 3 bytes
	// starting at addr
	public static inline function hash(addr : Int, mask : Int)
	{
		// MurmurHash3
		// Adapted from http://code.google.com/p/smhasher/source/browse/trunk/MurmurHash3.cpp
		
		var h1 = 0x2e352bcd;		// Seed (randomly chosen number in this case)
		var c1 = 0xcc9e2d51;
		var c2 = 0x1b873593;
		
		var k1 = (Memory.getUI16(addr + 1) << 8) | Memory.getByte(addr);
        k1 *= c1;
		k1 = (k1 << 15) | (k1 >>> (32 - 15));	// ROTL32(k1, 15)
		k1 *= c2;
		h1 ^= k1;
		h1 ^= 3;
		
		// fmix
		h1 ^= h1 >>> 16;
		h1 *= 0x85ebca6b;
		h1 ^= h1 >>> 13;
		h1 *= 0xc2b2ae35;
		h1 ^= h1 >>> 16;
		
		h1 &= mask;
		return h1 << 2;		// Multiply by 4 since each entry is 4 bytes
	}
	
	
	private inline function calcHashOffset(i : Int)
	{
		return addr + hash(i, HASH_MASK) * (SLOTS + 1);
	}
}


@:protected private class HuffmanTree
{
	public static var scratchAddr : Int;
	
	
	// Array-based wrapper around fast implementation that uses fast memory.
	// Provided for convenient testing -- do not use in production (slow)
	public static function fromWeightedAlphabet(weights : Array<Int>, maxCodeLength : Int)
	{
		var oldFastMem = ApplicationDomain.currentDomain.domainMemory;
		var oldScratchAddr = scratchAddr;
		
		var mem = new ByteArray();
		mem.length = Std.int(Math.max(weights.length * 4 * 2, ApplicationDomain.MIN_DOMAIN_MEMORY_LENGTH));
		Memory.select(mem);
		
		var offset = 0;
		var end = offset + weights.length * 4;
		scratchAddr = end;
		
		var i = 0;
		for (weight in weights) {
			Memory.setI32(offset + i, weight);
			i += 4;
		}
		
		weightedAlphabetToCodes(offset, end, maxCodeLength);
		
		var result = new Array<Int>();
		i = offset;
		while (i < end) {
			result.push(Memory.getI32(i));
			i += 4;
		}
		
		Memory.select(oldFastMem);
		scratchAddr = oldScratchAddr;
		
		return result;
	}
	
	
	
	// Converts an array of weights to a lookup table of codes.
	// The weights must be stored in 32-bit integers in fast memory, from offset
	// to end. The symbols are assumed to be the integers 0 ... (end - offset).
	// The codes will be stored starting at offset, replacing the weights.
	// Each code entry contains the code and the code length.
	// The code is stored in the highest 16 bits, and the length in the lowest.
	// The code's bits are reversed (RFC 1951 says Huffman codes get packed in
	// reverse order). Each code will be stored at the address corresponding to
	// the symbol (i.e. symbol 2 would be at address offset + 2 * 4).
	public static function weightedAlphabetToCodes(offset : Int, end : Int, maxCodeLength : Int) : Void
	{
		return _weightedAlphabetToCodes(offset, end, maxCodeLength);
	}
	
	private static inline function _weightedAlphabetToCodes(offset, end, maxCodeLength)
	{
		var originalN = (end - offset) >> 2;		// Number of weights
		var n = originalN;		// Number of non-zero weights (actual symbols)
		
		if (n > 0) {
			// There's at least one weight
			
			// Create, at scratchAddr, a parallel array of
			// symbols corresponding to the given weights.
			// This is necessary since we don't know how large each weight
			// is (so we can't stuff the symbol in the upper bits)
			
			for (i in 0 ... n) {
				set32(scratchAddr, i, i);
			}
			
			// Compact arrays by removing elements that have weight of 0 (unused)
			var jump = 0;
			for (i in 0 ... n) {
				if (get32(offset, i) == 0) {
					++jump;
				}
				else {
					set32(offset,      i - jump, get32(offset,      i));
					set32(scratchAddr, i - jump, get32(scratchAddr, i));
				}
			}
			n -= jump;
			end = offset + (n << 2);
			
			sortByWeightNonDecreasing(offset, end);
		}
		
		// Calculate unrestricted code lengths
		calculateOptimalCodeLengths(offset, n);
		
		// Restrict code lengths
		limitCodeLengths(offset, end, maxCodeLength);
		
		// Merge code lengths and symbols into the same 32-bit slots, in scratch memory.
		// Symbols will be in upper 16 bits
		for (i in 0 ... n) {
			set32(scratchAddr, i, (get32(scratchAddr, i) << 16) | get32(offset, i));
		}
		
		// Sort by code length, then by symbol (both decreasing)
		// Codelens are already sorted (or nearly, if some codelengths were limited), but
		// symbols are in increasing order for equal bitlengths.
		// See http://cstheory.stackexchange.com/questions/7420/relation-between-code-length-and-symbol-weight-in-a-huffman-code
		sortByCodeLengthAndSymbolDecreasing(scratchAddr, end - offset);
		
		// Calculate the actual codes (canonical Huffman tree).
		// Result is stored in lookup table (by symbol)
		
		if (n != originalN) {
			// Set all symbols to code length 0. Used
			// symbols will be overwritten in next step
			for (i in 0 ... originalN) {
				set32(offset, i, 0);
			}
		}
		
		calculateCanonicalCodes(scratchAddr, n, offset);
		
		var foo;
		if (false) {  }		// Causes jump in bytecode -- fixes mysterious stack error
	}
	
	
	private static inline function sortByWeightNonDecreasing(offset, end)
	{
		// TODO: Change to O(nlgn) or O(n) sort (it's measurably slow)
		
		// Insertion sort
		var i, j;
		var currentWeight, currentSymbol;
		
		var len = end - offset;
		
		i = 4;		// First entry is already "sorted"
		while (i < len) {
			currentWeight = Memory.getI32(offset + i);
			currentSymbol = Memory.getI32(scratchAddr + i);
			
			j = i - 4;
			while (j >= 0 && Memory.getI32(offset + j) > currentWeight) {
				Memory.setI32(offset + j + 4, Memory.getI32(offset + j));
				Memory.setI32(scratchAddr + j + 4, Memory.getI32(scratchAddr + j));
				j -= 4;
			}
			Memory.setI32(offset + j + 4, currentWeight);
			Memory.setI32(scratchAddr + j + 4, currentSymbol);
			
			i += 4;
		}
	}
	
	private static inline function sortByCodeLengthAndSymbolDecreasing(offset, len)
	{
		// Insertion sort
		var i, j;
		var current, currentCodeLen, currentSymbol;
		
		i = 4;
		while (i < len) {
			current = Memory.getI32(offset + i);
			currentSymbol = current >>> 16;
			currentCodeLen = current & 0xFFFF;
			
			j = i - 4;
			while (j >= 0 && compareCodeLengthAndSymbolDecreasing(currentCodeLen, currentSymbol, offset, j)) {
				Memory.setI32(offset + j + 4, Memory.getI32(offset + j));
				j -= 4;
			}
			Memory.setI32(offset + j + 4, current);
			
			i += 4;
		}
	}
	
	private static inline function compareCodeLengthAndSymbolDecreasing(currentCodeLen, currentSymbol, offset, j)
	{
		var codeLenDiff = Memory.getUI16(offset + j) - currentCodeLen;
		return codeLenDiff == 0 ? Memory.getUI16(offset + j + 2) < currentSymbol : codeLenDiff < 0;
	}
	
	
	// Transforms weights into a corresponding list of code lengths
	private static inline function calculateOptimalCodeLengths(offset, n)
	{
		// Uses Moffat's in-place algorithm for calculating the unrestricted Huffman code lengths
		// Adapted from http://ww2.cs.mu.oz.au/~alistair/inplace.c
		
		var root;                  /* next root node to be used */
        var leaf;                  /* next leaf to be used */
        var next;                  /* next value to be assigned */
        var avbl;                  /* number of available nodes */
        var used;                  /* number of internal nodes */
        var dpth;                  /* current depth of leaves */
		
        /* check for pathological cases */
        if (n!=0) {
			if (n != 1) {
				/* first pass, left to right, setting parent pointers */
				set32(offset, 0, get32(offset, 0) + get32(offset, 1));
				root = 0; leaf = 2; next = 1;
				while (next < n-1) {
					/* select first item for a pairing */
					if (leaf>=n || get32(offset, root) < get32(offset, leaf)) {
						set32(offset, next, get32(offset, root));
						set32(offset, root++, next);
					} else {
						set32(offset, next, get32(offset, leaf++));
					}
					
					/* add on the second item */
					if (leaf>=n || (root < next && get32(offset, root) < get32(offset, leaf))) {
						set32(offset, next, get32(offset, next) + get32(offset, root));
						set32(offset, root++, next);
					} else {
						set32(offset, next, get32(offset, next) + get32(offset, leaf++));
					}
					
					++next;
				}
				
				/* second pass, right to left, setting internal depths */
				set32(offset, n - 2, 0);
				next = n - 3;
				while (next>=0) {
					set32(offset, next, get32(offset, get32(offset, next)) + 1);
					--next;
				}
				
				/* third pass, right to left, setting leaf depths */
				avbl = 1; used = dpth = 0; root = n-2; next = n-1;
				while (avbl>0) {
					while (root>=0 && get32(offset, root)==dpth) {
						++used; --root;
					}
					while (avbl>used) {
						set32(offset, next--, dpth);
						--avbl;
					}
					avbl = 2*used; ++dpth; used = 0;
				}
			}
			else {		// n == 1
				set32(offset, 0, 1);
			}
		}
	}
	
	
	private static inline function get32(offset : Int, i : Int) : Int
	{
		return Memory.getI32(offset + (i << 2));
	}
	
	private static inline function set32(offset : Int, i : Int, v : Int)
	{
		Memory.setI32(offset + (i << 2), v);
	}
	
	
	private static inline function limitCodeLengths(offset : Int, end : Int, max : Int)
	{
		// Uses (non-optimal) heuristic algorithm described at http://cbloomrants.blogspot.com/2010/07/07-03-10-length-limitted-huffman-codes.html
		
		// Assumes codelens is sorted in non-decreasing order by weight
		
		var overflow = false;
		var n = (end - offset) >>> 2;
		
		// Set code lengths > max to max
		for (i in 0 ... n) {
			if (get32(offset, i) > max) {
				set32(offset, i, max);
				overflow = true;
			}
		}
		
		if (overflow) {
			// Calculate Kraft number
			var K = 0.0;
			for (i in 0 ... n) {
				K += Math.pow(2, -get32(offset, i));
			}
			
			// Pass 1
			var i = 0;
			while (K > 1 && i < n) {
				while (get32(offset, i) < max && K > 1) {
					set32(offset, i, get32(offset, i) + 1);	// ++
					
					// adjust K for change in codeLen
					K -= Math.pow(2, -get32(offset, i));
				}
				++i;
			}
			
			// Pass 2
			i = n - 1;
			while (i >= 0) {
				while ((K + Math.pow(2, -get32(offset, i))) <= 1) {
					// adjust K for change in codeLen
					K += Math.pow(2, -get32(offset, i));
					
					set32(offset, i, get32(offset, i) - 1);	// --
				}
				
				--i;
			}
		}
	}
	
	
	// Input is expected to be in sorted order, first by code length (decreasing), then by symbol (decreasing)
	private static inline function calculateCanonicalCodes(symbolCodeOffset, n, destOffset)
	{
		// Implements algorithm found on Wikipedia: http://en.wikipedia.org/wiki/Canonical_Huffman_code
		
		if (n != 0) {
			// Iterate over symbols in reverse order (i.e. increasing codelength)
			var i = n - 1;
			var code = 0;
			var curLen = Memory.getUI16(symbolCodeOffset + i * 4);
			var s;
			var newLen;
			while (i >= 0) {
				s = Memory.getI32(symbolCodeOffset + i * 4);
				newLen = s & 0xFFFF;
				code <<= newLen - curLen;
				Memory.setI32(destOffset + (s >>> 16) * 4, (reverseBits(code, newLen) << 16) | newLen);
				++code;
				curLen = newLen;
				if (code >= (1 << curLen)) {
					// We overflowed the current bit length by incrementing
					++curLen;
				}
				
				--i;
			}
		}
	}
	
	
	// Reverses count bits of v and returns the result (just the reversed bits)
	// Count must be <= 16 (i.e. max 2 bytes)
	private static inline function reverseBits(v : Int, count : Int) : UInt
	{
		// From http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel
		
		// swap odd and even bits
		v = ((v >>> 1) & 0x55555555) | ((v & 0x55555555) << 1);
		// swap consecutive pairs
		v = ((v >>> 2) & 0x33333333) | ((v & 0x33333333) << 2);
		// swap nibbles ... 
		v = ((v >>> 4) & 0x0F0F0F0F) | ((v & 0x0F0F0F0F) << 4);
		// swap bytes
		v = ((v >>> 8) & 0x00FF00FF) | ((v & 0x00FF00FF) << 8);
		
		// Because 2-byte blocks were not swapped, result is in lower word
		
		return (v & 0xFFFF) >>> (16 - count);
	}
}
