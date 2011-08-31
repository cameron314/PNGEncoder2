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
// - Reverse Parallel algorithm for reversing bits (http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel)
// - Wikipedia article on canonical Huffman codes (http://en.wikipedia.org/wiki/Canonical_Huffman_code)
// - http://cstheory.stackexchange.com/questions/7420/relation-between-code-length-and-symbol-weight-in-a-huffman-code

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
	NORMAL;				// Huffman + run length encoding
	MAXIMUM;			// Huffman + proper LZ77 compression
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
	
	public static inline var SCRATCH_MEMORY_SIZE : Int = (32 + 19 + MAX_SYMBOLS_IN_TREE * 2 + LENGTHS + MIN_LENGTH) * 4;		// Bytes
	
	private static inline var DISTANCE_OFFSET : Int = MAX_SYMBOLS_IN_TREE * 4;		// Where distance lookup is stored in scratch memory
	private static inline var CODE_LENGTH_OFFSET : Int = DISTANCE_OFFSET + 32 * 4;
	private static inline var HUFFMAN_SCRATCH_OFFSET : Int = CODE_LENGTH_OFFSET + 19 * 4;
	private static inline var LENGTH_EXTRA_BITS_OFFSET : Int = HUFFMAN_SCRATCH_OFFSET + MAX_SYMBOLS_IN_TREE * 4;
	// Next offset: LENGTH_EXTRA_BITS_OFFSET + (LENGTHS + MIN_LENGTH) * 4
	
	private static inline var OUTPUT_BYTES_BEFORE_NEW_BLOCK : Int = 48 * 1024;	// Only used with FAST
	private static inline var MAX_UNCOMPRESSED_BYTES_PER_BLOCK : UInt = 65535;
	private static inline var ADDLER_MAX : Int = 65521;		// Largest prime smaller than 65536
	private static inline var MAX_CODE_LENGTH : Int = 15;
	private static inline var MAX_CODE_LENGTH_CODE_LENGTH : Int = 7;
	private static inline var CODE_LENGTH_ORDER = [ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 ];
	private static inline var EOB = 256;		// End of block symbol
	
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
	
	
	private inline function _new(level : CompressionLevel, writeZLIBInfo : Bool, scratchAddr : Int, startAddr : Int)
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
		if (level != UNCOMPRESSED || bitOffset > 0) {
			// Copy in-progress byte to start
			Memory.setByte(startAddr, Memory.getByte(currentAddr));
		}
		
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
			_fastWriteHuffmanOnly(offset, end);
		}
		else if (level == NORMAL) {
			_fastWriteRunLength(offset, end);
		}
		else if (level == MAXIMUM) {
			_fastWriteHuffmanAndLZ77(offset, end);
		}
		else {
			throw new Error("Compression level not supported");
		}
	}
	
	
	private inline function _fastWriteHuffmanOnly(offset : Int, end : Int)
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
	
	
	private inline function _fastWriteRunLength(offset : Int, end : Int)
	{
		var cappedEnd;
		var len;
		
		var length;
		var lengthInfo;
		var i, j;
		
		// blockInProgress should always be false here
		
		while (end - offset > 0) {
			// Assume ~50% compression ratio
			cappedEnd = Std.int(Math.min(end, offset + OUTPUT_BYTES_BEFORE_NEW_BLOCK * 2));
			len = cappedEnd - offset;
			
			beginBlock();
			createAndWriteHuffmanTrees(offset, cappedEnd);
			
			i = offset;
			while (i < cappedEnd) {
				j = i + 1;
				while (j < cappedEnd && Memory.getByte(j) == Memory.getByte(i) && j - i <= MAX_LENGTH) {
					++j;
				}
				
				// Write the current byte whether it repeats or not
				writeSymbol(Memory.getByte(i));
				++i;
				
				if (j - i >= MIN_LENGTH) {
					length = j - i;
					lengthInfo = Memory.getI32(scratchAddr + LENGTH_EXTRA_BITS_OFFSET + length * 4);
					writeSymbol(lengthInfo >>> 8);
					writeBits(length - (lengthInfo & 0x1F), (lengthInfo & 0xFF) >>> 5);
					writeSymbol(0, DISTANCE_OFFSET);	// Distance of 1
					
					i += length;
				}
			}
			
			endBlock();
			
			offset += len;
		}
	}
	
	
	private inline function _fastWriteHuffmanAndLZ77(offset : Int, end : Int)
	{
		throw new Error("Not implemented");
	}
	
	
	private inline function beginBlock(lastBlock = false)
	{
		blockInProgress = true;
		
		if (level == CompressionLevel.UNCOMPRESSED) {
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
			}
			
			// Using Huffman compression with max 15 bits can't possibly
			// exceed twice the uncompressed length. Margin of 300 includes
			// header/footer data (think 285 length/literal codes at 7 bits max each),
			// rounded up for good luck.
			multiplier = 2;
			blockOverhead = 300;
		}
		
		// Include extra block to account for the one written during finalize()
		return inputByteCount * multiplier + blockOverhead * (blockCount + 1);
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
		if (level == NORMAL || level == MAXIMUM) {
			// Write length code, extra bits and lower bound that must be subtracted to
			// get the extra bits value for all the possible lengths.
			// This information is stuffed into 4 bytes per length, addressable
			// by Memory.getI32(LENGTH_EXTRA_BITS_OFFSET + length * 4).
			// Upper 3 bytes: Length code
			// Lower byte: The upper 3 bits are the extra bit count, and the lower 5 are
			// the lower bound.
			
			var offset = scratchAddr + LENGTH_EXTRA_BITS_OFFSET;
			
			// 3 ... 10:
			Memory.setI32(offset + 3 * 4, (257 << 8) | 3);
			Memory.setI32(offset + 4 * 4, (258 << 8) | 4);
			Memory.setI32(offset + 5 * 4, (259 << 8) | 5);
			Memory.setI32(offset + 6 * 4, (260 << 8) | 6);
			Memory.setI32(offset + 7 * 4, (261 << 8) | 7);
			Memory.setI32(offset + 8 * 4, (262 << 8) | 8);
			Memory.setI32(offset + 9 * 4, (263 << 8) | 9);
			Memory.setI32(offset + 10 * 4, (264 << 8) | 10);
			
			// 11 ... 257:
			var base = 11;
			var symbol = 265;
			for (extraBits in 1 ... 6) {
				for (_ in 0 ... 4) {
					for (i in base ... base + (1 << extraBits)) {
						Memory.setI32(offset + i * 4, (symbol << 8) | (extraBits << 5) | base);
					}
					base += 1 << extraBits;
					++symbol;
				}
			}
			
			Memory.setI32(offset + 258 * 4, (285 << 8) | 258);		// 258
		}
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
			
			s1 %= ADDLER_MAX;
			s2 %= ADDLER_MAX;
			
			offset += NMAX;
		}
		
		if (offset != end) {
			for (i in offset ... end) {
				s1 += Memory.getByte(i);
				s2 += s1;
			}
			
			s1 %= ADDLER_MAX;
			s2 %= ADDLER_MAX;
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
			if (len <= 8096) {
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
		else if (level == NORMAL) {
			// Get exact weights for given bytes; this is possible since all bytes
			// going into the block are known in advance (which is not the case with
			// the FAST level)
			
			n = 0;
			for (symbol in 0 ... 286) {
				Memory.setI32(scratchAddr + symbol * 4, 0);
			}
			
			// Weight literals and lengths
			var byte;
			var length, lengthCode;
			var j;
			var i = offset;
			while (i < end) {
				j = i + 1;
				byte = Memory.getByte(i);
				while (j < end && Memory.getByte(j) == byte && j - i <= MAX_LENGTH) {
					++j;
				}
				
				// Increment weight for the current symbol
				Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
				++i;
				
				if (j - i >= MIN_LENGTH) {
					length = j - i;
					lengthCode = Memory.getI32(scratchAddr + LENGTH_EXTRA_BITS_OFFSET + length * 4) >>> 8;
					Memory.setI32(scratchAddr + lengthCode * 4, Memory.getI32(scratchAddr + lengthCode * 4) + 1);
					
					i += length;
				}
			}
			
			// Find last non-zero weight (all symbols up to that point will have to be included)
			for (symbol in 0 ... 286) {
				if (Memory.getI32(scratchAddr + symbol * 4) > 0) {
					n = symbol + 1;
				}
			}
			
			// Make sure all weights up to the last one are weighted > 0
			for (symbol in 0 ... n) {
				Memory.setI32(scratchAddr + symbol * 4, Memory.getI32(scratchAddr + symbol * 4) + 1);
			}
		}
		else if (level == MAXIMUM) {
			// TODO
		}
		
		
		HuffmanTree.weightedAlphabetToCodes(scratchAddr, scratchAddr + n * 4, MAX_CODE_LENGTH);
		
		return n;
	}
	
	
	private inline function createDistanceTree(offset : Int, end : Int)
	{
		var scratchOffset = scratchAddr + DISTANCE_OFFSET;
		var n = 0;
		
		if (level == NORMAL) {
			Memory.setI32(scratchOffset, 100);
			++n;
		}
		else if (level == MAXIMUM) {
			// TODO
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


class HuffmanTree
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
	// to end. The symbols are assumed to be the integers 0...(end - offset).
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
		var n = (end - offset) >> 2;		// Number of weights
		
		if (n > 0) {
			// There's at least one weight
			
			// Create, at scratchAddr, a parallel array of
			// symbols corresponding to the given weights.
			// This is necessary since we don't know how large each weight
			// is (so we can't stuff the symbol in the upper bits)
			
			for (i in 0 ... n) {
				set32(scratchAddr, i, i);
			}
			
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
