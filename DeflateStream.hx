/*
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

	* Neither the name of the orignial author nor the names of any 
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


// References:
// - The standard zlib implementation (available from http://zlib.net/)
// - RFC 1950 (zlib) and 1951 (DEFLATE)
// - "An Explanation of the Deflate Algorithm" (http://www.zlib.net/feldspar.html)
// - "Length-Limitted Huffman Codes" (http://cbloomrants.blogspot.com/2010/07/07-02-10-length-limitted-huffman-codes.html)
// - "Length-Limitted Huffman Codes Heuristic" (http://cbloomrants.blogspot.com/2010/07/07-03-10-length-limitted-huffman-codes.html)
// - A C implementation of an in-place Huffman code length generator (http://ww2.cs.mu.oz.au/~alistair/inplace.c)
// - Reverse Parallel algorithm for reversing bits (http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel)
// - Wikipedia article on canonical Huffman codes (http://en.wikipedia.org/wiki/Canonical_Huffman_code)
// - http://cstheory.stackexchange.com/questions/7420/relation-between-code-length-and-symbol-weight-in-a-huffman-code

package;

import flash.errors.ArgumentsError;
import flash.Lib;
import flash.Memory;
import flash.system.ApplicationDomain;
import flash.utils.ByteArray;
import flash.utils.Endian;
import flash.Vector;
import flash.utils.TypedDictionary;
import haxe.Stack;

enum CompressionLevel {
	UNCOMPRESSED;		// Fastest
	FAST;
	NORMAL;
	MAXIMUM;
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
	
	public static inline var MAX_UNCOMPRESSED_BYTES_PER_BLOCK : UInt = 65535;
	public static inline var SCRATCH_MEMORY_SIZE : Int = (32 + 19 + MAX_SYMBOLS_IN_TREE * 2) * 4;		// Bytes
	
	private static inline var DISTANCE_OFFSET : Int = MAX_SYMBOLS_IN_TREE * 4;		// Where distance lookup is stored in scratch memory
	private static inline var CODE_LENGTH_OFFSET : Int = DISTANCE_OFFSET + 32 * 4;
	private static inline var HUFFMAN_SCRATCH_OFFSET : Int = CODE_LENGTH_OFFSET + 19 * 4;
	
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
	private var freshBlock : Bool;
	
	private var literalLengthCodes : Int;		// Count
	private var distanceCodes : Int;			// Count
	
	// For calculating Adler-32 sum
	private var s1 : UInt;
	private var s2 : UInt;
	
	private var bitOffset : Int;
	
	// TODO: Add compression ratio (keep track of bits written vs. bits seen)
	
	
	// If scratchAddr and startAddr are set, that signifies that memory has already been selected
	// outside of this DeflateStream, for use with it. Up to SCRATCH_MEMORY_SIZE
	// bytes will be used at scratchAddr. If scratchAddr is not given, then no manual
	// memory management is required. If scratchAddr is past startAddr, then it is the
	// caller's reponsiblity to ensure that there's enough room between startAddr and
	// scratchAddr for the compressed data.
	public function new(level : CompressionLevel, writeZLIBInfo = false, scratchAddr = -1, startAddr = 0)
	{
		this.level = level;
		this.zlib = writeZLIBInfo;
		
		if (scratchAddr < 0) {
			scratchAddr = 0;
			startAddr = SCRATCH_MEMORY_SIZE;
			var mem = new ByteArray();
			mem.length = ApplicationDomain.MIN_DOMAIN_MEMORY_LENGTH;
			Memory.select(mem);
		}
		
		this.scratchAddr = scratchAddr;
		this.startAddr = startAddr;
		this.currentAddr = startAddr;
		
		HuffmanTree.scratchAddr = scratchAddr + HUFFMAN_SCRATCH_OFFSET;
		
		// Ensure at least 3 bytes for possible zlib data and block header bits
		// writeBits requires 3 bytes past what is needed
		var mem = ApplicationDomain.currentDomain.domainMemory;
		var minLength : UInt = startAddr + 6;
		if (mem.length < minLength) {
			mem.length = minLength;
		}
		
		distanceCodes = -1;
		freshBlock = false;
		s1 = 1;
		s2 = 0;
		
		if (zlib) {
			writeByte(0x78);		// CMF with compression method 8 (deflate) 32K sliding window
			writeByte(0x9C);		// FLG: Check bits, no dict, default algorithm
		}
	}
	
	
	// Helper method. Same as sequentially calling beginBlock, update, and endBlock
	public inline function writeBlock(bytes : ByteArray, lastBlock = false)
	{
		beginBlock(lastBlock);
		var wroteAll = update(bytes);
		endBlock();
		return wroteAll;
	}
	
	
	// Helper method. Same as sequentially calling beginBlock, fastUpdate, and endBlock
	public inline function fastWriteBlock(offset : Int, end : Int, lastBlock = false)
	{
		beginBlock(lastBlock);
		var wroteAll = fastUpdate(offset, end);
		endBlock();
		return wroteAll;
	}
	
	
	public inline function beginBlock(lastBlock = false)
	{
		freshBlock = true;
		
		bitOffset = 0;
		
		if (level == CompressionLevel.UNCOMPRESSED) {
			writeByte(lastBlock ? 1 : 0);		// Uncompressed
		}
		else {
			// writeBits relies on first byte being initiazed to 0
			Memory.setByte(currentAddr, 0);
			
			writeBits(4 | (lastBlock ? 1 : 0), 3);		// Dynamic Huffman tree compression
		}
	}
	
	
	public inline function update(bytes : ByteArray)
	{
		// Put input bytes into fast mem *after* a gap for output bytes.
		// This allows multiple calls to update without needing to pre-delcare an input
		// buffer big enough (i.e. streaming).
		var offset = currentAddr + maxOutputBufferSize(bytes.bytesAvailable);
		var end = offset + bytes.bytesAvailable;
		
		// Reserve space
		var mem = ApplicationDomain.currentDomain.domainMemory;
		var uend : UInt = end;
		if (mem.length < uend) {
			mem.length = end;
		}
		
		memcpy(bytes, offset);
		return fastUpdate(offset, end);
	}
	
	
	// Updates the current block with the compressed representation of bytes between the from and to indexes
	// (of selected memory)
	// Only a maximum of MAX_UNCOMPRESSED_BYTES_PER_BLOCK bytes will be written when using UNCOMPRESSED level
	public function fastUpdate(offset : Int, end : Int)
	{
		return _fastUpdate(offset, end);
	}
	
	
	private inline function _fastUpdate(offset : Int, end : Int)
	{
		// TODO: Split this out into multiple methods, it's too large
		var wroteAll = true;
		
		var mem = ApplicationDomain.currentDomain.domainMemory;
		if (level == CompressionLevel.UNCOMPRESSED) {
			// TODO: Speed up uncompressed -- determine if a byte array can copy into itself; if not, unroll memcopy loop
			var len = Std.int(Math.min(end - offset, MAX_UNCOMPRESSED_BYTES_PER_BLOCK));
			
			if (len + 8 > mem.length - currentAddr) {
				mem.length += len + 8;
			}
			
			if (freshBlock) {
				// Write uncompressed header info
				writeShort(len);
				writeShort(~len);
			}
			
			var i = offset;
			var cappedEnd = (offset + len);
			var cappedEndMinus4 = cappedEnd - 4;
			while (i < cappedEndMinus4) {
				Memory.setI32(currentAddr, Memory.getI32(i));
				i += 4;
				currentAddr += 4;
			}
			while (i < offset + len) {
				Memory.setByte(currentAddr, Memory.getByte(i));
				++i;
				++currentAddr;
			}
			
			if (zlib) {
				updateAdler32(offset, offset + len);
			}
			
			wroteAll = (end - offset == len);
		}
		else {
			var len = end - offset;
			
			// TODO: Optimize overhead
			
			//var startTime = Lib.getTimer();
			
			// Make sure there's enough room in the output
			if (maxOutputBufferSize(len) > mem.length - currentAddr) {
				mem.length = maxOutputBufferSize(len) + currentAddr;
			}
			
			if (freshBlock) {
				// Write Huffman trees into the stream as per RFC 1951
				
				literalLengthCodes = createLiteralLengthTree(offset, end);
				if (distanceCodes == -1) {
					distanceCodes = createDistanceTree();
				}
				
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
				
				//var endTime = Lib.getTimer();
				//trace("DEFLATE block overhead took " + (endTime - startTime) + "ms");
			}
			
			// TODO: Use LZ77 (depending on compression settings)
			
			// Write data
			var i = offset;
			var end32 = offset + (len & 0xFFFFFFE0);		// Floor to nearest 32
			while (i < end32) {
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
			while (i < end) {
				writeSymbol(Memory.getByte(i));
				++i;
			}
			
			if (zlib) {
				updateAdler32(offset, end);
			}
		}
		
		freshBlock = false;
		
		return wroteAll;
	}
	
	
	public inline function endBlock()
	{
		if (level != UNCOMPRESSED) {
			writeSymbol(EOB);
		}
	}
	
	
	public inline function writeEmptyBlock(lastBlock : Bool)
	{
		var currentLevel = level;
		level = UNCOMPRESSED;
		fastWriteBlock(0, 0, lastBlock);
		level = currentLevel;
	}
	
	
	// Call only once. After called, no other methods should be called
	public inline function finalize() : ByteArray
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
	public inline function fastFinalize() : MemoryRange
	{
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
	public static inline function maxOutputBufferSize(uncompressedByteCount : Int, blockCount = 1)
	{
		// Using Huffman compression with max 15 bits can't possibly
		// exceed twice the uncompressed length. Margin of 300 includes
		// header/footer data (think 285 length/literal codes at 7 bits max each),
		// rounded up for good luck.
		return uncompressedByteCount * 2 + 300 * blockCount;
	}
	
	
	
	// Writes up to 25 bits into the stream (bits must be zero-padded)
	private inline function writeBits(bits : Int, bitCount : Int)
	{
		var current = Memory.getByte(currentAddr);
		current |= bits << bitOffset;
		Memory.setI32(currentAddr, current);
		bitOffset += bitCount;
		currentAddr += bitOffset >>> 3; // divided by 8
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
			//do16Adler(offset, offset += ((end - offset) & 0xFFFFFFF0));	// Floor to nearest 16
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
		// No lengths for now, just literals + EOB
		
		for (value in 0 ... 256) {		// Literals
			Memory.setI32(scratchAddr + value * 4, 10);
		}
		
		// Weight EOB less than more common literals
		Memory.setI32(scratchAddr + EOB * 4, 1);
		
		// No length codes yet
		
		
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
		var samples = Math.floor(len / sampleFrequency);
		for (i in 0 ... samples) {
			// TODO: Unroll loop
			byte = Memory.getByte(offset + i * sampleFrequency);
			Memory.setI32(scratchAddr + byte * 4, Memory.getI32(scratchAddr + byte * 4) + 1);
		}
		
		HuffmanTree.weightedAlphabetToCodes(scratchAddr, scratchAddr + 257 * 4, MAX_CODE_LENGTH);
		
		return 257;
	}
	
	
	private inline function createDistanceTree()
	{
		// No distances yet
		
		var offset = scratchAddr + DISTANCE_OFFSET;
		HuffmanTree.weightedAlphabetToCodes(offset, offset, MAX_CODE_LENGTH);
		
		return 0;
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
	
	private static function _weightedAlphabetToCodes(offset, end, maxCodeLength)
	{
		if (maxCodeLength > 16) {
			throw new ArgumentsError("Maximum code length must fit into 2 bytes or less");
		}
		
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
		
		for (i in 0 ... n) {
			Lib.trace(StringTools.hex(get32(scratchAddr, i)));
		}
		
		// Calculate the actual codes (canonical Huffman tree).
		// Result is stored in lookup table (by symbol)
		calculateCanonicalCodes(scratchAddr, n, offset);
	}
	
	
	private static inline function sortByWeightNonDecreasing(offset, end)
	{
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
				set32(offset, 0, 0);
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
	
	
	// TODO: Add inline -- first fix bug where adding inline causes unbalanced stack error (!)
	// Input is expected to be in sorted order, first by code length (decreasing), then by symbol (decreasing)
	private static function calculateCanonicalCodes(symbolCodeOffset, n, destOffset)
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
