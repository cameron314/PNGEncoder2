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
	public static inline var MAX_UNCOMPRESSED_BYTES_PER_BLOCK : UInt = 65535;
	public static inline var SCRATCH_MEMORY_SIZE : Int = 512 * 4;		// Bytes
	
	private static inline var DISTANCE_OFFSET : Int = 285 * 4;		// Where distance lookup is stored in scratch memory
	private static inline var CODE_LENGTH_OFFSET : Int = DISTANCE_OFFSET + 32 * 4;
	
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
	// memory management is required.
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
		
		// Ensure at least 3 bytes for possible zlib data and block header bits
		// writeBits requires 3 bytes past what is needed
		var mem = ApplicationDomain.currentDomain.domainMemory;
		var minLength : UInt = startAddr + 6;
		if (mem.length < minLength) {
			mem.length  = minLength;
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
		var wroteAll = true;
		
		var mem = ApplicationDomain.currentDomain.domainMemory;
		if (level == CompressionLevel.UNCOMPRESSED) {
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
			
			// Make sure there's enough room in the output
			if (maxOutputBufferSize(len) > mem.length - currentAddr) {
				mem.length = maxOutputBufferSize(len) + currentAddr;
			}
			
			if (freshBlock) {
				// Write Huffman trees into the stream as per RFC 1951
				
				var literalLengthLookup = createLiteralLengthTree(offset, end).codes;
				literalLengthCodes = literalLengthLookup.length;
				for (i in 0 ... literalLengthCodes) {
					Memory.setI32(scratchAddr + i * 4, literalLengthLookup[i]);
				}
				
				if (distanceCodes == -1) {
					var distanceTree = createDistanceTree();
					distanceCodes = distanceTree.codes.length;
					for (i in 0 ... distanceCodes) {
						Memory.setI32(scratchAddr + DISTANCE_OFFSET + i * 4, distanceTree.codes[i]);
					}
				}
				
				var codeLengthLookup = createCodeLengthTree().codes;
				for (i in 0 ... codeLengthLookup.length) {
					Memory.setI32(scratchAddr + CODE_LENGTH_OFFSET + i * 4, codeLengthLookup[i]);
				}
				
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
				writeBits(codeLengthLookup.length - 4, 4);
				
				// Write code lengths of code length code
				for (rank in CODE_LENGTH_ORDER) {
					writeBits(codeLengthLookup[rank] & 0xFFFF, 3);
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
	
	
	private static inline function createLiteralLengthTree(offset : Int, end : Int)
	{
		// No lengths for now, just literals + EOB
		
		var weights = new Array<UInt>();
		
		for (value in 0 ... 256) {		// Literals
			weights.push(10);
		}
		
		// Weight EOB less than more common literals
		weights.push(1);
		
		// No length codes yet
		
		
		// Sample given bytes to estimate literal weights
		var len = end - offset;
		var sampleFrequency;
		if (len <= 1024) {
			// Small sample, calculate exactly
			sampleFrequency = 1;
		}
		else if (len <= 20 * 1024) {
			sampleFrequency = 20;	// Sample every 20th byte
		}
		else {
			sampleFrequency = 30;
		}
		
		var samples = Math.floor(len / sampleFrequency);
		for (i in 0 ... samples) {
			++weights[Memory.getByte(offset + i * sampleFrequency)];
		}
		
		return HuffmanTree.fromWeightedAlphabet(weights, MAX_CODE_LENGTH);
	}
	
	
	private static inline function createDistanceTree()
	{
		var weights = new Array<Int>();
		
		// No distances yet
		
		return HuffmanTree.fromWeightedAlphabet(weights, MAX_CODE_LENGTH);
	}
	
	
	private inline function createCodeLengthTree()
	{
		var weights = new Array<Int>();
		
		for (len in 0 ... 19) {
			weights.push(1);
		}
		
		for (i in 0 ... literalLengthCodes) {
			++weights[Memory.getUI16(scratchAddr + i * 4) & 0xFFFF];
		}
		for (i in 0 ... distanceCodes) {
			++weights[Memory.getUI16(scratchAddr + DISTANCE_OFFSET + i * 4) & 0xFFFF];
		}
		
		return HuffmanTree.fromWeightedAlphabet(weights, MAX_CODE_LENGTH_CODE_LENGTH);
	}
	
	
	// Copies length bytes (all by default) from src into flash.Memory at the specified offset
	private static inline function memcpy(src : ByteArray, offset : Int, length : Int = 0) : Void
	{
		src.readBytes(ApplicationDomain.currentDomain.domainMemory, offset, length);
	}
}


class HuffmanTree
{
	// Each entry contains the code and the code length.
	// The code is stored in the highest 16 bits, and the length in the lowest.
	// The code's bits are reversed (RFC 1951 says Huffman codes get packed in
	// reverse order).
	public var codes : Array<Int>;
	
	
	private function new()
	{
	}
	
	// Creates a Huffman tree for the given weights. The symbols are assumed
	// to be the integers 0...weights.length. Each weight must not exceed 16 bits.
	public static function fromWeightedAlphabet(weights : Array<Int>, maxCodeLength : Int) : HuffmanTree
	{
		return _fromWeightedAlphabet(weights, maxCodeLength);
	}
	
	private static inline function _fromWeightedAlphabet(weights : Array<Int>, maxCodeLength : Int) : HuffmanTree
	{
		if (maxCodeLength > 16) {
			throw new ArgumentsError("Maximum code length must fit into 2 bytes or less");
		}
		
		var codelens = new Array<Int>();
		
		if (weights.length > 0) {
			// Make sure all weights fall between 0 and 65535
			var minWeight = weights[0];
			var maxWeight = weights[0];
			
			for (w in weights) {
				if (w > maxWeight) {
					maxWeight = w;
				}
				else if (w < minWeight) {
					minWeight = w;
				}
			}
			
			// Copy the weights and generate their corresponding symbols
			// The symbols will be stored in the upper 16 bits.
			
			if (maxWeight > 65535) {
				var range : Float = maxWeight - minWeight;
				for (i in 0 ... weights.length) {
					codelens[i] = (i << 16) | (Std.int((weights[i] - minWeight) / range * 65534) & 0xFFFF);
				}
			}
			else {
				for (i in 0 ... weights.length) {
					codelens[i] = (i << 16) | (weights[i] & 0xFFFF);
				}
			}
			
			codelens.sort(byWeightNonDecreasing);
		}
		
		// Calculate unrestricted code lengths
		calculateOptimalCodeLengths(codelens);
		
		// Restrict code lengths
		limitCodeLengths(codelens, maxCodeLength);
		
		// Sort by code length, then by symbol (both decreasing)
		// TODO: codelens are already sorted (or nearly, if some codelengths were limited), but
		// symbols are in increasing order for equal bitlengths. Using a custom insertion sort
		// may be faster.
		// See http://cstheory.stackexchange.com/questions/7420/relation-between-code-length-and-symbol-weight-in-a-huffman-code
		codelens.sort(byCodeLengthAndSymbolDecreasing);
		
		// Calculate the actual codes (canonical Huffman tree).
		// Result is stored in lookup table (by symbol)
		var codes = calculateCanonicalCodes(codelens);
		
		var tree = new HuffmanTree();
		tree.codes = codes;
		return tree;
	}
	
	
	private static function byWeightNonDecreasing(a, b) {
		return (a & 0xFFFF) - (b & 0xFFFF);
	}
	
	private static function byCodeLengthAndSymbolDecreasing(a, b) {
		var result = (b & 0xFFFF) - (a & 0xFFFF);
		if (result == 0) {
			result = (b >>> 16) - (a >>> 16);
		}
		
		return result;
	}
	
	
	// Transforms weights into a correspondingt list of code lengths
	private static inline function calculateOptimalCodeLengths(weights : Array<Int>)
	{
		var n  = weights.length;
		var A = weights;			// Alias
		
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
				setLow16(A, 0, getLow16(A, 0) + getLow16(A, 1));
				root = 0; leaf = 2; next = 1;
				while (next < n-1) {
					/* select first item for a pairing */
					if (leaf>=n || getLow16(A, root) < getLow16(A, leaf)) {
						setLow16(A, next, getLow16(A, root));
						setLow16(A, root++, next);
					} else {
						setLow16(A, next, getLow16(A, leaf++));
					}

					/* add on the second item */
					if (leaf>=n || (root < next && getLow16(A, root) < getLow16(A, leaf))) {
						setLow16(A, next, getLow16(A, next) + getLow16(A, root));
						setLow16(A, root++, next);
					} else {
						setLow16(A, next, getLow16(A, next) + getLow16(A, leaf++));
					}
					
					++next;
				}
				
				/* second pass, right to left, setting internal depths */
				setLow16(A, n - 2, 0);
				next = n - 3;
				while (next>=0) {
					setLow16(A, next, getLow16(A, getLow16(A, next)) + 1);
					--next;
				}
				
				/* third pass, right to left, setting leaf depths */
				avbl = 1; used = dpth = 0; root = n-2; next = n-1;
				while (avbl>0) {
					while (root>=0 && getLow16(A, root)==dpth) {
						++used; --root;
					}
					while (avbl>used) {
						setLow16(A, next--, dpth);
						--avbl;
					}
					avbl = 2*used; ++dpth; used = 0;
				}
			}
			else {		// n == 1
				setLow16(A, 0, 0);
			}
		}
	}
	
	
	private static inline function getLow16(a : Array<Int>, i : Int) : Int
	{
		return a[i] & 0xFFFF;
	}
	
	private static inline function setLow16(a : Array<Int>, i : Int, v : Int)
	{
		a[i] = (a[i] & 0xFFFF0000) | v;
	}
	
	
	private static inline function limitCodeLengths(codelens : Array<Int>, max : Int)
	{
		// Uses (non-optimal) heuristic algorithm described at http://cbloomrants.blogspot.com/2010/07/07-03-10-length-limitted-huffman-codes.html
		
		// Assumes codelens is sorted in non-decreasing order by weight
		
		var overflow = false;
		
		// Set code lengths > max to max
		for (i in 0 ... codelens.length) {
			if ((codelens[i] & 0xFFFF) > max) {
				setLow16(codelens, i, max);
				overflow = true;
			}
		}
		
		if (overflow) {
			// Calculate Kraft number
			var K = 0.0;
			for (i in 0 ... codelens.length) {
				K += Math.pow(2, -getLow16(codelens, i));
			}
			
			// Pass 1
			var i = 0;
			while (K > 1 && i < codelens.length) {
				while (getLow16(codelens, i) < max && K > 1) {
					++codelens[i];		// Only affects lower 16 bits
					
					// adjust K for change in codeLen
					K -= Math.pow(2, -getLow16(codelens, i));
				}
				++i;
			}
			
			// Pass 2
			i = codelens.length - 1;
			while (i >= 0) {
				while ((K + Math.pow(2, -getLow16(codelens, i))) <= 1) {
					// adjust K for change in codeLen
					K += Math.pow(2, -getLow16(codelens, i));
					
					--codelens[i];		// Only affects lower 16 bits
				}
				
				--i;
			}
		}
	}
	
	
	// Input is expected to be in sorted order, first by code length (decreasing), then by symbol (decreasing)
	private static inline function calculateCanonicalCodes(codelens : Array<Int>) : Array<Int>
	{
		// Implements algorithm found on Wikipedia: http://en.wikipedia.org/wiki/Canonical_Huffman_code
		
		var table = new Array<Int>();
		if (codelens.length != 0) {
			// Iterate over symbols in reverse order (i.e. increasing codelength)
			var i = codelens.length - 1;
			var code = 0;
			var curLen = codelens[i] & 0xFFFF;
			while (i >= 0) {
				var s = codelens[i];
				var newLen = s & 0xFFFF;
				code <<= newLen - curLen;
				table[s >>> 16] = (reverseBits(code, newLen) << 16) | newLen;
				++code;
				curLen = newLen;
				if (code >= (1 << curLen)) {
					// We overflowed the current bit length by incrementing
					++curLen;
				}
				
				--i;
			}
		}
		
		return table;
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
