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


// Compliant with RFC 1950 (ZLIB) and RFC 1951 (DEFLATE)
class DeflateStream
{
	public static inline var MAX_UNCOMPRESSED_BYTES_PER_BLOCK : UInt = 65535;
	
	private static inline var ADDLER_MAX : UInt = 65521;		// Largest prime smaller than 65536
	private static inline var MAX_CODE_LENGTH : UInt = 15;
	private static inline var MAX_CODE_LENGTH_CODE_LENGTH : UInt = 7;
	private static inline var CODE_LENGTH_ORDER = [ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 ];
	private static inline var EOB = 256;		// End of block symbol
	
	private var level : CompressionLevel;
	private var zlib : Bool;
	private var stream : ByteArray;
	private var freshBlock : Bool;
	
	// For calculating Adler-32 sum
	private var s1 : UInt;
	private var s2 : UInt;
	
	private var literalLengthLookup : Array<Int>;
	private var distanceLookup : Array<Int>;
	
	private var bitBuffer : UInt;
	private var bitBufferLength : Int;
	
	// TODO: Add compression ratio (keep track of bits written vs. bits seen)
	
	
	public function new(level : CompressionLevel, writeZLIBInfo = false)
	{
		this.level = level;
		this.zlib = writeZLIBInfo;
		
		stream = new ByteArray();
		stream.endian = LITTLE_ENDIAN;		// See RFC 1951
		
		freshBlock = false;
		s1 = 1;
		s2 = 0;
		
		if (zlib) {
			stream.writeByte(0x78);		// CMF with compression method 8 (deflate) 32K sliding window
			stream.writeByte(0x9C);		// FLG: Check bits, no dict, default algorithm
		}
	}
	
	
	// Helper method. Same as sequentially calling BeginBlock, Update, and EndBlock
	public inline function writeBlock(bytes : ByteArray, lastBlock = false)
	{
		beginBlock(lastBlock);
		update(bytes);
		endBlock();
	}
	
	
	public inline function beginBlock(lastBlock = false)
	{
		freshBlock = true;
		
		bitBuffer = 0;
		bitBufferLength = 0;
		
		if (level == CompressionLevel.UNCOMPRESSED) {
			stream.writeByte(lastBlock ? 1 : 0);		// Uncompressed
		}
		else {
			writeBits(4 | (lastBlock ? 1 : 0), 3);		// Dynamic Huffman tree compression
		}
	}
	
	
	// Updates the current block with the compressed representation of bytes
	// Only a maximum of MAX_UNCOMPRESSED_BYTES_PER_BLOCK bytes will be written when using UNCOMPRESSED level
	public function update(bytes : ByteArray)
	{
		if (level == CompressionLevel.UNCOMPRESSED) {
			var len = Std.int(Math.min(bytes.bytesAvailable, MAX_UNCOMPRESSED_BYTES_PER_BLOCK));
			
			if (freshBlock) {
				// Write uncompressed header info
				stream.writeShort(len);
				stream.writeShort(~len);
			}
			
			var byte : UInt;
			for (i in 0...len) {
				byte = bytes.readByte() & 0xFF;		// Because sometimes the other bytes of the returned int are garbage
				
				if (zlib) {
					s1 = (s1 + byte) % ADDLER_MAX;
					s2 = (s2 + s1) % ADDLER_MAX;
				}
				
				stream.writeByte(byte);
			}
		}
		else {
			if (freshBlock) {
				// Write Huffman trees into the stream as per RFC 1951
				
				var literalLengthTree = createLiteralLengthTree(bytes);
				literalLengthLookup = literalLengthTree.codes;
				
				if (distanceLookup == null) {
					var distanceTree = createDistanceTree();
					distanceLookup = distanceTree.codes;
				}
				
				var codeLengthLookup = createCodeLengthTree(literalLengthLookup, distanceLookup).codes;
				
				// HLIT
				writeBits(literalLengthLookup.length - 257, 5);
				
				// HDIST
				if (distanceLookup.length == 0) {
					writeBits(0, 5);		// Minimum one distance code
				}
				else {
					writeBits(distanceLookup.length - 1, 5);
				}
				
				// HCLEN
				writeBits(codeLengthLookup.length - 4, 4);
				
				// Write code lengths of code length code
				for (rank in CODE_LENGTH_ORDER) {
					writeBits(codeLengthLookup[rank] & 0xFFFF, 3);
				}
				
				// Write (compressed) code lengths of literal/length codes
				for (code in literalLengthLookup) {
					writeSymbol(code, codeLengthLookup);
				}
				
				// Write (compressed) code lengths of distance codes
				if (distanceLookup.length == 0) {
					writeSymbol(0, codeLengthLookup);
				}
				else {
					for (code in distanceLookup) {
						writeSymbol(code, codeLengthLookup);
					}
				}
			}
			
			// TODO: Use LZ77 (depending on compression settings)
			
			// Write data
			var byte : UInt;
			for (i in 0 ... bytes.bytesAvailable) {
				byte = bytes.readByte() & 0xFF;		// Because sometimes the other bytes of the returned int are garbage
				
				if (zlib) {
					s1 = (s1 + byte) % ADDLER_MAX;
					s2 = (s2 + s1) % ADDLER_MAX;
				}
				
				writeSymbol(byte, literalLengthLookup);
			}
		}
		
		freshBlock = false;
	}
	
	
	public inline function endBlock()
	{
		if (level != UNCOMPRESSED) {
			writeSymbol(EOB, literalLengthLookup);
		}
	}
	
	
	public inline function writeEmptyBlock(lastBlock : Bool)
	{
		var currentLevel = level;
		level = UNCOMPRESSED;
		writeBlock(new ByteArray(), lastBlock);
		level = currentLevel;
	}
	
	
	// Call only once. After called, no other methods can be called
	public inline function finalize() : ByteArray
	{
		// Flush stream
		while (bitBufferLength > 0) {
			stream.writeByte(bitBuffer);
			bitBuffer >>= 8;
			bitBufferLength -= 8;
		}
		
		if (zlib) {
			stream.endian = BIG_ENDIAN;		// Network byte order (RFC 1950)
			
			var adlerSum = (s2 << 16) | s1;
			stream.writeUnsignedInt(adlerSum);
		}
		
		stream.position = 0;
		return stream;
	}
	
	
	
	// Writes up to 32 bits into the stream (bits must be zero-padded)
	private inline function writeBits(bits : UInt, bitCount : UInt)
	{
		// TODO: Optimize by removing the if -- instead of buffering in int,
		// just use fast memory directly.
		
		bitBuffer |= bits << bitBufferLength;
		bitBufferLength += bitCount;
		
		
		if (bitBufferLength >= 32) {
			bitBufferLength -= 32;
			stream.writeByte(bitBuffer);
			stream.writeByte(bitBuffer >>> 8);
			stream.writeByte(bitBuffer >>> 16);
			stream.writeByte(bitBuffer >>> 24);
			bitBuffer = bits >>> (bitCount - bitBufferLength);
		}
	}
	
	private inline function writeSymbol(symbol : Int, lookup : Array<Int>)
	{
		var compressed = lookup[symbol & 0xFFFF];		// For codes, gets codelength. All symbols are <= 2 bytes
		writeBits(compressed >>> 16, compressed & 0xFFFF);
	}
	
	
	private static inline function createLiteralLengthTree(sampleData : ByteArray)
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
		var sampleFrequency;
		if (sampleData.bytesAvailable <= 1024) {
			// Small sample, calculate exactly
			sampleFrequency = 1;
		}
		else if (sampleData.bytesAvailable <= 20 * 1024) {
			sampleFrequency = 20;	// Sample every 20th byte
		}
		else {
			sampleFrequency = 30;
		}
		
		var oldPosition = sampleData.position;
		var samples = Math.floor(sampleData.bytesAvailable / sampleFrequency);
		for (i in 0 ... samples) {
			sampleData.position = oldPosition + i * sampleFrequency;
			++weights[sampleData.readByte() & 0xFF];
		}
		
		sampleData.position = oldPosition;
		
		return HuffmanTree.fromWeightedAlphabet(weights, MAX_CODE_LENGTH);
	}
	
	
	private static inline function createDistanceTree()
	{
		var weights = new Array<UInt>();
		
		// No distances yet
		
		return HuffmanTree.fromWeightedAlphabet(weights, MAX_CODE_LENGTH);
	}
	
	
	private static inline function createCodeLengthTree(literalLengthLookup : Array<Int>, distanceLookup : Array<Int>)
	{
		var weights = new Array<UInt>();
		
		for (len in 0 ... 19) {
			weights.push(1);
		}
		
		for (code in literalLengthLookup) {
			++weights[code & 0xFFFF];
		}
		for (code in distanceLookup) {
			++weights[code & 0xFFFF];
		}
		
		return HuffmanTree.fromWeightedAlphabet(weights, MAX_CODE_LENGTH_CODE_LENGTH);
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
	public static function fromWeightedAlphabet(weights : Array<UInt>, maxCodeLength : Int) : HuffmanTree
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
			
			// Sort by weight non-decreasing
			codelens.sort(function (a, b) return (a & 0xFFFF) - (b & 0xFFFF));
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
		codelens.sort(function (a, b) {
			var result = (b & 0xFFFF) - (a & 0xFFFF);
			if (result == 0) {
				result = (b >>> 16) - (a >>> 16);
			}
			
			return result;
		});
		
		// Calculate the actual codes (canonical Huffman tree).
		// Result is stored in lookup table (by symbol)
		var codes = calculateCanonicalCodes(codelens);
		
		var tree = new HuffmanTree();
		tree.codes = codes;
		return tree;
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
	private static inline function reverseBits(v : UInt, count : UInt) : UInt
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
