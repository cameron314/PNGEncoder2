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

package;

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
	
	private var level : CompressionLevel;
	private var zlib : Bool;
	private var stream : ByteArray;
	private var freshBlock : Bool;
	
	// For calculating Adler-32 sum
	private var s1 : UInt;
	private var s2 : UInt;
	
	private var literalLengthLookup : Array<UInt>;
	
	
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
		
		if (level == CompressionLevel.UNCOMPRESSED) {
			stream.writeByte(lastBlock ? 1 : 0);		// Uncompressed
		}
		else {
			writeBits(4 | (lastBlock ? 1 : 0), 3);		// Dynamic Huffman tree compression
		}
	}
	
	
	// Updates the current block with the compressed representation of bytes
	// Only a maximum of MAX_UNCOMPRESSED_BYTES_PER_BLOCK bytes will be written when using UNCOMPRESSED level
	public inline function update(bytes : ByteArray)
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
				// Write Huffman trees
				
				// No lengths for now, just literals + EOB
				var literalLengthTree = createLiteralLengthTree(bytes);
				literalLengthLookup = literalLengthTree.toLookupTable();
				
				// TODO: Write the trees into the stream as per RFC 1951
			}
			
			// TODO
		}
		
		freshBlock = false;
	}
	
	
	public inline function endBlock()
	{
		if (level != UNCOMPRESSED) {
			// TODO: Write end of block symbol
		}
	}
	
	
	// Call only once. After called, no other methods can be called
	public inline function finalize() : ByteArray
	{
		// TODO: Flush stream (pad with zero bits until next byte boundary)
		
		if (zlib) {
			stream.endian = BIG_ENDIAN;		// Network byte order (RFC 1950)
			
			var adlerSum = (s2 << 16) | s1;
			stream.writeUnsignedInt(adlerSum);
		}
		
		stream.position = 0;
		return stream;
	}
	
	
	
	
	
	// Writes up to 8 bits into the stream
	private inline function writeBits(bits : UInt, bitCount : UInt)
	{
		// TODO
	}
	
	
	private inline function createLiteralLengthTree(sampleData : ByteArray)
	{
		var alphabet = new Array<SymbolWeightPair>();
		
		for (value in 0 ... 256) {		// Literals
			alphabet.push(new SymbolWeightPair(value, 10));
		}
		
		// Weight EOB less than more common literals
		alphabet.push(new SymbolWeightPair(256, 1));
		
		// No length codes yet
		
		
		// Sample given bytes to estimate literal weights
		var sampleFrequency;
		if (sampleData.bytesAvailable <= 1024) {
			// Small sample, calculate exactly
			sampleFrequency = 1;
			while (sampleData.bytesAvailable != 0) {
				++alphabet[sampleData.readByte() & 0xFF].weight;
			}
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
			++alphabet[sampleData.readByte() & 0xFF].weight;
		}
		
		sampleData.position = oldPosition;
		
		return HuffmanTree.fromWeightedAlphabet(alphabet);
	}
}


class SymbolWeightPair {
	public var symbol : UInt;
	public var weight : UInt;
	
	public function new(symbol : UInt, weight : UInt)
	{
		this.symbol = symbol;
		this.weight = weight;
	}
}


class HuffmanNode extends SymbolWeightPair {
	public var left : HuffmanNode;
	public var right : HuffmanNode;
	public var lexicographicIndex : UInt;
	
	public function new(lexicographicIndex : UInt, symbol : UInt, weight : UInt)
	{
		super(symbol, weight);
		
		this.lexicographicIndex = lexicographicIndex;
	}
	
	
	public function height() : Int
	{
		if (left == null && right == null) {
			return 0;
		}
		
		var leftHeight = left == null ? -1 : left.height();
		var rightHeight = right == null ? -1 : right.height();
		
		return Std.int(Math.max(leftHeight, rightHeight)) + 1;
	}
}


class HuffmanTree
{
	private var root : HuffmanNode;
	
	private function new()
	{
	}
	
	public static function fromWeightedAlphabet(alphabet : Array<SymbolWeightPair>) : HuffmanTree
	{
		// TODO: Limit tree height to maxiumum 15 (i.e. codelength 15)
		
		
		var tree = new HuffmanTree();
		if (alphabet.length == 0) {
			tree.root = new HuffmanNode(null, null, null);
			return tree;
		}
		
		
		var pool = new Array<HuffmanNode>();
		for (i in 0 ... alphabet.length) {
			var entry = alphabet[i];
			pool.push(new HuffmanNode(i, entry.symbol, entry.weight));
		}
		
		if (pool.length == 1) {
			tree.root = new HuffmanNode(null, null, null);
			tree.root.left = pool.pop();
			return tree;
		}
		
		while (pool.length != 1) {
			var lowest = pool[0];
			var secondLowest = pool[1];
			
			if (precedesByWeight(secondLowest, lowest)) {
				secondLowest = pool[0];
				lowest = pool[1];
			}
			
			for (i in 2 ... pool.length) {
				if (precedesByWeight(pool[i], lowest)) {
					secondLowest = lowest;
					lowest = pool[i];
				}
				else if (precedesByWeight(pool[i], secondLowest)) {
					secondLowest = pool[i];
				}
			}
			
			pool.remove(lowest);
			pool.remove(secondLowest);
			
			var combined = new HuffmanNode(
				lowest.lexicographicIndex,
				null,
				lowest.weight + secondLowest.weight
			);
			
			var lHeight = lowest.height();
			var l2Height = secondLowest.height();
			if (lHeight < l2Height || lHeight == l2Height && lowest.lexicographicIndex < secondLowest.lexicographicIndex) {
				combined.left = lowest;
				combined.right = secondLowest;
			}
			else {
				combined.left = secondLowest;
				combined.right = lowest;
			}
			
			pool.push(combined);
		}
		
		tree.root = pool.pop();
		return tree;
	}
	
	
	// Each entry in the table contains the code and the code length.
	// The code is stored in the lowest 16 bits, and the length in the highest.
	public function toLookupTable() : Array<UInt>
	{
		var table = new Array<UInt>();
		if (root.left == null && root.right == null) {
			return table;		// Empty tree, no leaves
		}
		
		var nodeStack = new Array<HuffmanNode>();
		var pathStack = new Array<UInt>();
		var pathLengthStack = new Array<UInt>();
		
		nodeStack.push(root);
		pathStack.push(0);
		pathLengthStack.push(0);
		
		while (nodeStack.length != 0) {
			var current = nodeStack.pop();
			var path = pathStack.pop();
			var pathLength = pathLengthStack.pop();
			
			if (current.left == null && current.right == null) {
				// Leaf node (i.e. symbol)
				table[current.symbol] = pathLength << 16 | path;
			}
			else {
				pathLengthStack.push(pathLength + 1);
				
				if (current.right != null) {
					pathStack.push((path << 1) | 1);
					nodeStack.push(current.right);
				}
				if (current.left != null) {
					pathStack.push(path << 1);
					nodeStack.push(current.left);
				}
			}
		}
		
		return table;
	}
	
	
	public inline function height() : Int
	{
		return root.height();
	}
	
	
	private static inline function precedesByWeight(node1, node2)
	{
		return
			node1.weight < node2.weight ||
			node1.weight == node2.weight && node1.lexicographicIndex < node2.lexicographicIndex
		;
	}
}









