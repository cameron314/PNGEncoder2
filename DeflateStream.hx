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
	public inline function WriteBlock(bytes : ByteArray, lastBlock = false)
	{
		BeginBlock(lastBlock);
		Update(bytes);
		EndBlock();
	}
	
	
	public inline function BeginBlock(lastBlock = false)
	{
		freshBlock = true;
		
		if (level == CompressionLevel.UNCOMPRESSED) {
			stream.writeByte(lastBlock ? 1 : 0);		// Uncompressed
		}
	}
	
	
	// Updates the current block with the compressed representation of bytes
	// Only a maximum of MAX_UNCOMPRESSED_BYTES_PER_BLOCK bytes will be written when using UNCOMPRESSED level
	public inline function Update(bytes : ByteArray)
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
			}
			
			// TODO
		}
		
		freshBlock = false;
	}
	
	
	public inline function EndBlock()
	{
		if (level != UNCOMPRESSED) {
			// TODO
		}
	}
	
	
	// Call only once. After called, no other methods can be called
	public inline function Finalize() : ByteArray
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
	private inline function WriteBits(bits : UInt, bitCount : UInt)
	{
	}
}














