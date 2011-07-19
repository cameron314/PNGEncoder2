package;

import DeflateStream;


class DeflateStreamTests
{
	public static function run()
	{
		testSimpleHuffmanTree();
	}
	
	
	private static function testSimpleHuffmanTree()
	{
		// Arrange
		var weights = [ 1, 0, 2, 2 ];
		
		// Act
		var tree = HuffmanTree.fromWeightedAlphabet(weights);
		
		// Assert
		assert(tree.codes.length == 4);
		assert((tree.codes[0] >>> 16) == 2);		// Code
		assert((tree.codes[0] & 0xFFFF) == 2);		// Length
		assert((tree.codes[1] >>> 16) == 0);
		assert((tree.codes[1] & 0xFFFF) == 1);
		assert(((tree.codes[2] >>> 16) & 0xFFFFFFFE) == 6);
		assert((tree.codes[2] & 0xFFFF) == 3);
		assert(((tree.codes[3] >>> 16) & 0xFFFFFFFE) == 6);
		assert((tree.codes[3] & 0xFFFF) == 3);
	}
	
	
	private static function assert(condition, message = "Assertion failed")
	{
		if (!condition) {
			throw message;
		}
	}
}
