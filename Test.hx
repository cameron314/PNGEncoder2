package;
import flash.Boot;
import flash.display.Bitmap;
import flash.display.BitmapData;
import flash.display.Loader;
import flash.display.PixelSnapping;
import flash.display.Sprite;
import flash.events.Event;
import flash.events.IOErrorEvent;
import flash.events.MouseEvent;
import flash.Lib;
import flash.Memory;
import flash.net.FileReference;
import flash.text.TextField;
import flash.text.TextFieldAutoSize;
import flash.utils.ByteArray;
import flash.utils.Timer;
import flash.Vector;
import PNGEncoder2;
import DeflateStream;
//import com.remixtechnology.SWFProfiler;

class Test extends Sprite
{
	private static inline var MARGIN = 10;
	
	private var loadFileRef : FileReference;
	private var saveFileRef : FileReference;
	private var loader : Loader;
	private var encoder : PNGEncoder2;
	private var encoders : Vector<PNGEncoder2>;
	
	public static function main()
	{
		Lib.current.addChild(new Test(200, 200));
	}
	
	public function new(?width : Int, ?height : Int)
	{
		super();
		
		//testManyQueuedSmallImages();
		//return;
		
		//SWFProfiler.init();
		//SWFProfiler.show();
		
		DeflateStreamTests.run();
		trace("Tests passed\n");
		
		if (width == null) width = 1024;
		if (height == null) height = 2048;
		
		var bmp = new BitmapData(width, height, true, 0x00FFFFFF);
		bmp.perlinNoise(width, height, 2, 0xDEADBEEF, false, false);
		
		var display = new Bitmap(bmp);
		display.x = MARGIN;
		display.y = 250;
		addChild(display);
		
		//PNGEncoder2.level = CompressionLevel.GOOD;
		doBenchmark(bmp);
		
		/*
		var that = this;
		//PNGEncoder2.level = CompressionLevel.FAST;
		
		PNGEncoder2.encode(bmp);		// Warm up
		
		// Start encoding after first frame (avoids ~400ms delay)
		var frameCount = 0;
		addEventListener(Event.ENTER_FRAME, function (e:Event) {
			++frameCount;
			
			if (frameCount == 1) {
				var startTime = Lib.getTimer();
				var encoder = PNGEncoder2.encodeAsync(bmp);
				encoder.addEventListener(Event.COMPLETE, function (e) {
					trace("Async complete (" + (Lib.getTimer() - startTime) + "ms)");
					
					var loader = new Loader();
					loader.loadBytes(encoder.png);
					that.addChild(loader);
					loader.x = MARGIN + bmp.width + 10;
					loader.y = 250;
					
					that.doubleClickEnabled = true;
					that.addEventListener(MouseEvent.DOUBLE_CLICK, function (e2) {
						var fileReference = new FileReference();
						fileReference.save(encoder.png, "image.png");
					});
				});
			}
		});
		*/
		
		/*
		//PNGEncoder2.level = CompressionLevel.FAST;
		var png = PNGEncoder2.encode(bmp);
		trace("Sync complete");
		var loader = new Loader();
		loader.loadBytes(png);
		addChild(loader);
		doubleClickEnabled = true;
		addEventListener(MouseEvent.DOUBLE_CLICK, function (e2) {
			var fileReference = new FileReference();
			fileReference.save(png, "image.png");
		});*/
	}
	
	
	private function doBenchmark(bmp)
	{
		// Warm up
		var data1 = PNGEncoder.encode(bmp);
		var data2 = PNGEncoder2.encode(bmp);
		
		loader = new Loader();
		/*loader.addEventListener(IOErrorEvent.IO_ERROR, function (e) {
			trace("Error reading PNG that was compressed with optimized encoder\n");
		});*/
		
		var that = this;
		doubleClickEnabled = true;
		addEventListener(MouseEvent.DOUBLE_CLICK, function (e) {
			/*
			var fileReference = new FileReference();
			fileReference.save(data2, "test_png.png");
			//*/
			
			//*
			that.loadFileRef = new FileReference();
			that.loadFileRef.addEventListener(Event.SELECT, function (e2) {
				that.loadFileRef.load();
			});
			
			that.loadFileRef.addEventListener(Event.COMPLETE, function (e2) {
				that.loader = new Loader();
				that.loader.contentLoaderInfo.addEventListener(Event.COMPLETE, function (e3) {
					var bmp = new BitmapData(Std.int(that.loader.width), Std.int(that.loader.height), true, 0x00FFFFFF);
					bmp.draw(that.loader);
					
					that.encoder = PNGEncoder2.encodeAsync(bmp);
					var startTime = Lib.getTimer();
					that.encoder.addEventListener(Event.COMPLETE, function (e : Event) {
						var percent = 100 - e.target.png.length / (bmp.width * bmp.height * 4) * 100;
						trace("Async complete (" + (Lib.getTimer() - startTime) + "ms; " + Std.int(percent) + "%)");
						
						/*var loader = new Loader();
						loader.loadBytes(e.target.png);
						that.addChild(loader);
						loader.x = 250;
						loader.y = 250;*/
						startTime = Lib.getTimer();
						var decodedBitmapData = PNGEncoder2.decode(cast(e.target.png, ByteArray));
						e.target.png.position = 0;
						var decodedBitmap = new Bitmap(decodedBitmapData);
						trace("Decode complete (" + (Lib.getTimer() - startTime) + "ms)");
						var decoded = new Sprite();
						decoded.addChild(decodedBitmap);
						that.addChild(decoded);
						decoded.x = 250;
						decoded.y = 250;
						
						decoded.doubleClickEnabled = true;
						decoded.addEventListener(MouseEvent.DOUBLE_CLICK, function (e4) {
							that.saveFileRef = new FileReference();
							e4.stopPropagation();
							that.saveFileRef.save(that.encoder.png, "image.png");
						});
					});
				});
				
				that.loader.loadBytes(that.loadFileRef.data);
			});
			
			that.loadFileRef.browse();
			
			//*/
		});
		
		//*
		loader.loadBytes(data2);
		loader.x = MARGIN + bmp.width + 10;
		loader.y = 250;
		addChild(loader);
		//*/
		
		
		trace("Encoders yield same bytes: " + compare(data1, data2) + "\n");
		trace("PNGEncoder byte count:\t\t\t" + data1.length);
		trace("PNGEncoder2 byte count:\t\t\t" + data2.length);
		trace("% better:\t\t\t\t\t\t\t\t" + round((1 - 1.0 * data2.length / data1.length) * 100, 1) + "%\n");
		
		var runs = 5;
		var pngTime = testPNGEncoder(bmp, runs);
		var opPngTime = testOptimizedPNGEncoder(bmp, runs);
		
		trace("PNGEncoder:\t\t\t" + pngTime + "ms");
		trace("PNGEncoder2:\t\t\t" + opPngTime + "ms");
		trace("x better:\t\t\t\t\t" + round(1.0 * pngTime / opPngTime, 2) + "x\n");
	}
	
	
	private function testManyQueuedSmallImages()
	{
		var bmp = new BitmapData(80, 80, true, 0x00FFFFFF);
		bmp.perlinNoise(80, 80, 2, 0xDEADBEEF, false, false);
		
		encoders = new Vector<PNGEncoder2>();
		for (i in 0 ... 14) {
			var encoder = PNGEncoder2.encodeAsync(bmp);
			encoder.targetFPS = 13;
			encoders.push(encoder);
		}
	}
	
	
	private static function testPNGEncoder(bmp : BitmapData, runs : Int) : Int
	{
		return time(makeCallback(PNGEncoder.encode, bmp), runs);
	}
	
	private static function round(num : Float, decimalPlaces : Int) : Float
	{
		var multiplier = Math.pow(10, decimalPlaces);
		return Math.round(num * multiplier) / multiplier;
	}
	
	private static function compare(a : ByteArray, b : ByteArray) : Bool
	{
		if (a == b) {
			return true;
		}
		
		if (a == null || b == null) {
			return false;
		}
		
		if (a.length != b.length) {
			return false;
		}
		
		for (i in 0...a.length) {
			if (a.readUnsignedByte() != b.readUnsignedByte()) {
				return false;
			}
		}
		
		return true;
	}
	
	private static function makeCallback(encode : BitmapData -> Dynamic, bmp : BitmapData) : Void -> Dynamic
	{
		return function() {
			return encode(bmp);
		};
	}
	
	private static function testOptimizedPNGEncoder(bmp : BitmapData, runs : Int) : Int
	{
		return time(makeCallback(PNGEncoder2.encode, bmp), runs);
	}
	
	
	private static function time(func : Void -> Dynamic, runs : Int) : Int
	{
		var times = [  ];
		
		for (i in 0...runs) {
			var start = Lib.getTimer();
			func();
			var stop = Lib.getTimer();
			times.push(stop - start);
		}
		
		// Use smallest time as benchmark result
		var smallest = times[0];
		for (time in times) {
			if (time < smallest) {
				smallest = time;
			}
		}
		
		return smallest;
	}
}
