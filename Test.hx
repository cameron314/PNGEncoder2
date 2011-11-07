package;
import com.remixtechnology.SWFProfiler;
import flash.Boot;
import flash.display.Bitmap;
import flash.display.BitmapData;
import flash.display.Loader;
import flash.display.Sprite;
import flash.events.Event;
import flash.events.IOErrorEvent;
import flash.events.MouseEvent;
import flash.Lib;
import flash.net.FileReference;
import flash.text.TextField;
import flash.text.TextFieldAutoSize;
import flash.utils.ByteArray;
import flash.utils.Timer;
import PNGEncoder2;
import DeflateStream;

class Test extends Sprite
{
	private static inline var MARGIN = 10;
	
	
	public static function main()
	{
		Lib.current.addChild(new Test(200, 200));
	}
	
	
	public function new(?width : Int, ?height : Int)
	{
		super();
		
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
		
		var loader = new Loader();
		/*loader.addEventListener(IOErrorEvent.IO_ERROR, function (e) {
			trace("Error reading PNG that was compressed with optimized encoder\n");
		});*/
		
		var that = this;
		doubleClickEnabled = true;
		addEventListener(MouseEvent.DOUBLE_CLICK, function (e) {
			//*
			var fileReference = new FileReference();
			fileReference.save(data2, "test_png.png");
			//*/
			
			/*
			var fileReference = new FileReference();
			fileReference.addEventListener(Event.SELECT, function (e2) {
				fileReference.load();
			});
			
			fileReference.addEventListener(Event.COMPLETE, function (e2) {
				var loader = new Loader();
				loader.contentLoaderInfo.addEventListener(Event.COMPLETE, function (e3) {
					var bmp = new BitmapData(Std.int(loader.width), Std.int(loader.height), true, 0x00FFFFFF);
					bmp.draw(loader);
					
					var encoder = PNGEncoder2.encodeAsync(bmp);
					var startTime = Lib.getTimer();
					encoder.addEventListener(Event.COMPLETE, function (e) {
						var percent = 100 - encoder.png.length / (bmp.width * bmp.height * 4) * 100;
						trace("Async complete (" + (Lib.getTimer() - startTime) + "ms; " + Std.int(percent) + "%)");
						
						var loader = new Loader();
						loader.loadBytes(encoder.png);
						that.addChild(loader);
						loader.x = 250;
						loader.y = 250;
						
						that.doubleClickEnabled = true;
						that.addEventListener(MouseEvent.DOUBLE_CLICK, function (e2) {
							var fileReference = new FileReference();
							fileReference.save(encoder.png, "image.png");
						});
					});
				});
				
				loader.loadBytes(fileReference.data);
			});
			
			fileReference.browse();
			
			*/
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
	
	
	private static function testPNGEncoder(bmp : BitmapData, runs : Int) : Int
	{
		return time(callback(PNGEncoder.encode, bmp), runs);
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
	
	
	private static function testOptimizedPNGEncoder(bmp : BitmapData, runs : Int) : Int
	{
		return time(callback(PNGEncoder2.encode, bmp), runs);
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
		
		if (runs == 1) {
			return times[0];
		}
		
		// Sum times, and find the largest time (will be excluded)
		var largest = 0;
		var total = 0;
		for (time in times) {
			total += time;
			if (time > largest) {
				largest = time;
			}
		}
		
		return Std.int((total - largest) / (runs - 1));
	}
}
