import java.util.Scanner
import java.io.File
import java.io._
import scala.util.matching.Regex 
import com.typesafe.config._

object resultProcess {
  def main(args:Array[String]){
    var confFileName="resultProcess.conf";
	  var confDir="C://Users//wangc//workspace//ResultProcess//Scala//PostProcess//src//main//resource//";
	  val myConfigFile = new File(confDir+confFileName);	  
	  val fileConfig = ConfigFactory.parseFile(myConfigFile);
	  val config = ConfigFactory.load(fileConfig);
	  
	  var prefix="bioSensor."     
	  var objDir=config.getString(prefix+"objDir");
	  var outPut=objDir+config.getString(prefix+"objFile");
    var srcDir=config.getString(prefix+"srcDir");   
    var filePattern=config.getString(prefix+"filePattern");
    var metricPattern=config.getString(prefix+"metricPattern"); 
    var func=config.getString(prefix+"function");
    if (func=="getOutlier"){
    	getOutlier(srcDir,outPut,filePattern,metricPattern);  
    } else {
    	getMetrics(srcDir,outPut,filePattern);  
    }
        
  }
  
  def getOutlier(srcDir:String,outPut:String,filePattern:String,metricPattern:String){
    val writer = new PrintWriter(new File(outPut));
    writer.write("year month date hour status nn radius \n");
	  val srcDirFile=new File(srcDir);
	  if (srcDirFile.exists && srcDirFile.isDirectory) {
			val tempFiles=srcDirFile.listFiles.toList;			
			for (fileIter<-tempFiles){
			  val tPattern=new Regex(filePattern);			   
				var subFileName=fileIter.getName;
				println(subFileName);				
				var objFilePattern= tPattern findFirstIn subFileName;
				if (!objFilePattern.isEmpty){
					var tp1=new Regex(metricPattern);	
					var tp2=new Regex("_\\d+");
					var radius=tp2.findFirstIn(subFileName).getOrElse("_0").stripPrefix("_").toInt; 
					var test1=srcDir+subFileName;				   
					val tempFile = scala.io.Source.fromFile(test1);
					var tLine=tempFile.getLines();
					var metric=tLine.filter(x=>tp1.findAllIn(x).isEmpty==false).toArray
							.map(_.stripPrefix("----------")+"-"+radius+"\n").map(_.split("-").mkString(" ")).mkString;				   
					writer.write(metric);

				}			   			   
			}
	  }
	  writer.close;
  }
  
  def getMetrics(srcDir:String,outPut:String,filePattern:String){
    var avgRes=Array[Double]();
    val writer = new PrintWriter(new File(outPut));
	  val srcDirFile=new File(srcDir);
	  if (srcDirFile.exists && srcDirFile.isDirectory) {
			val tempFiles=srcDirFile.listFiles.toList;
			for (fileIter<-tempFiles){
			  val tPattern=new Regex(filePattern);
			   //val tPattern=new Regex("\\d{5}");
			   var subFileName=fileIter.getName;
			   var objFilePattern= tPattern findFirstIn subFileName;
			   if (!objFilePattern.isEmpty){
			     var tpattern1=new Regex("\\d+");
				   //var objFileName=objFilePattern.mkString(",").toInt+".csv";
				   //println(objFileName)
				   var tp1=new Regex("memory usage is");
				   var tp2=new Regex("\\d+.\\d+");
				   var test1=srcDir+subFileName;
				   
				   val tempFile = scala.io.Source.fromFile(test1);
				   var tLine=tempFile.getLines();
				   var metric=tLine.filter(x=>tp1.findAllIn(x).isEmpty==false)				   
						   .toArray.map(x=>tp2.findAllIn(x).toArray);
				   var avgMem=metric.map(x=>x(0).toDouble).sum/metric.length;
				   var avgCpu=metric.map(x=>x(1).toDouble).sum/metric.length;
				   var x=tp2.findAllIn(subFileName).next;
				   writer.write(x+","+avgMem+","+avgCpu+"\n");
			   }			   
			   
			}
	  }
	  writer.close;
  }
  
}