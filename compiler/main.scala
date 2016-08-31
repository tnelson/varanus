package edu.brown.simon

import java.io._

// Option map setup taken from 
// https://stackoverflow.com/questions/2315912/scala-best-way-to-parse-command-line-parameters-cli
// with modification

object SimonRunner {
	val usage = "edu.brown.simon.SimonRunner [-test] [-v] [-o outputfile] [-dynamic] <filename.sq>"
 	type OptionMap = Map[String, String]

    def buildOptionMap(map : OptionMap, list: List[String]) : OptionMap = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        // Terminal case
        case List() => map

        // Run tests (no input file)
        case "-test" :: rest =>
                buildOptionMap(map ++ Map("test" -> "true"), rest)
		
		// Use old, dynamic-table allocation compiler
		// (Means no forward-chaining requirement for WFQs)
        case "-dynamic" :: rest =>
                buildOptionMap(map ++ Map("dynamic" -> "true"), rest)
		
        // Verbose mode (param is amount of verbosity)
        case "-v" :: num :: rest => 
        		buildOptionMap(map ++ Map("v" -> num), rest)        

        // Output file (optional)
        case "-o" :: outfile :: rest => 
        		buildOptionMap(map ++ Map("o" -> outfile), rest)        
               
        // Last option is always query file name
        case string :: Nil =>  map ++ Map("infile" -> string)
        
        case option :: tail => println("Unknown option "+option) 
                               System.exit(1) 
                               map
      }
    }

    def runTests() {
    	println("Running tests...")
		ParserTests.main(Array[String]())
		QueryTests.main(Array[String]())
		QueryCompilerTests.main(Array[String]())
    }

    def runParser(infile: String, outfile: Option[String], verbose: Int, dynamic: Boolean) {
    	val thePath = java.nio.file.Paths.get(infile)
    	if(! java.nio.file.Files.exists(thePath)) {
    		println("Could not find file: "+infile+" ("+thePath+")")
    		System.exit(1)
    	}

    	println("Opening file "+infile+" ...")
    	val source = scala.io.Source.fromFile(infile)
		val qryString = try source.getLines mkString "\n" finally source.close()    	
    	
    	val parseResult = QueryParser.parseAll(QueryParser.query, qryString)  
        if(!parseResult.successful) {
            println("*** PARSING ERROR ***")
            println(parseResult)
            System.exit(1)
        }    	

    	val qry = parseResult.get
    	val compiledRuleString = QueryCompiler.compileQueryString(qry, dynamic)
    	if(verbose>1)
    		println("compiler produced rule: \n"+compiledRuleString)

    	// Write file if output given
    	outfile match {
    		case Some(of) =>     		
    			val writer = new PrintWriter(new File(of))
      			writer.write(compiledRuleString)
      			writer.close()
    			
    		case None => ()
    	}
    }

	def main(args: Array[String]) {
		if (args.length == 0) println(usage)
    	val arglist = args.toList
    	type OptionMap = Map[Symbol, Any]
    	val options = buildOptionMap(Map(),arglist)
    	
    	println("Running Simon compiler. Parameters given: "+options)
    	if(options.contains("test")) {
    		runTests()
    	}
    	else if(options.contains("infile")) {
    		runParser(options.getOrElse("infile", "shouldneverhappen"),
    				  options.get("o"),
    				  options.getOrElse("v", "0").toInt,
    				  options.getOrElse("dynamic", "false").toBoolean)
    	}
    	else {
    		println("No file name provided.\n(Check to make sure filename isn't being used as switch parameter.)")
    	}
	}
	
}