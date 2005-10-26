// saxonbatch.java
//
// Dumb utility to batch process any XML files it finds in a directory
// Syntax:
//   java saxonbatch <stylesheet file> <directory>

// Sigh, it's been far too long since I wrote any java

import javax.xml.transform.*;
import javax.xml.transform.stream.*;
import java.lang.reflect.*;
import java.util.*;
import java.io.*;

public class saxonbatch {
    public static void showSyntax() {
        System.out.println("Usage: saxonbatch <stylesheet file> <directory>");
        System.exit(1); 
    }

    public static void main(String argv[]) 
        throws TransformerConfigurationException, TransformerException {
        System.out.println("SaxonBatch utility written by Andrew Hunter");
        System.out.println("");
        
        if (Array.getLength(argv) != 2) {
            showSyntax();
            return;
        }
        
        // Load up the stylesheet
        System.out.println("Loading stylesheet...");
        TransformerFactory tFactory = TransformerFactory.newInstance();
        
        Transformer transformer = tFactory.newTransformer(new StreamSource(argv[0]));
        
        // Iterate through the directory to find the .xml files
        System.out.print("Searching ");
        System.out.flush();
        
        LinkedList toSearch = new LinkedList();
        toSearch.addLast(new File(argv[1]));
        
        LinkedList xmlFiles = new LinkedList();
        
        while (toSearch.size() > 0) {
            File fileToCheck = (File)toSearch.getFirst();
            toSearch.removeFirst();
            
            String name = fileToCheck.getName();
            String extension = "";
            if (name.length() > 4) {
                extension = name.substring(name.length()-4);
            }
            
            if (fileToCheck.isDirectory()) {
                int x;
                String list[] = fileToCheck.list();
                
                for (x=0; x<Array.getLength(list); x++) {
                    toSearch.addLast(new File(fileToCheck, list[x]));
                }
            } else if (extension.equals(".xml")) {
                xmlFiles.addLast(fileToCheck);
            }
        }
        
        System.out.print("- done: ");
        System.out.print(xmlFiles.size());
        System.out.println(" files found");
        
        // Process each of the .xml files into .html files
        
        // Animation style thing
        System.out.print("Processing: >");
        int x;
        for (x=0; x<xmlFiles.size(); x++) {
            System.out.print(".");
        }
        System.out.print("<\b");
          for (x=0; x<xmlFiles.size(); x++) {
            System.out.print("\b");
        }
        System.out.flush();
        
        // List of exceptions
        LinkedList transformerExceptions = new LinkedList();
      
        while (xmlFiles.size() > 0) {
            File theFile = (File)xmlFiles.getFirst();
            xmlFiles.removeFirst();
       		
       		long originalModificationDate = theFile.lastModified();
       		
            String outputFile = theFile.getAbsolutePath();
            outputFile = outputFile.substring(0, outputFile.length() - 4);
            outputFile = outputFile.concat(".html");
            
            System.out.print("o\b");
            System.out.flush();
            
            try {
                transformer.transform(new StreamSource(theFile.getAbsolutePath()),
                                      new StreamResult(outputFile));
                theFile.delete();
 
                System.out.print("O");
                System.out.flush();
                
                File outputResult = new File(outputFile);
                outputResult.setLastModified(originalModificationDate);
            } catch (TransformerException e) {
                Object details[] = { theFile, e };
                transformerExceptions.addLast(details);
 
                System.out.print("X");
                System.out.flush();
            }
        }
        System.out.println("<");
        
        if (transformerExceptions.size() > 0) {
            System.out.println("");
            System.err.println("Failures occured: ");
            
            for (x=0; x<transformerExceptions.size(); x++) {
                Object details[] = (Object[]) transformerExceptions.get(x);

                File theFile = (File) details[0];
                TransformerException e = (TransformerException)details[1];
                
                SourceLocator where = e.getLocator();
                System.err.print("  ");
                System.err.print(theFile.getPath());
                System.err.print(": line ");
                System.err.print(where.getLineNumber());
                if (where.getColumnNumber() > 0) {
                    System.err.print(" column ");
                    System.err.print(where.getColumnNumber());
                }
                System.err.print("\n");
                System.err.print("    ");
                System.err.println(e.getMessageAndLocation());
            }
            
            System.err.flush();
            System.exit(1);
        }
        
        System.out.println("");
        System.out.println("OK");
    }
};
