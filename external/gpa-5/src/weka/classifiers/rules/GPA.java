/*
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 *    GPA.java (based on Prism.java)
 *    Copyright (C) 2006 Deniz Yuret
 *
 */

package weka.classifiers.rules;

import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import java.io.*;
import java.util.*;
import weka.core.*;

/**
 * Class for building and using a decision list for classification.  
 * Uses the greedy prepend algorithm (GPA) for construction.  The
 * actual model building is done by an external C program.
 * For more information, see <p>
 *
 * D. Yuret and M. de la Maza (2006) Greedy Prepend Algorithm, 
 * submitted to ICML
 * 
 * @author Deniz Yuret (dyuret@ku.edu.tr)
 * @version $Revision: 1.15 $ 
*/
public class GPA extends Classifier implements OptionHandler {

  /** The number of folds to split data into Train and Validate for GPA */
  private int m_Folds = 5;

  /** Pruning mode: 0=none, 1=short, 2=long */
  private int m_Prune = 2;

  /** The maximum number of antecedents in a rule */
  private int m_NumAntds = 5;

  /** The number of rules to consider once validation starts dropping */
  private int m_Wait = 0;

  /** Whether to use srand(time(NULL)) in GPA */
  private boolean m_Randomization = false;


  /**
   * Returns a string describing classifier
   * @return a description suitable for
   * displaying in the explorer/experimenter gui
   */
  public String globalInfo() {
    return "Class for building and using a decision list for classification.  "
      + "Uses the greedy prepend algorithm (GPA) for construction.  "
      + "Missing values are handled by ignoring them during rule construction.  "
      + "For numeric attributes try the FilteredClassifier metalearner with "
      + "supervised Discretize filter with -D (binary) option.  "
      + "For more information, see <p>"
      + "D. Yuret (2006) ISCIS"
      + "<p>";
  }    

  /**
   * Class for storing a GPA ruleset, i.e. a list of rules
   */
  private class GPARule implements Serializable {
    
    /** The classification */
    private int m_classification;

    /** The instance */
    private Instances m_instances;

    /** First test of this rule */
    private Test m_test; 

    /** Number of errors made by this rule (will end up 0) */
    private int m_errors; 

    /** The next rule in the list */
    private GPARule m_next;

    /**
     * Constructor that takes string output from gpa executable.
     *
     * @param d   instances
     * @param str the string
     * @exception Exception if something goes wrong
     */

    public GPARule(Instances d, String str) throws Exception {
      m_instances = d;
      String[] attr = str.split("\\s+");
      String eqstr = new String(new char[]{eqChar});
      if (attr.length == 0) {
	throw new Exception("Empty line");
      }

      for (int i = 0; i < attr.length; i++) {
	//System.out.println("Parsing: " + attr[i]);
	if (attr[i].charAt(0) == '#') {
	  break;
	}
	String[] nameval = attr[i].replace(spChar,' ').split(eqstr);
	if (nameval.length != 2) {
	  throw new Exception("Bad attr format: " + attr[i]);
	}

	Test t = new Test();
	t.m_attr = d.attribute(nameval[0]).index();
	if (t.m_attr == -1) {
	  throw new Exception("Unknown attribute name: " + nameval[0]);
	}
	//System.out.println("Attr name: " + nameval[0]);
	t.m_val = d.attribute(t.m_attr).indexOfValue(nameval[1]);
	if (t.m_val == -1) {
	  throw new Exception("Unknown attribute value: " + attr[i]);
	}
	//System.out.println("Attr value: " + nameval[1]);
	
	if (i == 0) {
	  if (t.m_attr != d.classIndex()) {
	    throw new Exception("First attribute is not the class: " 
				+ attr[0] + " != " + d.classAttribute().name());
	  }
	  //System.out.println("Class name: " + nameval[0]);
	  m_classification = t.m_val;
	} else {
	  t.m_next = m_test;
	  m_test = t;
	}
      }
    }      


    /**
     * Constructor that takes instances and the classification.
     *
     * @param data the instances
     * @param cl the class
     * @exception Exception if something goes wrong
     */
    public GPARule(Instances data, int cl) throws Exception {

      m_instances = data;
      m_classification = cl;
      m_test = null;
      m_next = null;
      m_errors = 0;
      Enumeration enu = data.enumerateInstances();
      while (enu.hasMoreElements()) {
        if ((int) ((Instance) enu.nextElement()).classValue() != cl) {
	  m_errors++;
	}
      }
      m_instances = new Instances(m_instances, 0);
    }  

    /**
     * Returns the result assigned by this rule to a given instance.
     *
     * @param inst the instance to be classified
     * @return the classification
     */
    public int resultRule(Instance inst) {

      if (m_test == null || m_test.satisfies(inst)) {
	return m_classification;
      } else {
	return -1;
      }
    }

    /**
     * Returns the result assigned by these rules to a given instance.
     *
     * @param inst the instance to be classified
     * @return the classification
     */
    public int resultRules(Instance inst) {

      if (resultRule(inst) != -1) {
	return m_classification;
      } else if (m_next != null) {
	return m_next.resultRules(inst);
      } else {
	return -1;
      }
    }

    /**
     * Returns the set of instances that are covered by this rule.
     *
     * @param data the instances to be checked
     * @return the instances covered
     */
    public Instances coveredBy(Instances data) {

      Instances r = new Instances(data, data.numInstances());
      Enumeration enu = data.enumerateInstances();
      while (enu.hasMoreElements()) {
	Instance i = (Instance) enu.nextElement();
	if (resultRule(i) != -1) {
	  r.add(i);
	}
      }
      r.compactify();
      return r;
    }

    /**
     * Returns the set of instances that are not covered by this rule.
     *
     * @param data the instances to be checked
     * @return the instances not covered
     */
    public Instances notCoveredBy(Instances data) {

      Instances r = new Instances(data, data.numInstances());
      Enumeration enu = data.enumerateInstances();
      while (enu.hasMoreElements()) {
	Instance i = (Instance) enu.nextElement();
	if (resultRule(i) == -1) {
	  r.add(i);
	}
      }
      r.compactify();
      return r;
    }

    /**
     * Prints the set of rules.
     *
     * @return a description of the rules as a string
     */
    public String toString() {

      try {
	StringBuffer text = new StringBuffer();
	if (m_test != null) {
	  text.append("If ");
	  for (Test t = m_test; t != null; t = t.m_next) {
	    if (t.m_attr == -1) {
	      text.append("?");
	    } else if (t.m_val == -1) {
	      text.append(m_instances.attribute(t.m_attr).name() + " = ?");
	    } else {
	      text.append(m_instances.attribute(t.m_attr).name() + " = " +
			  m_instances.attribute(t.m_attr).value(t.m_val));
	    }
	    if (t.m_next != null) {
	      text.append("\n   and ");
	    }
	  }
	  text.append(" then ");
	}
	text.append(m_instances.classAttribute().value(m_classification) + "\n");
	if (m_next != null) {
	  text.append(m_next.toString());
	}
	return text.toString();
      } catch (Exception e) {
	return "Can't print GPA classifier!";
      }
    }
  }
  
  /**
   * Class for storing a list of attribute-value tests
   */
  private class Test implements Serializable { 

    /** Attribute to test */
    private int m_attr = -1; 

    /** The attribute's value */
    private int m_val; 

    /** The next test in the rule */
    private Test m_next = null; 

    /**
     * Returns whether a given instance satisfies this test.
     *
     * @param inst the instance to be tested
     * @return true if the instance satisfies the test
     */
    private boolean satisfies(Instance inst) {

      if ((inst.isMissing(m_attr) && (m_val == -1)) ||
	  (!inst.isMissing(m_attr) &&
	   ((int) inst.value(m_attr) == m_val))) {
        if (m_next == null) {
	  return true;
	} else {
	  return m_next.satisfies(inst);
	}
      }
      return false;    
    }
  }

  /** The first rule in the list of rules */
  private GPARule m_rules;

  /**
   * Classifies a given instance.
   *
   * @param inst the instance to be classified
   * @return the classification
   */
  public double classifyInstance(Instance inst) {

    int result = m_rules.resultRules(inst);
    if (result == -1) {
      return Instance.missingValue();
    } else {
      return (double)result;
    }
  }

  private char eqChar = '~';
  private char spChar = '@';

  private String instanceToString(Instance x) throws Exception {
    StringBuffer text = new StringBuffer();
    int ci = x.classIndex();
    if (x.isMissing(ci)) {
      throw new Exception("Instance has no class");
    }

    // Need to use eqChar to separate name and value, and spChar to escape
    // spaces.  If the actual attr contains these characters we have a 
    // problem.  Ideally we should use safer characters.

    for (int i = 0; i < x.numAttributes(); i++) {
      // Just don't print the missing attributes
      // GPA doesn't care how many attr each instance has
      if (x.isMissing(i)) {
	continue;
      }
      String name = x.attribute(i).name();
      String value = x.stringValue(i);
      if ((name.indexOf(eqChar) != -1) ||
	  (name.indexOf(spChar) != -1) ||
	  (value.indexOf(eqChar) != -1) ||
	  (value.indexOf(spChar) != -1)) {
	throw new Exception("Attribute has special characters");
      }
      String str = name + eqChar + value;
      str = str.replace(' ', spChar);
      if (i == ci) {
	text.insert(0, str);
      } else {
	text.append(" " + str);
      }
    }
    return text.toString();
  }


  private BufferedReader streamToReader(InputStream s) {
    return new BufferedReader(new InputStreamReader(s));
  }

  /**
   * Generates the classifier.
   *
   * @param data the data to be used
   * @exception Exception if the classifier can't built successfully
   */
  public void buildClassifier(Instances data) throws Exception {

    // Check the data
    if (data.checkForStringAttributes()) {
      throw new UnsupportedAttributeTypeException("Cannot handle string attributes!");
    }
    if (data.classAttribute().isNumeric()) {
      throw new UnsupportedClassTypeException("GPA can't handle a numeric class!");
    }
    data = new Instances(data);
    Enumeration enumAtt = data.enumerateAttributes();
    while (enumAtt.hasMoreElements()) {
      Attribute attr = (Attribute) enumAtt.nextElement();
      if (!attr.isNominal()) {
	throw new UnsupportedAttributeTypeException("GPA can only deal with nominal attributes!");
      }

      // Maybe we should consider treating missing values as a seperate value.
      // data.deleteWithMissing(attr);
    }
    //System.out.println(data.numInstances() + " instances");
    data.deleteWithMissingClass(); // delete all instances with a missing class
    if (data.numInstances() == 0) {
      throw new Exception("No instances with a class value!");
    }


    Runtime runtime = Runtime.getRuntime();
    StringBuffer cmd = new StringBuffer("gpa -q");
    cmd.append(" -d " + m_NumAntds);
    cmd.append(" -v " + m_Folds);
    cmd.append(" -p " + m_Prune);
    cmd.append(" -w " + m_Wait);
    if (m_Randomization) cmd.append(" -r");
    //System.err.println("Running " + cmd);
    Process p = runtime.exec(cmd.toString());
    BufferedReader p_out = streamToReader(p.getInputStream());
    BufferedReader p_err = streamToReader(p.getErrorStream());
    PrintWriter p_in = new PrintWriter(p.getOutputStream());
    Enumeration e = data.enumerateInstances();
    //System.out.println("==> Printing instances");
    while (e.hasMoreElements()) {
      Instance x = (Instance) e.nextElement();
      p_in.println(instanceToString(x));
      //System.out.println(instanceToString(x));
    }
    p_in.close();
    String line;
    m_rules = null;
    //while ((line = p_err.readLine()) != null) {
    //System.out.println("ERR>" + line);
    //}
    //System.out.println("==> Reading rules");
    while ((line = p_out.readLine()) != null) {
      //System.out.println(line);
      GPARule r = null;
      try {
	r = new GPARule(data, line);
      } catch (Exception exc) {
	System.err.println(exc);
	System.exit(0);
      }
      r.m_next = m_rules;
      m_rules = r;
    }
    if (p.waitFor() != 0) {
      throw new Exception("GPA exit value = " + p.exitValue());
    }
    //System.out.println("==> Constructed model");
    //System.out.println(toString());
  }

  /**
   * Add a rule to the ruleset.
   *
   * @param lastRule the last rule in the rule set
   * @param newRule the rule to be added
   * @return the new last rule in the rule set
   */
  private GPARule addRule(GPARule lastRule, GPARule newRule) {

    if (lastRule == null) {
      m_rules = newRule;
    } else {
      lastRule.m_next = newRule;
    }
    return newRule;
  }

  /**
   * Add a test to this rule.
   *
   * @param rule the rule to which test is to be added
   * @param lastTest the rule's last test
   * @param newTest the test to be added
   * @return the new last test of the rule
   */
  private Test addTest(GPARule rule, Test lastTest, Test newTest) {

    if (rule.m_test == null) {
      rule.m_test = newTest;
    } else {
      lastTest.m_next = newTest;
    }
    return newTest;
  }

  /**
   * Does E contain any examples in the class C?
   *
   * @param E the instances to be checked
   * @param C the class
   * @return true if there are any instances of class C
   */
  private static boolean contains(Instances E, int C) throws Exception {

    Enumeration enu = E.enumerateInstances();
    while (enu.hasMoreElements()) {
      if ((int) ((Instance) enu.nextElement()).classValue() == C) {
	return true;
      }
    }
    return false;
  }

  /**
   * Is this attribute mentioned in the rule?
   *
   * @param attr the attribute to be checked for
   * @param t test contained by rule
   */
  private static boolean isMentionedIn(Attribute attr, Test t) {

    if (t == null) { 
      return false;
    }
    if (t.m_attr == attr.index()) {
      return true;
    }
    return isMentionedIn(attr, t.m_next);
  }    

  /**
   * Prints a description of the classifier.
   *
   * @return a description of the classifier as a string
   */
  public String toString() {

    if (m_rules == null) {
      return "GPA: No model built yet.";
    }
    return "GPA rules\n----------\n" + m_rules.toString();
  }

  /**
   * Returns an enumeration describing the available options
   *
   * @return an enumeration of all the available options
   */
  public Enumeration listOptions() {
    Vector<Option> newVector = new Vector<Option>(5);
	
    newVector.addElement
      (new Option("\t" + numAntdsTipText().replace("\n", "\n\t"),
		  "A", 1, "-A <number of antecedents>"));
    
    newVector.addElement
      (new Option("\t" + foldsTipText().replace("\n", "\n\t"),
		  "V", 1, "-V <number of folds>"));
    
    newVector.addElement
      (new Option("\t" + pruneTipText().replace("\n", "\n\t"),
		  "P", 1, "-P <pruning mode>"));

    newVector.addElement
      (new Option("\t" + waitTipText().replace("\n", "\n\t"),
		  "W", 1, "-W <number of rules>"));

    newVector.addElement
      (new Option("\t" + randomizationTipText().replace("\n", "\n\t"),
		  "R", 0, "-R"));

    return newVector.elements();
  }
    
  /**
   * Parses a given list of options.
   *
   * @param options the list of options as an array of strings
   * @exception Exception if an option is not supported
   */
  public void setOptions(String[] options) throws Exception {

    String numAntdsString = Utils.getOption('A', options);
    if (numAntdsString.length() != 0) 
      m_NumAntds = Integer.parseInt(numAntdsString);
    else 
      m_NumAntds = 5;

    String numFoldsString = Utils.getOption('V', options);
    if (numFoldsString.length() != 0) 
      m_Folds = Integer.parseInt(numFoldsString);
    else 
      m_Folds = 5;

    String numPruneString = Utils.getOption('P', options);
    if (numPruneString.length() != 0) 
      m_Prune = Integer.parseInt(numPruneString);
    else 
      m_Prune = 2;

    String numWaitString = Utils.getOption('W', options);
    if (numWaitString.length() != 0) 
      m_Wait = Integer.parseInt(numWaitString);
    else 
      m_Wait = 0;

    m_Randomization = Utils.getFlag('R', options);
  }
    
  /**
   * Gets the current settings of the Classifier.
   *
   * @return an array of strings suitable for passing to setOptions
   */
  public String [] getOptions() {
	
    String [] options = new String [9];
    int current = 0;
    options[current++] = "-A"; options[current++] = "" + m_NumAntds;
    options[current++] = "-V"; options[current++] = "" + m_Folds;
    options[current++] = "-P"; options[current++] = "" + m_Prune;
    options[current++] = "-W"; options[current++] = "" + m_Wait;

    if(m_Randomization)
      options[current++] = "-R";
	
    while (current < options.length) 
      options[current++] = "";
    return options;
  }
    
  /** The access functions for parameters */

  /**
   * Returns the tip text for this property
   * @return tip text for this property suitable for
   * displaying in the explorer/experimenter gui
   */
  public String numAntdsTipText() {
    return "The maximum number of antecedents allowed in a single rule.\n"
      + "Set this higher if more antecedents might improve performance.\n"
      + "Set this lower if the search is taking too much time.\n"
      + "Default = 5.  Use 0 for unlimited search.";
  }

  public void setNumAntds(int n){  m_NumAntds = n; }
  public int getNumAntds(){ return m_NumAntds; }
    
  /**
   * Returns the tip text for this property
   * @return tip text for this property suitable for
   * displaying in the explorer/experimenter gui
   */
  public String foldsTipText() {
    return "Determines the amount of training data used for validation.\n"
      + "One fold is used for validation, the rest for growing the rules.\n"
      + "Default = 5.  Use 0 to turn off validation.";
  }

  public void setFolds(int folds){  m_Folds = folds; }
  public int getFolds(){ return m_Folds; }

  /**
   * Returns the tip text for this property
   * @return tip text for this property suitable for
   * displaying in the explorer/experimenter gui
   */
  public String pruneTipText() {
    return "Determines when to stop adding rules to the decision list.\n"
      + "If 0, rules are added as long as they improve the training set accuracy.\n"
      + "If 1, stops adding rules when validation set accuracy stops improving.\n"
      + "If 2, stops adding rules when validation set accuracy starts dropping.\n"
      + "Default = 2.  Use 0 to ignore the validation set accuracy.";
  }

  public void setPrune(int prune){  m_Prune = prune; }
  public int getPrune(){ return m_Prune; }

  /**
   * Returns the tip text for this property
   * @return tip text for this property suitable for
   * displaying in the explorer/experimenter gui
   */
  public String waitTipText() {
    return "The number of rules to consider after the validation set accuracy starts dropping.\n"
      + "Set this higher if you think more rules may increase validation performance.\n"
      + "Set this lower if you want the search to stop sooner.\n"
      + "Default = 0.  Use 0 to consider all rules.";
  }

  public void setWait(int wait){  m_Wait = wait; }
  public int getWait(){ return m_Wait; }

  /**
   * Returns the tip text for this property
   * @return tip text for this property suitable for
   * displaying in the explorer/experimenter gui
   */
  public String randomizationTipText() {
    return "Set if you wish to randomize the training vs. validation data split.\n"
      + "Default = False.";
  }

  public boolean getRandomization(){ return m_Randomization;}
  public void setRandomization(boolean e){ m_Randomization = e;}

  /**
   * Main method for testing this class
   */
  public static void main(String[] args) {
    try {
      System.out.println(Evaluation.evaluateModel(new GPA(), args));
    } catch (Exception e) {
      System.err.println(e.getMessage());
    }
  }
}
