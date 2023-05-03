public class Bug2{

   String abbreviate(String str, int lower, int upper){
   	
   	//fix: if (lower>str.length())
   	lower = str.length();
   	if (upper == -1 || upper > str.length())
   		upper = str.length();
   	if (upper < lower)
   		upper = lower;
   	StringBuffer result = new StringBuffer();
   	int index = str.indexOf(" ",lower);
   	if (index == -1)
   		result.append(str.substring(0,upper));
   	return result.toString();
   }
}
