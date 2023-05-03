public class Bug1 {
   int translateInner(String input, int index){
   	int seqEnd = input.length();
   	int start = index + 2;
   	boolean isHex = false;
   	char firstChar = input.charAt(start);
   	if (firstChar == 'x'||firstChar == 'X'){
   		start++;
   		isHex = true;
   		//FIX: if (start == seqEnd)
   		return 0;
   	}
   	else {return 2;}
   }
}
