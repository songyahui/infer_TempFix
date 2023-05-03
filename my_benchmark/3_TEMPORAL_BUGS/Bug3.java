public class Bug3{

	double evaluate(double[] values,double p){
	int length = values.length;
	int n = length;
	double pos = p * (n+1) / 100;
	double fpos = Math.floor(pos);
	int intPos = (int) fpos;
	double dif = pos -fpos;
	double[] sorted = new double[n];
	System.arraycopy(values,0,sorted,0,n);
	if (pos<1)
		return sorted[0];
	if (pos>n)
		return sorted[n-1];
	//fix: if (pos==n) return sorted[n-1];
	double lower = sorted[intPos-1];
	double upper = sorted[intPos];
	return lower + dif * (upper -lower);
	
	}
}
