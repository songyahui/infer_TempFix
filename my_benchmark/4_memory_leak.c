case IDENTIFIER: {
2 MethodSymbol mtd =
3 resolver.resolveMethod(node, id.getName());
4 // method may be null
5+ checkGuardedBy(mtd != null, id.toString());
6 return bindSelect(computeBase(context , mtd), mtd); 7}
8 case MEMBER_SELECT: {
9 ...
MethodSymbol mtd = resolver.resolveMethod(node, id.getName());
// same problem!
return bindSelect(computeBase(context , mtd), mtd);
}

void checkGuardedBy(boolean cnd,
String fmtStr, Object... fmtArgs) {
    if (!cnd) {
        throw new IllegalGuardedBy(String.format(fmtStr , fmtArgs));
    }
}