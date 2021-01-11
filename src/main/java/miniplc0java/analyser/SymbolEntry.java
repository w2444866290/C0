package miniplc0java.analyser;

public class SymbolEntry {
    String name;
    String type;
    Integer localIndex;
    Integer globalIndex;
    Integer functionIndex;
    int paramSlots;
    int locSlots;
    int bodyCount;
    boolean isFuncIdent;
    boolean isConstant;
    boolean isInitialized;
    int stackOffset;

    /**
     * @param isConstant
     * @param isInitialized
     * @param stackOffset
     */
    public SymbolEntry(String name, String type, Integer localIndex, Integer globalIndex, Integer functionIndex,
                       boolean isFuncIdent, boolean isConstant, boolean isInitialized, int stackOffset) {
        this.name = name;
        this.type = type;
        this.localIndex = localIndex;
        this.globalIndex = globalIndex;
        this.functionIndex = functionIndex;
        this.isFuncIdent = isFuncIdent;
        this.isConstant = isConstant;
        this.isInitialized = isInitialized;
        this.stackOffset = stackOffset;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @return the localIndex
     */
    public int getLocalIndex() {
        return localIndex;
    }

    /**
     * @return the globalIndex
     */
    public int getGlobalIndex() {
        return globalIndex;
    }

    /**
     * @return the functionIndex
     */
    public int getFunctionIndex() {
        return functionIndex;
    }

    /**
     * @return the paramSlots
     */
    public int getParamSlots() {
        return paramSlots;
    }

    /**
     * @return the locSlots
     */
    public int getLocSlots() {
        return locSlots;
    }

    /**
     * @return the bodyCount
     */
    public int getBodyCount() {
        return bodyCount;
    }

    /**
     * @return the stackOffset
     */
    public int getStackOffset() {
        return stackOffset;
    }

    /**
     * @return the bitLength
     */
    public int getBitLength() {
        var bitLength = 0;
        if (isFunction() || type == "string") {
            bitLength = name.length();
        }
        else {
            bitLength = type == "void" ? 0: 8;
        }
        return bitLength;
    }

    /**
     * @return the isGlobal
     */
    public boolean isGlobal() {
        return globalIndex > -1;
    }

    /**
     * @return the isLocal
     */
    public boolean isLocal() {
        return localIndex > -1;
    }

    /**
     * @return the isFunction
     */
    public boolean isFunction() {
        return isFuncIdent;
    }

    /**
     * @return the isConstant
     */
    public boolean isConstant() {
        return isConstant;
    }

    /**
     * @return the isInitialized
     */
    public boolean isInitialized() {
        return isInitialized;
    }

    /**
     * @param paramSlots the paramSlots to set
     */
    public void setParamSlots(Integer paramSlots) {
        this.paramSlots = paramSlots;
    }

    /**
     * @param locSlots the locSlots to set
     */
    public void setLocSlots(Integer locSlots) {
        this.locSlots = locSlots;
    }

    /**
     * @param bodyCount the bodyCount to set
     */
    public void setBodyCount(Integer bodyCount) {
        this.bodyCount = bodyCount;
    }

    /**
     * @param isConstant the isConstant to set
     */
    public void setConstant(boolean isConstant) {
        this.isConstant = isConstant;
    }

    /**
     * @param isInitialized the isInitialized to set
     */
    public void setInitialized(boolean isInitialized) {
        this.isInitialized = isInitialized;
    }

    /**
     * @param stackOffset the stackOffset to set
     */
    public void setStackOffset(int stackOffset) {
        this.stackOffset = stackOffset;
    }
}
