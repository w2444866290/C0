package miniplc0java.instruction;

import java.util.Objects;

public class Instruction {
    private Operation opt;
    Object x;
    Integer funcIndex;

    public Instruction(Operation opt, Integer funcIndex) {
        this.opt = opt;
        this.x = 0;
        this.funcIndex = funcIndex;
    }

    public Instruction(Operation opt, Object x, Integer funcIndex) {
        this.opt = opt;
        this.x = x;
        this.funcIndex = funcIndex;
    }

    public Instruction() {
        this.opt = Operation.nop;
        this.x = 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Instruction that = (Instruction) o;
        return opt == that.opt && Objects.equals(x, that.x);
    }

    public Operation getOpt() {
        return opt;
    }

    public void setOpt(Operation opt) {
        this.opt = opt;
    }

    public Object getX() {
        return x;
    }

    public void setX(Integer x) {
        this.x = x;
    }

    public Integer getFuncIndex() {
        return funcIndex;
    }

    public void setFuncIndex(Integer funcIndex) {
        this.funcIndex = funcIndex;
    }

    @Override
    public String toString() {
        switch (this.opt) {
            case nop:
            case pop:
            case dup:
            case load8:
            case load16:
            case load32:
            case load64:
            case store8:
            case store16:
            case store32:
            case store64:
            case alloc:
            case free:
            case addi:
            case subi:
            case muli:
            case divi:
            case addf:
            case subf:
            case mulf:
            case divf:
            case divu:
            case shl:
            case shr:
            case and:
            case or:
            case xor:
            case not:
            case cmpi:
            case cmpu:
            case cmpf:
            case negi:
            case negf:
            case itof:
            case ftoi:
            case shrl:
            case setlt:
            case setgt:
            case ret:
            case scani:
            case scanc:
            case scanf:
            case printi:
            case printc:
            case printf:
            case prints:
            case println:
            case panic:
                return String.format("%s", this.opt);
            case push:
            case popn:
            case loca:
            case arga:
            case globa:
            case stackalloc:
            case br:
            case brfalse:
            case brtrue:
            case call:
            case callname:
                return String.format("%s %s", this.opt, this.x);
            default:
                return "panic";
        }
    }
}
