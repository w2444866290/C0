package miniplc0java.analyser;

import miniplc0java.util.Pos;

import java.util.ArrayList;
import java.util.List;

public class BBlock {
    private int No;
    private ArrayList<Integer> Goto;
    private Pos begin;
    private Pos end;
    private boolean isRet;

    public BBlock(int No) {
        this.No = No;
        this.Goto = new ArrayList<>();
        this.isRet = false;
    }

    public int getNo() {
        return No;
    }

    public void setNo(int no) {
        No = no;
    }

    public ArrayList<Integer> getGoto() {
        return Goto;
    }

    public void setGoto(int No) {
        Goto.add(No);
    }

    public boolean isRet() {
        return isRet;
    }

    public void setRet(boolean ret) {
        isRet = ret;
    }
}
