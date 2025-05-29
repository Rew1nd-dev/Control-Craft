package cimulink;

import java.util.List;

public abstract class Component {

    public final boolean immediate;
    private final int N;

    public String debug_name = "";

    public int M() {
        return M;
    }

    public int N() {
        return N;
    }

    private final int M;
    protected Component(boolean immediate, int N, int M) {
        this.immediate = immediate;
        this.N = N;
        this.M = M;
    }


    public abstract List<Double> supply();

    public abstract void consume(List<Double> inputs);

    /*
    * public final void transit(){
        if(immediate){
            throw new IllegalCallerException("This Component Is Not Defined As A Temporal Component, Cannot Transit");
        }
        transitInternal();
    }

    protected void transitInternal(){}
    * */

}
