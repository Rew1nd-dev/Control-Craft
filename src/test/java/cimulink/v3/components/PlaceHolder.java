package cimulink.v3.components;

import cimulink.v3.components.NamedComponent;

import java.util.List;

public class PlaceHolder extends NamedComponent {

    public PlaceHolder(List<String> inputs, List<String> outputs) {
        super(inputs, outputs);
    }


    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {

    }


    @Override
    public void onPositiveEdge() {

    }

}
