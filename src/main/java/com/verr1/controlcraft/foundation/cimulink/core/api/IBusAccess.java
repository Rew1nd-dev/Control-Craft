package com.verr1.controlcraft.foundation.cimulink.core.api;

import com.verr1.controlcraft.content.links.integration.LuaBlockEntity;

public interface IBusAccess {

    IBusAccess EMPTY = new IBusAccess() {
        @Override
        public double retrieve(String componentName, String port) {
            return 0;
        }

        @Override
        public void propagate(String componentName, String port, double value) {

        }

//        @Override
//        public void onPositiveEdge() {
//
//        }
    };


    double retrieve(String componentName, String port);

    void propagate(String componentName, String port, double value);

    // void onPositiveEdge();

    static IBusAccess of(LuaBlockEntity be){
        return new IBusAccess() {
            @Override
            public double retrieve(String componentName, String port) {
                return be.retrieveFrom(componentName, port);
            }

            @Override
            public void propagate(String componentName, String port, double value) {
                be.propagateTo(componentName, port, value);
            }

//            @Override
//            public void onPositiveEdge() {
//                be.onPositiveEdge();
//            }
        };
    }


}
