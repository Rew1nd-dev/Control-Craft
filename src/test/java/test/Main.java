package test;

import com.verr1.controlcraft.foundation.cimulink.game.debug.Debug;
import test.game.BlockLinkPortTest;
import test.game.FactoryTest;
import test.module.Packaging;

public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, Cimulink!");
        // Here you can add code to test your components or run simulations

        // CircuitConnectivityTest.propagateMapTest();
        // Module.dcTest();
        // Packaging.packageTest_1();
        // BlockLinkPortTest.loopTest();

        Debug.TEST_ENVIRONMENT = true;

        FactoryTest.buildTag();

    }




}
