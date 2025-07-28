package test;

import com.verr1.controlcraft.foundation.cimulink.game.port.types.AnalogTypes;
import com.verr1.controlcraft.utils.MathUtils;
import org.joml.Quaterniond;

public class TestMain {
    public static void main(String[] args) {
        System.out.println("Hello, Cimulink!");
        // Here you can add code to test your components or run simulations

        // CircuitConnectivityTest.propagateMapTest();
        // Module.dcTest();
        // Packaging.packageTest_1();
        // BlockLinkPortTest.loopTest();

        System.out.println(new Quaterniond(0.4963, 0.3301, 0.7746, 0.2115).lengthSquared());

        // Debug.TEST_ENVIRONMENT = true;

        // FactoryTest.buildTag();

    }




}
