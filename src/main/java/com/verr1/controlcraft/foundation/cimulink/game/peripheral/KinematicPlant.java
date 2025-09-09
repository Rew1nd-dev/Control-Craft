package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.content.blocks.motor.AbstractKinematicMotor;
import com.verr1.controlcraft.content.blocks.slider.KinematicSliderBlockEntity;

public class KinematicPlant extends Plant{



    public KinematicPlant(KinematicDevice device) {
        super(new builder()
                .in("target", device::setControlTarget)
                .out("current", device::currentValue)
                .out("control", device::currentControlValue)
        );
    }


    public interface KinematicDevice{

        void setControlTarget(double target);

        double currentValue();

        double currentControlValue();

        static KinematicDevice of(AbstractKinematicMotor motor){
            return new KinematicDevice() {
                @Override
                public void setControlTarget(double target) {
                    motor.getController().setControlTarget(target);
                }

                @Override
                public double currentValue() {
                    return motor.getServoAngle();
                }

                @Override
                public double currentControlValue() {
                    return motor.getController().getTarget();
                }
            };
        }


        static KinematicDevice of(KinematicSliderBlockEntity slider){
            return new KinematicDevice() {
                @Override
                public void setControlTarget(double target) {
                    slider.getController().setControlTarget(target);
                }

                @Override
                public double currentValue() {
                    return slider.getSlideDistance();
                }

                @Override
                public double currentControlValue() {
                    return slider.getController().getControlTarget();
                }
            };
        }

    }

}
