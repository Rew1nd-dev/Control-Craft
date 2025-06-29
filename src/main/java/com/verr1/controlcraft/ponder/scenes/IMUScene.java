package com.verr1.controlcraft.ponder.scenes;

import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.ponder.Constants;
import com.verr1.controlcraft.ponder.util.CimulinkPonderUtil;
import net.minecraft.core.Direction;

import static com.verr1.controlcraft.ponder.scenes.BasicScene.READING_TIME;
import static com.verr1.controlcraft.ponder.scenes.BasicScene.of;

public class IMUScene {

    public static void scene(SceneBuilder scene, SceneBuildingUtil util){
        var imu = of(3, 1, 3);

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util);

        cu
                .init()
                .setBlock(Constants.IMU, imu).idle(4)
                .text("This is imu", imu, READING_TIME).idle(READING_TIME)
                .text("You can get velocity/omega/.. values of the vs ship its located on", imu, READING_TIME + 2).idle(READING_TIME + 2);

    }

}
