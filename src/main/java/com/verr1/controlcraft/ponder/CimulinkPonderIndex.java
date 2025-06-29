package com.verr1.controlcraft.ponder;

import com.simibubi.create.foundation.ponder.PonderRegistrationHelper;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ponder.scenes.*;
import com.verr1.controlcraft.registry.CimulinkBlocks;
import com.verr1.controlcraft.registry.ControlCraftItems;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;


public class CimulinkPonderIndex {

    private static final PonderRegistrationHelper PONDER_HELPER = new PonderRegistrationHelper(ControlCraft.MODID);

    @OnlyIn(Dist.CLIENT)
    public static void register(){
        PONDER_HELPER
                .forComponents(CimulinkBlocks.LOGIC_GATE)
                .addStoryBoard(BasicScene.EMPTY, GatesScene::scene);

        PONDER_HELPER
                .forComponents(ControlCraftItems.CIRCUIT_COMPILER)
                .addStoryBoard(BasicScene.EMPTY, BasicScene::scene_0)
                .addStoryBoard(BasicScene.EMPTY, BasicScene::scene_1)
                .addStoryBoard(BasicScene.EMPTY, BasicScene::scene_2)
                .addStoryBoard(BasicScene.EMPTY, BasicScene::scene_3);

        PONDER_HELPER
                .forComponents(CimulinkBlocks.INPUT, CimulinkBlocks.OUTPUT)
                .addStoryBoard(BasicScene.EMPTY, IOScene::scene);

        PONDER_HELPER
                .forComponents(CimulinkBlocks.FF)
                .addStoryBoard(BasicScene.EMPTY, FFScene::scene)
                .addStoryBoard(BasicScene.EMPTY, FFScene::scene_1);

        PONDER_HELPER
                .forComponents(CimulinkBlocks.SHIFTER)
                .addStoryBoard(BasicScene.EMPTY, ShifterScene::scene)
                .addStoryBoard(BasicScene.EMPTY, ShifterScene::scene_1);

        PONDER_HELPER
                .forComponents(CimulinkBlocks.FMA)
                .addStoryBoard(BasicScene.EMPTY, CombinationalScene::fma);

        PONDER_HELPER
                .forComponents(CimulinkBlocks.MUX)
                .addStoryBoard(BasicScene.EMPTY, CombinationalScene::mux);

        PONDER_HELPER
                .forComponents(CimulinkBlocks.COMPARATOR)
                .addStoryBoard(BasicScene.EMPTY, CombinationalScene::cmp);

        PONDER_HELPER
                .forComponents(CimulinkBlocks.PROXY)
                .addStoryBoard(BasicScene.EMPTY, ProxyScene::scene);

        PONDER_HELPER
                .forComponents(CimulinkBlocks.SENSOR)
                .addStoryBoard(BasicScene.EMPTY, IMUScene::scene);

        PONDER_HELPER
                .forComponents(CimulinkBlocks.CIRCUIT)
                .addStoryBoard(BasicScene.EMPTY, CircuitScene::scene);
    }

}
