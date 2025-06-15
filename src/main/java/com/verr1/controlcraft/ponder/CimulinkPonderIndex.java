package com.verr1.controlcraft.ponder;

import com.simibubi.create.foundation.ponder.PonderRegistrationHelper;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.registry.CimulinkBlocks;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;


public class CimulinkPonderIndex {

    private static final PonderRegistrationHelper PONDER_HELPER = new PonderRegistrationHelper(ControlCraft.MODID);

    @OnlyIn(Dist.CLIENT)
    public static void register(){
        PONDER_HELPER
                .forComponents(CimulinkBlocks.LOGIC_GATE)
                .addStoryBoard(GatesScene.ID, GatesScene::scene)
                .addStoryBoard(GatesScene.AND_OR_XOR, GatesScene::scene_1);
    }

}
