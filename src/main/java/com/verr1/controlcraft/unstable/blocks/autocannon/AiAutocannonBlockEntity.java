package com.verr1.controlcraft.unstable.blocks.autocannon;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.compact.createbigcannons.APAutocannonAccess;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.unstable.ai.api.IAirCannon;
import com.verr1.controlcraft.unstable.blocks.AiCannonBaseBlockEntity;
import com.verr1.controlcraft.unstable.blocks.AiUtilBlockEntity;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.AIBlockNetwork;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.projectile.Arrow;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

import static com.verr1.controlcraft.unstable.ai.game.SharedAIKeys.ARROW;

public class AiAutocannonBlockEntity extends AiCannonBaseBlockEntity {

    protected boolean arrow = false;


    public AiAutocannonBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        registerBoolean(this::useArrow, this::setUseArrow, ARROW);
    }



    public boolean useArrow() {
        return arrow;
    }

    public void setUseArrow(boolean arrow) {
        this.arrow = arrow;
    }



    @Override
    public Projectile getProjectile() {
        if(level == null)return null;
        if(useArrow()){
            Arrow a = new Arrow(EntityType.ARROW, level);
            a.setNoGravity(true);
            return a;
        }
        APAutocannonAccess ap = CreateBigCannonsCompact.createAutocannonAp(level);
        if(ap == null)return null;
        ap.setLifetime(40);
        ap.setTracer(true);
        ap.getProjectile().setNoGravity(true);
        return ap.getProjectile();
    }




}
