package com.verr1.controlcraft.unstable.blocks.cannon;

import com.verr1.controlcraft.unstable.blocks.AiCannonBaseBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.projectile.Arrow;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import rbasamoyai.createbigcannons.index.CBCEntityTypes;
import rbasamoyai.createbigcannons.index.CBCItems;
import rbasamoyai.createbigcannons.munitions.big_cannon.he_shell.HEShellProjectile;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;
import static com.verr1.controlcraft.unstable.ai.game.SharedAIKeys.ARROW;

public class AiBigCannonBlockEntity extends AiCannonBaseBlockEntity {

    protected boolean arrow = false;

    public AiBigCannonBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
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
            Arrow ap = new Arrow(level, 0, 0, 0);
            ap.setNoGravity(true);
            return ap;
        }else{
            HEShellProjectile ap = new HEShellProjectile(CBCEntityTypes.HE_SHELL.get(), level);
            ap.setFuze(CBCItems.IMPACT_FUZE.asStack());
            ap.setNoGravity(true);

            return ap;
        }
    }
}
