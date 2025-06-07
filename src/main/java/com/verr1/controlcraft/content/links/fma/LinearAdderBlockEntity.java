package com.verr1.controlcraft.content.links.fma;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.FMALinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.links.Coefficients;
import com.verr1.controlcraft.foundation.data.links.NamedCoeff;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class LinearAdderBlockEntity extends CimulinkBlockEntity<FMALinkPort> {

    public static final SerializeUtils.Serializer<NamedCoeff> NAMED_COEFF_SERIALIZER =
            SerializeUtils.of(
                    NamedCoeff::serialize,
                    NamedCoeff::deserialize
            );

    public static final SerializeUtils.Serializer<List<NamedCoeff>> COEFF_LIST_SERIALIZER =
            SerializeUtils.ofList(NAMED_COEFF_SERIALIZER);

    public static final SerializeUtils.Serializer<Coefficients> COEFF_SERIALIZER =
            SerializeUtils.of(
                    c -> COEFF_LIST_SERIALIZER.serialize(c.content()),
                    t -> new Coefficients(COEFF_LIST_SERIALIZER.deserialize(t))
            );

    public static final NetworkKey COEFF = NetworkKey.create("coeffs");
    public static final NetworkKey INC = NetworkKey.create("increase_input");
    public static final NetworkKey DEC = NetworkKey.create("decrease_input");

    public LinearAdderBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(COEFF)
                .withBasic(SerializePort.of(
                        this::viewCoefficients,
                        this::setCoefficients,
                        COEFF_SERIALIZER
                ))
                .withClient(new ClientBuffer<>(COEFF_SERIALIZER, Coefficients.class))
                .register();


        panel().registerUnit(INC, this::increaseInput);
        panel().registerUnit(DEC, this::decreaseInput);
    }


    public void increaseInput(){
        List<Double> coeffs = linkPort().viewCoefficients();
        List<Double> newCoeffs = new ArrayList<>(coeffs);
        newCoeffs.add(1.0);
        linkPort().setCoefficients(newCoeffs);
    }

    public void decreaseInput(){
        List<Double> coeffs = linkPort().viewCoefficients();
        linkPort().setCoefficients(coeffs.subList(0, coeffs.size() - 1));
    }

    public void setCoefficients(Coefficients coeffs){
        linkPort().setNamedCoefficients(coeffs.content().stream().map(NamedCoeff::mapToPair).toList());
    }

    public Coefficients viewCoefficients(){
        return new Coefficients(linkPort().viewNamedCoefficients().stream().map(NamedCoeff::fromPair).toList());
    }

    @Override
    protected FMALinkPort create(@NotNull Level level, BlockPos pos) {
        return new FMALinkPort(WorldBlockPos.of(level, pos));
    }
}
