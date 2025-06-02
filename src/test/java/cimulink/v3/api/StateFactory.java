package cimulink.v3.api;

import org.jetbrains.annotations.Contract;

import java.util.function.Supplier;


/**
 * A generic interface that extends the {@link Supplier} interface to provide a factory for creating instances of state S.
 *
 * @param <S> the type of state holder that this factory produces
 */
public interface StateFactory<S> extends Supplier<S> {

    /**
     * Gets a new instance of type S.
     * <p>
     * This should always create a new instance, should not return a cached one.
     *
     * @return an instance of type S
     */
    @Contract("-> new")
    @Override
    S get();

}
