package saki.cli;

import picocli.CommandLine;

import java.lang.reflect.Constructor;

public class InnerClassFactory implements CommandLine.IFactory {
    private final Object outer;

    public InnerClassFactory(Object outer) {
        this.outer = outer;
    }

    public <K> K create(final Class<K> cls) {
        try {
            return CommandLine.defaultFactory().create(cls);
        } catch (Exception ex0) {
            try {
                Constructor<K> constructor = cls.getDeclaredConstructor(outer.getClass());
                return constructor.newInstance(outer);
            } catch (Exception ex) {
                try {
                    @SuppressWarnings("deprecation") // Class.newInstance is deprecated in Java 9
                    K result = cls.newInstance();
                    return result;
                } catch (Exception ex2) {
                    try {
                        Constructor<K> constructor = cls.getDeclaredConstructor();
                        return constructor.newInstance();
                    } catch (Exception ex3) {
                        throw new CommandLine.InitializationException("Could not instantiate " + cls.getName() + " either with or without construction parameter " + outer + ": " + ex, ex);
                    }
                }
            }
        }
    }
}
