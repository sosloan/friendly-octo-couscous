package com.hft.config;

import org.pkl.config.java.ConfigEvaluator;
import org.pkl.core.ModuleSource;

/**
 * Loads application configuration from a PKL file on the classpath using
 * the pkl-config-java {@link ConfigEvaluator}.
 *
 * <p>Usage:
 * <pre>{@code
 * try (PklConfigLoader loader = new PklConfigLoader()) {
 *     AppConfig config = loader.load();
 * }
 * }</pre>
 *
 * <p>By default the loader reads {@code config/app.pkl} from the classpath.
 * A different classpath-relative path can be supplied via
 * {@link #PklConfigLoader(String)}.
 */
public class PklConfigLoader implements AutoCloseable {

    private static final String DEFAULT_CONFIG_PATH = "config/app.pkl";

    private final String configPath;
    private final ConfigEvaluator evaluator;

    /** Creates a loader that reads {@code config/app.pkl} from the classpath. */
    public PklConfigLoader() {
        this(DEFAULT_CONFIG_PATH);
    }

    /**
     * Creates a loader that reads the given classpath-relative PKL file.
     *
     * @param configPath classpath-relative path to the PKL instance file
     */
    public PklConfigLoader(String configPath) {
        this.configPath = configPath;
        this.evaluator = ConfigEvaluator.preconfigured();
    }

    /**
     * Evaluates the PKL configuration file and maps it to an {@link AppConfig}.
     *
     * @return the fully-populated application configuration
     */
    public AppConfig load() {
        var config = evaluator.evaluate(ModuleSource.modulePath(configPath));

        var alpacaCfg = config.get("alpaca");
        var serverCfg = config.get("server");

        var alpaca = new AlpacaSettings(
            alpacaCfg.get("baseUrl").as(String.class),
            alpacaCfg.get("dataUrl").as(String.class),
            alpacaCfg.get("timeoutSeconds").as(int.class),
            alpacaCfg.get("mode").as(String.class)
        );

        var server = new ServerSettings(
            serverCfg.get("port").as(int.class),
            serverCfg.get("backlog").as(int.class),
            serverCfg.get("workerThreads").as(int.class)
        );

        return new AppConfig(alpaca, server);
    }

    @Override
    public void close() {
        evaluator.close();
    }
}
