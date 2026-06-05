package com.verr1.controlcraft.content.links.computer;

public record ComputerDisplayMetrics(
        int pixelWidth,
        int pixelHeight,
        float surfaceWidth,
        float surfaceHeight) {

    public static final ComputerDisplayMetrics DEFAULT = new ComputerDisplayMetrics(256, 256, 1.0f, 1.0f);

    public ComputerDisplayMetrics {
        if (pixelWidth <= 0) {
            throw new IllegalArgumentException("pixelWidth must be positive");
        }
        if (pixelHeight <= 0) {
            throw new IllegalArgumentException("pixelHeight must be positive");
        }
        if (surfaceWidth <= 0.0f) {
            throw new IllegalArgumentException("surfaceWidth must be positive");
        }
        if (surfaceHeight <= 0.0f) {
            throw new IllegalArgumentException("surfaceHeight must be positive");
        }
    }

    public static ComputerDisplayMetrics ofPixels(int pixelWidth, int pixelHeight) {
        return new ComputerDisplayMetrics(pixelWidth, pixelHeight, 1.0f, 1.0f);
    }
}