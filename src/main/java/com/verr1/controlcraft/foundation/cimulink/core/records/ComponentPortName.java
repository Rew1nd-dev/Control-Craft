package com.verr1.controlcraft.foundation.cimulink.core.records;

public record ComponentPortName(String componentName, String portName) {
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ComponentPortName other) {
            return componentName.equals(other.componentName) &&
                    portName.equals(other.portName);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return componentName.hashCode() ^ portName.hashCode();
    }

    @Override
    public String toString() {
        return "[" + componentName + " | " + portName + "]";
    }
}
