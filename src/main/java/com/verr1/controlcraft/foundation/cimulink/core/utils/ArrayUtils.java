package com.verr1.controlcraft.foundation.cimulink.core.utils;

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;

public class ArrayUtils {
    public static final String INPUT_PREFIX = "i_";
    public static final String INPUT_I = "i";
    public static final String OUTPUT_O = "o";
    public static final String OUTPUT_PREFIX = "o_";

    public static final List<String> SINGLE_OUTPUT = List.of(OUTPUT_O);
    public static final List<String> SINGLE_INPUT = List.of(INPUT_I);


    public static String __in(int index){
        return INPUT_PREFIX + index;
    }

    public static String __o() {
        return OUTPUT_O;
    }

    public static List<String> createInputNames(int n){
        return IntStream.range(0, n).mapToObj(ArrayUtils::__in).toList();
    }

    public static List<String> createWithPrefix(String prefix, int n){
        return IntStream.range(0, n).mapToObj(i -> prefix + i).toList();
    }

    public static void AssertSize(Collection<?> array, int size) throws IllegalArgumentException{
        if (array.size() != size) {
            throw new IllegalArgumentException("Expected size: " + size + ", but got: " + array.size());
        }
    }

    public static void AssertSameSize(Collection<?> array, Collection<?> array1) throws IllegalArgumentException{
        if (array.size() != array1.size()) {
            throw new IllegalArgumentException("Size of Collections Are Different");
        }
    }

    public static<T extends Comparable<T>> void AssertDifferent(Collection<T> c0, Collection<T> c1) throws IllegalArgumentException{
        for (T t: c0){
            if(c1.contains(t)){
                throw new IllegalArgumentException("Object: " + t + " is duplicate in both Sets: " + c0 + " " + c1);
            }
        }
    }

    public static <T> List<T> ListOf(int n, T defaultValue){
        return IntStream.range(0, n)
                .mapToObj(i -> defaultValue)
                .toList();
    }
    public static <T> List<T> ListOf(int n, Supplier<T> defaultValue){
        return IntStream.range(0, n)
                .mapToObj(i -> defaultValue.get())
                .toList();
    }

    @SafeVarargs
    public static<T> List<T> flatten(List<T>... lists){
        return Arrays.stream(lists).flatMap(List::stream).toList();
    }


    public static<T extends Comparable<T>> void AssertDifferent(Collection<T> c0) throws IllegalArgumentException{
        Set<T> visited = new HashSet<>();
        for (T t: c0){
            if(visited.contains(t)){
                throw new IllegalArgumentException("Object: " + t + " is duplicate in Collection: " + c0);
            }
            visited.add(t);
        }
    }


    public static void AssertRange(int index, int max) throws IndexOutOfBoundsException{
        if (index < 0 || index >= max) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + max);
        }
    }

    public static<T> void AssertPresence(Collection<T> c, T member) throws IllegalArgumentException{
        if (!c.contains(member)){
            throw new IllegalArgumentException("Object: " + member + " is not present in: " + c);
        }
    }

    public static<T> void AssertAbsence(Collection<T> c, T member) throws IllegalArgumentException{
        if (c.contains(member)){
            throw new IllegalArgumentException("Object: " + member + " is already exist in: " + c);
        }
    }

    public static List<String> createOutputNames(int n){
        return IntStream.range(0, n).mapToObj(ArrayUtils::__out).toList();
    }

    public static String __out(int index){
        return OUTPUT_PREFIX + index;
    }

    public static<T, S> List<T> mapToList(Collection<S> collection, Function<S, T> mapper) {
        return collection.stream().map(mapper).toList();
    }


    public static void checkName(String name){
        if(name.contains("@")){
            throw new IllegalArgumentException("@ is preserved, should not be used as normal name");
        }
    }
}
