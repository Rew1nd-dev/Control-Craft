package com.verr1.controlcraft.mixin;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.protocol.game.ClientboundCustomPayloadPacket;
import net.minecraft.resources.ResourceLocation;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Pseudo;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

@Pseudo
@Mixin(ClientboundCustomPayloadPacket.class)
public class MixinCustomPayloadPacket {
/*

* */

//    @Redirect(
//            method = "<init>(Lnet/minecraft/resources/ResourceLocation;Lnet/minecraft/network/FriendlyByteBuf;)V",
//            at = @At(
//                    value = "INVOKE",
//                    target = "Lnet/minecraft/network/FriendlyByteBuf;writerIndex()I"
//            )
//    )
//    int printStackTrace(FriendlyByteBuf buffer){
//        int size = buffer.readableBytes();
//        if (size > 1048576) {
//            printOversizedPayloadStack(buffer);
//        }
//        return size; // 返回原始值，不影响后续逻辑
//    }
//
//    @Redirect(
//            method = "<init>(Lnet/minecraft/network/FriendlyByteBuf;)V",
//            at = @At(
//                    value = "INVOKE",
//                    target = "Lnet/minecraft/network/FriendlyByteBuf;readableBytes()I"
//            )
//    )
//    int printStackTrace2(FriendlyByteBuf buffer){
//        int size = buffer.readableBytes();
//        if (size > 1048576) {
//            printOversizedPayloadStack(buffer);
//        }
//        return size; // 返回原始值，不影响后续逻辑
//    }
//
//    private void printOversizedPayloadStack(FriendlyByteBuf data) {
//        // 创建并打印调用栈
//        StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
//        StringBuilder sb = new StringBuilder("\n[CustomPayload Debug] Oversized packet detected! Call stack:\n");
//
//        // 跳过前几个无关的栈帧
//        for (int i = 4; i < stackTrace.length && i < 15; i++) { // 限制栈深度
//            StackTraceElement element = stackTrace[i];
//            // 跳过内部调用
//            if (!element.getClassName().startsWith("java.lang.Thread") &&
//                    !element.getClassName().startsWith("your.mixins.packet")) {
//                sb.append("\t").append(element).append("\n");
//            }
//        }
//
//        System.err.println(sb);
//        System.err.println("Payload size: " + data.readableBytes() + " bytes");
//
//        // 打印数据包部分内容
//        if (data.readableBytes() > 0) {
//            int bytesToDump = Math.min(data.readableBytes(), 128);
//            byte[] partialData = new byte[bytesToDump];
//            data.getBytes(data.readerIndex(), partialData);
//            System.err.println("First 128 bytes (hex): " + bytesToHex(partialData));
//        }
//    }
//
//    // 辅助方法：字节数组转十六进制字符串
//    private static String bytesToHex(byte[] bytes) {
//        StringBuilder sb = new StringBuilder();
//        for (byte b : bytes) {
//            sb.append(String.format("%02X ", b));
//        }
//        return sb.toString().trim();
//    }

}
