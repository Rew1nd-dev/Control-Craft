package luaj;

import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.Luacuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitConstructor;
import org.luaj.vm2.LuaError;

public class LuaErrorTraceTest {
    public static void main(String[] args) {
        String longLuaScript = "function loop()\n" +
                "    -- This is a very long script to test the error message\n" +
                "    -- " + "A".repeat(100) + "\n" +
                "    error('This is a custom error')\n" +
                "end";

        try {
            LuacuitConstructor constructor = new LuacuitConstructor(longLuaScript);
            Luacuit luacuit = constructor.build();
            luacuit.onPositiveEdge();
        } catch (LuaError e) {
            String originalMessage = e.getMessage();
            String sanitizedMessage = sanitizeLuaError(originalMessage, longLuaScript);
            System.out.println("Original LuaError Message:");
            System.out.println(originalMessage);
            System.out.println("Sanitized LuaError Message:");
            System.out.println(sanitizedMessage);
            System.out.println("-------------------------");
            
            if (sanitizedMessage.contains("[script]") && !sanitizedMessage.contains("function loop()")) {
                System.out.println("Verification SUCCESS: Message sanitized correctly.");
            } else {
                System.out.println("Verification FAILED: Message not sanitized correctly.");
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Luacuit.close();
        }
    }

    private static String sanitizeLuaError(String message, String code) {
        if (message == null) return "";
        String sanitized = message.replaceAll("\\[string \".*?\"\\]", "[script]");
        if (code != null && !code.isEmpty()) {
            sanitized = sanitized.replace(code, "[script]");
        }
        return sanitized;
    }
}
