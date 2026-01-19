import com.verr1.controlcraft.unstable.management.CoroutineBase;
import com.verr1.controlcraft.unstable.management.Coroutines;
import com.verr1.controlcraft.unstable.management.RoutineStatus;

public class Test {

    public static void main(String[] args) {
        CoroutineBase c0 = Coroutines.immediate(() -> log(0));
        CoroutineBase c1 = Coroutines.immediate(() -> log(1));
        CoroutineBase c2 = Coroutines.immediate(() -> log(2));
        CoroutineBase c3 = Coroutines.immediate(() -> log(3));
        CoroutineBase c = Coroutines.chained(c0, c1, Coroutines.chained(c2, c3));
        c.force();
    }

    public static void log(Object obj){
        System.out.println(obj);
    }
}
