import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;

public class Main {

    public static void main(String[] args) {

        if(args.length < 4) {
            System.err.println("Usage: native.jar <width> <height> <rule> <zoom>");
        } else {
            int width = Integer.parseInt(args[0]);
            int height = Integer.parseInt(args[1]);
            int rule = Integer.parseInt(args[2]);
            int zoom = Integer.parseInt(args[3]);

            int ca_width = width / zoom;
            int ca_height = height / zoom;
            int margin = (ca_height -1) / zoom;

            CellularAutomata automata = new CellularAutomata(ca_width + 2 * margin, ca_height, rule);
            automata.trimMargins(margin);
            automata.resizeXY(zoom);

            System.out.println(automata.hashCode());

//            String output = String.format("fractals/jv-%d-%d-%dx%d.txt", rule, zoom, width, height);
//            try {
//                PrintWriter writer = new PrintWriter(new File(output));
//                writer.print(automata.toString());
//                writer.close();
//            } catch (IOException e) {
//                e.printStackTrace();
//            }
        }
    }
}
