import java.util.ArrayList;
import java.util.Arrays;
import java.util.stream.IntStream;

class CellularAutomata {

    private ArrayList<int[]> automata;

    public CellularAutomata(int width, int height, int rule) {
        this.automata = this.generateCellularAutomata(width, height, rule);
    }

    // <-- generating CA

    private ArrayList<int[]> generateCellularAutomata(int width, int height, int rule) {
        ArrayList<int[]> cellular_automata = new ArrayList<int[]>();

        int[] row = initialRow(width);

        for(int h = 0; h < height; h++) {
            cellular_automata.add(row);

            row = nextRow(rule, row);
        }

        return cellular_automata;
    }

    private int[] initialRow(int width){

        int half_of_width = width / 2;
        int[] row = new int[2 * half_of_width + 1];

        Arrays.fill(row, 0);
        row[half_of_width] = 1;

        return row;
    }

    private int[] nextRow(int rule, int[] previous_row) {
        int[] new_row = new int[previous_row.length];

        new_row[0] = new_row[previous_row.length-1] = 0;

        int c1, c2, c3;

        for(int i=0, n=1; i < previous_row.length - 2; i++, n++) {
            c1 = previous_row[i];
            c2 = previous_row[i+1];
            c3 = previous_row[i+2];

            new_row[n] = transformTrio2Cell(rule, c1, c2, c3);
        }

        return new_row;
    }

    private int transformTrio2Cell(int rule, int c1, int c2, int c3) {
        int decimal_value = 4*c1 + 2*c2 + c3;

        return (rule >> decimal_value ) % 2;
    }

    // <-- resizing CA

    public void resizeXY(int zoom) {
        ArrayList<int[]> resizedAutomata = new ArrayList<int[]>();

        for(int[] row : this.automata) {
            int[] resized_row = resizeRow(zoom, row);

            for(int z = 0; z < zoom; z++) {
                resizedAutomata.add(resized_row);
            }
        }

        this.automata = resizedAutomata;
    }

    private int[] resizeRow(int zoom, int[] row) {
        return Arrays.stream(row)
                .flatMap((c)-> replicate(zoom, c))
                .toArray();
    }

    private IntStream replicate(int zoom, int cell) {
        int[] replicated = new int[zoom];

        return Arrays.stream(replicated).map((c) -> cell);
    }

    // <-- trimming margins

    public void trimMargins(int margin) {
        int start_position = margin;
        int end_position = this.automata.get(0).length - margin;

        ArrayList<int[]> trimmed = new ArrayList<int[]>();

        this.automata.stream().map(
                (row) -> Arrays.copyOfRange(row, start_position, end_position)
        ).forEach(
                trimmed::add
        );

        this.automata = trimmed;
    }

    // <-- printing CA

    @Override
    public String toString() {
        StringBuilder prettyString = new StringBuilder();

        automata.stream()
                .forEach(row -> {
                    Arrays.stream(row).forEach(cell -> prettyString.append(asciiArt(cell)));
                    prettyString.append('\n');
                });

        return prettyString.toString();
    }

    private char asciiArt(int cell) {
        return (cell==0) ? '□' : '■';
    }

    @Override
    public int hashCode() {
        return this.automata.stream().flatMapToInt(Arrays::stream).sum();
    }
}
