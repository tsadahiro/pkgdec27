#include <iostream>
#include <vector>
#include <unordered_map>
#include <random>
#include <algorithm>
#include <cassert>
#include <string>
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// =============================
// Java class Cell に対応
// =============================
struct Cell {
    int x;
    int y;

    Cell(int _x=0, int _y=0) : x(_x), y(_y) {}

    bool operator==(const Cell &other) const {
        return x == other.x && y == other.y;
    }
};

// Cell を unordered_map のキーに使うためのハッシュ
struct CellHash {
    size_t operator()(const Cell &c) const noexcept {
        return ((size_t)c.x << 32) ^ (size_t)c.y;
    }
};


// =============================
// Java class ShiftedBoard に対応
// =============================
class ShiftedBoard {
public:
    vector<int> rowLength;   // Java の int[] rowLength
    mt19937 rng;             // Java の new Random()

    ShiftedBoard(const vector<int> &rl) {
        rowLength = rl;
        rng.seed(random_device{}());
    }

    // Java: Cell[] hook(Cell cell)
    vector<Cell> hook(const Cell &cell) {
        vector<Cell> h;

        // Java: if (cell.y > rowLength[cell.x]+cell.x-1) return empty
        if (cell.y > rowLength[cell.x] + cell.x - 1)
            return h;

        // Java 第一ループ
        for (int i = cell.x + 1; i <= cell.y && i < (int)rowLength.size(); i++) {
            if (i + rowLength[i] <= cell.y) break;
            h.push_back(Cell(i, cell.y));
        }

        // Java 第二ループ
        for (int i = cell.y + 1; i < rowLength[cell.x] + cell.x; i++) {
            h.push_back(Cell(cell.x, i));
        }

        // Java 第三ループ
        if (cell.y < (int)rowLength.size() - 1) {
            for (int j = cell.y + 1; j <= cell.y + rowLength[cell.y + 1]; j++) {
                h.push_back(Cell(cell.y + 1, j));
            }
        }
        return h;
    }

    // Java: boolean isCourner
    bool isCorner(const Cell &cell) {
        if (cell.y != cell.x + rowLength[cell.x] - 1) return false;
        if (!(cell.x == (int)rowLength.size() - 1 ||
              cell.x + rowLength[cell.x + 1] < cell.y))
            return false;
        return true;
    }

    // Java: hookWalkStep
    Cell hookWalkStep(const Cell &cell) {
        auto h = hook(cell);
        if (h.empty()) return cell;
        //uniform_int_distribution<int> dist(0, h.size() - 1);
        //return h[dist(rng)];
	// 修正後（Rの乱数を使う）
	int idx = floor(R::unif_rand() * h.size());
	return h[idx];	
    }

    // Java: hookWalk
    Cell hookWalk(const Cell &start) {
        Cell c = start;
        while (!isCorner(c)) {
            c = hookWalkStep(c);
        }
        return c;
    }

    // Java: randomCell
    Cell randomCell() {
        int total = 0;
        for (int L : rowLength) total += L;

        uniform_int_distribution<int> dist(0, total - 1);
        int idx = dist(rng);

        int sum = 0;
        for (int row = 0; row < rowLength.size(); row++) {
            if (idx < sum + rowLength[row]) {
                return Cell(row, row + (idx - sum));
            }
            sum += rowLength[row];
        }
        return Cell(0, 0);
    }

    // Java: ShiftedBoard remove(Cell cell)
    ShiftedBoard removeCell(const Cell &cell) {
        vector<int> newb = rowLength;

        newb[cell.x]--;
        if (newb[cell.x] == 0) {
            newb.erase(newb.begin() + cell.x);
        }
        return ShiftedBoard(newb);
    }

    int size() const {
        int s = 0;
        for (int L : rowLength) s += L;
        return s;
    }

    int numRow() const { return rowLength.size(); }
};


// =============================
// Java class SYT に対応
// =============================
class SYT {
public:
    unordered_map<Cell, int, CellHash> tab;   // Java の HashMap<Cell,Integer>
    vector<Cell> positions;                   // Java の Cell[] positions
    ShiftedBoard board;                       // Java field board
    int maxv = 0;                             // Java の int max

    // SYT のコンストラクタ：board を空の ShiftedBoard で初期化する
    SYT() : board(vector<int>()) {
        // tab はデフォルトで空の unordered_map になるので特に何もしなくて良い
        // positions は sample() でセットされるためここでは初期化不要
    }

    // Java: sample(ShiftedBoard)
    SYT sample(const ShiftedBoard &orig) {
        SYT s;
        s.board = orig;

        int max = orig.size();
        s.positions.resize(max + 1);

        ShiftedBoard B = orig;

        while (B.numRow() > 0) {
            Cell start = B.randomCell();
            Cell corner = B.hookWalk(start);

            B = B.removeCell(corner);
            s.tab[corner] = B.size() + 1;
            s.positions[B.size() + 1] = corner;
        }
        s.maxv = max;
        return s;
    }

    Cell maxCell() const {
        return positions[maxv];
    }

    // Java: EGCell()
    Cell EGCell() {
        Cell m = maxCell();
        Cell west(m.x, m.y - 1);
        Cell north(m.x - 1, m.y);

        bool hasW = tab.count(west);
        bool hasN = tab.count(north);

        if (hasW && hasN) {
            return (tab.at(west) < tab.at(north)) ? north : west;
        }
        if (hasW) return west;
        if (hasN) return north;
        return m;
    }

    // Java: EGCell(Cell cell)
    Cell EGCell(const Cell &cell) {
        Cell west(cell.x, cell.y - 1);
        Cell north(cell.x - 1, cell.y);

        bool hasW = tab.count(west);
        bool hasN = tab.count(north);

        if (hasW && hasN)
            return (tab.at(west) < tab.at(north)) ? north : west;

        if (hasW) return west;
        if (hasN) return north;
        return cell;
    }

    // Java: void evacuate()
    void evacuate() {
        Cell c = maxCell();

        while (!(c == EGCell(c))) {
            Cell next = EGCell(c);
            int val = tab[next];

            tab[c] = val;
            positions[val] = c;

            c = next;
        }

        tab.erase(c);
        maxv--;
    }

    int size() const {
        return tab.size();
    }
};




// [[Rcpp::export]]
IntegerVector shifted_hook_walk_xtrace(int n) {

    Rcpp::RNGScope scope;

    // shifted staircase shape: (2n-1, 2n-3, ..., 1)
    std::vector<int> shape(n);
    for (int i = 0; i < n; i++) {
        shape[i] = 2 * (n - i) - 1;
    }

    ShiftedBoard board(shape);
    SYT syt;
    syt = syt.sample(board);

    std::vector<int> xs;
    while (syt.size() > 1) {
      //xs.push_back(syt.maxCell().x);
      xs.push_back(n-1-syt.maxCell().x);
      syt.evacuate();
    }
    xs.push_back(n-1-syt.maxCell().x);

    return wrap(xs);
}


//int main() {
//    int n = 1000;
//
//    vector<int> b(n);
//    for (int i = 0; i < n; i++) {
//        b[i] = 2 * (n - i) - 1;   // Java の b[i] = 2*(n-i)-1;
//    }
//
//    ShiftedBoard board(b);
//
//    SYT syt;
//    syt = syt.sample(board);
//
//    while (syt.size() > 1) {
//        // Java: System.out.println(syt.size() + "," + syt.maxCell().x);
//        cout << syt.size() << "," << syt.maxCell().x << "\n";
//        syt.evacuate();
//    }
//
//    cout << syt.size() << "," << syt.maxCell().x << "\n";
//    return 0;
//}
