#include "mazegenerator.h"

void resize_cell_array(cell** array, cell** new_array, int length) {
    int j = 0;
    
    for(j = 0; j < length; j++) {
        new_array[j] = array[j];
        array[j]     = 0x0;
    }
    
    free((void *) array);
}

cell* remove_cell_from_array(cell** array, int position, int length) {
    int i = 0;
    
    cell* rcell = array[position];
    
    if(rcell == NULL) {
        return NULL;
    }
    
    rcell->ref--;
    array[position] = 0x0;
    
    for(i = position+1; i < length; i++) {
        if(array[i-1] == 0x0) {
            array[i-1] = array[i];
            array[i]   = 0x0;
        }
    }
    
    return rcell;
}

int get_neighbors(cell** maze, cell** neighbors, int cell_x, int cell_y, 
  int rows, int cols, int num_neighbors) {
    int c = 0;
    
    if(cell_y > 0) {            // Top
        neighbors[c] = &maze[cell_y-1][cell_x];
        maze[cell_y-1][cell_x].ref++;
        c++;
    }
    
    if(cell_x < (cols-1)) {     // Right
        neighbors[c] = &maze[cell_y][cell_x+1];
        maze[cell_y][cell_x+1].ref++;
        c++;
    }
    
    if(cell_y < (rows-1)) {     // Bottom
        neighbors[c] = &maze[cell_y+1][cell_x];
        maze[cell_y+1][cell_x].ref++;
        c++;
    }
    
    if(cell_x > 0) {            // Left
        neighbors[c] = &maze[cell_y][cell_x-1];
        maze[cell_y][cell_x-1].ref++;
        c++;
    }
    
    int c_c = c;
    for(c_c; c_c < num_neighbors; c_c++) {
        neighbors[c] = 0x0;
    }
    
    return c;
}

int get_wall_value(cell* current, cell* neighbor) {
    int cx = current->x;
    int cy = current->y;
    int nx = neighbor->x;
    int ny = neighbor->y;
    
    int value;
    if(cx < nx) { 
        value = 2; 
    } else if(cx > nx) { 
        value = 4; 
    } else if(cy > ny) {
        value = 1; 
    } else if(cy < ny) { 
        value = 3; 
    }
    
    return value;
}

void depth_first_search(cell** maze, int cell_x, int cell_y, int rows, int cols) {
    int i                        = 0;
    maze[cell_y][cell_x].visited = 1;

    cell** neighbors = (cell **) malloc(NUM_OF_NEIGHBORS * sizeof(cell *));
    
    int num_neighbors = get_neighbors(maze, neighbors, cell_x, cell_y, rows, 
      cols, NUM_OF_NEIGHBORS);
    
    int pick;
    while(num_neighbors > 0) {
        pick            = rand() % num_neighbors;
        cell* neighbor  = remove_cell_from_array(neighbors, pick, num_neighbors);
        num_neighbors--;
            
        drawx = (cell_x*2)+20;
        drawy = (cell_y*2);
        print_path(drawx,drawy);
        //sleep(1); 
        if(neighbor->visited == 0) {   // Not been visited
            int wall_value = get_wall_value(&maze[cell_y][cell_x], neighbor);

            if(wall_value == 1) {           // Top
                maze[cell_y][cell_x].top = 0;
                neighbor->bottom         = 0;
                
                print_path(drawx,drawy-1);
            } else if(wall_value == 2) {   // Right
                maze[cell_y][cell_x].right = 0;
                neighbor->left             = 0;
                
                print_path(drawx+1, drawy);
            } else if(wall_value == 3) {  // Bottom
                maze[cell_y][cell_x].bottom = 0;
                neighbor->top               = 0;
            
                print_path(drawx, drawy+1);
            } else if(wall_value == 4) {    // Left
                maze[cell_y][cell_x].left = 0;
                neighbor->right           = 0;
                
                print_path(drawx-1,drawy);
            }

            find_new_draw_coords(cell_x,cell_y, neighbor->x, neighbor->y);
            depth_first_search(maze, neighbor->x, neighbor->y, rows, cols);            
        }
    } while (num_neighbors > 0);
}

cell** generate_maze(int rows, int cols) {
    int i = 0;
    int j = 0;
    
    cell** maze = (cell **) malloc(rows * sizeof(cell *));
    
    if(maze == NULL) {
        return NULL;
    } else {
        for(i = 0; i < rows; i++) {
            maze[i] = (cell *) malloc(cols * sizeof(cell));
        }
        
        for(i = 0; i < rows; i++) {
            for(j = 0; j < cols; j++) {
                maze[i][j].top       = 1;
                maze[i][j].right     = 1;
                maze[i][j].bottom    = 1;
                maze[i][j].left      = 1;
                maze[i][j].visited   = 0;
                maze[i][j].y         = i;
                maze[i][j].x         = j;
                maze[i][j].ref       = 1;
            }
        }
    }
    
    int exit_row = rand() % (rows-1);
    depth_first_search(maze, cols-1, exit_row,rows,cols);

    return maze;
}

void find_new_draw_coords(int cx,int cy, int nx, int ny) {
    if(cx < nx) {
        drawx += 2;
    } else if(cx > nx) {
        drawx -= 2;
    }

    if(cy < ny) {
        drawy += 2;
    } else if(cy > ny) {
        drawy -= 2;
    }
}

void setupncurses() {
    if(initscr() == NULL) {
        fprintf(stderr, "Error initialising ncurses.\n");
    }
    
    scrollok(stdscr,TRUE);

    start_color();
    init_pair(1, COLOR_BLACK, COLOR_WHITE);
    init_pair(2, COLOR_WHITE, COLOR_BLACK);
    init_pair(3, COLOR_RED, COLOR_BLACK);
}

void print_path(int x, int y) {
    attron(COLOR_PAIR(1));
    mvaddstr(y,x," ");
    attron(COLOR_PAIR(1));
    refresh();
}

void restorescreen() {
    nocbreak();
    echo();
    endwin();
}

int main(int argc, char* argv[]) {
    int rows,cols;
    int i, j;
    
    if(argc == 3) {
        rows = atoi(argv[1]);
        cols = atoi(argv[2]);
    } else {
        rows = 30;
        cols = 20;
    }
    
    srand(time(NULL));

    setupncurses();

    cell** maze = generate_maze(rows,cols);

    getch();
    restorescreen();

    return 0;
}
