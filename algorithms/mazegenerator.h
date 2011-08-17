/******************************************************************************
 *  @author     Thomas Shaffer                                                *
 *  @version    1.0.0                                                         *
 *  @modified   11/08/2011                                                    *
 *  @brief Create a maze                                                      *
 *****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <time.h>
#include <ncurses.h>
#include <unistd.h>

#define NUM_OF_NEIGHBORS 4

/******************************************************************************
 *  A cell is an object with 4 walls.                                         *
 *  If the wall bits (top,right,bottom,left) are set then a wall exists in    *
 *  that direction.                                                           *
 *  If the visited bit is set then the cell has been visited before.          *
 *****************************************************************************/
typedef struct {
    unsigned int top:1;
    unsigned int right:1;
    unsigned int bottom:1;
    unsigned int left:1;
    unsigned int visited:1;
    int x,y;
    int ref;
} cell;

int drawx;  // X-coordinate used for ncurses
int drawy;  // Y-coordinate used for ncurses

/******************************************************************************
 *  Initialize and set desired ncurse parameters.                             *
 *****************************************************************************/
inline void setupncurses();

/******************************************************************************
 *  Properly exit ncurses session.                                            *
 *****************************************************************************/
inline void restorescreen();

/******************************************************************************
 * Determine new drawing coordinates based on current and next coordinates.   *
 * @param int cx - Current x                                                  *
 * @param int cy - Current y                                                  *
 * @param int nx - Next x                                                     *
 * @param int ny - Next y                                                     *
 * @note Modifies globals drawx and drawy.                                    *
 *****************************************************************************/
void find_new_draw_coords(int,int,int,int);

/******************************************************************************
 * Abstract ncurses print lines into a function.                              *
 * @param int x - X coordinate to write at.                                   *
 * @param int y - Y coordinate to write at.                                   *
 * @note Used to print during traversal.                                      *
 *****************************************************************************/
inline void print_path(int,int);

/******************************************************************************
 *  Resize array to remove invalid entries (0x0)                              *
 *  @param cell** array - The array to resize.                                *
 *  @note Assumes 1D array.                                                   *
 *  @note Assumes that all invalid entries are located at the end of the      *
 *        array.                                                              *
 *  @note array is set to the new array.                                      *
 *  @return int - The number of elements in the array.                        *
 *****************************************************************************/
void resize_cell_array(cell**, cell**, int);
 
/******************************************************************************
 *  Remove a cell from the array.                                             *
 *  @param cell** array - Array to modify.                                    *
 *  @param int position - Position of cell to remove.                         *
 *  @param int length   - The length of the array.                            *
 *                                                                            *
 *  @note The given position is set to 0x0 and then shifted to back of array. *
 *  @note cell->ref is decremented                                            *
 *  @return cell* - The cell that was removed.                                *
 *****************************************************************************/
cell* remove_cell_from_array(cell**, int, int);
 
/******************************************************************************
 *  Get the neighbors of the current cell.                                    *
 *  @param cell** maze                                                        *
 *  @param cell** neighbors     - Array of pointers to store neighbors        *
 *  @param int cell_x           - Current x                                   *
 *  @param int cell_y           - Current y                                   *
 *  @param int rows                                                           *
 *  @param int cols                                                           *
 *  @param int num_neighbors    - Total number of neighbors                   *
 *                                                                            *
 *  @note Neighbors are stored in neighbors array.                            *
 *  @note Empty positions are set to 0x0, at the end of the array.            *
 *  @return int - Number of neighbors stored.                                 *
 *****************************************************************************/
int get_neighbors(cell**, cell**, int, int, int, int,int);
 
/*******************************************************************************
 *  Get the wall that is to be removed.                                        *
 *  @param cell* current    - The current cell.                                *
 *  @param cell* neighbor   - The next cell.                                   *
 *                                                                             *
 *  @return int - The integer value of the wall that is to be removed.         *
 *  @note 1 - Top, 2 - Right, 3 - Bottom, 4 - Left                             *
 ******************************************************************************/
int get_wall_value(cell*, cell*);
 
/******************************************************************************
 *  @param cell** maze                                                        *
 *  @param int cell_x - The current x position                                *
 *  @param int cell_y - The current y position                                *
 *  @param int rows                                                           *
 *  @param int cols                                                           *
 *  Algorithm:                                                                *
 *      1. Start at a particular cell and call it the "exit."                 *
 *      2. Mark the current cell as visited, and get a list of its neighbors. *
 *         For each neighbor, start with a randomly selected neighbor:        *
 *          a) If that neighbor hasn't been visited, remove the wall between  *
 *             this cell and that neighbor, then recurse with that neighbor   *
 *             as the current cell.                                           *
 *****************************************************************************/
 void depth_first_search(cell**, int, int, int, int);

/******************************************************************************
 *  Initialize maze and begin generation algorithm.                           *
 *  @param int rows                                                           *
 *  @param int cols                                                           *
 *  @return cell**  - The generated maze                                      *
 *****************************************************************************/
cell** generate_maze(int, int);

