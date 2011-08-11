/*****************************************************************************
 *  @author     Thomas Shaffer                                               *
 *  @version    1.0.0                                                        *
 *  @modified   ??/??/2011                                                   *
 *  @brief
 *                                                                           *
 *  Description:                                                             *
 ****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <ncurses.h>
#include <unistd.h>

/*****************************************************************************
 *  A cell is an object with 4 walls.                                        *
 *  If the wall bits (top,right,bottom,left) are set then a wall exists in   *
 *  that direction.                                                          *
 *  If the visited bit is set then the cell has been visited before.         *
 ****************************************************************************/
typedef struct {
    unsigned int top:1;     // Top wall
    unsigned int right:1;   // Right wall
    unsigned int bottom:1;  // Bottom wall
    unsigned int left:1;    // Left wall
    unsigned int visited:1; // Been visited
    int x,y;                
    int ref;                // Number of times referenced.
} cell;

int drawx;  // X-coordinate used for ncurses
int drawy;  // Y-coordinate used for ncurses

void setupncurses();
void restorescreen();

/*****************************************************************************
 * Determine new drawing coordinates based on current and next coordinates.  *
 * @param int cx - Current x                                                 *
 * @param int cy - Current y                                                 *
 * @param int nx - Next x                                                    *
 * @param int ny - Next y                                                    *
 * @note Modifies globals drawx and drawy.                                   *
 ****************************************************************************/
void find_new_draw_coords(int,int,int,int);

/*****************************************************************************
 * Abstract ncurses print lines into a function.                             *
 * @param int x - X coordinate to write at.                                  *
 * @param int y - Y coordinate to write at.                                  *
 * @note Used to print during traversal.                                     *
 ****************************************************************************/
void print_path(int,int);

/*****************************************************************************
 *  Resize array to remove invalid entries (0x0)                             *
 *  @param cell** array - The array to resize.                               *
 *  @note Assumes 1D array.                                                  *
 *  @note Assumes that all invalid entries are located at the end of the     *
 *        array.                                                             *
 *  @note array is set to the new array.                                     *
 *  @return int - The number of elements in the array.                       *
 ****************************************************************************/
void resize_cell_array(cell**, cell**, int);
 
/*****************************************************************************
 *  Remove a cell from the array.                                            *
 *  @param cell** array - Array to modify.                                   *
 *  @param int position - Position of cell to remove.                        *
 *  @param int length   - The length of the array.                           *
 *                                                                           *
 *  @note The given position is set to 0x0 and then shifted to back of array.*
 *  @note cell->ref is decremented                                           *
 *  @return cell* - The cell that was removed.                               *
 ****************************************************************************/
cell* remove_cell_from_array(cell**, int, int);
 
/*****************************************************************************
 *  Get the neighbors of the current cell.                                   *
 *  @param cell** map           - The map that is being generated.           *
 *  @param cell** neighbors     - Array of pointers to store neighbors       *
 *  @param int cell_x           - Current x                                  *
 *  @param int cell_y           - Current y                                  *
 *  @param int rows             - Total number of rows                       *
 *  @param int cols             - Total number of columns                    *
 *  @param int num_neighbors    - Total number of neighbors                  *
 *                                                                           *      
 *  @note Neighbors are stored in neighbors array.                           *
 *  @note Empty positions are set to 0x0, at the end of the array.           *
 *  @return int - Number of neighbors stored.                                *
 ****************************************************************************/
int get_neighbors(cell**, cell**, int, int, int, int,int);
 
/*****************************************************************************
 *  Get the wall that is to be removed.                                      *
 *  @param cell* current    - The current cell.                              *
 *  @param cell* neighbor   - The next cell.                                 *
 *                                                                           *
 *  @return int - The integer value of the wall that is to be removed.       *
 *  @note 1 - Top, 2 - Right, 3 - Bottom, 4 - Left                           *
 ****************************************************************************/
int get_wall_value(cell*, cell*);
 
/*****************************************************************************
 *  Algorithm:                                                               *
 *      1. Start at a particular cell and call it the "exit."                *
 *      2. Mark the current cell as visited, and get a list of its neighbors.*
 *         For each neighbor, start with a randomly selected neighbor:       *
 *          a) If that neighbor hasn't been visited, remove the wall between *
 *             this cell and that neighbor, then recurse with that neighbor  *
 *             as the current cell.                                          *
 ****************************************************************************/
 void depth_first_search(cell**, int, int, int, int);

void find_dead_ends(cell**, int, int);

cell** generate_map(int, int);
 
