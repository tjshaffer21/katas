def bubble_sort(list)
    for i in range(0, list.length-1)
        if list[i] > list[i+1]
            e1 = list[i]
            e2 = list[i+1]
            
            list[i]   = e2
            list[i+1] = e1
        end
    end
end

list = [3,2,4,5,1,0]
bubble_sort(list)