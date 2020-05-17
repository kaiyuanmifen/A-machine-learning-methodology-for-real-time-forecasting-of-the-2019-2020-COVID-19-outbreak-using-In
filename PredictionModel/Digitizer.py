import matplotlib.pyplot as plt
import pandas as pd
import cv2
import numpy as np
import datetime
import glob
import os

#Mapper Functions
def mapper(value_to_map, x1,x2,a1,a2):
    result_value = ((value_to_map - x1)*(a2-a1))/(x2-x1) + a1
    return result_value

# Digitizer
def find_color_within_img(matrix, graph_line_color, eps=25):
    # Finds values corresponding to v1,v2 and v3 in array dimensions 1,2 and 3
    for dimension, (dim_label, dim_color_value) in enumerate(graph_line_color.items()):

        if dimension == 0:
            color_array = np.greater_equal(matrix[:, :, dimension], dim_color_value - eps) * np.less_equal(
                matrix[:, :, dimension], dim_color_value + eps)

        else:
            color_array *= np.greater_equal(matrix[:, :, dimension], dim_color_value - eps) * np.less_equal(
                matrix[:, :, dimension], dim_color_value + eps)

    return color_array
#Mapping values
def get_y_values(filtered_img, x_list, y1, y2, y_map1, y_map2, mapper_funct=mapper):
    """
    filtered_img : array
        2 dimensional array where the values of the function are
    x_list :
        values of interest within img (columns) to map values
    y1,y2 :
        y-coordinate range
    y_map1, y_map2 :
        y-coordinate range to map
    """
    y_list = []
    for x_pos in x_list:

        img_col = filtered_img[:, x_pos] * 1
        # print(img_col.sum())

        y_vals = []
        for i, val in enumerate(img_col):
            if val == 1 and not np.isnan(mapper(i, y1, y2, y_map1, y_map2)):
                y_vals.append(mapper(i, y1, y2, y_map1, y_map2))

                # [mapper(i, y1,y2, y_map1, y_map2) for i, val  in enumerate(img_col) if val == 1]
        if len(y_vals) == 0:
            print(img_col)
            print(y_vals)
            print(x_pos)

        y_list.append(np.mean(y_vals))

    return y_list


if __name__ == "__main__":

    #

    #
    # img_folder = '/Users/dianboliu/Google_Drive/research/Coronavirus/data/coronavirus_data/baiduSearch/20200216_early/coronavirus_symptom/'
    # import os

    # All_files = []
    # for root, dirs, files in os.walk(img_folder):
    #     for filename in files:
    #         if "png" in filename:
    #             print(filename)
    #             All_files.append(filename)
    #
    # AllNUmber=[int(FileName[7:20]) for FileName in All_files ]
    #
    # AllNUmber.sort()
    #
    #order of the files to int in provice


    img_folder = '/Users/dianboliu/Google_Drive/research/Coronavirus/data/coronavirus_data/baiduSearch/20200331_late/how_many_degree_is_fever/'
    import os

    files = glob.glob(img_folder+"*.png")

    files.sort(key=os.path.getmtime, reverse=False)
    AllFilesName=[]
    for file in files:
        print(file)
        AllFilesName.append(os.path.split(file)[1])

    Max_values=[12000,800,120,1500,600,600,1500,500,300,400,
                1200,700,1200,1000,1200,300,500,1200,600,800,400,
                210,180,1000,1000,1200,600,800,600,140,140,150,250,400,1000]

    NamesOfProvinces_in_order = ["china","anhui", "macau", "beijing","chongqin", "fujian", "guangdong", "guangxi", "gansu", "guizhou",
                                 "hebei", "helongjiang",
                                 "henan", "hunan", "hubei", "hainan", "jilin", "jiangsu", "jiangxi", "liaoning",
                                 "innermongolia", "ningxia",
                                 "qinghai", "shanghai", "sichuan", "shandong", "shanxi", "shaanxi", "tianjin", "taiwan",
                                 "tibet", "hongkong", "xinjiang", "yunnan", "zhejiang"
                                 ]

    FileToLocation={AllFilesName[i]: NamesOfProvinces_in_order[i] for i in range(len(AllFilesName))}

    trend_max ={AllFilesName[i]: Max_values[i] for i in range(len(AllFilesName))}

    [print(i) for i in trend_max]




    os.walk(img_folder)
    #city = list(trend_max.keys())[1]
    #image = cv2.imread(img_folder + city )

    # STEP 1 check img is ok
    from PIL import Image
    city = list(trend_max.keys())[1]
    image =  np.array(Image.open(img_folder+city).resize((1536,860)))
    plt.imshow(image)
    plt.gcf().set_facecolor('xkcd:white')
    plt.gcf().set_size_inches([20,24])




    # STEP 2 DEFINE VALUES
    # STEP 2 DEFINE VALUES
    # Params DEFINING DATES
    resolution = 90  # Days
    start_date = '2020-01-01'
    end_date = '2020-03-31'
    graph_line_color = {'dim0': 0, 'dim1': 0, 'dim2': 0}
    # Params DEFINING PIXEL LIMITS and MAPPING VALUES
    x1, x2 = 40, 1494
    y1, y2 = 691, 290

    x_map1, x_map2 = 1, resolution
    y_map1, y_map2 = 0, 1  # y_map is the max value of the graph (leave it 1 since it gets scaled using trend_max in the loop)

    # position of color square on baidu image
    color_x1, color_x2 = 49, 70
    color_y1, color_y2 = 214, 190  # upside down
    eps = 5  # extra range to remove color values

    # Getting Date vector (check if ok)

    start_date_obj = datetime.datetime.strptime(start_date, "%Y-%m-%d")
    date_values = []
    for i in range(0, resolution):
        date_values.append((start_date_obj + datetime.timedelta(days=i)).strftime("%Y-%m-%d"))
    print(date_values)



    #STEP 3 CHECK LIMITS OF FUNCTIONS ARE CONSISTENT (start and end of function and 0 and max value for y), if not fix the values in
    #STEP 2

    plt.imshow(image)
    plt.xticks([x1, x2], [x1, x2])
    plt.plot([10, 1500], [y1, y1], '--', color='tab:red')
    plt.plot([10, 1500], [y2, y2], '--', color='tab:red', label='Y-axis limits')

    plt.plot([x1, x1], [0, 800], '--', color='tab:blue')
    plt.plot([x2, x2], [0, 800], '--', color='tab:blue', label='X-axis limits')

    plt.legend()
    plt.gcf().set_facecolor('xkcd:white')
    plt.gcf().set_size_inches([20, 24])
    # code
    graph_line_color = {'dim0': 0, 'dim1': 0, 'dim2': 0}

    for i, k in enumerate(list(graph_line_color.keys())):
        graph_line_color[k] = list(set(image[color_y2:color_y1, color_x1:color_x2, i].ravel()))[0]

    print(graph_line_color)


    # Run for all cities
    for city, max_val in trend_max.items():
        y_map2 = max_val
        image = np.array(Image.open(img_folder + city).resize((1536, 860)))  # cv2.imread(img_folder+city)
        # erasing color square within baidu img
        image[color_y2 - eps:color_y1 + eps, color_x1 - eps:color_x2 + eps, :] = 255

        # code FINDING CURVE
        filtered_array = find_color_within_img(image, graph_line_color, eps=40)

        '''
        f, axarr = plt.subplots(2,1)
        axarr[0].imshow(image)
        axarr[1].imshow(filtered_array)

        plt.gcf().set_facecolor('xkcd:white')
        plt.gcf().set_size_inches([20,24])
        '''
        # Code getting values

        # code mapping values
        x_list = list(np.arange(x1, x2, (x2 - x1) / (resolution - 1)))
        x_list = [int(val) for val in x_list]
        #x_list.append(x2)
        print('pixel values for {0} days =>'.format(resolution), x_list)
        print(len(x_list))
        print(len(date_values))
        mapped_x = [mapper(x_val, x1, x2, x_map1, x_map2) for x_val in x_list]
        print('mapped x values =>', mapped_x)
        mapped_y = get_y_values(filtered_array, x_list, y1, y2, y_map1, y_map2, mapper_funct=mapper)
        print('mapped y values =>', mapped_y)

        '''
        f, axarr = plt.subplots(2,1)
        axarr[0].imshow(image[y2:y1,x1:x2])
        axarr[1].scatter(mapped_x, mapped_y)
        axarr[1].plot(mapped_x, mapped_y)
        
        plt.gcf().set_facecolor('xkcd:white')
        plt.gcf().set_size_inches([10,12])
        '''


        df = pd.DataFrame({'day':mapped_x,'search_trend':mapped_y}, index=date_values)
        df.to_csv(img_folder +'/{0}.csv'.format(FileToLocation[city]))




