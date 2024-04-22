from treeDataScraper import TestTreedata
import pandas as pd
import sys
import re
from bs4 import BeautifulSoup

# folderPath = ''
# sys.path.append(folderPath)

treeFilePath = 'C:\\Users\\alek-\\Documents\\Wageningen Period 5\\Planning and Design of Urban Spaces\\Project Data\\Types of trees.csv'
treeFileOutputPath = 'C:\\Users\\alek-\\Documents\\Wageningen Period 5\\Planning and Design of Urban Spaces\\Project Data\\Tree_Data\\Data_'
treeFileFullOutputPath = 'C:\\Users\\alek-\\Documents\\Wageningen Period 5\\Planning and Design of Urban Spaces\\Project Data\\Tree_Data\\Data_All_Trees.xlsx'
treeDFPickleOutputPath = 'C:\\Users\\alek-\\Documents\\Wageningen Period 5\\Planning and Design of Urban Spaces\\Project Data\\Tree_data\\Tree_Data.pickle'

data_trees = pd.read_csv(
    treeFilePath,  # .. to move up one directory from current directory
    skipinitialspace=True,  # ignore spaces after comma separator
    header=0,  # row with column names, 0-indexed, excluding spaces
    usecols=['SOORT_WET', 'Count'],  # columns to use
    encoding='latin1'
    # usecols = ['YEAR','MO', 'DY', 'TG', 'Q', 'RH'],
    # index_col=0,  # column with row names from used columns, 0-indexed
)

def dataGatherPage(page_source):
    soup = BeautifulSoup(page_source, 'html.parser')
    # treeDataNames = list()
    # s_category = {}
    category = {}
    details = {}
    detail_selectors = soup.find_all('div', class_=['plant-specs plant-detail-column specs double',
                                                'plant-specs plant-detail-column specs double two'])
    # detail_selector2 = soup.find('div', class_='plant-specs plant-detail-column specs double two')
    for detail_selector in detail_selectors:
        details_select = detail_selector.find_all('div', class_='spec-group')
        print(type(details_select))
        # print(details_select)
        for detail_div in details_select:
            # category = {}
            print("Details_div:")
            print(type(detail_div))
            try:
                detail_name = detail_div.find('div', class_='title').find('h3').get_text()
                print("Detail Name:")
                print(detail_name)
                details_sub_cat = detail_div.find_all('div', class_='row')
                for sub_cat in details_sub_cat:
                    try:
                        inner = sub_cat.find_all('div',class_=re.compile('inner*'))
                        for in_sub in inner:
                            sub_category = in_sub.find('h4').get_text()
                            print("Sub category:")
                            print(sub_category)
                            vals = in_sub.find_all('span')
                            val_list = list()
                            prev_item = ''
                            prev_end_item = ''
                            value = ''
                            for val in vals:
                                prev_end_item = value
                                value = val.get_text().strip()
                                if value != prev_item:
                                    print("Value:")
                                    print(value)
                                    val_list.append(value)
                                    prev_item = value
                            if value == 'm' or prev_end_item == '-':
                                value = ''.join(val_list)
                                print(value)
                                category[sub_category] = [value]
                            else:
                                category[sub_category] = val_list
                        pass
                    except:
                        # inner = sub_cat.find_all('div',class_=re.compile('inner*'))
                        # for in_sub in sub_cat:
                        sub_category = sub_cat.find('h4').get_text()
                        print("Sub category:")
                        print(sub_category)
                        vals = sub_cat.find_all('span')
                        val_list = list()
                        prev_item = ''
                        prev_end_item = ''
                        value = ''
                        for val in vals:
                            prev_end_item = value
                            value = val.get_text().strip()
                            if value != prev_item:
                                print("Value:")
                                print(value)
                                val_list.append(value)
                            prev_item = value
                        if value == 'm' or prev_end_item == '-':
                            value = ''.join(val_list)
                            print(value)
                            category[sub_category] = [value]
                        else:
                            category[sub_category] = val_list
                        pass

                #details[detail_name] = category
                pass
            except:
                continue
    # print(category)
    details_df = pd.DataFrame.from_dict(category, orient='index')
    print(details_df)
    details_df.transpose()
    return details_df




treeScrape = TestTreedata()
treeHTMLData = list()
treeHTMLPageSource = list()
TreeDataDict = {}
# MALUS ONBEKEND = Malus Domestica -> need to handle this later
# Had to replace Crataegus crus-galli with Crataegus x lavalleei since that was the closest equivalent in the database
# malus baccata instead of malus almey
# treeHTMLData = treeScrape.test_treedata(data_trees['SOORT_WET'])
# Prunus domestica 'Reine Claude d'Althan' instead of Prunus domestica 'Reine Claude'\
# Cydonia oblonga instead of Cydonia oblonga 'Champion'
#Cydonia oblonga 'Leskovacka' instead of Cydonia oblonga 'Lescova
#Liriodendron tulipifera instead of Liriodendron tulipiferum
# Styphnolobium japonicum instead of Sophora japonica
# Ulmus x hollandica 'Commelin' instead of Ulmus x hollandica
# Aesculus x carnea 'Briotii' instead of Aesculus carnea 'Briotii'
#Fagus sylvatica 'Aspleniifolia' instead of Fagus sylvatica 'Asplenifolia'
# Fagus sylvatica 'Atropunicea' instead of Fagus sylvatica 'Atropurpurea'
# Sorbus x thuringiaca 'Fastigiata' instead of Sorbus thuringiaca
i = 0
with pd.ExcelWriter(treeFileFullOutputPath) as writer:
    for tree in data_trees['SOORT_WET']:
        print(tree)
        print(i)
        i += 1
        treeN = tree
        if tree == 'Malus Onbekend':
            tree = 'Malus domestica'
        elif tree == "Prunus 'Onbekend'":
            tree = 'Prunus serrulata'
        treeScrape.setup_method()
        element, source = treeScrape.test_treedata(tree)
        # print(element)
        # print(source)
        treeHTMLData.append(element)
        treeHTMLPageSource.append(element)
        treeScrape.teardown_method()
        treeDF = dataGatherPage(source)
        # path = treeFileOutputPath + treeN + '.xlsx'
        treeDF.to_excel(writer, sheet_name=treeN)
        TreeDataDict[treeN] = treeDF
        print(i)
