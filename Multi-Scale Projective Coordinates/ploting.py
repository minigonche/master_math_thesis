import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

def plot_persistence_diagram(diagram, d, number_of_lines = np.inf, plot_title = None, show = True):
    '''
    Description
    -----------------------------
    Plots the persistence barplot for the given Dionysus diagram. Excludes all intervals
    end at infinity.
    -----------------------------    
    Parameters
    -----------------------------
    Diagram : dionysus.Diagram
        diagrams for a specific prime from omnifield persistence and filtration.
    d : integer >= 0
        The dimension one wishes to plot
    number_of_lines : Integer > 0
        The number of lines the plot will contain. Will select from the longest ones.
    -----------------------------
    '''

    
    #Converts  the diagram into a pandas DataFrame
    birth = []
    death = []
    for i in range(len(diagram[d])):
        if(not(np.isinf(diagram[d][i].death))):
            birth.append(diagram[d][i].birth)
            death.append(diagram[d][i].death)
    
    if(len(birth) == 0):
        raise ValueError('All intervals are from 0 to infinity')
    
    birth = np.array(birth)
    death = np.array(death)
    
    dic = {'birth': birth, 'death': death, 'length': death - birth}
    data = pd.DataFrame(data=dic)
    
    #Sorts data
    data.sort_values('length', ascending = False, inplace = True)
    
    #Real Number of lines
    stop = min(number_of_lines,data.shape[0])
    
    #Subsets the data
    data = data.iloc[0:stop]
    
    #Resorts by birth so the plot looks good
    data.sort_values('birth', ascending = True, inplace = True)
        
    #Starts Plot
    #Declares the upper shift of the first bar
    shift = 0.5
    fig = plt.figure()
    ax = fig.add_subplot(111)
    
    for i in range(stop):
        x = [data.iloc[i]['birth'],data.iloc[i]['death']]
        y = [i + shift,i + shift]
        line = Line2D(x, y, color = 'red')
        ax.add_line(line)
        
    ax.set_xlim(0, max(data['death']) + 1)
    ax.set_ylim(0, stop+1)

    if(plot_title is None):
        plot_title = str(number_of_lines) + ' Longest Persisten Bar Codes for Dimension ' + str(d)        

    plt.title(plot_title)    

    if(show):
        plt.show()

        

def plot_persistence_diagram_many(diagrams, d, ncol, nrow, number_of_lines = np.inf, plot_title = None, show = True,  figsize=(28, 22)):
    '''
    Description
    -----------------------------
    Plots the persistence barplot for the given Dionysus diagram. Excludes all intervals
    end at infinity.
    -----------------------------    
    Parameters
    -----------------------------
    Diagrams : list[dionysus.Diagram]
        diagrams for a specific prime from omnifield persistence and filtration.
    d : integer >= 0
        The dimension one wishes to plot
    number_of_lines : Integer > 0
        The number of lines the plot will contain. Will select from the longest ones.
    -----------------------------
    '''
    
    fig, ax = plt.subplots(nrow,ncol, figsize = figsize, facecolor='w', edgecolor='k')
    
    ax = ax.ravel()
    
    figure_id = 0
    for diagram in diagrams:
        
        #Converts  the diagram into a pandas DataFrame
        birth = []
        death = []
        for i in range(len(diagram[d])):
            if(not(np.isinf(diagram[d][i].death))):
                birth.append(diagram[d][i].birth)
                death.append(diagram[d][i].death)
        if(len(birth) == 0):
            raise ValueError('All intervals are from 0 to infinity')
        birth = np.array(birth)
        death = np.array(death)
        dic = {'birth': birth, 'death': death, 'length': death - birth}
        data = pd.DataFrame(data=dic)
        #Sorts data
        data.sort_values('length', ascending = False, inplace = True)
        #Real Number of lines
        stop = min(number_of_lines,data.shape[0])
        #Subsets the data
        data = data.iloc[0:stop]
        #Resorts by birth so the plot looks good
        data.sort_values('birth', ascending = True, inplace = True)
        #Starts Plot
        #Declares the upper shift of the first bar
        shift = 0.5
        for i in range(stop):
            x = [data.iloc[i]['birth'],data.iloc[i]['death']]
            y = [i + shift,i + shift]
            line = Line2D(x, y, color = 'red')
            ax[figure_id].add_line(line)
        ax[figure_id].set_xlim(0, max(data['death']) + 1)
        ax[figure_id].set_ylim(0, stop+1)
        figure_id = figure_id +1
    if(plot_title is None):
        plot_title = str(number_of_lines) + ' Longest Persisten Bar Codes for Dimension ' + str(d)        
    plt.suptitle(plot_title, fontsize=28)    
    if(show):
        plt.show()        
