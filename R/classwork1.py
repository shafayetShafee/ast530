#!/usr/bin/env python
# coding: utf-8

# Exercise-1
# -----------------------------------
# 
# **(a)** The data below represent average monthly precipitation for [Boulder, Colorado provided by the U.S. National Oceanic and Atmospheric Administration (NOAA).](https://www.esrl.noaa.gov/psd/boulder/Boulder.mm.precip.html)
# 
# <table><thead><tr><th>Month</th><th>Precipitation (inches)</th></tr></thead><tbody><tr><td>jan</td><td>0.70</td></tr><tr><td>feb</td><td>0.75</td></tr><tr><td>mar</td><td>1.85</td></tr><tr><td>apr</td><td>2.93</td></tr><tr><td>may</td><td>3.05</td></tr><tr><td>june</td><td>2.02</td></tr><tr><td>july</td><td>1.93</td></tr><tr><td>aug</td><td>1.62</td></tr><tr><td>sept</td><td>1.84</td></tr><tr><td>oct</td><td>1.31</td></tr><tr><td>nov</td><td>1.39</td></tr><tr><td>dec</td><td>0.84</td></tr></tbody></table>
# 
# Create two **Python** lists as follows:
# 
# 1.  the first list should contain the month abbreviations and should be called `boulder_precip_months`.
# 2.  the second list should be a list of precipitation values and should be called `boulder_precip_inches`.
# 
# 
# **(b)** Next, convert each floating point value in the `boulder_precip_inches` to millimeters and create a new list variable called `boulder_precip_mm`.
# 
# To convert inches to millimeters, you need to multiply the inches by `25.4` (1 inch = 25.4 mm).
# 
# **(c)** Next, create a dictionary in `Python` that should have the months as keys and the percipitation in milimeters as values. 
# 
# 
# **(d)** Write functions to calculate the mean, standard deviation and coefficient of variation of data contained in a list. Next, calculate the mean, standard deviation and coefficient of variation of the average monthly percipitation for Boulder.
# 
# **(e)** Finally, find out the months for which the `boulder_precip_inches` was greater than the mean level of percipitation in inches.

# ## Redo (22 nov, 2022)

# In[1]:


# a

boulder_precip_months = ['jan', 'feb', 'mar', 'apr', 'may', 'june', 'july', 'aug', 
                        'sep', 'oct', 'nov', 'dec']

boulder_precip_inches = [0.70, 0.75, 1.85, 2.93, 3.05, 2.02, 1.93, 1.62, 1.84, 1.31, 1.39, 0.84]


# In[5]:


# b

boulder_precip_mm = [round(inches * 25.4, 2) for inches in boulder_precip_inches]
print(boulder_precip_mm)


# In[8]:


# c

# hard way

d1 = {}

for i in range(len(boulder_precip_months)):
    d1[boulder_precip_months[i]] = boulder_precip_mm[i]
        
d1


# In[10]:


# less hard way

d2 = {boulder_precip_months[i]: boulder_precip_mm[i] for i in range(len(boulder_precip_months))}

d2


# In[12]:


# simplest way

d3 = dict(zip(boulder_precip_months, boulder_precip_mm))
d3


# In[16]:


# d

def mean(x):
    n = len(x)
    total = sum(x)
    return total / n

def sd(x):
    avg = mean(x)
    n = len(x)
    numerator = sum([(val - avg) ** 2 for val in x])
    sdv = (numerator / n) ** (1/2)
    return sdv

def cov(x):
    return sd(x) / mean(x)


# In[17]:


print(f"mean: {mean(boulder_precip_inches)}, sd: {sd(boulder_precip_inches)}, cov: {cov(boulder_precip_inches)}")


# In[20]:


# e

d_inches = dict(zip(boulder_precip_months, boulder_precip_inches))

[key for key in d_inches if d_inches[key] > mean(boulder_precip_inches)]


# In[22]:


# or

mons = [boulder_precip_months[i] for i in range(len(boulder_precip_months)) 
        if boulder_precip_inches[i] > mean(boulder_precip_inches)]
mons


# 

# ## Original Copy

# In[1]:


# a

boulder_precip_months = ['jan', 'feb', 'mar', 'apr', 'may', 'june', 'july', 'aug', 
                        'sep', 'oct', 'nov', 'dec']

boulder_precip_inches = [0.70, 0.75, 1.85, 2.93, 3.05, 2.02, 1.93, 1.62, 1.84, 1.31, 1.39, 0.84]


# In[4]:


# b

boulder_precip_mm = [round(inches * 25.4, 2) for inches in boulder_precip_inches]

boulder_precip_mm


# In[8]:


# c

d1 = dict(zip(boulder_precip_months, boulder_precip_mm))

d2 = {boulder_precip_months[i]: boulder_precip_mm[i] for i in range(len(boulder_precip_mm))}

print(d1)
print(d2)


# In[15]:


# d

def mean(x):
    n = len(x)
    avg = sum(x) / n
    return avg

def sd(x):
    n = len(x)
    avg = sum(x) / n
    devs = [(num - avg) ** 2 for num in x]
    var = sum(devs) / n
    return var ** (1/2)

def covar(x):
    avg = mean(x)
    sdv = sd(x)
    return sdv / avg

print(f"mean: {mean(boulder_precip_inches)}, sd: {sd(boulder_precip_inches)}, cov: {covar(boulder_precip_inches)}b")


# In[21]:


# e

filtered_mon = [boulder_precip_months[i] for i in range(len(boulder_precip_inches)) if boulder_precip_inches[i] > mean(boulder_precip_inches)]


# In[22]:


filtered_mon


# In[24]:


d1_inches = dict(zip(boulder_precip_months, boulder_precip_inches))

[key for key, val in d1_inches.items() if val > mean(boulder_precip_inches)]


# In[29]:


## dictionary creating in raw way

d1_manual = {}

for i in range(len(boulder_precip_inches)):
    d1_manual[boulder_precip_months[i]] = boulder_precip_inches[i]
    
d1_manual


# In[31]:


test_zip = list(zip(boulder_precip_months, boulder_precip_inches))

type(test_zip[1])

