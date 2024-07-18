'''
Created on 03/12/2015

@author: bill.smith
'''

#plot
import gdal
import numpy as np
from matplotlib import rc
from matplotlib.colors import rgb2hex
from matplotlib import cm
import matplotlib.pyplot as plt
from matplotlib.colors import from_levels_and_colors
import cartopy.crs as ccrs
import cartopy.feature as cfeature
from cartopy.io.shapereader import Reader
import xarray as xr
import rasterio

#PROJECTION_GEO_WGS84 = 6341 #EPSG Code

def figure_rgb(src,filename):
    font = {'weight' : 'bold'}
    rc('font', **font)
    rc('ytick', labelsize=12)
       
    #Read Data
    red_data=src.read(1)
    grn_data=src.read(2)
    blu_data=src.read(3)
    red_data[np.logical_and(red_data==0,blu_data==0)]=255
    grn_data[np.logical_and(grn_data==0,blu_data==0)]=255
    blu_data[np.logical_and(red_data==255,blu_data==0)]=255
    rgb = np.dstack((red_data,grn_data,blu_data))
    print(rgb.shape)
    
    #extent
    minx = src.transform[2]
    maxx = src.transform[2] + src.transform[0]*src.width
    miny = src.transform[5] + src.transform[4]*src.height
    maxy = src.transform[5]
    print(minx,maxx,miny,maxy)
    
    #plot
    crs=ccrs.UTM('12N')
    ax = plt.axes(projection=crs)
    ax.set_xmargin(0.05)
    ax.set_ymargin(0.10)
        
    #plt.imshow(rgb, transform=crs)
    plt.imshow(rgb, extent=[minx,maxx,miny,maxy], transform=crs)
    plt.axis('off')
    
    ###EXTRAs
    d='/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/'
    print(d)
    #Bplots
    B1_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_Control.shp').geometries(),crs,edgecolor='cyan',facecolor='none',linewidth=.4)
    ax.add_feature(B1_Control)
    B2_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B2_Control.shp').geometries(),crs,edgecolor='cyan',facecolor='none',linewidth=.4)
    ax.add_feature(B2_Control)
    #B3_Control=cfeature.ShapelyFeature(Reader(d+'B3_Control.shp').geometries(),crs,edgecolor='cyan',facecolor='none',linewidth=1)
    #ax.add_feature(B3_Control)
    #B4_Control=cfeature.ShapelyFeature(Reader(d+'B4_Control.shp').geometries(),crs,edgecolor='cyan',facecolor='none',linewidth=1)
    #ax.add_feature(B4_Control)
    #B5_Control=cfeature.ShapelyFeature(Reader(d+'B5_Control.shp').geometries(),crs,edgecolor='cyan',facecolor='none',linewidth=1)
    #ax.add_feature(B5_Control)
    #Border
    bplots=cfeature.ShapelyFeature(Reader(d+'BPlots_utm83.shp').geometries(),crs,edgecolor='black',facecolor='none',linewidth=1)
    ax.add_feature(bplots)
    
    ###Save Image###
    plt.savefig(filename,bbox_inches='tight',dpi=600)
    plt.show()
    plt.close()
    
def figure_mesh(fid,scale,clrs,levels,extnd,filename):
    font = {'weight' : 'bold'}
    rc('font', **font)
    rc('ytick', labelsize=12)
       
    #Read Data
    band=fid.GetRasterBand(1)
    data=band.ReadAsArray()
    geot=fid.GetGeoTransform()
    data=np.ma.masked_where(data==0,data)
    data=data*scale
    
    #Get lat,lon
    rows=data.shape[0]
    cols=data.shape[1]
    minx = geot[0]
    miny = geot[3] + cols*geot[4] + rows*geot[5] 
    maxx = geot[0] + cols*geot[1] + rows*geot[2]
    maxy = geot[3] 
    lat=np.linspace(maxy, miny, rows)
    lon=np.linspace(minx, maxx, cols)
    xGrid, yGrid = np.meshgrid(lon, lat)
    print([minx, maxx, miny, maxy])
    
    ###PLOT###
    #Setup Basemap with projection
    crs=ccrs.UTM('12N')
    ax = plt.axes(projection=crs)
    ax.set_extent([minx,maxx,miny,maxy],crs=crs)
    ax.set_xmargin(0.05)
    ax.set_ymargin(0.10)
    ax.axis('off') #turn off border
    #Set colormap
    cmap, norm = from_levels_and_colors(levels, clrs, extend=extnd)
    #Plot colormesh
    plt.pcolormesh(xGrid,yGrid,data,transform=crs,cmap=cmap,norm=norm)
    
   
    
    ###EXTRAs
    d='/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/'
    print(d)
    #Bplots
    B1_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_Control.shp').geometries(),crs,edgecolor='cyan',facecolor='none',linewidth=.4)
    ax.add_feature(B1_Control)
    B2_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B2_Control.shp').geometries(),crs,edgecolor='cyan',facecolor='none',linewidth=.4)
    ax.add_feature(B2_Control)
    #B3_Control=cfeature.ShapelyFeature(Reader(d+'B3_Control.shp').geometries(),crs,edgecolor='cyan',facecolor='none',linewidth=1)
    #ax.add_feature(B3_Control)
    #B4_Control=cfeature.ShapelyFeature(Reader(d+'B4_Control.shp').geometries(),crs,edgecolor='cyan',facecolor='none',linewidth=1)
    #ax.add_feature(B4_Control)
    #B5_Control=cfeature.ShapelyFeature(Reader(d+'B5_Control.shp').geometries(),crs,edgecolor='cyan',facecolor='none',linewidth=1)
    #ax.add_feature(B5_Control)
    #Border
    bplots=cfeature.ShapelyFeature(Reader(d+'BPlots_utm83.shp').geometries(),crs,edgecolor='black',facecolor='none',linewidth=1)
    ax.add_feature(bplots)
    
    ###Save Image###
    #plt.show()
    plt.savefig(filename,bbox_inches='tight',dpi=600)
    plt.show()
    plt.close()
    
#########################################################################################################

if __name__ == '__main__':
    data_dir='/Users/wksmith/Data/USGS_Biocrust_S22/'
    out_dir='/Users/wksmith/Documents/GitHub/Biocrust-USGS/figures/'
    
    ###Plot RGB
    rgb_path=data_dir+'rgb/Biocrust_Flight2_021222_RGB_Ortho_Metashape_utm83_clipped.tif'
    src = rasterio.open(rgb_path)
    figure_rgb(src,out_dir+'test_rgb.png')
    
    ###Plot Thermal
    scale=1
    extnd='both'
    levels = np.linspace(650,850,num=50)
    cmap=cm.get_cmap('plasma',51)
    color_list = [rgb2hex(cmap(i)[:3]) for i in range(cmap.N)]
    fid=gdal.Open(data_dir+'thermal/20220214_Flight2_XT2_IR/Biocrust_Flight2_XT2_IR_Ortho_Metashape_utm83_clipped.tif')
    figure_mesh(fid,scale,color_list,levels,extnd,out_dir+'test_thermal.png')
    
    