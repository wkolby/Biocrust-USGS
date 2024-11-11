'''
Created on 03/12/2015

@author: bill.smith
'''

#plot
import gdal
import numpy as np
from matplotlib import rc
from matplotlib.colors import Normalize,rgb2hex
from mpl_toolkits.axes_grid1.anchored_artists import AnchoredSizeBar
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
        
    #Plot
    plt.imshow(rgb, extent=[minx,maxx,miny,maxy], transform=crs)
    #plt.axis('off')
    
    ###EXTRAs
    d='/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/'
    print(d)
    #Bplots Control
    B1_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_Control.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=1)
    ax.add_feature(B1_Control)
    B2_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B2_Control.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=1)
    ax.add_feature(B2_Control)
    B3_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B3_Control.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=1)
    ax.add_feature(B3_Control)
    B4_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B4_Control.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=1)
    ax.add_feature(B4_Control)
    B5_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B5_Control.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=1)
    ax.add_feature(B5_Control)
    #Bplots Warmed
    B1_Warmed=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_Warmed.shp').geometries(),crs,edgecolor="#9E0142",facecolor='none',linewidth=1)
    ax.add_feature(B1_Warmed)
    B2_Warmed=cfeature.ShapelyFeature(Reader(d+'BPlots_B2_Warmed.shp').geometries(),crs,edgecolor="#9E0142",facecolor='none',linewidth=1)
    ax.add_feature(B2_Warmed)
    B3_Warmed=cfeature.ShapelyFeature(Reader(d+'BPlots_B3_Warmed.shp').geometries(),crs,edgecolor="#9E0142",facecolor='none',linewidth=1)
    ax.add_feature(B3_Warmed)
    B4_Warmed=cfeature.ShapelyFeature(Reader(d+'BPlots_B4_Warmed.shp').geometries(),crs,edgecolor="#9E0142",facecolor='none',linewidth=1)
    ax.add_feature(B4_Warmed)
    B5_Warmed=cfeature.ShapelyFeature(Reader(d+'BPlots_B5_Warmed.shp').geometries(),crs,edgecolor="#9E0142",facecolor='none',linewidth=1)
    ax.add_feature(B5_Warmed)
    #Bplots Warmed AlteredP
    B1_WarmAltP=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_WarmAltP.shp').geometries(),crs,edgecolor="#5E4FA2",facecolor='none',linewidth=1)
    ax.add_feature(B1_WarmAltP)
    B2_WarmAltP=cfeature.ShapelyFeature(Reader(d+'BPlots_B2_WarmAltP.shp').geometries(),crs,edgecolor="#5E4FA2",facecolor='none',linewidth=1)
    ax.add_feature(B2_WarmAltP)
    B3_WarmAltP=cfeature.ShapelyFeature(Reader(d+'BPlots_B3_WarmAltP.shp').geometries(),crs,edgecolor="#5E4FA2",facecolor='none',linewidth=1)
    ax.add_feature(B3_WarmAltP)
    B4_WarmAltP=cfeature.ShapelyFeature(Reader(d+'BPlots_B4_WarmAltP.shp').geometries(),crs,edgecolor="#5E4FA2",facecolor='none',linewidth=1)
    ax.add_feature(B4_WarmAltP)
    B5_WarmAltP=cfeature.ShapelyFeature(Reader(d+'BPlots_B5_WarmAltP.shp').geometries(),crs,edgecolor="#5E4FA2",facecolor='none',linewidth=1)
    ax.add_feature(B5_WarmAltP)
    #Bplots Altered P
    B1_AlteredP=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_AlteredP.shp').geometries(),crs,edgecolor="#99D594",facecolor='none',linewidth=1)
    ax.add_feature(B1_AlteredP)
    B2_AlteredP=cfeature.ShapelyFeature(Reader(d+'BPlots_B2_AlteredP.shp').geometries(),crs,edgecolor="#99D594",facecolor='none',linewidth=1)
    ax.add_feature(B2_AlteredP)
    B3_AlteredP=cfeature.ShapelyFeature(Reader(d+'BPlots_B3_AlteredP.shp').geometries(),crs,edgecolor="#99D594",facecolor='none',linewidth=1)
    ax.add_feature(B3_AlteredP)
    B4_AlteredP=cfeature.ShapelyFeature(Reader(d+'BPlots_B4_AlteredP.shp').geometries(),crs,edgecolor="#99D594",facecolor='none',linewidth=1)
    ax.add_feature(B4_AlteredP)
    B5_AlteredP=cfeature.ShapelyFeature(Reader(d+'BPlots_B5_AlteredP.shp').geometries(),crs,edgecolor="#99D594",facecolor='none',linewidth=1)
    ax.add_feature(B5_AlteredP)
    
    #Border
    bplots=cfeature.ShapelyFeature(Reader(d+'BPlots_utm83.shp').geometries(),crs,edgecolor='black',facecolor='none',linewidth=1)
    ax.add_feature(bplots)
    
    # Add scale bar
    scalebar = AnchoredSizeBar(ax.transData,
                           5, '5 m', 'lower right', 
                           pad=0.1,
                           color='black',
                           frameon=False,
                           size_vertical=1)

    ax.add_artist(scalebar)
    
    
    ###Save Image###
    plt.savefig(filename,bbox_inches='tight',dpi=600) 
    plt.show()
    plt.close()

def figure_rgb_zoom(src,filename):
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
        
    #Plot
    zoom=[637845,637850,4281840,4281845]
    plt.imshow(rgb, extent=zoom, transform=crs)
    #plt.axis('off')
        
    # Add scale bar
    scalebar = AnchoredSizeBar(ax.transData,
                           5, '5 m', 'lower right', 
                           pad=0.1,
                           color='black',
                           frameon=False,
                           size_vertical=1)

    ax.add_artist(scalebar)
    
    
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
    data=np.ma.masked_where(data<-5,data)
    data=np.ma.masked_where(data==np.nan,data)
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
    #Setup projection
    crs=ccrs.UTM('12N')
    ax = plt.axes(projection=crs)
    ax.set_extent([minx,maxx,miny,maxy],crs=crs)
    ax.set_xmargin(30)
    ax.set_ymargin(30)
    #ax.axis('off') #turn off border
    
    #Set colormap
    cmap, norm = from_levels_and_colors(levels, clrs, extend=extnd)
    #Plot colormesh
    plt.pcolormesh(xGrid,yGrid,data,transform=crs,cmap=cmap,norm=norm)

    ###EXTRAs
    d='/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/'
    print(d)
    #Bplots Control
    B1_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_Control.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=1)
    ax.add_feature(B1_Control)
    B2_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B2_Control.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=1)
    ax.add_feature(B2_Control)
    B3_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B3_Control.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=1)
    ax.add_feature(B3_Control)
    B4_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B4_Control.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=1)
    ax.add_feature(B4_Control)
    B5_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B5_Control.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=1)
    ax.add_feature(B5_Control)
    #Bplots Warmed
    B1_Warmed=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_Warmed.shp').geometries(),crs,edgecolor="#9E0142",facecolor='none',linewidth=1)
    ax.add_feature(B1_Warmed)
    B2_Warmed=cfeature.ShapelyFeature(Reader(d+'BPlots_B2_Warmed.shp').geometries(),crs,edgecolor="#9E0142",facecolor='none',linewidth=1)
    ax.add_feature(B2_Warmed)
    B3_Warmed=cfeature.ShapelyFeature(Reader(d+'BPlots_B3_Warmed.shp').geometries(),crs,edgecolor="#9E0142",facecolor='none',linewidth=1)
    ax.add_feature(B3_Warmed)
    B4_Warmed=cfeature.ShapelyFeature(Reader(d+'BPlots_B4_Warmed.shp').geometries(),crs,edgecolor="#9E0142",facecolor='none',linewidth=1)
    ax.add_feature(B4_Warmed)
    B5_Warmed=cfeature.ShapelyFeature(Reader(d+'BPlots_B5_Warmed.shp').geometries(),crs,edgecolor="#9E0142",facecolor='none',linewidth=1)
    ax.add_feature(B5_Warmed)
    #Bplots Warmed AlteredP
    B1_WarmAltP=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_WarmAltP.shp').geometries(),crs,edgecolor="#5E4FA2",facecolor='none',linewidth=1)
    ax.add_feature(B1_WarmAltP)
    B2_WarmAltP=cfeature.ShapelyFeature(Reader(d+'BPlots_B2_WarmAltP.shp').geometries(),crs,edgecolor="#5E4FA2",facecolor='none',linewidth=1)
    ax.add_feature(B2_WarmAltP)
    B3_WarmAltP=cfeature.ShapelyFeature(Reader(d+'BPlots_B3_WarmAltP.shp').geometries(),crs,edgecolor="#5E4FA2",facecolor='none',linewidth=1)
    ax.add_feature(B3_WarmAltP)
    B4_WarmAltP=cfeature.ShapelyFeature(Reader(d+'BPlots_B4_WarmAltP.shp').geometries(),crs,edgecolor="#5E4FA2",facecolor='none',linewidth=1)
    ax.add_feature(B4_WarmAltP)
    B5_WarmAltP=cfeature.ShapelyFeature(Reader(d+'BPlots_B5_WarmAltP.shp').geometries(),crs,edgecolor="#5E4FA2",facecolor='none',linewidth=1)
    ax.add_feature(B5_WarmAltP)
    #Bplots Altered P
    B1_AlteredP=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_AlteredP.shp').geometries(),crs,edgecolor="#99D594",facecolor='none',linewidth=1)
    ax.add_feature(B1_AlteredP)
    B2_AlteredP=cfeature.ShapelyFeature(Reader(d+'BPlots_B2_AlteredP.shp').geometries(),crs,edgecolor="#99D594",facecolor='none',linewidth=1)
    ax.add_feature(B2_AlteredP)
    B3_AlteredP=cfeature.ShapelyFeature(Reader(d+'BPlots_B3_AlteredP.shp').geometries(),crs,edgecolor="#99D594",facecolor='none',linewidth=1)
    ax.add_feature(B3_AlteredP)
    B4_AlteredP=cfeature.ShapelyFeature(Reader(d+'BPlots_B4_AlteredP.shp').geometries(),crs,edgecolor="#99D594",facecolor='none',linewidth=1)
    ax.add_feature(B4_AlteredP)
    B5_AlteredP=cfeature.ShapelyFeature(Reader(d+'BPlots_B5_AlteredP.shp').geometries(),crs,edgecolor="#99D594",facecolor='none',linewidth=1)
    ax.add_feature(B5_AlteredP)
    
    #Border
    bplots=cfeature.ShapelyFeature(Reader(d+'BPlots_utm83.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=1)
    ax.add_feature(bplots)
    
    ###Save Image###
    #plt.show()
    plt.savefig(filename,bbox_inches='tight',dpi=600)
    plt.show()
    plt.close()

def figure_mesh_zoom(fid,scale,clrs,levels,extnd,zoom,filename):
    font = {'weight' : 'bold'}
    rc('font', **font)
    rc('ytick', labelsize=12)
       
    #Read Data
    band=fid.GetRasterBand(1)
    data=band.ReadAsArray()
    geot=fid.GetGeoTransform()
    data=np.ma.masked_where(data<-5,data)
    data=np.ma.masked_where(data==np.nan,data)
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
    #Setup projection
    crs=ccrs.UTM('12N')
    ax = plt.axes(projection=crs)
    ax.set_extent(zoom,crs=crs)
    ax.set_xmargin(30)
    ax.set_ymargin(30)
    #ax.axis('off') #turn off border
    
    #Set colormap
    cmap, norm = from_levels_and_colors(levels, clrs, extend=extnd)
    #Plot colormesh
    plt.pcolormesh(xGrid,yGrid,data,transform=crs,cmap=cmap,norm=norm)
    
    ###EXTRAs
    d='/Users/wksmith/Data/USGS_Biocrust_S22/Boundaries/'
    print(d)
    #Bplots Control
    B1_Control=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_Control.shp').geometries(),crs,edgecolor="#3288BD",facecolor='none',linewidth=3)
    ax.add_feature(B1_Control)
    #Bplots Warmed
    B1_Warmed=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_Warmed.shp').geometries(),crs,edgecolor="#9E0142",facecolor='none',linewidth=3)
    ax.add_feature(B1_Warmed)
    #Bplots Warmed AlteredP
    B1_WarmAltP=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_WarmAltP.shp').geometries(),crs,edgecolor="#5E4FA2",facecolor='none',linewidth=3)
    ax.add_feature(B1_WarmAltP)
    #Bplots Altered P
    B1_AlteredP=cfeature.ShapelyFeature(Reader(d+'BPlots_B1_AlteredP.shp').geometries(),crs,edgecolor="#99D594",facecolor='none',linewidth=3)
    ax.add_feature(B1_AlteredP)
    
    ###Save Image###
    #plt.show()
    plt.savefig(filename,bbox_inches='tight',dpi=600)
    plt.show()
    plt.close()

def figure_colorbar(clrs,levels,extnd,lab,filename):
    font = {'weight' : 'bold'}
    rc('font', **font)
    rc('ytick', labelsize=12)
    
    #set up plot area
    fig, ax = plt.subplots(figsize=(6, .4))
    #Set colormap
    cmap, norm = from_levels_and_colors(levels, clrs, extend=extnd)
    print(norm)
    #Plot
    cb=fig.colorbar(cm.ScalarMappable(norm=norm, cmap=cmap),cax=ax, orientation='horizontal')
    cb.set_label(label=lab,size=15,weight='bold')
    ###Save Image###
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
    #figure_rgb(src,out_dir+'BPlots_RGB.png')
    #figure_rgb_zoom(src,out_dir+'BPlots_RGB_Zoom.png')
    
    ###Plot Thermal
    scale=1
    extnd='both'
    levels = np.linspace(0,1,num=20)
    cmap=cm.get_cmap('winter',21)
    color_list = [rgb2hex(cmap(i)[:3]) for i in range(cmap.N)]
    zoom_a=[637891,637902.5,4281842.5,4281850]
    zoom_c=[637890,637905,4281840,4281855]
    zoom_w=[637860,637864,4281842.5,4281845]
    fid=gdal.Open(data_dir+'thermal/20220213_Flight1_XT2_IR/Biocrust_Flight1_XT2_IRnorm_Ortho_Metashape_utm83_clipped.tif')
    #figure_mesh(fid,scale,color_list,levels,extnd,out_dir+'BPlots_Thermal_Flight1.png')
    figure_mesh_zoom(fid,scale,color_list,levels,extnd,zoom_a,out_dir+'BPlots_Thermal_Flight1_Zoom_B1.png')
    fid=gdal.Open(data_dir+'thermal/20220214_Flight2_XT2_IR/Biocrust_Flight2_XT2_IRnorm_Ortho_Metashape_utm83_clipped.tif')
    #figure_mesh(fid,scale,color_list,levels,extnd,out_dir+'BPlots_Thermal_Flight2.png')
    fid=gdal.Open(data_dir+'thermal/20220212_Flight3_XT2_IR/Biocrust_Flight3_XT2_IRnorm_Ortho_Metashape_utm83_clipped.tif')
    #figure_mesh(fid,scale,color_list,levels,extnd,out_dir+'BPlots_Thermal_Flight3.png')
    figure_mesh_zoom(fid,scale,color_list,levels,extnd,zoom_a,out_dir+'BPlots_Thermal_Flight3_Zoom_B1.png')
    fid=gdal.Open(data_dir+'thermal/20220212_Flight4_XT2_IR/Biocrust_Flight4_XT2_IRnorm_Ortho_Metashape_utm83_clipped.tif')
    #figure_mesh(fid,scale,color_list,levels,extnd,out_dir+'BPlots_Thermal_Flight4.png')
    levels = np.linspace(0,1,num=11)
    cmap=cm.get_cmap('winter',12)
    color_list = [rgb2hex(cmap(i)[:3]) for i in range(cmap.N)]
    #figure_colorbar(color_list, levels, extnd,'Surface Temperature Index',out_dir+'BPlots_Thermal_Flight2_colorbar.png')
    
    ###Plot NDVI
    scale=1
    extnd='both'
    levels = np.linspace(0,0.5,num=20)
    cmap=cm.get_cmap('PRGn',21)
    color_list = [rgb2hex(cmap(i)[:3]) for i in range(cmap.N)]
    fid=gdal.Open(data_dir+'/MicaSense_Dual_Tarp/moab_micasense_ortho_utm83_ndvi_clipped.tif')
    #figure_mesh(fid,scale,color_list,levels,extnd,out_dir+'BPlots_NDVI_Micasense.png')
    figure_mesh_zoom(fid,scale,color_list,levels,extnd,zoom_a,out_dir+'BPlots_NDVI_Micasense_Zoom_B1.png')
    #figure_mesh_zoom(fid,scale,color_list,levels,extnd,zoom_w,out_dir+'BPlots_NDVI_Micasense_ZoomW.png')
    levels = np.linspace(0,0.5,num=11)
    cmap=cm.get_cmap('PRGn',12)
    color_list = [rgb2hex(cmap(i)[:3]) for i in range(cmap.N)]
    #figure_colorbar(color_list, levels, extnd,'Chlorophyll Index',out_dir+'BPlots_NDVI_Micasense_colorbar.png')
    
    ###Plot BI
    scale=1
    extnd='both'
    levels = np.linspace(0.05,0.15,num=20)
    colorsList = ['black','lightgray','#fff5b6']
    cmap = cm.colors.LinearSegmentedColormap.from_list("Custom",colorsList,N=len(levels)+1)
    color_list = [rgb2hex(cmap(i)[:3]) for i in range(cmap.N)]
    fid=gdal.Open(data_dir+'/MicaSense_Dual_Tarp/moab_micasense_ortho_utm83_bi_clipped.tif')
    #figure_mesh(fid,scale,color_list,levels,extnd,out_dir+'BPlots_BI_Micasense.png')
    levels = np.linspace(0.05,0.15,num=11)
    colorsList = ['black','lightgray','#fff5b6']
    cmap = cm.colors.LinearSegmentedColormap.from_list("Custom",colorsList,N=len(levels)+1)
    color_list = [rgb2hex(cmap(i)[:3]) for i in range(cmap.N)]
    #figure_colorbar(color_list, levels, extnd,'Brightness Index',out_dir+'BPlots_BI_Micasense_colorbar.png')
    