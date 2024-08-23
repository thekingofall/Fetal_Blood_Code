
col1=["#8A4198FF",  "#145096","#f6c619","#e9412f", "#008EA0FF", 
 "#266b69", "#3178a2"]
def create_palette(cell_types, colors):
    cell_type_colors = {}
    for i, cell_type in enumerate(cell_types):
        color = colors[i % len(colors)]
        cell_type_colors[cell_type] = color
    return cell_type_colors
col1=["#C71000FF","#8A4198FF",  "#023f75", "#008EA0FF", 
 "#266b69","#f6c619", "#FF6348FF","#c29f62"]


col3=[ "#C71000FF", "#008EA0FF", "#8A4198FF", "#5A9599FF", 
"#FF6348FF", "#84D7E1FF", "#FF95A8FF", "#3D3B25FF", "#ADE2D0FF",
"#1A5354FF", "#3F4041FF"]

colname4=[ "#C71000FF", "#008EA0FF", "#8A4198FF", "#5A9599FF", 
"#FF6348FF", "#84D7E1FF", "#FF95A8FF", "#3D3B25FF", "#ADE2D0FF",
"#1A5354FF", "#3F4041FF","#fa6e01","#972b1d", "#e6a84b", "#4c211b", "#ff717f", "#009966",
"#c62d17", "#023f75", "#ea894e", "#266b69", "#eb4601", "#f6c619",
"#f49128", "#194a55", "#c29f62", "#83ba9e","#187c65", "#A6CEE3",
"#223e9c", "#aebea6", "#edae11", "#c74732",
"#6a73cf", "#edd064", "#0eb0c8", "#f2ccac", "#a1d5b9", "#e1abbc",
"#46A040", "#00AF99", "#FFC179", "#98D9E9", "#F6313E", "#FFA300", 
"#333366", "#663366", "#FF6666", "#8F1336", "#0081C9",  "#CC0033",
"#CC9966", "#CC0033", "#999933", "#CCCC33", "#CCFF99", "#333399","#001588",
 "#490C65", "#BA7FD0", "#1F78B4", "#DE77AE" , "#2f2f2f",
"#006D2C", "#868686", "#9DA8E2", "#91C392", "#FF9900", "#339966","#993333"]


color5=["#8dd3c7","#00ffff","#00b1bf","#ff1593","#80b1d3","#b3de69","#e78bc4","#bbbbbb","#e62722","#a3cce2",
        "#fde8d2","#fa9274","#e9412f","#f5fad0","#fec48b","#edf8b3","#c1e6bc","#c02750","#111a3d","#b76235",
        "#145096"]

color6=["#eb6f5d","#71c9dd","#33b39f","#6376a0","#f5af98"]

seqcolor1_bwr=["#111a3d","#3178a2","#cde0eb","#d99d93","#8a252b"]
seqcolor2_byr=["#1a2d69","#2685c9","#96c52c","#d2dd36","#f4c91e","#e73718"]
seqcolor3_wr=["#fef3ed","#fcb398","#fb7858","#e84433","#b11518"]
seqcolor4_bwr=["#2166ac","#518bbd","#fef3ed","#d25e55","#b2182b"]
seqcolor5_bwr=["#4393c3","#a5cee4","#fbf9f9","#ea8e70","#b92632"]
colorgan=['#C71000FF',"#f49128","#023f75","#5A9599FF"]

cmpcool=['magma', 'inferno', 'plasma', 
'viridis', 'cividis', 'twilight', 
'twilight_shifted', 'turbo', 'Blues', 'BrBG', 'BuGn', 
'BuPu', 'CMRmap', 'GnBu', 'Greens', 'Greys', 'OrRd', 
'Oranges', 'PRGn', 'PiYG', 'PuBu', 'PuBuGn', 'PuOr', 
'PuRd', 'Purples', 'RdBu', 'RdGy', 'RdPu', 'RdYlBu', 
'RdYlGn', 'Reds', 'Spectral', 'Wistia', 'YlGn', 'YlGnBu',
'YlOrBr', 'YlOrRd', 'afmhot', 'autumn', 'binary', 'bone', 
'brg', 'bwr', 'cool', 'coolwarm', 'copper', 'cubehelix', 
'flag', 'gist_earth', 'gist_gray', 'gist_heat', 'gist_ncar',
'gist_rainbow', 'gist_stern', 'gist_yarg', 'gnuplot', 'gnuplot2',
'gray', 'hot', 'hsv', 'jet', 'nipy_spectral', 'ocean', 'pink',
'prism', 'rainbow', 'seismic', 'spring', 'summer', 'terrain',
'winter', 'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 
'Set1', 'Set2', 'Set3', 'tab10', 'tab20', 'tab20b', 'tab20c', 
'magma_r', 'inferno_r', 'plasma_r', 'viridis_r', 'cividis_r', 
'twilight_r', 'twilight_shifted_r', 'turbo_r', 'Blues_r', 'BrBG_r',
'BuGn_r', 'BuPu_r', 'CMRmap_r', 'GnBu_r', 'Greens_r', 'Greys_r', 
'OrRd_r', 'Oranges_r', 'PRGn_r', 'PiYG_r', 'PuBu_r', 'PuBuGn_r',
'PuOr_r', 'PuRd_r', 'Purples_r', 'RdBu_r', 'RdGy_r', 'RdPu_r', 
'RdYlBu_r', 'RdYlGn_r', 'Reds_r', 'Spectral_r', 'Wistia_r', 'YlGn_r',
'YlGnBu_r', 'YlOrBr_r', 'YlOrRd_r', 'afmhot_r', 'autumn_r', 
'binary_r', 'bone_r', 'brg_r', 'bwr_r', 'cool_r', 'coolwarm_r', 
'copper_r', 'cubehelix_r', 'flag_r', 'gist_earth_r', 'gist_gray_r', 
'gist_heat_r', 'gist_ncar_r', 'gist_rainbow_r', 'gist_stern_r',
'gist_yarg_r', 'gnuplot_r', 'gnuplot2_r', 'gray_r', 'hot_r', 'hsv_r', 
'jet_r', 'nipy_spectral_r', 'ocean_r', 'pink_r', 'prism_r', 'rainbow_r', 
'seismic_r', 'spring_r', 'summer_r', 'terrain_r', 'winter_r', 
'Accent_r', 'Dark2_r', 'Paired_r', 'Pastel1_r', 'Pastel2_r',
'Set1_r', 'Set2_r', 'Set3_r', 'tab10_r', 'tab20_r', 'tab20b_r',
'tab20c_r', 'rocket', 'rocket_r', 'mako', 'mako_r', 'icefire', 'icefire_r', 
'vlag', 'vlag_r', 'flare', 'flare_r', 'crest', 'crest_r']