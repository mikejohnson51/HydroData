value = c(0,
          1:6,
          10:14,
          21:39,
          41:61,
          63:72,
          74:77,
          81:83,
          87,88,
          92,
          111:112,
          121:124,
          131,
          141:143,
          152,
          176,
          190,
          195,
          204:214,
          216:227,
          229:250,
          254)

crops = c("Background",

          c("Corn", "Cotton", "Rice", "Sorghum", "Soybeans", "Sunflower"),

          c("Peanuts","Tobacco", "Sweet Corn","Pop or Orn Corn","Mint"),

          c("Barley","Durum Wheat", "Spring Wheat","Winter Wheat", "Other Small Grains", "Dbl Crop WinWht/Soybeans",
            "Rye", "Oats", "Millet","Speltz", "Canola", "Flaxseed", "Safflower", "Rape Seed", "Mustard", "Alfalfa",
            "Other Hay/Non Alfalfa", "Camelina", "Buckwheat"),

          c("Sugarbeets", "Dry Beans", "Potatoes", "Other Crops", "Sugarcane", "Sweet Potatoes", "Misc Vegs & Fruits",
            "Watermelons", "Onions", "Cucumbers", "Chick Peas", "Lentils", "Peas", "Tomatoes", "Caneberries", "Hops",
            "Herbs", "Clover/Wildflowers", "Sod/Grass Seed", "Switchgrass", "Fallow/Idle Cropland"),

          c("Forest", "Shrubland", "Barren", "Cherries", "Peaches", "Apples", "Grapes", "Christmas Trees", "Other Tree Crops",
            "Citrus", "Pecans", "Almonds", "Walnuts", "Pears"),

          c("Clouds/No Data", "Developed", "Water"),

          c("Wetlands", "Nonag/Undefined"),

          c("Aquaculture"),

          c("Open Water", "Perennial Ice/Snow"),

          c("Developed/Open Space", "Developed/Low Intensity", "Developed/Med Intensity", "Developed/High Intensity"),

            "Barren",

          c("Deciduous Forest", "Evergreen Forest", "Mixed Forest"),

            "Shrubland",

            "Grassland/Pasture",

            "Woody Wetlands",

            "Herbaceous Wetlands",

          c("Pistachios", "Triticale", "Carrots", "Asparagus","Garlic", "Cantaloupes", "Prunes", "Olives",
             "Oranges", "Honeydew Melons", "Broccoli"),

          c("Peppers", "Pomegranates", "Nectarines", "Greens", "Plums", "Strawberries", "Squash", "Apricots", "Vetch", "Dbl Crop WinWht/Corn",
             "Dbl Crop Oats/Corn", "Lettuce"),

          c("Pumpkins", "Dbl Crop Lettuce/Durum Wht", "Dbl Crop Lettuce/Cantaloupe", "Dbl Crop Lettuce/Cotton", "Dbl Crop Lettuce/Barley",
              "Dbl Crop Durum Wht/Sorghum", "Dbl Crop Barley/Sorghum", "Dbl Crop WinWht/Sorghum", "Dbl Crop Barley/Corn", "Dbl Crop WinWht/Cotton",
              "Dbl Crop Soybeans/Cotton", "Dbl Crop Soybeans/Oats", "Dbl Crop Corn/Soybeans", "Blueberries", "Cabbage", "Cauliflower", "Celery",
              "Radishes", "Turnips", "Eggplants", "Gourds", "Cranberries"),

            "Dbl Crop Barley/Soybeans")

description = c(rep("Background", 1),
                rep("CROPS", 11),
                rep("GRAINS,HAY,SEEDS", 19),
                rep("CROPS", 20),
                rep("NON-CROP", 4),
                rep("CROPS", 11),
                rep("OTHER", 6),
                rep("NLCD-DERIVED CLASSES", 14),
                rep("CROPS", 46))

colors = c("#FFD300", "#FF2626", "#00A8E5", "#FF9E0C", "#267000", "#FFFF00",
           "#70A500", "#00AF4C", "#DDA50C", "#DDA50C", "#7FD3FF", "#E2007C",
           "#896354", "#D8B56B", "#A57000", "#D69EBC", "#707000", "#AD007C",
           "#A05989", "#700049", "#D69EBC", "#D1FF00", "#7F99FF", "#D6D600",
           "#D1FF00", "#00AF4C", "#FFA5E2", "#A5F28C", "#00AF4C", "#D69EBC",
           "#A800E5", "#A50000", "#702600", "#00AF4C", "#B27FFF", "#702600",
           "#FF6666", "#FF6666", "#FFCC66", "#FF6666", "#00AF4C", "#00DDAF",
           "#54FF00", "#F2A377", "#FF6666", "#00AF4C", "#7FD3FF", "#E8BFFF",
           "#AFFFDD", "#00AF4C", "#BFBF77", "#93CC93", "#C6D69E", "#CCBFA3",
           "#FF00FF", "#FF8EAA", "#BA004F", "#704489", "#007777", "#B29B70",
           "#FFFF7F", "#B5705B", "#00A582", "#EAD6AF", "#B29B70", "#F2F2F2",
           "#9B9B9B", "#4C70A3", "#7FB2B2", "#E8FFBF", "#00FFFF", "#4C70A3",
           "#D3E2F9", "#9B9B9B", "#9B9B9B", "#9B9B9B", "#9B9B9B", "#CCBFA3",
           "#93CC93", "#93CC93", "#93CC93", "#C6D69E", "#E8FFBF", "#7FB2B2",
           "#7FB2B2", "#00FF8C", "#D69EBC", "#FF6666", "#FF6666", "#FF6666",
           "#FF6666", "#FF8EAA", "#334933", "#E57026", "#FF6666", "#FF6666",
           "#FF6666", "#B29B70", "#FF8EAA", "#FF6666", "#FF8EAA", "#FF6666",
           "#FF6666", "#FF8EAA", "#00AF4C", "#FFD300", "#FFD300", "#FF6666",
           "#FF6666", "#896354", "#FF6666", "#FF2626", "#E2007C", "#FF9E0C",
           "#FF9E0C", "#A57000", "#FFD300", "#A57000", "#267000", "#267000",
           "#FFD300", "#000099", "#FF6666", "#FF6666", "#FF6666", "#FF6666",
           "#FF6666", "#FF6666", "#FF6666", "#FF6666", "#FFD300", "#267000",
           "#A57000", "#267000")


crops.legend = data.frame(value, crops)
