coord_cartesian <- CoordCartesian$build_accessor()
coord_equal <- CoordEqual$build_accessor()
coord_flip <- CoordFlip$build_accessor()
coord_map <- CoordMap$build_accessor()
coord_polar <- CoordPolar$build_accessor()
coord_trans <- CoordTrans$build_accessor()
facet_grid <- FacetGrid$build_accessor()
geom_abline <- GeomAbline$build_accessor()
geom_area <- GeomArea$build_accessor()
geom_bar <- GeomBar$build_accessor()
geom_blank <- GeomBlank$build_accessor()
geom_boxplot <- GeomBoxplot$build_accessor()
geom_contour <- GeomContour$build_accessor()
geom_crossbar <- GeomCrossbar$build_accessor()
geom_density <- GeomDensity$build_accessor()
geom_density_2d <- GeomDensity2d$build_accessor()
geom_errorbar <- GeomErrorbar$build_accessor()
geom_histogram <- GeomHistogram$build_accessor()
geom_hline <- GeomHline$build_accessor()
geom_interval <- GeomInterval$build_accessor()
geom_jitter <- GeomJitter$build_accessor()
geom_line <- GeomLine$build_accessor()
geom_linerange <- GeomLinerange$build_accessor()
geom_path <- GeomPath$build_accessor()
geom_point <- GeomPoint$build_accessor()
geom_pointrange <- GeomPointrange$build_accessor()
geom_polygon <- GeomPolygon$build_accessor()
geom_quantile <- GeomQuantile$build_accessor()
geom_ribbon <- GeomRibbon$build_accessor()
geom_rug <- GeomRug$build_accessor()
geom_segment <- GeomSegment$build_accessor()
geom_smooth <- GeomSmooth$build_accessor()
geom_step <- GeomStep$build_accessor()
geom_text <- GeomText$build_accessor()
geom_tile <- GeomTile$build_accessor()
geom_vline <- GeomVline$build_accessor()
position_dodge <- PositionDodge$build_accessor()
position_fill <- PositionFill$build_accessor()
position_identity <- PositionIdentity$build_accessor()
position_jitter <- PositionJitter$build_accessor()
position_stack <- PositionStack$build_accessor()
scale_area <- ScaleArea$build_accessor()
scale_colour <- ScaleColour$build_accessor()
scale_colour_brewer <- ScaleBrewer$build_accessor(c(variable = "\"colour\""))
scale_colour_gradient <- ScaleGradient$build_accessor(c(variable = "\"colour\""))
scale_colour_gradient2 <- ScaleGradient2$build_accessor(c(variable = "\"colour\""))
scale_colour_grey <- ScaleGrey$build_accessor(c(variable = "\"colour\""))
scale_colour_hue <- ScaleHue$build_accessor(c(variable = "\"colour\""))
scale_colour_identity <- ScaleIdentity$build_accessor(c(variable = "\"colour\""))
scale_colour_manual <- ScaleManual$build_accessor(c(variable = "\"colour\""))
scale_fill_brewer <- ScaleBrewer$build_accessor(c(variable = "\"fill\""))
scale_fill_gradient <- ScaleGradient$build_accessor(c(variable = "\"fill\""))
scale_fill_gradient2 <- ScaleGradient2$build_accessor(c(variable = "\"fill\""))
scale_fill_grey <- ScaleGrey$build_accessor(c(variable = "\"fill\""))
scale_fill_hue <- ScaleHue$build_accessor(c(variable = "\"fill\""))
scale_fill_identity <- ScaleIdentity$build_accessor(c(variable = "\"fill\""))
scale_fill_manual <- ScaleManual$build_accessor(c(variable = "\"fill\""))
scale_linetype <- ScaleLinetype$build_accessor()
scale_linetype_identity <- ScaleIdentity$build_accessor(c(variable = "\"linetype\""))
scale_linetype_manual <- ScaleManual$build_accessor(c(variable = "\"linetype\""))
scale_shape <- ScaleShape$build_accessor()
scale_shape_identity <- ScaleIdentity$build_accessor(c(variable = "\"shape\""))
scale_shape_manual <- ScaleManual$build_accessor(c(variable = "\"shape\""))
scale_size <- ScaleSize$build_accessor()
scale_size_discrete <- ScaleSizeDiscrete$build_accessor()
scale_size_identity <- ScaleIdentity$build_accessor(c(variable = "\"size\""))
scale_size_manual <- ScaleManual$build_accessor(c(variable = "\"size\""))
scale_x_asn <- ScaleAsn$build_accessor(c(variable = "\"x\""))
scale_x_atanh <- ScaleAtanh$build_accessor(c(variable = "\"x\""))
scale_x_continuous <- ScaleContinuous$build_accessor(c(variable = "\"x\""))
scale_x_date <- ScaleDate$build_accessor(c(variable = "\"x\""))
scale_x_discrete <- ScaleDiscrete$build_accessor(c(variable = "\"x\""))
scale_x_exp <- ScaleExp$build_accessor(c(variable = "\"x\""))
scale_x_inverse <- ScaleInverse$build_accessor(c(variable = "\"x\""))
scale_x_log <- ScaleLog$build_accessor(c(variable = "\"x\""))
scale_x_log10 <- ScaleLog10$build_accessor(c(variable = "\"x\""))
scale_x_log2 <- ScaleLog2$build_accessor(c(variable = "\"x\""))
scale_x_logit <- ScaleLogit$build_accessor(c(variable = "\"x\""))
scale_x_pow <- ScalePow$build_accessor(c(variable = "\"x\""))
scale_x_pow10 <- ScalePow10$build_accessor(c(variable = "\"x\""))
scale_x_prob <- ScaleProb$build_accessor(c(variable = "\"x\""))
scale_x_probit <- ScaleProbit$build_accessor(c(variable = "\"x\""))
scale_x_reverse <- ScaleReverse$build_accessor(c(variable = "\"x\""))
scale_x_sqrt <- ScaleSqrt$build_accessor(c(variable = "\"x\""))
scale_xend_asn <- ScaleAsn$build_accessor(c(variable = "\"xend\""))
scale_xend_atanh <- ScaleAtanh$build_accessor(c(variable = "\"xend\""))
scale_xend_continuous <- ScaleContinuous$build_accessor(c(variable = "\"xend\""))
scale_xend_exp <- ScaleExp$build_accessor(c(variable = "\"xend\""))
scale_xend_inverse <- ScaleInverse$build_accessor(c(variable = "\"xend\""))
scale_xend_log <- ScaleLog$build_accessor(c(variable = "\"xend\""))
scale_xend_log10 <- ScaleLog10$build_accessor(c(variable = "\"xend\""))
scale_xend_log2 <- ScaleLog2$build_accessor(c(variable = "\"xend\""))
scale_xend_logit <- ScaleLogit$build_accessor(c(variable = "\"xend\""))
scale_xend_pow <- ScalePow$build_accessor(c(variable = "\"xend\""))
scale_xend_pow10 <- ScalePow10$build_accessor(c(variable = "\"xend\""))
scale_xend_prob <- ScaleProb$build_accessor(c(variable = "\"xend\""))
scale_xend_probit <- ScaleProbit$build_accessor(c(variable = "\"xend\""))
scale_xend_reverse <- ScaleReverse$build_accessor(c(variable = "\"xend\""))
scale_xend_sqrt <- ScaleSqrt$build_accessor(c(variable = "\"xend\""))
scale_y_asn <- ScaleAsn$build_accessor(c(variable = "\"y\""))
scale_y_atanh <- ScaleAtanh$build_accessor(c(variable = "\"y\""))
scale_y_continuous <- ScaleContinuous$build_accessor(c(variable = "\"y\""))
scale_y_date <- ScaleDate$build_accessor(c(variable = "\"y\""))
scale_y_discrete <- ScaleDiscrete$build_accessor(c(variable = "\"y\""))
scale_y_exp <- ScaleExp$build_accessor(c(variable = "\"y\""))
scale_y_inverse <- ScaleInverse$build_accessor(c(variable = "\"y\""))
scale_y_log <- ScaleLog$build_accessor(c(variable = "\"y\""))
scale_y_log10 <- ScaleLog10$build_accessor(c(variable = "\"y\""))
scale_y_log2 <- ScaleLog2$build_accessor(c(variable = "\"y\""))
scale_y_logit <- ScaleLogit$build_accessor(c(variable = "\"y\""))
scale_y_pow <- ScalePow$build_accessor(c(variable = "\"y\""))
scale_y_pow10 <- ScalePow10$build_accessor(c(variable = "\"y\""))
scale_y_prob <- ScaleProb$build_accessor(c(variable = "\"y\""))
scale_y_probit <- ScaleProbit$build_accessor(c(variable = "\"y\""))
scale_y_reverse <- ScaleReverse$build_accessor(c(variable = "\"y\""))
scale_y_sqrt <- ScaleSqrt$build_accessor(c(variable = "\"y\""))
scale_yend_asn <- ScaleAsn$build_accessor(c(variable = "\"yend\""))
scale_yend_atanh <- ScaleAtanh$build_accessor(c(variable = "\"yend\""))
scale_yend_continuous <- ScaleContinuous$build_accessor(c(variable = "\"yend\""))
scale_yend_exp <- ScaleExp$build_accessor(c(variable = "\"yend\""))
scale_yend_inverse <- ScaleInverse$build_accessor(c(variable = "\"yend\""))
scale_yend_log <- ScaleLog$build_accessor(c(variable = "\"yend\""))
scale_yend_log10 <- ScaleLog10$build_accessor(c(variable = "\"yend\""))
scale_yend_log2 <- ScaleLog2$build_accessor(c(variable = "\"yend\""))
scale_yend_logit <- ScaleLogit$build_accessor(c(variable = "\"yend\""))
scale_yend_pow <- ScalePow$build_accessor(c(variable = "\"yend\""))
scale_yend_pow10 <- ScalePow10$build_accessor(c(variable = "\"yend\""))
scale_yend_prob <- ScaleProb$build_accessor(c(variable = "\"yend\""))
scale_yend_probit <- ScaleProbit$build_accessor(c(variable = "\"yend\""))
scale_yend_reverse <- ScaleReverse$build_accessor(c(variable = "\"yend\""))
scale_yend_sqrt <- ScaleSqrt$build_accessor(c(variable = "\"yend\""))
scale_z_asn <- ScaleAsn$build_accessor(c(variable = "\"z\""))
scale_z_atanh <- ScaleAtanh$build_accessor(c(variable = "\"z\""))
scale_z_continuous <- ScaleContinuous$build_accessor(c(variable = "\"z\""))
scale_z_discrete <- ScaleDiscrete$build_accessor(c(variable = "\"z\""))
scale_z_exp <- ScaleExp$build_accessor(c(variable = "\"z\""))
scale_z_inverse <- ScaleInverse$build_accessor(c(variable = "\"z\""))
scale_z_log <- ScaleLog$build_accessor(c(variable = "\"z\""))
scale_z_log10 <- ScaleLog10$build_accessor(c(variable = "\"z\""))
scale_z_log2 <- ScaleLog2$build_accessor(c(variable = "\"z\""))
scale_z_logit <- ScaleLogit$build_accessor(c(variable = "\"z\""))
scale_z_pow <- ScalePow$build_accessor(c(variable = "\"z\""))
scale_z_pow10 <- ScalePow10$build_accessor(c(variable = "\"z\""))
scale_z_prob <- ScaleProb$build_accessor(c(variable = "\"z\""))
scale_z_probit <- ScaleProbit$build_accessor(c(variable = "\"z\""))
scale_z_reverse <- ScaleReverse$build_accessor(c(variable = "\"z\""))
scale_z_sqrt <- ScaleSqrt$build_accessor(c(variable = "\"z\""))
stat_bin <- StatBin$build_accessor()
stat_boxplot <- StatBoxplot$build_accessor()
stat_contour <- StatContour$build_accessor()
stat_density <- StatDensity$build_accessor()
stat_density_2d <- StatDensity2d$build_accessor()
stat_function <- StatFunction$build_accessor()
stat_identity <- StatIdentity$build_accessor()
stat_qq <- StatQq$build_accessor()
stat_quantile <- StatQuantile$build_accessor()
stat_smooth <- StatSmooth$build_accessor()
stat_sort <- StatSort$build_accessor()
stat_sort_angle <- StatSortAngle$build_accessor()
stat_spoke <- StatSpoke$build_accessor()
stat_step <- StatStep$build_accessor()
stat_sum <- StatSum$build_accessor()
stat_summary <- StatSummary$build_accessor()
stat_unique <- StatUnique$build_accessor()
