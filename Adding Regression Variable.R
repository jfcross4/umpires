umpire_full_data <- mutate(umpire_full_data, based_on_sztop = plate_z-sz_top,  based_on_avgsz_top = plate_z - mean(sz_top))
