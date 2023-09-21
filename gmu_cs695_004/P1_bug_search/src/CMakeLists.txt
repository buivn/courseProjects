cmake_minimum_required(VERSION 3.1...3.20)

project(cgal_voronoi_example_cs695)
find_package(CGAL REQUIRED)

#create the executable of the application
create_single_source_cgal_program("vd_2_point_location.cpp")
#end of the file
