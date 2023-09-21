// From: https://doc.cgal.org/latest/Voronoi_diagram_2/Voronoi_diagram_2_2vd_2_point_location_8cpp-example.html
// Data from: https://github.com/CGAL/cgal/blob/master/Voronoi_diagram_2/examples/Voronoi_diagram_2/data/
// standard includes
#include <iostream>
#include <fstream>
#include <cassert>

// includes for defining the Voronoi diagram adaptor
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Segment_Delaunay_graph_Linf_filtered_traits_2.h>
#include <CGAL/Segment_Delaunay_graph_Linf_2.h>
#include <CGAL/Voronoi_diagram_2.h>
#include <CGAL/Segment_Delaunay_graph_adaptation_traits_2.h>
#include <CGAL/Segment_Delaunay_graph_adaptation_policies_2.h>

// typedefs for defining the adaptor
typedef CGAL::Exact_predicates_inexact_constructions_kernel                  K;
typedef CGAL::Segment_Delaunay_graph_Linf_filtered_traits_2<K>               Gt;
typedef CGAL::Segment_Delaunay_graph_Linf_2<Gt>                              DT;
typedef CGAL::Segment_Delaunay_graph_adaptation_traits_2<DT>                 AT;
typedef CGAL::Segment_Delaunay_graph_degeneracy_removal_policy_2<DT>         AP;
typedef CGAL::Voronoi_diagram_2<DT,AT,AP>                                    VD;

// typedef for the result type of the point location
typedef AT::Site_2                    Site_2;
typedef AT::Point_2                   Point_2;

typedef VD::Locate_result             Locate_result;
typedef VD::Vertex_handle             Vertex_handle;
typedef VD::Face_handle               Face_handle;
typedef VD::Halfedge_handle           Halfedge_handle;
typedef VD::Ccb_halfedge_circulator   Ccb_halfedge_circulator;

void print_endpoint(Halfedge_handle e, bool is_src) {
  std::cout << "\t";
  if ( is_src ) {
    if ( e->has_source() )  std::cout << e->source()->point() << std::endl;
    else  std::cout << "point at infinity" << std::endl;
  } else {
    if ( e->has_target() )  std::cout << e->target()->point() << std::endl;
    else  std::cout << "point at infinity" << std::endl;
  }
}

int main() {
  std::ifstream ifs("data/data_cs695.cin");
  assert(ifs);

  VD vd;

  Site_2 t;
  while ( ifs >> t ) { vd.insert(t); }
  ifs.close();

  assert(vd.is_valid());

  // Printing code from: https://stackoverflow.com/a/37446663
  VD::Face_iterator it = vd.faces_begin(), beyond = vd.faces_end();
  for (int f=0; it != beyond; ++f, ++it) {
    VD::Ccb_halfedge_circulator hec = it->ccb();
    do {
      VD::Halfedge_handle heh = static_cast<VD::Halfedge_handle>(hec);
      if (heh->has_target())
        std::cout << heh->target()->point() << " ";
    } while (++hec != it->ccb());
    std::cout << std::endl;
  }
  return 0;
}
