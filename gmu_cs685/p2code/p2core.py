import matplotlib
import numpy as np

# Environment Definition & Plotting Functions
LINEWIDTH = 2.0

# Motion Primitive Starter Code
import copy
import numpy as np
import math
import matplotlib
import matplotlib.pyplot as plt
import random
import shapely
import shapely.geometry
import shapely.affinity


class Pose(object):
    counter = 0

    def __init__(self, x, y, yaw=0.0):
        self.x = x
        self.y = y
        self.yaw = yaw
        self.index = Pose.counter
        Pose.counter += 1

    def __repr__(self):
        return "<Pose x:%4f, y:%4f, yaw:%4f>" % (self.x, self.y, self.yaw)

    @staticmethod
    def cartesian_distance(pose_a, pose_b):
        return math.sqrt(
            math.pow(pose_a.x - pose_b.x, 2) +
            math.pow(pose_a.y - pose_b.y, 2))

    def __mul__(self, oth):
        return oth.__rmul__(self)

    def __rmul__(self, oth):
        """Define transform out = oth*self. This should be the equivalent
        of adding an additional pose 'oth' to the current pose 'self'.
        This means that, for example, if we have a robot in pose 'self' and
        a motion primitive that ends at 'oth' the position of the end of the
        motion primitive in the world frame is oth*self.
        """

        try:
            x = self.x + np.cos(self.yaw) * oth.x - np.sin(
                self.yaw) * oth.y
            y = self.y + np.cos(self.yaw) * oth.y + np.sin(
                self.yaw) * oth.x
            yaw = (self.yaw + oth.yaw) % (2 * np.pi)
            return Pose(x, y, yaw)
        except AttributeError:
            return Pose(oth * self.x, oth * self.y, self.yaw)
        else:
            raise TypeError(('Type {0} cannot rmul a Pose object.').format(
                type(oth).__name__))


class MotionPrimitive():
    def __init__(self, poses, cost):
        self.poses = poses
        self.cost = cost

    def transform(self, pose):
        """Transform the motion primitive by a given pose.
        This is equivalent to 'right multiplying' the pose/transform.
        The resulting primitive should be such that the output poses
        are the input poses applied to the pose.
        """
        new_poses = [p * pose for p in self.poses]
        return MotionPrimitive(poses=new_poses, cost=self.cost)


class RobotState(object):
    """A simple robot that moves using motion primitives.
    This is the base class for robots that use motion primitives to move.
    As such, the 'get_motion_primitives' function must be defined and the
    move class has been updated to include the selected primitive for motion.
    """
    def __init__(self, start_pose, footprint=None):
        self.pose = copy.copy(start_pose)
        self.all_poses = [copy.copy(self.pose)]
        self.net_motion = 0
        self.num_moves = 0
        self.did_collide = False
        self.is_updated = False

        if footprint is None:
            self.footprint = shapely.geometry.Point(0, 0)
        else:
            self.footprint = footprint

    def move(self, primitive, does_collide_fn=None, cost_grid=None):
        # Transform from the robot coordinate frame
        if self.net_motion < 1:
            self.first_transformed_primitive = copy.copy(self)
            self.first_pose = copy.copy(self.pose)
            self.is_updated = True

        new_rstate = copy.copy(self)
        new_rstate.all_poses = [p for p in new_rstate.all_poses]
        transformed_primitive = primitive.transform(new_rstate.pose)

        # Keep track of the first movement for use later
        if self.net_motion < 1:
            new_rstate.first_transformed_primitive = transformed_primitive.poses
            new_rstate.first_pose = transformed_primitive.poses[-1]

        for pose in transformed_primitive.poses:
            if does_collide_fn is not None:
                foot = new_rstate._get_footprint_for_pose(pose)
                new_rstate.did_collide = new_rstate.did_collide or does_collide_fn(foot)
            new_rstate.all_poses.append(copy.copy(pose))

        # Update some terms
        if cost_grid is not None:
            xs = np.array([int(p.x) for p in transformed_primitive.poses])
            ys = np.array([int(p.y) for p in transformed_primitive.poses])
            try:
                new_rstate.net_motion += transformed_primitive.cost * np.mean(cost_grid[xs, ys])
            except:
                new_rstate.net_motion += 10000
            pass
        else:
            new_rstate.net_motion += transformed_primitive.cost
        new_rstate.pose = new_rstate.all_poses[-1]

        return new_rstate

    def move_along_rstate(self, rstate):
        self.all_poses += rstate.first_transformed_primitive
        self.pose = rstate.first_pose

    def get_current_footprint(self):
        return self._get_footprint_for_pose(self.pose)

    def _get_footprint_for_pose(self, pose):
        rfootprint = shapely.affinity.rotate(self.footprint,
                                             pose.yaw,
                                             origin=(0, 0),
                                             use_radians=True)
        lfootprint = shapely.affinity.translate(rfootprint,
                                                xoff=pose.x,
                                                yoff=pose.y)
        return lfootprint


def get_primitive_library(max_theta=np.pi/6, curvature=1.0):
    """Returns the motion primitives available to the robot at the current
    time (can be a function of robot state)."""
    # Create motion primitives
    r = curvature
    return [MotionPrimitive(poses=[Pose(
        x=r * np.sin(d * max_theta / 10),
        y=r * (1 - np.cos(d * max_theta / 10)),
        yaw=d*max_theta/10) for d in range(1, 11)], cost=r * max_theta),
            MotionPrimitive(poses=[Pose(x=r * max_theta, y=0, yaw=0)], cost=r * max_theta),
            MotionPrimitive(poses=[Pose(
        x=r * np.sin(d * max_theta / 10),
        y=r * (np.cos(d * max_theta / 10) - 1),
        yaw=-d*max_theta/10) for d in range(1, 11)], cost=r * max_theta),
           ]

primitive_library = get_primitive_library()


class World(object):
    """Stores the shapely polygons that define the world (base structure for
    many environments).
    Attributes:
        obstacles: list of shapely polygons
        boudary: shapely polygon which defines the outer obstacle of world
        clutter_element_poses: positions of clutter
        known_space_poly: shapely polygon representing known, free space
        area: area of known space polygon
    """
    def __init__(self, obstacles):
        self.obstacles = obstacles

    def does_poly_collide(self, poly):
        for obstacle in self.obstacles:
            if obstacle.intersects(poly):
                return True

        return False


def plot_world(ax, world, do_show_points=False, alpha=1.0):
    for obstacle in world.obstacles:
        x, y = obstacle.exterior.xy
        if do_show_points:
            ax.plot(x, y, 'ko-', linewidth=LINEWIDTH, alpha=alpha)
        else:
            ax.plot(x, y, 'k', linewidth=LINEWIDTH, alpha=alpha)


def plot_polygon(ax, poly, color=[0.0, 0.0, 1.0], alpha=1.0):
    # Plotting code from: https://sgillies.net/2010/04/06/painting-punctured-polygons-with-matplotlib.html

    if isinstance(poly, shapely.geometry.MultiPolygon):
        [plot_polygon(ax, p, color, alpha) for p in poly]
        return

    def ring_coding(ob):
        # The codes will be all "LINETO" commands, except for "MOVETO"s at the
        # beginning of each subpath
        n = len(ob.coords)
        codes = np.ones(n, dtype=matplotlib.path.Path.code_type) \
            * matplotlib.path.Path.LINETO
        codes[0] = matplotlib.path.Path.MOVETO
        return codes

    def pathify(polygon):
        # Convert coordinates to path vertices. Objects produced by Shapely's
        # analytic methods have the proper coordinate order, no need to sort.
        vertices = np.concatenate([np.asarray(polygon.exterior.coords)] +
                                  [np.asarray(r) for r in polygon.interiors])
        codes = np.concatenate([ring_coding(polygon.exterior)] +
                               [ring_coding(r) for r in polygon.interiors])
        return matplotlib.path.Path(vertices, codes)

    path = pathify(poly)
    patch = matplotlib.patches.PathPatch(path,
                                         facecolor=color,
                                         linewidth=0,
                                         alpha=alpha)
    ax.add_patch(patch)


def plot_robot_state(ax, robot_state, do_plot_poly=False, alpha=1.0):
    xs = [pose.x for pose in robot_state.all_poses]
    ys = [pose.y for pose in robot_state.all_poses]
    plt.plot(xs, ys, alpha=alpha)
    if do_plot_poly:
        plot_polygon(ax, robot_state.get_current_footprint(), alpha=alpha)
