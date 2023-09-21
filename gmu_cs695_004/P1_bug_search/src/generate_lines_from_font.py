# Adapted from: https://catherineh.github.io/programming/2018/02/01/text-to-svg-paths
# Note: this is a mess; didn't intend for you all to read it :)

from svgpathtools import wsvg, Line, QuadraticBezier, Path
import numpy as np
import matplotlib.pyplot as plt

def tuple_to_imag(t):
    return t[0] + t[1] * 1j

from freetype import Face
face = Face('/Users/gjstein/Library/Fonts/iosevka-term-regular.ttf')


def get_segments_for_letter(letter):
    face.set_char_size(48 * 64)
    face.load_char(letter)
    outline = face.glyph.outline
    y = [t[1] for t in outline.points]
    # flip the points
    outline_points = [(p[0], max(y) - p[1]) for p in outline.points]
    start, end = 0, 0
    paths = []

    all_segments = []
    print(outline.contours)
    # for i in range(len(outline.contours)):
    for i in range(1):
        end = outline.contours[i]
        points = outline_points[start:end + 1]
        tags = outline.tags[start:end + 1]
        tags.append(tags[0])
        points.append(points[0])
        segments = [[points[0], ], ]
        for j in range(1, len(points)):
            segments[-1].append(points[j])
            if tags[j] and j < (len(points) - 1):
                segments.append([points[j], ])

        all_segments += segments
        start = end+1

    return all_segments

segments = get_segments_for_letter('C')
plt.figure()
for segment in np.array(segments):
    segment = np.array(segment)
    plt.plot(segment[:, 0], segment[:, 1], 'b')

def write_letter(f, letter, offset=0):
    segments = get_segments_for_letter(letter)
    for segment in segments:
        for sa, sb in zip(segment, segment[1:]):
            f.write(f"s {sa[0] + offset} {3000-sa[1]} {sb[0] + offset} {3000-sb[1]}\n")


import os
path = os.path.join(os.path.dirname(os.path.realpath("__file__")),
                             'src', 'data', 'data_cs695.cin')
map_lines = []
with open(path, 'w') as f:
    write_letter(f, 'C')
    write_letter(f, 'S', offset=1500)

    write_letter(f, '6', offset=3500)
    write_letter(f, '9', offset=5000)
    write_letter(f, '5', offset=6500)

path = os.path.join(os.path.dirname(os.path.realpath("__file__")),
                             'src', 'data', 'data_cs695.cin')
map_lines = []
with open(path, 'r') as f:
    lines = f.readlines()
    for line in lines:
        map_lines.append([float(num) for num in line[2:].split(" ")])

plt.figure()
for map_line in np.array(map_lines):
    plt.plot(map_line[::2], map_line[1::2], 'b')
