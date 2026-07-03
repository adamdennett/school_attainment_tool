from PIL import Image
import math

SRC = "E:/school_attainment_tool/slides_assets/title-bg.png"
OUT = "E:/school_attainment_tool/slides_assets/title-bg-composite.png"

img = Image.open(SRC).convert("RGB")
W, H = img.size
px = img.load()

purple = (58, 24, 87)
# CSS: linear-gradient(110deg, stops). angle 110deg (clockwise from up).
A = math.radians(110)
dx, dy = math.sin(A), -math.cos(A)          # screen coords, y down
L = abs(W * dx) + abs(H * dy)               # gradient line length
cx, cy = W / 2.0, H / 2.0

stops = [(0.0, 0.97), (0.42, 0.93), (0.62, 0.70), (0.80, 0.40), (1.0, 0.18)]

def alpha_at(t):
    if t <= stops[0][0]:
        return stops[0][1]
    if t >= stops[-1][0]:
        return stops[-1][1]
    for i in range(len(stops) - 1):
        p0, a0 = stops[i]
        p1, a1 = stops[i + 1]
        if p0 <= t <= p1:
            f = (t - p0) / (p1 - p0)
            return a0 + f * (a1 - a0)
    return stops[-1][1]

# precompute alpha per gradient position along one axis isn't enough (2D), do per-pixel
for y in range(H):
    for x in range(W):
        t = 0.5 + ((x - cx) * dx + (y - cy) * dy) / L
        t = max(0.0, min(1.0, t))
        a = alpha_at(t)
        r, g, b = px[x, y]
        px[x, y] = (
            int(r * (1 - a) + purple[0] * a),
            int(g * (1 - a) + purple[1] * a),
            int(b * (1 - a) + purple[2] * a),
        )

# accent bar: bottom strip, gradient bright-purple -> heritage-blue -> bright-purple
bar_h = max(2, round(H * 4 / 720))   # 4px at 720-tall slide
bp = (153, 58, 255)   # $ucl-bright-purple #993AFF
hb = (48, 214, 255)   # $ucl-heritage-blue #30D6FF
for x in range(W):
    f = x / (W - 1)
    # triangle 0->1->0
    tt = (f * 2) if f < 0.5 else (2 - f * 2)
    col = tuple(int(bp[i] + (hb[i] - bp[i]) * tt) for i in range(3))
    for y in range(H - bar_h, H):
        px[x, y] = col

img.save(OUT)
print("wrote", OUT, img.size)
