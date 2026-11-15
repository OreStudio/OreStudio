// SPDX-License-Identifier: GPL-3.0-or-later
export function finite(value: number, label: string): number {
    if (!Number.isFinite(value)) throw new Error(`${label} is not finite.`);
    return value;
}

export function bisect(
    fn: (x: number) => number,
    lo: number,
    hi: number,
    tolerance = 1e-13,
): number {
    let a = finite(fn(lo), 'Root endpoint');
    const b = finite(fn(hi), 'Root endpoint');
    if (a === 0) return lo;
    if (b === 0) return hi;
    if (a * b > 0) throw new Error('No bracketed solution under the supplied conventions.');
    for (let i = 0; i < 100; i++) {
        const mid = (lo + hi) / 2;
        const c = finite(fn(mid), 'Root evaluation');
        if (Math.abs(hi - lo) <= tolerance) return mid;
        if (a * c <= 0) hi = mid;
        else {
            lo = mid;
            a = c;
        }
    }
    throw new Error('Numerical solver did not converge.');
}

// Normal tail via an incomplete-gamma continued fraction / convergent series.
// Avoids a low-precision CDF approximation limiting quote recovery tolerances.
export function normalCdf(x: number): number {
    if (x === 0) return 0.5;
    if (Math.abs(x) > 38) return x > 0 ? 1 : 0;
    const z = (x * x) / 2,
        a = 0.5;
    const factor = Math.exp(-z + a * Math.log(z) - 0.5723649429247001);
    let q: number;
    if (z < a + 1) {
        let sum = 1 / a,
            term = sum;
        for (let n = 1; n < 300; n++) {
            term *= z / (a + n);
            sum += term;
            if (Math.abs(term) < Math.abs(sum) * 1e-16) break;
        }
        q = 1 - factor * sum;
    } else {
        let b = z + 1 - a,
            c = 1e300,
            d = 1 / b,
            h = d;
        for (let n = 1; n < 300; n++) {
            const an = -n * (n - a);
            b += 2;
            d = an * d + b;
            if (Math.abs(d) < 1e-300) d = 1e-300;
            c = b + an / c;
            if (Math.abs(c) < 1e-300) c = 1e-300;
            d = 1 / d;
            const step = d * c;
            h *= step;
            if (Math.abs(step - 1) < 1e-15) break;
        }
        q = factor * h;
    }
    return x > 0 ? 1 - q / 2 : q / 2;
}

export const normalInverse = (p: number) => bisect((x) => normalCdf(x) - p, -12, 12);
export const years = (a: string, b: string) => (Date.parse(b) - Date.parse(a)) / 86400000 / 365;

export function interpolate(points: { x: number; y: number }[], x: number): number {
    if (!points.length || x < points[0]!.x || x > points.at(-1)!.x)
        throw new Error('Requested date is outside the supplied curve coverage.');
    const exact = points.find((p) => p.x === x);
    if (exact) return exact.y;
    const i = points.findIndex((p) => p.x > x),
        left = points[i - 1]!,
        right = points[i]!;
    return left.y + ((right.y - left.y) * (x - left.x)) / (right.x - left.x);
}

// Natural cubic spline of total variance in log-strike; exact at supplied nodes.
export function smileSpline(nodes: { x: number; y: number }[]): (x: number) => number {
    const n = nodes.length,
        second = Array<number>(n).fill(0),
        u = Array<number>(n).fill(0);
    for (let i = 1; i < n - 1; i++) {
        const l = nodes[i - 1]!,
            m = nodes[i]!,
            r = nodes[i + 1]!;
        if (!(l.x < m.x && m.x < r.x)) throw new Error('Smile strikes are not strictly ordered.');
        const sig = (m.x - l.x) / (r.x - l.x),
            p = sig * second[i - 1]! + 2;
        second[i] = (sig - 1) / p;
        u[i] =
            ((6 * ((r.y - m.y) / (r.x - m.x) - (m.y - l.y) / (m.x - l.x))) / (r.x - l.x) -
                sig * u[i - 1]!) /
            p;
    }
    for (let i = n - 2; i >= 0; i--) second[i] = second[i]! * second[i + 1]! + u[i]!;
    return (x) => {
        const j = Math.max(
            1,
            Math.min(
                n - 1,
                nodes.findIndex((p) => p.x >= x) < 0 ? n - 1 : nodes.findIndex((p) => p.x >= x),
            ),
        );
        const l = nodes[j - 1]!,
            r = nodes[j]!,
            h = r.x - l.x;
        const a = (r.x - x) / h,
            b = (x - l.x) / h;
        return (
            a * l.y +
            b * r.y +
            (((a * a * a - a) * second[j - 1]! + (b * b * b - b) * second[j]!) * h * h) / 6
        );
    };
}
