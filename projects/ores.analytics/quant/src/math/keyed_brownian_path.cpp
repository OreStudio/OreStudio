/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/math/keyed_brownian_path.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>

namespace ores::analytics::quant::math {
namespace {

constexpr unsigned max_depth = 32;

[[nodiscard]] std::uint64_t nodes_per_cell(unsigned depth) {
    return std::uint64_t{1} << depth;
}

[[nodiscard]] std::uint64_t
draw_index(const keyed_path_params& params, std::int64_t cell, std::uint64_t node) {
    const auto cells = static_cast<std::uint64_t>(cell);
    if (cells > (std::numeric_limits<std::uint64_t>::max() >> params.depth))
        throw std::out_of_range("keyed_path_span: time is too far from the origin");
    return cells * nodes_per_cell(params.depth) + node;
}

/// The finest node a normalised time falls in, clamped to the cell.
[[nodiscard]] std::uint64_t finest_node(const keyed_path_params& params, double u) {
    const auto nodes = nodes_per_cell(params.depth);
    const double scaled = u * static_cast<double>(nodes);
    if (!(scaled > 0.0))
        return 0;
    const auto node = static_cast<std::uint64_t>(scaled);
    return node < nodes ? node : nodes - 1;
}

/// The weight a time has inside its finest interval, in node units.
[[nodiscard]] double node_weight(const keyed_path_params& params, double u, std::uint64_t node) {
    return u * static_cast<double>(nodes_per_cell(params.depth)) - static_cast<double>(node);
}

}

keyed_path_params
make_keyed_path(stream_key key, double cell_width, unsigned depth, double origin) {
    if (!std::isfinite(cell_width) || cell_width <= 0.0)
        throw std::invalid_argument("make_keyed_path: cell_width must be finite and positive");
    if (depth < 1 || depth > max_depth)
        throw std::invalid_argument("make_keyed_path: depth must be between 1 and 32");
    if (!std::isfinite(origin) || origin < 0.0)
        throw std::invalid_argument("make_keyed_path: origin must be finite and not negative");

    // A bridge at level l spans one cell, halved l - 1 times, so its standard
    // deviation depends on the level alone and is paid for once per factor.
    keyed_path_params params{};
    params.key = key;
    params.cell_width = cell_width;
    params.depth = depth;
    params.origin = origin;
    params.origin_cell = static_cast<std::int64_t>(std::floor(origin / cell_width));
    const double cell_sd = std::sqrt(cell_width);
    for (unsigned level = 1; level <= max_depth; ++level)
        params.bridge_sd[level] =
            0.5 * cell_sd * std::sqrt(std::ldexp(1.0, -static_cast<int>(level - 1)));

    params.origin_value = keyed_path_value_in_cell(params, params.origin_cell, 0.0, origin);
    return params;
}

std::int64_t keyed_path_cell_of(const keyed_path_params& params, double t) {
    if (!std::isfinite(t))
        throw std::invalid_argument("keyed_path_cell_of: time must be finite");
    if (t < params.origin)
        throw std::invalid_argument("keyed_path_cell_of: time is before the origin");
    const auto cell = static_cast<std::int64_t>(std::floor(t / params.cell_width));
    if (cell < 0)
        throw std::invalid_argument("keyed_path_cell_of: time maps to a cell before zero");
    return cell;
}

double keyed_path_finest_step(const keyed_path_params& params) {
    return params.cell_width / static_cast<double>(nodes_per_cell(params.depth));
}

double keyed_path_cell_increment(const keyed_path_params& params, std::int64_t cell) {
    return std::sqrt(params.cell_width) *
           standard_normal_at(params.key, draw_index(params, cell, 0));
}

double keyed_path_boundary_at(const keyed_path_params& params, std::int64_t cell) {
    double sum = 0.0;
    for (std::int64_t c = params.origin_cell; c < cell; ++c)
        sum += keyed_path_cell_increment(params, c);
    return sum;
}

void keyed_path_span(const keyed_path_params& params,
                     std::int64_t cell,
                     double boundary,
                     std::uint64_t node,
                     double& from,
                     double& to) {
    const auto nodes = nodes_per_cell(params.depth);
    if (node >= nodes)
        throw std::out_of_range("keyed_path_span: node is outside the cell");

    double ua = 0.0;
    double ub = 1.0;
    double fa = boundary;
    double fb = boundary + keyed_path_cell_increment(params, cell);
    for (unsigned level = 1; level <= params.depth; ++level) {
        // The node a level creates is the midpoint of its interval, so the heap
        // index counts nodes, not intervals: level l holds 2^(l-1) of them.
        const std::uint64_t ancestor = node >> (params.depth - level + 1);
        const std::uint64_t h = (std::uint64_t{1} << (level - 1)) + ancestor;
        const double z = standard_normal_at(params.key, draw_index(params, cell, h));
        const double fm = 0.5 * (fa + fb) + params.bridge_sd[level] * z;
        if (((node >> (params.depth - level)) & 1u) == 0) {
            ub = 0.5 * (ua + ub);
            fb = fm;
        } else {
            ua = 0.5 * (ua + ub);
            fa = fm;
        }
    }
    from = fa;
    to = fb;
}

double keyed_path_value_in_cell(const keyed_path_params& params,
                                std::int64_t cell,
                                double boundary,
                                double t) {
    const double u = (t - static_cast<double>(cell) * params.cell_width) / params.cell_width;
    const auto node = finest_node(params, u);
    double from = 0.0;
    double to = 0.0;
    keyed_path_span(params, cell, boundary, node, from, to);
    return from + (to - from) * node_weight(params, u, node);
}

double keyed_path_value_at(const keyed_path_params& params, double t) {
    const std::int64_t cell = keyed_path_cell_of(params, t);
    return keyed_path_value_in_cell(params, cell, keyed_path_boundary_at(params, cell), t) -
           params.origin_value;
}

keyed_walk make_keyed_walk(const keyed_path_params& params) {
    keyed_walk walk{};
    walk.cell = params.origin_cell;
    return walk;
}

double keyed_walk_value(const keyed_path_params& params, keyed_walk& walk, double t) {
    const std::int64_t cell = keyed_path_cell_of(params, t);
    if (cell < walk.cell)
        throw std::invalid_argument("keyed_walk_value: time is behind the walk");

    while (walk.cell < cell) {
        walk.boundary += keyed_path_cell_increment(params, walk.cell);
        ++walk.cell;
    }

    const double u = (t - static_cast<double>(cell) * params.cell_width) / params.cell_width;
    const auto node = finest_node(params, u);
    if (!walk.span_valid || walk.span_cell != cell || walk.span_node != node) {
        keyed_path_span(params, cell, walk.boundary, node, walk.span_from, walk.span_to);
        walk.span_cell = cell;
        walk.span_node = node;
        walk.span_valid = true;
    }
    return walk.span_from + (walk.span_to - walk.span_from) * node_weight(params, u, node) -
           params.origin_value;
}

}
