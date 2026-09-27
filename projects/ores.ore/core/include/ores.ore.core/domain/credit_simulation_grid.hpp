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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ORE_CORE_DOMAIN_CREDIT_SIMULATION_GRID_HPP
#define ORES_ORE_CORE_DOMAIN_CREDIT_SIMULATION_GRID_HPP

#include <cstddef>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief The grid ORE writes inside a transition matrix Data element.
 *
 * ORE carries a whole transition matrix in the text of one element: a comment
 * naming the states, then one row of probabilities per state, separated by
 * commas or whitespace. The labels are not derivable from the numbers, so they
 * are held here rather than discarded.
 */
struct credit_simulation_grid {
    std::vector<std::string> labels;
    std::vector<double> values;

    std::size_t side() const {
        std::size_t n = 0;
        while (n * n < values.size())
            ++n;
        return n;
    }
};

/**
 * @brief Reads the labels and the probabilities out of a Data element's text.
 */
inline credit_simulation_grid parse_credit_simulation_grid(std::string_view text) {
    credit_simulation_grid grid;
    std::string body(text);
    const std::string open = "<!--";
    const std::string close = "-->";
    const auto open_at = body.find(open);
    if (open_at != std::string::npos) {
        const auto close_at = body.find(close, open_at);
        if (close_at != std::string::npos) {
            std::istringstream labels(
                body.substr(open_at + open.size(), close_at - open_at - open.size()));
            std::string label;
            while (labels >> label)
                grid.labels.push_back(label);
            body.erase(open_at, close_at - open_at + close.size());
        }
    }
    for (auto& c : body) {
        if (c == ',')
            c = ' ';
    }
    std::istringstream values(body);
    double v = 0.0;
    while (values >> v)
        grid.values.push_back(v);
    return grid;
}

/**
 * @brief Writes the grid back into the text of a Data element.
 *
 * ORE writes four decimal places and separates the values with a comma and a
 * space, so the formatting is fixed here rather than left to the default.
 */
inline std::string format_credit_simulation_grid(const credit_simulation_grid& grid) {
    std::ostringstream out;
    if (!grid.labels.empty()) {
        out << "<!--";
        for (const auto& label : grid.labels)
            out << " " << label;
        out << " -->\n";
    }
    const auto side = grid.side();
    out.setf(std::ios::fixed, std::ios::floatfield);
    out.precision(4);
    for (std::size_t row = 0; row < side; ++row) {
        out << "\t";
        for (std::size_t col = 0; col < side; ++col)
            out << grid.values[row * side + col] << ", ";
        out << "\n";
    }
    return out.str();
}

}

#endif
