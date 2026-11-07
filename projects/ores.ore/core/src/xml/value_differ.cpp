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
#include "ores.ore.core/xml/value_differ.hpp"
#include <algorithm>
#include <cstddef>
#include <iterator>

namespace ores::ore::xml {

namespace {

/// The run of entries that share the path the first of them states.
auto path_run(const std::vector<value_entry>& values, std::size_t from) -> std::size_t {
    auto to = from;
    while (to < values.size() && values[to].path == values[from].path)
        ++to;
    return to;
}

auto values_of(const std::vector<value_entry>& entries,
               std::size_t from,
               std::size_t to) -> std::vector<std::string> {
    std::vector<std::string> out;
    out.reserve(to - from);
    for (auto at = from; at < to; ++at)
        out.push_back(entries[at].value);
    return out;
}

/**
 * @brief Compares the values both fingerprints state at one path.
 *
 * The values each side alone states are paired as changes in sorted order, not
 * by likeness. Where a path holds several values, a change can name two values
 * that are not related; the count of differences is still exact.
 */
auto diff_path(const std::string& path,
               const std::vector<std::string>& expected,
               const std::vector<std::string>& actual,
               std::vector<value_difference>& out) -> void {
    std::vector<std::string> only_expected;
    std::vector<std::string> only_actual;
    std::ranges::set_difference(expected, actual, std::back_inserter(only_expected));
    std::ranges::set_difference(actual, expected, std::back_inserter(only_actual));

    const auto paired = std::min(only_expected.size(), only_actual.size());
    for (std::size_t at = 0; at < paired; ++at)
        out.push_back({path, value_change::changed, only_expected[at], only_actual[at]});
    for (auto at = paired; at < only_expected.size(); ++at)
        out.push_back({path, value_change::lost, only_expected[at], {}});
    for (auto at = paired; at < only_actual.size(); ++at)
        out.push_back({path, value_change::added, {}, only_actual[at]});
}

}

auto diff_values(const std::vector<value_entry>& expected,
                 const std::vector<value_entry>& actual) -> std::vector<value_difference> {
    // Both fingerprints are sorted by path and then value, so the two are
    // merged a path at a time.
    std::vector<value_difference> out;
    std::size_t e = 0;
    std::size_t a = 0;
    while (e < expected.size() || a < actual.size()) {
        const bool take_expected =
            e < expected.size() && (a >= actual.size() || expected[e].path <= actual[a].path);
        const bool take_actual =
            a < actual.size() && (e >= expected.size() || actual[a].path <= expected[e].path);
        const auto& path = take_expected ? expected[e].path : actual[a].path;

        const auto e_to = take_expected ? path_run(expected, e) : e;
        const auto a_to = take_actual ? path_run(actual, a) : a;
        diff_path(path, values_of(expected, e, e_to), values_of(actual, a, a_to), out);
        e = e_to;
        a = a_to;
    }
    return out;
}

auto describe(const value_difference& difference) -> std::string {
    switch (difference.change) {
        case value_change::lost:
            return "lost " + difference.path + ": '" + difference.expected + "'";
        case value_change::added:
            return "added " + difference.path + ": '" + difference.actual + "'";
        case value_change::changed:
            break;
    }
    return "changed " + difference.path + ": '" + difference.expected + "' became '" +
           difference.actual + "'";
}

}
