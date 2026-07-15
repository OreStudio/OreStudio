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
#include "ores.synthetic.api/domain/curve_template_validation.hpp"
#include <exception>
#include <optional>
#include <unordered_map>

namespace ores::synthetic::domain {

namespace {

using ores::refdata::domain::resolve_end_date;
using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention;
using ores::refdata::domain::tenor_convention_resolution;
using ores::refdata::domain::tenor_window;
using ores::refdata::domain::windows_overlap;

// A resolved entry's collision surface: [start, end) for an interval
// instrument (start_tenor_code != "SPOT"), or the degenerate [end, end)
// point for a point instrument (start_tenor_code == "SPOT") -- see the
// header comment for why point instruments never collide with each other
// under this representation.
struct resolved_entry final {
    int sequence_index;
    std::string instrument_code;
    std::chrono::year_month_day start;
    std::chrono::year_month_day end;
};

std::string describe(const curve_template_entry_ref& e) {
    return "entry " + std::to_string(e.sequence_index) + " (" + e.instrument_code + ")";
}

}

curve_template_validation_result
validate_curve_template(const std::vector<curve_template_entry_ref>& entries,
                        const std::vector<tenor>& tenors,
                        const tenor_convention& convention,
                        const std::vector<tenor_convention_resolution>& resolutions,
                        std::chrono::year_month_day horizon,
                        std::chrono::year_month_day spot) {
    std::unordered_map<std::string, tenor> tenor_by_code;
    for (const auto& t : tenors)
        tenor_by_code[t.code] = t;

    std::unordered_map<std::string, tenor_convention_resolution> resolution_by_tenor_code;
    for (const auto& r : resolutions)
        resolution_by_tenor_code[r.tenor_code] = r;

    auto resolve = [&](const std::string& code) -> std::chrono::year_month_day {
        const auto tit = tenor_by_code.find(code);
        if (tit == tenor_by_code.end())
            throw std::invalid_argument("unknown tenor code '" + code + "'");

        std::optional<tenor_convention_resolution> res;
        if (const auto rit = resolution_by_tenor_code.find(code);
            rit != resolution_by_tenor_code.end())
            res = rit->second;

        return resolve_end_date(tit->second, convention, res, horizon, spot);
    };

    std::vector<resolved_entry> resolved;
    resolved.reserve(entries.size());
    for (const auto& e : entries) {
        std::chrono::year_month_day start_date;
        std::chrono::year_month_day end_date;
        try {
            start_date = resolve(e.start_tenor_code);
            end_date = resolve(e.end_tenor_code);
        } catch (const std::exception& ex) {
            return {.valid = false, .message = describe(e) + ": " + ex.what()};
        }

        if (!(start_date < end_date)) {
            return {.valid = false,
                   .message = describe(e) + ": start tenor '" + e.start_tenor_code +
                              "' does not resolve to a date before end tenor '" +
                              e.end_tenor_code + "'"};
        }

        const bool is_point = e.start_tenor_code == "SPOT";
        resolved.push_back({.sequence_index = e.sequence_index,
                            .instrument_code = e.instrument_code,
                            .start = is_point ? end_date : start_date,
                            .end = end_date});
    }

    for (std::size_t i = 0; i < resolved.size(); ++i) {
        for (std::size_t j = i + 1; j < resolved.size(); ++j) {
            const auto& a = resolved[i];
            const auto& b = resolved[j];
            if (windows_overlap(tenor_window{a.start, a.end}, tenor_window{b.start, b.end})) {
                return {.valid = false,
                       .message = "entry " + std::to_string(a.sequence_index) + " (" +
                                  a.instrument_code + ") collides with entry " +
                                  std::to_string(b.sequence_index) + " (" + b.instrument_code +
                                  ")"};
            }
        }
    }

    return {};
}

}
