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
#include "ores.marketdata.core/service/series_shape.hpp"
#include "ores.marketdata.core/repository/series_axis_repository.hpp"
#include "ores.marketdata.core/repository/series_axis_value_repository.hpp"
#include <algorithm>
#include <array>
#include <compare>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

namespace ores::marketdata::service {

std::optional<std::string> coordinate_of(const datum::market_datum& point, datum::field axis) {
    if (!point.holds(axis))
        return std::nullopt;
    const auto& held = point.at(axis);
    if (std::holds_alternative<datum::none_t>(held))
        return std::nullopt;
    return datum::text_of(held);
}

series_grid::series_grid(const series_shape& shape)
    : shape_(shape)
    , strides_(shape.axes.size(), 1) {
    // The last axis varies fastest, so a node's index is the sum of each value's
    // position times that axis's stride.
    for (std::size_t i = shape_.axes.size(); i-- > 0;) {
        strides_[i] = size_;
        size_ *= shape_.values.at(shape_.axes[i]).size();
    }
}

std::vector<std::string> series_grid::coordinates(std::size_t index) const {
    std::vector<std::string> labels;
    labels.reserve(shape_.axes.size());
    for (std::size_t i = 0; i < shape_.axes.size(); ++i) {
        const auto& values = shape_.values.at(shape_.axes[i]);
        labels.push_back(values[(index / strides_[i]) % values.size()]);
    }
    return labels;
}

std::optional<std::size_t> series_grid::node_of(const datum::market_datum& point) const {
    std::size_t index = 0;
    for (std::size_t i = 0; i < shape_.axes.size(); ++i) {
        const auto held = coordinate_of(point, shape_.axes[i]);
        if (!held)
            return std::nullopt;
        const auto& values = shape_.values.at(shape_.axes[i]);
        const auto at = std::ranges::find(values, *held);
        if (at == values.end())
            return std::nullopt;
        index += static_cast<std::size_t>(at - values.begin()) * strides_[i];
    }
    return index;
}

namespace {

/// Where a term sits among terms: its kind first, then its length.
struct term_rank {
    int kind;
    long long magnitude;
    friend auto operator<=>(const term_rank&, const term_rank&) = default;
};

/// The days a period such as 1Y6M spans, a month counted as 30 and a year as 365.
long long period_days(const std::string& text) {
    long long total = 0;
    long long count = 0;
    for (const char c : text) {
        if (c >= '0' && c <= '9') {
            count = count * 10 + (c - '0');
            continue;
        }
        switch (c) {
            case 'Y':
                total += count * 365;
                break;
            case 'M':
                total += count * 30;
                break;
            case 'W':
                total += count * 7;
                break;
            case 'D':
            default:
                total += count;
                break;
        }
        count = 0;
    }
    return total;
}

/// The digits of @p text as a number, ignoring every other character.
long long digits_of(const std::string& text) {
    long long number = 0;
    for (const char c : text) {
        if (c >= '0' && c <= '9')
            number = number * 10 + (c - '0');
    }
    return number;
}

/// The place of an FX tenor among the overnight tenors: ON, then TN, then SN.
long long fx_tenor_order(const std::string& text) {
    constexpr std::array<std::string_view, 3> order{"ON", "TN", "SN"};
    const auto at = std::ranges::find(order, text);
    return static_cast<long long>(at - order.begin());
}

std::optional<term_rank> rank_of(const std::string& text) {
    const auto parsed = datum::term::parse(text);
    if (!parsed)
        return std::nullopt;
    switch (parsed->which()) {
        case datum::term::kind::fx_tenor:
            return term_rank{0, fx_tenor_order(text)};
        case datum::term::kind::period:
            return term_rank{1, period_days(text)};
        case datum::term::kind::date:
            // The term parser accepts YYYY-MM-DD and YYYYMMDD only, so the digits of
            // a date sort as the date does. A date written another way would not.
            return term_rank{2, digits_of(text)};
        case datum::term::kind::continuation:
            return term_rank{3, digits_of(text)};
    }
    return std::nullopt;
}

}

std::vector<std::string> in_term_order(std::vector<std::string> stated) {
    // Each rank is worked out once. A value that is no term has none and sorts
    // after every term, and the stable sort keeps equal ranks in stated order.
    std::vector<std::pair<std::optional<term_rank>, std::string>> ranked;
    ranked.reserve(stated.size());
    for (auto& value : stated)
        ranked.emplace_back(rank_of(value), std::move(value));
    std::ranges::stable_sort(ranked, [](const auto& a, const auto& b) {
        if (a.first.has_value() != b.first.has_value())
            return a.first.has_value();
        return a.first.has_value() && *a.first < *b.first;
    });
    std::vector<std::string> ordered;
    ordered.reserve(ranked.size());
    for (auto& entry : ranked)
        ordered.push_back(std::move(entry.second));
    return ordered;
}

series_shape read_series_shape(ores::database::context ctx, const std::string& series_id) {
    using namespace ores::marketdata;
    const std::vector<std::string> ids{series_id};

    std::vector<std::pair<int, datum::field>> ordered_axes;
    for (const auto& a : repository::series_axis_repository{}.read_latest_for_series(ctx, ids)) {
        const auto named = datum::field_named(a.axis_field);
        if (!named)
            throw std::runtime_error("the shape declares '" + a.axis_field +
                                     "', which is not an oresmd field name");
        ordered_axes.emplace_back(a.sequence, *named);
    }
    std::ranges::sort(ordered_axes);

    std::map<datum::field, std::vector<std::pair<int, std::string>>> ordered_values;
    for (const auto& v :
         repository::series_axis_value_repository{}.read_latest_for_series(ctx, ids)) {
        const auto named = datum::field_named(v.axis_field);
        if (!named)
            throw std::runtime_error("the shape declares a value of '" + v.axis_field +
                                     "', which is not an oresmd field name");
        ordered_values[*named].emplace_back(v.sequence, v.value);
    }

    series_shape result;
    result.axes.reserve(ordered_axes.size());
    for (auto& [sequence, axis] : ordered_axes) {
        result.axes.push_back(axis);
        auto& values = ordered_values[axis];
        std::ranges::sort(values);
        auto& declared = result.values[axis];
        declared.reserve(values.size());
        for (auto& [position, value] : values)
            declared.push_back(std::move(value));
        // A term axis is read in order of length, whatever order the source stated.
        if (datum::kind_of(axis) == datum::value_kind::term)
            declared = in_term_order(std::move(declared));
    }
    return result;
}

}
