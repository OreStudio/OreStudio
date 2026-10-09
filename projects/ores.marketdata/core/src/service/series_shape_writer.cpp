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
#include "ores.marketdata.core/service/series_shape_writer.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/series_axis.hpp"
#include "ores.marketdata.api/domain/series_axis_value.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/series_axis_repository.hpp"
#include "ores.marketdata.core/repository/series_axis_value_repository.hpp"
#include "ores.refdata.core/repository/tenor_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <map>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <variant>

namespace ores::marketdata::service {

using namespace ores::logging;

namespace {

inline std::string_view logger_name = "ores.marketdata.core.series_shape_writer";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

// The series URI, for a refusal to name the series it is about. A series the
// codec cannot write has no shape to declare and fails here.
[[nodiscard]] std::string uri_of(const datum::market_datum& series) {
    const auto uri = datum::oresmd_uri_codec::write(series);
    if (!uri)
        throw std::invalid_argument("series_shape_writer: series has no oresmd URI: " +
                                    uri.error());
    return *uri;
}

// The distinct texts @p point holds for @p f, in the order it presents them. A
// point that does not hold the field, or holds none, contributes nothing.
void collect(const datum::market_datum& point, datum::field f, std::vector<std::string>& values) {
    if (!point.holds(f))
        return;
    const auto& held = point.at(f);
    if (std::holds_alternative<datum::none_t>(held))
        return;
    auto text = datum::text_of(held);
    if (std::find(values.begin(), values.end(), text) == values.end())
        values.push_back(std::move(text));
}

}

void series_shape_writer::declare(ores::database::context ctx,
                                  const datum::market_datum& series,
                                  const boost::uuids::uuid& series_id,
                                  const boost::uuids::uuid& party_id,
                                  const std::vector<datum::market_datum>& points,
                                  term_check check) {
    const auto& row = datum::schema_of(series.type());

    const std::string series_uri = uri_of(series);

    // A coordinate the type makes optional may be absent from every point the
    // source states, in which case the axis constrains nothing and is left out.
    // A required coordinate that no point carries is an error.
    std::vector<datum::field> axes;
    std::vector<std::vector<std::string>> axis_values;
    for (const auto& spec : row.fields) {
        if (spec.role != datum::field_role::coordinate)
            continue;
        std::vector<std::string> values;
        for (const auto& point : points)
            collect(point, spec.name, values);
        if (values.empty()) {
            if (spec.may_be_none)
                continue;
            throw std::invalid_argument("series_shape_writer: series '" + series_uri +
                                        "' has no value for axis '" +
                                        std::string(datum::name_of(spec.name)) + "'");
        }
        axes.push_back(spec.name);
        axis_values.push_back(std::move(values));
    }
    if (axes.empty())
        return;

    // A term value is checked against the reference tenor table before any row
    // is written, and only when the caller asks for the check. The table is
    // read once, whatever the number of values.
    if (check == term_check::against_reference_tenors) {
        bool any_term = false;
        for (const auto axis : axes)
            any_term = any_term || datum::kind_of(axis) == datum::value_kind::term;
        if (any_term) {
            ores::refdata::repository::tenor_repository tenor_repo;
            std::unordered_set<std::string> tenor_codes;
            for (const auto& t : tenor_repo.read_latest(ctx))
                tenor_codes.insert(t.code);
            for (std::size_t i = 0; i < axes.size(); ++i) {
                if (datum::kind_of(axes[i]) != datum::value_kind::term)
                    continue;
                for (const auto& value : axis_values[i])
                    if (!tenor_codes.contains(value))
                        throw std::invalid_argument(
                            "series_shape_writer: value '" + value + "' of axis '" +
                            std::string(datum::name_of(axes[i])) +
                            "' is not a reference tenor code, for series '" + series_uri + "'");
            }
        }
    }

    // The values the series already declares. A second source for the same
    // series appends to that vocabulary rather than renumbering it, so the order
    // the shape stores stays the order the first source gave, with each new value
    // after it.
    const std::vector<std::string> series_ids{boost::uuids::to_string(series_id)};
    std::map<std::string, std::unordered_set<std::string>> declared;
    std::map<std::string, int> next_sequence;
    for (const auto& v :
         repository::series_axis_value_repository{}.read_latest_for_series(ctx, series_ids)) {
        declared[v.axis_field].insert(v.value);
        next_sequence[v.axis_field] = std::max(next_sequence[v.axis_field], v.sequence + 1);
    }

    std::vector<domain::series_axis> axis_rows;
    axis_rows.reserve(axes.size());
    std::vector<domain::series_axis_value> value_rows;
    for (std::size_t i = 0; i < axes.size(); ++i) {
        const auto field_name = std::string(datum::name_of(axes[i]));
        domain::series_axis r;
        r.tenant_id = ctx.tenant_id();
        r.series_id = series_id;
        r.axis_field = field_name;
        r.party_id = party_id;
        r.sequence = static_cast<int>(i);
        axis_rows.push_back(std::move(r));

        for (const auto& value : axis_values[i]) {
            if (declared[field_name].contains(value))
                continue;
            domain::series_axis_value v;
            v.tenant_id = ctx.tenant_id();
            v.series_id = series_id;
            v.axis_field = field_name;
            v.value = value;
            v.party_id = party_id;
            v.sequence = next_sequence[field_name]++;
            value_rows.push_back(std::move(v));
        }
    }

    repository::series_axis_repository axis_repo;
    axis_repo.write(ctx, axis_rows);
    if (!value_rows.empty()) {
        repository::series_axis_value_repository value_repo;
        value_repo.write(ctx, value_rows);
    }

    BOOST_LOG_SEV(lg(), info) << "Declared shape of series " << series_uri << ": "
                              << axis_rows.size() << " axes, " << value_rows.size()
                              << " new values";
}

}
