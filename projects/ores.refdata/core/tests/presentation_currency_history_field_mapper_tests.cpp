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
#include "ores.refdata.core/presentation/currency_history_field_mapper.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[presentation][history_field_mapper]");

}

using ores::refdata::domain::currency;
using ores::refdata::presentation::render_currency_fields;

namespace {

std::string field(const std::vector<ores::diff::domain::field_value>& fields, const std::string& name) {
    const auto it = std::ranges::find_if(fields, [&](const auto& f) { return f.name == name; });
    return it == fields.end() ? std::string{"<missing>"} : it->value;
}

}

TEST_CASE("render_currency_fields_renders_every_column_exactly_once", tags) {
    currency c;
    c.iso_code = "USD";
    c.name = "US Dollar";

    const auto fields = render_currency_fields(c);

    // 21 columns in the domain struct as of this test: primary key,
    // natural keys, columns, 4 audit columns, recorded_at.
    CHECK(fields.size() == 21);
    // No duplicate field names.
    std::vector<std::string> names;
    for (const auto& f : fields)
        names.push_back(f.name);
    std::ranges::sort(names);
    CHECK(std::ranges::adjacent_find(names) == names.end());
}

TEST_CASE("render_currency_fields_renders_string_fields_directly", tags) {
    currency c;
    c.iso_code = "USD";
    c.name = "US Dollar";
    c.symbol = "$";

    const auto fields = render_currency_fields(c);

    CHECK(field(fields, "ISO Code") == "USD");
    CHECK(field(fields, "Name") == "US Dollar");
    CHECK(field(fields, "Symbol") == "$");
}

TEST_CASE("render_currency_fields_renders_int_fields_via_to_string", tags) {
    currency c;
    c.fractions_per_unit = 100;
    c.rounding_precision = 2;

    const auto fields = render_currency_fields(c);

    CHECK(field(fields, "Fractions Per Unit") == "100");
    CHECK(field(fields, "Rounding Precision") == "2");
}

TEST_CASE("render_currency_fields_renders_empty_optional_uuid_as_empty_string", tags) {
    currency c;
    c.image_id = std::nullopt;

    const auto fields = render_currency_fields(c);

    CHECK(field(fields, "Image ID").empty());
}

TEST_CASE("render_currency_fields_renders_populated_optional_uuid", tags) {
    currency c;
    c.image_id = boost::uuids::uuid{
        {0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10}};

    const auto fields = render_currency_fields(c);

    CHECK(field(fields, "Image ID") == "01020304-0506-0708-090a-0b0c0d0e0f10");
}

TEST_CASE("render_currency_fields_renders_recorded_at_as_iso8601_utc", tags) {
    currency c;
    c.recorded_at = std::chrono::system_clock::time_point{};

    const auto fields = render_currency_fields(c);

    CHECK(field(fields, "Recorded At") == "1970-01-01 00:00:00Z");
}
