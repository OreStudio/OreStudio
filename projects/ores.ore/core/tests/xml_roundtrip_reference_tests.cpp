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
#include "ores.ore.core/domain/calendar_adjustment_mapper.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/currency_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <filesystem>
#include <string>
#include <vector>

/**
 * @file xml_roundtrip_reference_tests.cpp
 * @brief The shared reference configuration kinds, through the entity mappers.
 *
 * Calendar adjustments round trip. Currencies and conventions do not, and the
 * reasons are a lost field and an incomplete mapper rather than a harness
 * defect, so they are measured by the hidden probe below and recorded in
 * =evidence/round_trip_reference_kinds.md= instead of being asserted green.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][reference]");

using namespace ores::ore::domain;
namespace fs = std::filesystem;

fs::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

bool same_optional_text(const xsd::optional<calendar>& lhs, const xsd::optional<calendar>& rhs) {
    const bool left = static_cast<bool>(lhs);
    const bool right = static_cast<bool>(rhs);
    if (left != right)
        return false;
    return !left || std::string(*lhs) == std::string(*rhs);
}

/**
 * @brief Whether two optional date lists carry the same dates.
 *
 * ORE writes a date list it has no dates for as an empty element, and the
 * export omits it. An empty list is the same list either way, which is the one
 * normalisation this kind declares. A list with dates in it is compared date
 * by date, so the rule cannot hide a dropped date.
 */
bool same_dates(const xsd::optional<Dates>& lhs, const xsd::optional<Dates>& rhs) {
    const std::size_t left = lhs ? lhs->Date.size() : 0;
    const std::size_t right = rhs ? rhs->Date.size() : 0;
    if (left != right)
        return false;
    for (std::size_t i = 0; i < left; ++i) {
        if (std::string(lhs->Date[i]) != std::string(rhs->Date[i]))
            return false;
    }
    return true;
}

std::string calendar_difference(const calendaradjustment& original,
                                const calendaradjustment& exported,
                                const std::string& path) {
    if (original.Calendar.size() != exported.Calendar.size())
        return path + ": calendar count differs: original " +
               std::to_string(original.Calendar.size()) + ", exported " +
               std::to_string(exported.Calendar.size());

    for (std::size_t i = 0; i < original.Calendar.size(); ++i) {
        const auto& l = original.Calendar[i];
        const auto& r = exported.Calendar[i];
        const std::string name(l.name);
        if (name != std::string(r.name))
            return path + ": calendar " + std::to_string(i) + " name differs: original '" + name +
                   "', exported '" + std::string(r.name) + "'";
        if (!same_optional_text(l.BaseCalendar, r.BaseCalendar))
            return path + ": calendar '" + name + "' base calendar differs";
        if (!same_dates(l.AdditionalHolidays, r.AdditionalHolidays))
            return path + ": calendar '" + name + "' additional holidays differ";
        if (!same_dates(l.AdditionalBusinessDays, r.AdditionalBusinessDays))
            return path + ": calendar '" + name + "' additional business days differ";
    }
    return {};
}

ores::ore::xml::roundtrip_kind calendar_kind() {
    return ores::ore::xml::make_roundtrip_kind<
        calendaradjustment,
        std::vector<ores::refdata::messaging::calendar_adjustment>>(
        "calendar adjustments",
        "calendaradjustment",
        static_cast<std::vector<ores::refdata::messaging::calendar_adjustment> (*)(
            const calendaradjustment&)>(&calendar_adjustment_mapper::map),
        static_cast<calendaradjustment (*)(
            const std::vector<ores::refdata::messaging::calendar_adjustment>&)>(
            &calendar_adjustment_mapper::reverse),
        calendar_difference);
}

ores::ore::xml::roundtrip_kind currency_kind() {
    return ores::ore::xml::make_roundtrip_kind<currencyConfig,
                                               std::vector<ores::refdata::domain::currency>>(
        "currencies",
        "currencies",
        static_cast<std::vector<ores::refdata::domain::currency> (*)(const currencyConfig&)>(
            &currency_mapper::map),
        static_cast<currencyConfig (*)(const std::vector<ores::refdata::domain::currency>&)>(
            &currency_mapper::map),
        ores::ore::xml::parsed_text_difference<currencyConfig>);
}

ores::ore::xml::roundtrip_kind conventions_kind() {
    return ores::ore::xml::make_roundtrip_kind<conventions,
                                               ores::refdata::messaging::conventions_document>(
        "conventions",
        "conventions",
        &conventions_mapper::map,
        &conventions_mapper::reverse,
        ores::ore::xml::parsed_text_difference<conventions>);
}

}

TEST_CASE("reference_calendar_adjustments_round_trip", tags) {
    const auto walk = ores::ore::xml::walk_kind(calendar_kind(), corpus_root());

    for (const auto& failure : walk.failures)
        WARN(failure);

    CHECK(walk.files == 5);
    CHECK(walk.passed == walk.files);
    CHECK(walk.failures.empty());
}

TEST_CASE("reference_currencies_round_trip", tags) {
    const auto walk = ores::ore::xml::walk_kind(currency_kind(), corpus_root());

    for (const auto& failure : walk.failures)
        WARN(failure);

    CHECK(walk.files == 13);
    CHECK(walk.passed == walk.files);
    CHECK(walk.failures.empty());
}

// Hidden by default, and run on demand:
//   ores.ore.core.tests "[.measurement]"
// It measures the kind that does not round trip yet. Its reason is a mapper
// that skips categories rather than a comparison that needs stating, so it
// cannot be asserted green without hiding the loss, and it is recorded in the
// evidence file.
TEST_CASE("reference_kinds_measurement", "[.][measurement][reference]") {
    const auto conventions = ores::ore::xml::walk_kind(conventions_kind(), corpus_root());
    WARN("conventions files=" + std::to_string(conventions.files) +
         " passed=" + std::to_string(conventions.passed));
    for (const auto& failure : conventions.failures)
        WARN(failure);
    CHECK(conventions.files == 72);
}
