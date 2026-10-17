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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][binding]");

namespace fs = std::filesystem;
using namespace ores::logging;
using namespace ores::ore::xml;

// A hand-written table for CalendarAdjustments, which has every shape the
// engine needs: three levels of nesting, a required attribute, two optional
// children, and a repeated leaf two levels down. The generator replaces this
// in the next task; the engine it exercises is the point here.

struct calendar_dates {
    std::uint16_t present = 0;
    std::vector<std::string> dates;
};

struct calendar_entry {
    std::uint16_t present = 0;
    std::string name;
    std::string base_calendar;
    calendar_dates additional_holidays;
    calendar_dates additional_business_days;
};

struct calendar_adjustments {
    std::uint16_t present = 0;
    std::vector<calendar_entry> calendars;
};

constexpr auto hash_name(std::string_view name) -> std::uint32_t {
    std::uint32_t h = 2166136261u;
    for (const char c : name) {
        h ^= static_cast<std::uint8_t>(c);
        h *= 16777619u;
    }
    return h;
}

template <typename T>
auto append_to(void* container, void** slot) -> void {
    auto* vector = static_cast<std::vector<T>*>(container);
    vector->emplace_back();
    *slot = &vector->back();
}

const field calendar_dates_fields[] = {
    {"Date",
     hash_name("Date"),
     field_kind::repeated_text,
     offsetof(calendar_dates, dates),
     0,
     false,
     false,
     nullptr,
     nullptr,
     nullptr},
};
const record_type calendar_dates_type{"Dates", calendar_dates_fields, 1};

const field calendar_entry_fields[] = {
    {"name",
     hash_name("name"),
     field_kind::text,
     offsetof(calendar_entry, name),
     0,
     true,
     true,
     nullptr,
     nullptr,
     nullptr},
    {"BaseCalendar",
     hash_name("BaseCalendar"),
     field_kind::text,
     offsetof(calendar_entry, base_calendar),
     1,
     false,
     false,
     nullptr,
     nullptr,
     nullptr},
    {"AdditionalHolidays",
     hash_name("AdditionalHolidays"),
     field_kind::nested,
     offsetof(calendar_entry, additional_holidays),
     2,
     false,
     false,
     nullptr,
     &calendar_dates_type,
     nullptr},
    {"AdditionalBusinessDays",
     hash_name("AdditionalBusinessDays"),
     field_kind::nested,
     offsetof(calendar_entry, additional_business_days),
     3,
     false,
     false,
     nullptr,
     &calendar_dates_type,
     nullptr},
};
const record_type calendar_entry_type{"Calendar", calendar_entry_fields, 4};

const field calendar_adjustments_fields[] = {
    {"Calendar",
     hash_name("Calendar"),
     field_kind::repeated_nested,
     offsetof(calendar_adjustments, calendars),
     0,
     false,
     false,
     nullptr,
     &calendar_entry_type,
     &append_to<calendar_entry>},
};
const record_type calendar_adjustments_type{"CalendarAdjustments", calendar_adjustments_fields, 1};

auto calendar_adjustments_document() -> const document_type& {
    static const document_type doc{"CalendarAdjustments", &calendar_adjustments_type};
    return doc;
}

auto read_file(const fs::path& path) -> std::string {
    std::ifstream file(path, std::ios::binary);
    return std::string{std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>()};
}

}

TEST_CASE("a_nested_document_binds_into_one_record", tags) {
    auto lg(make_logger(test_suite));

    const std::string text =
        "<CalendarAdjustments>"
        "<Calendar name=\"TARGET\">"
        "<BaseCalendar>WeekendsOnly</BaseCalendar>"
        "<AdditionalHolidays><Date>2024-01-01</Date><Date>2024-12-25</Date></AdditionalHolidays>"
        "</Calendar>"
        "<Calendar name=\"Empty\"/>"
        "</CalendarAdjustments>";

    calendar_adjustments record;
    bind_error error;
    const bool bound = binder::bind(calendar_adjustments_document(), text, &record, error);

    INFO(error.message);
    REQUIRE(bound);
    REQUIRE(record.calendars.size() == 2);
    CHECK(record.calendars[0].name == "TARGET");
    CHECK(record.calendars[0].base_calendar == "WeekendsOnly");
    CHECK(record.calendars[0].additional_holidays.dates.size() == 2);
    CHECK(record.calendars[0].additional_holidays.dates[0] == "2024-01-01");
    CHECK(record.calendars[0].additional_business_days.dates.empty());

    // A child that is absent stays absent, and one that arrives sets its bit.
    CHECK((record.calendars[1].present & 0x2u) == 0u);
    CHECK((record.calendars[0].present & 0x2u) != 0u);
}

TEST_CASE("a_missing_required_attribute_is_refused", tags) {
    auto lg(make_logger(test_suite));

    calendar_adjustments record;
    bind_error error;
    const bool bound = binder::bind(calendar_adjustments_document(),
                                    "<CalendarAdjustments><Calendar/></CalendarAdjustments>",
                                    &record,
                                    error);

    CHECK_FALSE(bound);
    CHECK(error.message.find("name") != std::string::npos);
}

TEST_CASE("an_unknown_element_is_refused_rather_than_dropped", tags) {
    auto lg(make_logger(test_suite));

    calendar_adjustments record;
    bind_error error;
    const bool bound = binder::bind(calendar_adjustments_document(),
                                    "<CalendarAdjustments><Holiday/></CalendarAdjustments>",
                                    &record,
                                    error);

    CHECK_FALSE(bound);
    CHECK(error.message.find("Holiday") != std::string::npos);
}

TEST_CASE("every_calendar_adjustment_in_the_corpus_binds", tags) {
    auto lg(make_logger(test_suite));

    const auto root = fs::path(ORES_SOURCE_DIR) / "external" / "ore";
    REQUIRE(fs::is_directory(root));

    std::size_t files = 0;
    std::size_t calendars = 0;
    std::string report;

    for (const auto& entry : fs::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().filename() != "calendaradjustment.xml")
            continue;
        ++files;
        calendar_adjustments record;
        bind_error error;
        if (!binder::bind(
                calendar_adjustments_document(), read_file(entry.path()), &record, error)) {
            report += entry.path().string() + ": " + error.message + "\\n";
            continue;
        }
        calendars += record.calendars.size();
    }

    INFO("files: " << files << ", calendars: " << calendars);
    INFO(report);
    CHECK(files > 0);
    CHECK(report.empty());
    CHECK(calendars > 40);
}
