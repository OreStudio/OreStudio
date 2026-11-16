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
#include "ores.ore.core/domain/script_library_projection.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <pugixml.hpp>
#include <string>
#include <vector>

namespace {

using ores::ore::domain::script_library_rows;

/// One variant of a script, as a document states it, for comparison.
struct variant_view {
    std::string code;
    std::string npv;
    std::vector<std::string> results;
    std::vector<std::string> calibrations;
    std::vector<std::string> coarsenings;
    friend bool operator==(const variant_view&, const variant_view&) = default;
};

std::map<std::string, std::map<std::string, variant_view>> view_of(const pugi::xml_node library) {
    std::map<std::string, std::map<std::string, variant_view>> out;
    for (const auto script : library.children("Script")) {
        auto& variants = out[script.child_value("Name")];
        variants["tag"].npv = script.child_value("ProductTag");
        for (const auto variant : script.children("Script")) {
            const std::string purpose = variant.attribute("purpose").as_string("default");
            auto& v = variants[purpose];
            v.code = variant.child("Code").text().as_string();
            v.npv = variant.child_value("NPV");
            for (const auto result : variant.child("Results").children("Result"))
                v.results.push_back(std::string(result.text().as_string()) + "|" +
                                    result.attribute("rename").as_string());
            for (const auto calibration :
                 variant.child("CalibrationSpec").children("Calibration")) {
                std::string text = calibration.child_value("Index");
                for (const auto strike : calibration.child("Strikes").children("Strike"))
                    text += "|" + std::string(strike.text().as_string());
                v.calibrations.push_back(text);
            }
            for (const auto schedule :
                 variant.child("ScheduleCoarsening").children("EligibleSchedule"))
                v.coarsenings.push_back(schedule.text().as_string());
        }
    }
    return out;
}

/// The rows a library document states, as the seed holds them.
script_library_rows rows_of(const pugi::xml_node library) {
    script_library_rows rows;
    for (const auto script : library.children("Script")) {
        ores::trading::domain::script s;
        s.name = script.child_value("Name");
        if (const std::string tag = script.child_value("ProductTag"); !tag.empty())
            s.product_tag = tag;
        rows.scripts.push_back(s);
        for (const auto variant : script.children("Script")) {
            ores::trading::domain::script_variant v;
            v.script_name = s.name;
            v.purpose = variant.attribute("purpose").as_string("default");
            v.code = variant.child("Code").text().as_string();
            v.npv_variable = variant.child_value("NPV");
            rows.variants.push_back(v);
            int sequence = 0;
            for (const auto result : variant.child("Results").children("Result")) {
                ores::trading::domain::script_result r;
                r.script_name = s.name;
                r.purpose = v.purpose;
                r.sequence_number = ++sequence;
                r.variable = result.text().as_string();
                if (const auto rename = result.attribute("rename"))
                    r.rename = rename.as_string();
                rows.results.push_back(r);
            }
            int calibration_number = 0;
            for (const auto calibration :
                 variant.child("CalibrationSpec").children("Calibration")) {
                ores::trading::domain::script_calibration c;
                c.script_name = s.name;
                c.purpose = v.purpose;
                c.sequence_number = ++calibration_number;
                c.index_variable = calibration.child_value("Index");
                rows.calibrations.push_back(c);
                int strike_number = 0;
                for (const auto strike : calibration.child("Strikes").children("Strike")) {
                    ores::trading::domain::script_calibration_strike k;
                    k.script_name = s.name;
                    k.purpose = v.purpose;
                    k.calibration_number = c.sequence_number;
                    k.sequence_number = ++strike_number;
                    k.strike_variable = strike.text().as_string();
                    rows.calibration_strikes.push_back(k);
                }
            }
            sequence = 0;
            for (const auto schedule :
                 variant.child("ScheduleCoarsening").children("EligibleSchedule")) {
                ores::trading::domain::script_coarsening o;
                o.script_name = s.name;
                o.purpose = v.purpose;
                o.sequence_number = ++sequence;
                o.eligible_schedule = schedule.text().as_string();
                rows.coarsenings.push_back(o);
            }
        }
    }
    return rows;
}

}

TEST_CASE("the script library written from its rows states every script ORE ships",
          "[ore][script_library]") {
    const auto path = ores::testing::project_root::resolve(
        "external/ore/examples/Products/Input/scriptlibrary.xml");
    pugi::xml_document original;
    REQUIRE(original.load_file(path.c_str()));
    const auto library = original.child("ScriptLibrary");

    const auto written = ores::ore::domain::write_script_library(rows_of(library));
    pugi::xml_document reread;
    REQUIRE(reread.load_string(written.c_str()));

    const auto expected = view_of(library);
    const auto actual = view_of(reread.child("ScriptLibrary"));
    CHECK(expected.size() == 54u);
    CHECK(actual == expected);
}

TEST_CASE("a default variant states no purpose and another states its own",
          "[ore][script_library]") {
    script_library_rows rows;
    rows.scripts.push_back({.name = "Example"});
    rows.variants.push_back(
        {.script_name = "Example", .purpose = "FD", .code = "B = 1;", .npv_variable = "B"});
    rows.variants.push_back(
        {.script_name = "Example", .purpose = "default", .code = "A = 1;", .npv_variable = "A"});

    pugi::xml_document document;
    REQUIRE(document.load_string(ores::ore::domain::write_script_library(rows).c_str()));
    const auto script = document.child("ScriptLibrary").child("Script");
    const auto first = script.child("Script");
    CHECK_FALSE(first.attribute("purpose"));
    CHECK(std::string(first.child_value("NPV")) == "A");
    CHECK(std::string(first.next_sibling("Script").attribute("purpose").as_string()) == "FD");
}
