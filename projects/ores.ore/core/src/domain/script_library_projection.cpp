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
#include <algorithm>
#include <pugixml.hpp>
#include <sstream>
#include <string_view>

namespace ores::ore::domain {

namespace {

constexpr std::string_view default_purpose = "default";

/// The rows of one variant, in their sequence.
template <typename Row, typename Key>
std::vector<const Row*>
of_variant(const std::vector<Row>& rows, const trading::domain::script_variant& variant, Key key) {
    std::vector<const Row*> out;
    for (const auto& row : rows)
        if (row.script_name == variant.script_name && row.purpose == variant.purpose)
            out.push_back(&row);
    std::ranges::sort(out, {}, key);
    return out;
}

void write_variant(pugi::xml_node script,
                   const trading::domain::script_variant& variant,
                   const script_library_rows& rows) {
    auto node = script.append_child("Script");
    if (variant.purpose != default_purpose)
        node.append_attribute("purpose") = variant.purpose.c_str();
    node.append_child("Code").append_child(pugi::node_cdata).set_value(variant.code.c_str());
    node.append_child("NPV").text() = variant.npv_variable.c_str();

    const auto results =
        of_variant(rows.results, variant, &trading::domain::script_result::sequence_number);
    if (!results.empty()) {
        auto list = node.append_child("Results");
        for (const auto* result : results) {
            auto item = list.append_child("Result");
            item.text() = result->variable.c_str();
            if (result->rename)
                item.append_attribute("rename") = result->rename->c_str();
        }
    }

    const auto calibrations = of_variant(
        rows.calibrations, variant, &trading::domain::script_calibration::sequence_number);
    if (!calibrations.empty()) {
        auto spec = node.append_child("CalibrationSpec");
        for (const auto* calibration : calibrations) {
            auto item = spec.append_child("Calibration");
            item.append_child("Index").text() = calibration->index_variable.c_str();
            auto strikes = item.append_child("Strikes");
            std::vector<const trading::domain::script_calibration_strike*> chosen;
            for (const auto& strike : rows.calibration_strikes)
                if (strike.script_name == variant.script_name &&
                    strike.purpose == variant.purpose &&
                    strike.calibration_number == calibration->sequence_number)
                    chosen.push_back(&strike);
            std::ranges::sort(
                chosen, {}, &trading::domain::script_calibration_strike::sequence_number);
            for (const auto* strike : chosen)
                strikes.append_child("Strike").text() = strike->strike_variable.c_str();
        }
    }

    const auto coarsenings =
        of_variant(rows.coarsenings, variant, &trading::domain::script_coarsening::sequence_number);
    if (!coarsenings.empty()) {
        auto coarsening = node.append_child("ScheduleCoarsening");
        for (const auto* row : coarsenings)
            coarsening.append_child("EligibleSchedule").text() = row->eligible_schedule.c_str();
    }
}

}

std::string write_script_library(const script_library_rows& rows) {
    pugi::xml_document document;
    auto declaration = document.append_child(pugi::node_declaration);
    declaration.append_attribute("version") = "1.0";
    auto library = document.append_child("ScriptLibrary");

    std::vector<const trading::domain::script*> scripts;
    for (const auto& script : rows.scripts)
        scripts.push_back(&script);
    std::ranges::sort(scripts, {}, &trading::domain::script::name);

    for (const auto* script : scripts) {
        auto node = library.append_child("Script");
        node.append_child("Name").text() = script->name.c_str();
        if (script->product_tag)
            node.append_child("ProductTag").text() = script->product_tag->c_str();
        std::vector<const trading::domain::script_variant*> variants;
        for (const auto& variant : rows.variants)
            if (variant.script_name == script->name)
                variants.push_back(&variant);
        std::ranges::sort(variants, [](const auto* a, const auto* b) {
            const bool a_default = a->purpose == default_purpose;
            const bool b_default = b->purpose == default_purpose;
            return a_default != b_default ? a_default : a->purpose < b->purpose;
        });
        for (const auto* variant : variants)
            write_variant(node, *variant, rows);
    }

    std::ostringstream out;
    document.save(out, "  ");
    return out.str();
}

}
