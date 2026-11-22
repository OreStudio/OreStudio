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
#include "ores.ore.core/domain/pinned_script_library.hpp"
#include "ores.ore.core/domain/script_library_projection.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <format>
#include <map>
#include <optional>
#include <set>
#include <tuple>

namespace ores::ore::domain {

namespace {

using trading::domain::scripted_instrument;
using trading::messaging::script_pin;

/// A version of a script, ordered by name first so the library is written in name order.
using pin_key = std::tuple<std::string, boost::uuids::uuid, int>;

pin_key key_of(const script_pin& pin) {
    return {pin.name, pin.tenant_id, pin.version};
}

pin_key key_of(const utility::uuid::tenant_id& tenant, const std::string& name, int version) {
    return {name, tenant.to_uuid(), version};
}

std::string describe(const pin_key& key) {
    return std::format("version {} held by tenant {}",
                       std::get<2>(key),
                       boost::uuids::to_string(std::get<1>(key)));
}

/// The name a version is written under when the run holds its script at more than one version.
std::string qualified_name(const pin_key& key) {
    return std::format(
        "{}.{}.v{}", std::get<0>(key), boost::uuids::to_string(std::get<1>(key)), std::get<2>(key));
}

/// Whether a trade states its variables in the generic form, whose script name the run may set.
bool is_generic(const scripted_instrument& r) {
    return !r.data_element || *r.data_element == "ScriptedTradeData";
}

std::optional<pin_key> pin_of(const scripted_instrument& r) {
    if (!r.script_tenant_id || !r.script_version)
        return std::nullopt;
    return pin_key{r.script_name, *r.script_tenant_id, *r.script_version};
}

}

pinned_script_library pin_script_library(trading::domain::instrument_batch& batch) {
    pinned_script_library out;

    std::map<std::string, std::set<pin_key>> versions_of;
    std::map<std::string, std::map<pin_key, std::vector<std::string>>> element_trades_of;
    for (auto& r : batch.scripted_instruments) {
        const auto trade = boost::uuids::to_string(r.identity.trade_id);
        // A trade with its script inline runs that text. ORE reads a script
        // name before an inline script, so the name is not written for it.
        if (!r.script_body.empty()) {
            r.script_name.clear();
            continue;
        }
        if (r.script_name.empty())
            continue;
        const auto pin = pin_of(r);
        if (!pin) {
            out.refusals.push_back(std::format(
                "The trade {} runs the script {}, which no library held when the trade was "
                "booked, so the run has no version of it to give the engine.",
                trade,
                r.script_name));
            continue;
        }
        versions_of[r.script_name].insert(*pin);
        if (!is_generic(r))
            element_trades_of[r.script_name][*pin].push_back(trade);
    }

    for (const auto& [name, trades_by_pin] : element_trades_of) {
        if (trades_by_pin.size() < 2)
            continue;
        std::string stated;
        for (const auto& [pin, trades] : trades_by_pin)
            for (const auto& trade : trades)
                stated += std::format(
                    "{}the trade {} at {}", stated.empty() ? "" : "; ", trade, describe(pin));
        out.refusals.push_back(std::format(
            "The trades that state their terms on the {}Data element run {} at more than one "
            "version: {}. ORE names their script by that element, so one run cannot hold both.",
            name,
            name,
            stated));
    }

    std::map<pin_key, std::string> library_name_of;
    for (const auto& [name, versions] : versions_of) {
        std::optional<pin_key> plain;
        if (versions.size() == 1)
            plain = *versions.begin();
        else if (const auto element = element_trades_of.find(name);
                 element != element_trades_of.end() && element->second.size() == 1)
            plain = element->second.begin()->first;

        for (const auto& version : versions) {
            auto library_name = version == plain ? name : qualified_name(version);
            // A qualified name is the run's own, so a script that holds it already
            // would be overwritten in the library.
            if (library_name != name && versions_of.contains(library_name))
                out.refusals.push_back(std::format(
                    "The run would write {} of the script {} as {}, a name another script "
                    "of the run already has.",
                    describe(version),
                    name,
                    library_name));
            library_name_of[version] = library_name;
            out.entries.push_back({.pin = {.tenant_id = std::get<1>(version),
                                           .name = name,
                                           .version = std::get<2>(version)},
                                   .library_name = std::move(library_name)});
        }
    }

    for (auto& r : batch.scripted_instruments) {
        if (!is_generic(r))
            continue;
        if (const auto pin = pin_of(r); pin && library_name_of.contains(*pin))
            r.script_name = library_name_of.at(*pin);
    }
    return out;
}

std::vector<trading::messaging::script_pin> pins_of(const pinned_script_library& plan) {
    std::vector<trading::messaging::script_pin> pins;
    pins.reserve(plan.entries.size());
    for (const auto& entry : plan.entries)
        pins.push_back(entry.pin);
    return pins;
}

std::string
write_pinned_script_library(const pinned_script_library& plan,
                            const trading::messaging::export_script_library_response& rows) {
    std::map<pin_key, std::string> library_name_of;
    for (const auto& entry : plan.entries)
        library_name_of[key_of(entry.pin)] = entry.library_name;

    script_library_rows library;
    const auto take = [&library_name_of](auto& into, const auto& from) {
        for (auto row : from) {
            const auto name =
                library_name_of.find(key_of(row.tenant_id, row.script_name, row.script_version));
            if (name == library_name_of.end())
                continue;
            row.script_name = name->second;
            into.push_back(std::move(row));
        }
    };
    for (auto script : rows.scripts) {
        const auto name =
            library_name_of.find(key_of(script.tenant_id, script.name, script.version));
        if (name == library_name_of.end())
            continue;
        script.name = name->second;
        library.scripts.push_back(std::move(script));
    }
    take(library.variants, rows.variants);
    take(library.results, rows.results);
    take(library.calibrations, rows.calibrations);
    take(library.calibration_strikes, rows.calibration_strikes);
    take(library.coarsenings, rows.coarsenings);
    return write_script_library(library);
}

}
