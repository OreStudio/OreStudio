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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_SCRIPTED_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_SCRIPTED_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Scripted instrument.
 *
 * Represents the AMC script-based product types ORE states:
 * ScriptedTrade with its inline or library script, Autocallable_01,
 * DoubleDigitalOption and PerformanceOption_01. trade_type_code
 * discriminates the exact product and script_name names the payoff
 * script, while the optional JSON fields carry the script's events, its
 * underlyings and its parameters. The table stays flat by design: it holds
 * no nested sub-struct, because the script definition is embedded text and
 * not a set of typed columns.
 *
 * The table is a flat instrument sub-type, so it binds
 * :profile: trading-instrument. Two table features justify that binding:
 * the table is tenant-scoped through tenant_id and the tenant isolation
 * policy, its insert
 * trigger stamps party_id from the session variable app.current_party_id
 * rather than taking it from the client. The profile also fixes the identity
 * and audit field groups, the batch read and the generator facet, and leaves
 * the table with no UI surface -- the per-instrument forms were hand-crafted
 * in the removed desktop client and consumed the generated messaging
 * protocol.
 */
struct scripted_instrument final {
    instrument_identity identity;

    /**
     * @brief Name of the ORE script that defines the payoff.
     *
     * Names a library script, or the named product a scripted trade carries.
     */
    std::string script_name;

    /**
     * @brief Optional embedded ORE script body. Empty when the trade names a library script
     * instead.
     */
    std::string script_body;

    /**
     * @brief Optional JSON array of event schedule entries. Empty when not applicable.
     */
    std::string events_json;

    /**
     * @brief Optional JSON array of underlying asset codes. Empty when not applicable.
     */
    std::string underlyings_json;

    /**
     * @brief Optional JSON object of script parameters. Empty when not applicable.
     */
    std::string parameters_json;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const scripted_instrument&, const scripted_instrument&) = default;
};

/**
 * @brief Dispatch-key identifier for scripted_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const scripted_instrument&) {
    return "ores.trading.scripted_instrument";
}

}

#endif
