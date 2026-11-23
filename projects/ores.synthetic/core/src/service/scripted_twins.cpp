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
#include "ores.synthetic.core/service/scripted_twins.hpp"
#include "ores.synthetic.core/service/trade_population_planner.hpp"
#include "ores.trading.api/builders/scripted_twins.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include <boost/uuid/name_generator_sha1.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <utility>

namespace ores::synthetic::service {

namespace {

namespace tb = ores::trading::builders;

/// The namespace the twins' name-based ids are drawn in.
boost::uuids::uuid native_namespace() {
    return boost::uuids::string_generator()("6f1e2c0a-4b8d-4e2a-9a31-5c7d8e9f0a1b");
}

/// Every scripted form of a native slot, a refused recipe stated as the plan's error.
std::vector<tb::scripted_form> forms_of(const domain::trade_population_slot& native) {
    try {
        return tb::scripted_forms_of(native.instruments, native.trade_type_code);
    } catch (const tb::scripted_twin_error& e) {
        throw trade_population_error(e.what());
    }
}

/// The slot of one scripted form of a native slot.
domain::trade_population_slot slot_of(const domain::trade_population_slot& native,
                                      tb::scripted_form form) {
    domain::trade_population_slot twin = native;
    if (form.twin) {
        twin.trade_id = twin_id_for(native.trade_id, native.trade_type_code);
        twin.internal_identifier = native.internal_identifier + "-scripted";
    } else {
        twin.trade_id = carried_id_for(native.trade_id, native.trade_type_code, form.script_name);
        twin.internal_identifier = native.internal_identifier + "-scripted-" + form.script_name;
    }
    twin.trade_type_code = "ScriptedTrade";
    twin.instruments = ores::trading::domain::instrument_batch{};
    ores::trading::domain::append_instrument(
        twin.instruments, ores::trading::domain::trade_instrument{std::move(form.data)});
    twin.bond_issue.reset();
    twin.structure_id.reset();
    twin.structure_role.clear();
    twin.structure_sequence = 0;
    return twin;
}

}

boost::uuids::uuid twin_id_for(const boost::uuids::uuid& native, std::string_view trade_type) {
    static const boost::uuids::name_generator_sha1 generate(native_namespace());
    return generate(boost::uuids::to_string(native) + "/" + std::string(trade_type) + "/scripted");
}

boost::uuids::uuid carried_id_for(const boost::uuids::uuid& native,
                                  std::string_view trade_type,
                                  std::string_view script) {
    static const boost::uuids::name_generator_sha1 generate(native_namespace());
    return generate(boost::uuids::to_string(native) + "/" + std::string(trade_type) + "/" +
                    std::string(script));
}

std::optional<domain::trade_population_slot>
scripted_twin_of(const domain::trade_population_slot& native) {
    for (auto& form : forms_of(native))
        if (form.twin)
            return slot_of(native, std::move(form));
    return std::nullopt;
}

std::vector<domain::trade_population_slot>
scripted_forms_of(const domain::trade_population_slot& native) {
    std::vector<domain::trade_population_slot> out;
    for (auto& form : forms_of(native))
        out.push_back(slot_of(native, std::move(form)));
    return out;
}

void add_scripted_twins(domain::trade_population_plan& plan) {
    const auto natives = plan.slots.size();
    for (std::size_t i = 0; i < natives; ++i)
        for (auto& form : scripted_forms_of(plan.slots[i]))
            plan.slots.push_back(std::move(form));
}

}
