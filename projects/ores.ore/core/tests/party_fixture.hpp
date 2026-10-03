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
#ifndef ORES_ORE_CORE_TESTS_PARTY_FIXTURE_HPP
#define ORES_ORE_CORE_TESTS_PARTY_FIXTURE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/uuid.hpp>
#include <stdexcept>

namespace ores::ore::tests {

/**
 * @brief Two operational parties of the test tenant, and a context for each.
 *
 * A configuration document belongs to a party, and row level security shows
 * a session only the rows of the parties it may see. Each context sees its own
 * party alone, so a row one writes is invisible to the other.
 */
struct two_parties {
    boost::uuids::uuid a;
    boost::uuids::uuid b;
    database::context a_context;
    database::context b_context;
};

inline two_parties make_two_parties(testing::scoped_database_helper& h) {
    auto& ctx = h.context();
    const auto tid = h.tenant_id();
    refdata::repository::party_repository repo;

    boost::uuids::uuid system_party{};
    for (const auto& p : repo.read_latest(ctx))
        if (p.tenant_id == tid && p.party_category == "System")
            system_party = p.id;
    if (system_party == boost::uuids::uuid{})
        throw std::runtime_error("the test tenant has no system party");

    auto gen_ctx = testing::make_generation_context(h);
    const auto make_party = [&] {
        auto party = refdata::generators::generate_synthetic_party(gen_ctx);
        party.party_category = "Operational";
        party.parent_party_id = system_party;
        party.change_reason_code = "system.test";
        repo.write(ctx, party);
        return party.id;
    };
    const auto a = make_party();
    const auto b = make_party();
    return {a, b, ctx.with_party(tid, a, {a}, ""), ctx.with_party(tid, b, {b}, "")};
}

}

#endif
