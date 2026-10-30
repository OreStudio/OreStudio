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
#ifndef ORES_REFDATA_CORE_TESTS_TEST_HELPERS_HPP_
#define ORES_REFDATA_CORE_TESTS_TEST_HELPERS_HPP_

#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/publish_helper.hpp"

using ores::refdata::repository::party_repository;

inline boost::uuids::uuid find_system_party_id(party_repository& repo,
                                               ores::database::context ctx,
                                               const ores::utility::uuid::tenant_id& tid) {
    auto parties = repo.read_latest(ctx);
    for (const auto& p : parties)
        if (p.tenant_id == tid && p.party_category == "System")
            return p.id;
    throw std::runtime_error("No system party for tenant: " + tid.to_string());
}

/**
 * Publishes the ISO currency codes.
 *
 * A curve configuration states its currency, and the guard on the curve tables
 * checks the code against the currency catalogue. The database build publishes
 * nothing, so a case that writes a curve publishes the codes it writes first.
 */
inline void publish_currencies(const ores::database::context& ctx) {
    if (!ores::testing::publish_refdata_dataset(
            ctx, ctx.tenant_id().to_string(), "iso.currencies", "currencies"))
        throw std::runtime_error("The tree carries no iso.currencies dataset to publish.");
}

// An import belongs to a party, so each case writes one and acts for it.
inline ores::database::context party_context(ores::testing::scoped_database_helper& h) {
    auto gen = ores::testing::make_generation_context(h);
    ores::refdata::repository::party_repository repo;
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& e : repo.read_latest(h.context()))
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    repo.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

#endif
