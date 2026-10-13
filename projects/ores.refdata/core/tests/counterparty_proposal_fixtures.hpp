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
#ifndef ORES_REFDATA_CORE_TESTS_COUNTERPARTY_PROPOSAL_FIXTURES_HPP
#define ORES_REFDATA_CORE_TESTS_COUNTERPARTY_PROPOSAL_FIXTURES_HPP

#include "book_proposal_fixtures.hpp"
#include "ores.refdata.api/generators/counterparty_change_generator.hpp"
#include "ores.refdata.api/generators/counterparty_identifier_change_generator.hpp"
#include "ores.refdata.api/generators/party_counterparty_change_generator.hpp"
#include "ores.refdata.core/repository/counterparty_repository.hpp"
#include "ores.refdata.core/repository/party_counterparty_repository.hpp"
#include "ores.refdata.core/service/counterparty_service.hpp"
#include "ores.refdata.service/service/counterparty_proposal.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <string>

// What the cases of the counterparty proposal and its gate share: lines of the
// three kinds, and live rows written under the approval gate.

namespace ores::refdata::tests {

using ores::refdata::service::counterparty_lines;
using ores::utility::domain::precondition;
using ores::utility::domain::precondition_kind;

inline boost::uuids::uuid fresh() {
    static boost::uuids::random_generator gen;
    return gen();
}

inline domain::counterparty_change new_counterparty_line(ores::testing::scoped_database_helper& h) {
    auto gctx = ores::testing::make_generation_context(h);
    auto line = ores::refdata::generators::generate_synthetic_counterparty_change(gctx);
    line.entity_id = fresh();
    line.parent_counterparty_id = std::nullopt;
    line.image_id = std::nullopt;
    line.operation = "put";
    line.base_version = 0;
    line.change_reason_code = "system.test";
    return line;
}

inline domain::counterparty_identifier_change
new_identifier_line(ores::testing::scoped_database_helper& h, const boost::uuids::uuid& owner) {
    auto gctx = ores::testing::make_generation_context(h);
    auto line = ores::refdata::generators::generate_synthetic_counterparty_identifier_change(gctx);
    line.entity_id = fresh();
    line.counterparty_id = owner;
    line.id_scheme = "LEI";
    line.operation = "put";
    line.base_version = 0;
    line.change_reason_code = "system.test";
    return line;
}

inline domain::party_counterparty_change new_link_line(ores::testing::scoped_database_helper& h,
                                                       const boost::uuids::uuid& party,
                                                       const boost::uuids::uuid& counterparty) {
    auto gctx = ores::testing::make_generation_context(h);
    auto line = ores::refdata::generators::generate_synthetic_party_counterparty_change(gctx);
    line.party_id = party;
    line.counterparty_id = counterparty;
    line.operation = "put";
    line.base_version = 0;
    line.change_reason_code = "system.test";
    return line;
}

inline precondition creates() {
    return {precondition_kind::must_not_exist, std::nullopt};
}

/**
 * Writes a counterparty to the live table through the repository.
 *
 * The approval gate sits in the service, so a fixture that arranges a live row
 * goes under it, to a store the gate does not guard.
 */
inline void write_live(const world& w, domain::counterparty c, const precondition& claim) {
    ores::service::messaging::stamp(c, w.ctx);
    ores::refdata::repository::counterparty_repository{}.write(w.ctx, c, claim);
}

/** Writes the line's counterparty to the live table, as an unapproved write would. */
inline domain::counterparty make_live(const world& w, const domain::counterparty_change& line) {
    domain::counterparty c;
    c.id = line.entity_id;
    c.short_code = line.short_code;
    c.full_name = line.full_name;
    c.transliterated_name = line.transliterated_name;
    c.party_type = line.party_type;
    c.parent_counterparty_id = line.parent_counterparty_id;
    c.status = line.status;
    c.image_id = line.image_id;
    c.change_reason_code = "system.test";
    write_live(w, c, creates());
    return *ores::refdata::service::counterparty_service(w.ctx).get_counterparty(c.id);
}

/** Renames a live counterparty as another caller would, from the version it holds. */
inline void rename_live(const world& w, const boost::uuids::uuid& id, const std::string& name) {
    auto c = *ores::refdata::service::counterparty_service(w.ctx).get_counterparty(id);
    const auto claim =
        precondition{precondition_kind::must_match_version, static_cast<std::uint32_t>(c.version)};
    c.full_name = name;
    c.change_reason_code = "system.test";
    write_live(w, c, claim);
}

/** Links a party to a counterparty in the live table, under the gate. */
inline void
link_live(const world& w, const boost::uuids::uuid& party, const boost::uuids::uuid& counterparty) {
    domain::party_counterparty l;
    l.party_id = party;
    l.counterparty_id = counterparty;
    l.change_reason_code = "system.test";
    ores::service::messaging::stamp(l, w.ctx);
    ores::refdata::repository::party_counterparty_repository{w.ctx}.write(l, creates());
}

/** The line that rewrites a live counterparty with another full name. */
inline domain::counterparty_change rename(const domain::counterparty& live,
                                          const std::string& name) {
    domain::counterparty_change line;
    line.entity_id = live.id;
    line.short_code = live.short_code;
    line.full_name = name;
    line.transliterated_name = live.transliterated_name;
    line.party_type = live.party_type;
    line.parent_counterparty_id = live.parent_counterparty_id;
    line.status = live.status;
    line.image_id = live.image_id;
    line.operation = "put";
    line.base_version = live.version;
    line.change_reason_code = "system.test";
    return line;
}

inline counterparty_lines whole_request(ores::testing::scoped_database_helper& h, const world& w) {
    counterparty_lines lines;
    lines.counterparties.push_back(new_counterparty_line(h));
    const auto owner = lines.counterparties.front().entity_id;
    lines.identifiers.push_back(new_identifier_line(h, owner));
    lines.links.push_back(new_link_line(h, w.party_id, owner));
    return lines;
}

inline bool exists(const world& w, const boost::uuids::uuid& id) {
    return ores::refdata::service::counterparty_service(w.ctx).get_counterparty(id).has_value();
}

}

#endif
