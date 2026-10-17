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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_REFDATA_CORE_SERVICE_CONVENTIONS_ROW_HELPERS_HPP
#define ORES_REFDATA_CORE_SERVICE_CONVENTIONS_ROW_HELPERS_HPP

#include "ores.database/domain/context.hpp"
#include <map>
#include <utility>
#include <vector>

namespace ores::refdata::service {

// One definition of how a convention family reaches its table. The generated
// dispatch and the document service both write rows through these, so the two
// paths cannot disagree about the claim.
template <typename Repository, typename Row>
void replace_party_rows(const ores::database::context& ctx,
                        Repository repo,
                        std::vector<Row> rows) {
    if (rows.empty())
        return;
    std::map<std::pair<std::string, boost::uuids::uuid>, int> held;
    for (const auto& r : repo.read_latest(ctx))
        held[{r.id, r.party_id}] = r.version;
    for (auto& r : rows) {
        const auto it = held.find({r.id, r.party_id});
        r.version = it == held.end() ? 0 : it->second;
    }
    repo.write(ctx, rows);
}

// A row's version comes from the live row with the same key, so an import
// replaces the row it names rather than colliding with it. The central row and
// its parts are addressed by the key the identity resolved to, which the
// resolver below sets, so the id alone is the key by the time a row is written.
template <typename Repository, typename Row>
void replace_rows_by_id(const ores::database::context& ctx,
                        Repository repo,
                        std::vector<Row> rows) {
    if (rows.empty())
        return;
    std::map<std::string, int> held;
    for (const auto& r : repo.read_latest(ctx))
        held[boost::uuids::to_string(r.id)] = r.version;
    for (auto& r : rows) {
        const auto it = held.find(boost::uuids::to_string(r.id));
        r.version = it == held.end() ? 0 : it->second;
    }
    repo.write(ctx, rows);
}

template <typename Repository, typename Row>
void add_missing_world_rows(const ores::database::context& ctx,
                            Repository repo,
                            const std::vector<Row>& rows,
                            std::vector<std::string>& kept) {
    std::set<std::string> held;
    for (const auto& r : repo.read_latest(ctx))
        held.insert(r.id);
    std::vector<Row> missing;
    for (const auto& r : rows) {
        if (held.contains(r.id))
            kept.push_back(r.id);
        else
            missing.push_back(r);
    }
    if (!missing.empty())
        repo.write(ctx, missing);
}

}

#endif
