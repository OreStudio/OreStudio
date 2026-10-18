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
#include "conventions_identity.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_pay_receive_repository.hpp"
#include "ores.refdata.core/service/convention_service.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <limits>
#include <map>
#include <utility>

namespace ores::refdata::service {

using namespace ores::refdata::repository;

namespace {

boost::uuids::uuid new_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

// Closes every live row one part table holds for a convention.
template <typename Repository>
void close_live_rows(const ores::database::context& ctx,
                     Repository repo,
                     const std::string& convention_id) {
    const auto live = repo.read_latest_by_convention_id(
        ctx, convention_id, 0, std::numeric_limits<std::uint32_t>::max(), {}, std::nullopt);
    for (const auto& row : live)
        repo.remove(ctx, boost::uuids::to_string(row.id));
}

}

tenor_basis_swap_forms tenor_basis_swap_forms_of(
    const std::vector<domain::convention>& central,
    const std::vector<domain::tenor_basis_swap_pay_receive>& pay_receive,
    const std::vector<domain::tenor_basis_swap_long_short>& long_short) {
    tenor_basis_swap_forms forms;
    for (const auto& c : central)
        if (c.kind_code == tenor_basis_swap_kind)
            forms.central.push_back(c.id);
    for (const auto& p : pay_receive)
        forms.pay_receive.insert(boost::uuids::to_string(p.convention_id));
    for (const auto& l : long_short)
        forms.long_short.insert(boost::uuids::to_string(l.convention_id));
    return forms;
}

void resolve_tenor_basis_swap_identity(
    const ores::database::context& ctx,
    std::vector<domain::convention>& central,
    std::vector<domain::tenor_basis_swap_pay_receive>& pay_receive,
    std::vector<domain::tenor_basis_swap_long_short>& long_short) {
    convention_service conventions(ctx);
    std::map<std::string, boost::uuids::uuid> resolved;
    for (auto& c : central) {
        if (c.kind_code != tenor_basis_swap_kind)
            continue;
        const auto provisional = c.id;
        const auto existing = conventions.find_convention_by_code(c.party_id, c.kind_code, c.name);
        c.id = existing ? existing->id : new_uuid();
        resolved[boost::uuids::to_string(provisional)] = c.id;
    }
    if (resolved.empty())
        return;

    const auto relink = [&](auto& parts, auto repo) {
        for (auto& p : parts) {
            const auto it = resolved.find(boost::uuids::to_string(p.convention_id));
            if (it == resolved.end())
                continue;
            p.convention_id = it->second;
            const auto held =
                repo.read_latest_by_convention_id(ctx,
                                                  boost::uuids::to_string(it->second),
                                                  0,
                                                  std::numeric_limits<std::uint32_t>::max(),
                                                  {},
                                                  std::nullopt);
            p.id = held.empty() ? new_uuid() : held.front().id;
        }
    };
    relink(pay_receive, tenor_basis_swap_pay_receive_repository());
    relink(long_short, tenor_basis_swap_long_short_repository());
}

void close_omitted_leg_forms(const ores::database::context& ctx,
                             const tenor_basis_swap_forms& forms) {
    for (const auto& id : forms.central) {
        const auto key = boost::uuids::to_string(id);
        if (!forms.pay_receive.contains(key))
            close_live_rows(ctx, tenor_basis_swap_pay_receive_repository(), key);
        if (!forms.long_short.contains(key))
            close_live_rows(ctx, tenor_basis_swap_long_short_repository(), key);
    }
}

}
