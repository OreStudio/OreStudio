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
#include "ores.marketdata.core/repository/tick_entitlement.hpp"
#include "ores.marketdata.core/repository/subscription_repository.hpp"

namespace ores::marketdata::repository {

std::set<std::string> readable_providers(const ores::database::context& ctx) {
    const auto party = ctx.party_id();
    if (!party)
        return {};
    std::set<std::string> providers;
    for (const auto& s : subscription_repository{}.read_latest(ctx))
        if (s.party_id == *party && (s.display || s.non_display))
            providers.insert(s.provider_code);
    return providers;
}

bool may_read_ticks(const ores::database::context& ctx, const std::string& provider_code) {
    return readable_providers(ctx).contains(provider_code);
}

}
