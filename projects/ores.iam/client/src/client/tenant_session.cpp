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
#include "ores.iam.client/client/tenant_session.hpp"
#include <algorithm>
#include <utility>

namespace ores::iam::client {

tenant_session::tenant_session(std::string party_id, std::chrono::seconds refresh_margin)
    : party_id_(std::move(party_id))
    , margin_(refresh_margin) {}

void tenant_session::entered(std::string token,
                             int lifetime_s,
                             std::chrono::system_clock::time_point now) {
    token_ = std::move(token);
    const auto lifetime = std::chrono::seconds(lifetime_s);
    // Half the lifetime is the most a margin may be, so a short session is
    // still renewed before it runs out.
    margin_ = std::min(margin_, lifetime / 2);
    expires_at_ = now + lifetime;
}

bool tenant_session::needs_entry(std::chrono::system_clock::time_point now) const {
    return token_.empty() || now >= expires_at_ - margin_;
}

}
