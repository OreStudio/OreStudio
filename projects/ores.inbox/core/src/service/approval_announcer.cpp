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
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/notification_center.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <exception>

namespace ores::inbox::service {

using namespace ores::logging;

approval_announcer::approval_announcer(ores::database::context ctx)
    : ctx_(std::move(ctx)) {}

void approval_announcer::tell_open_parts(approval_lifecycle& lifecycle,
                                         const domain::approval_kind& kind,
                                         const domain::approval_request& raised) {
    const auto id = boost::uuids::to_string(raised.id);
    const auto parts = lifecycle.parts_of(id);
    if (parts.empty()) {
        tell_deciders(kind, raised, kind.decide_permission_code);
        return;
    }
    for (const auto& open : lifecycle.open_parts_of(id))
        tell_deciders(kind, raised, open.decide_permission_code);
}

void approval_announcer::tell_deciders(const domain::approval_kind& kind,
                                       const domain::approval_request& raised,
                                       const std::string& permission_code) {
    try {
        notification_center center(ctx_);
        auto deciders = center.holders_of(permission_code);
        std::erase(deciders, boost::uuids::to_string(raised.requested_by));
        if (deciders.empty())
            return;
        messaging::raise_notification_request n{
            .kind_code = "inbox.approval_waiting",
            .link_route = "requests",
            .link_id = boost::uuids::to_string(raised.id),
            .arguments = {{.name = "kind", .value = kind.name},
                          {.name = "requester", .value = ctx_.actor()},
                          {.name = "reason", .value = raised.reason}},
            .account_ids = {},
            .audience_permission_code = permission_code};
        center.raise(n, deciders, raised.requested_by);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Request " << boost::uuids::to_string(raised.id)
                                  << " raised, but its deciders were not told: " << e.what();
    }
}

}
